#' Compare analysis models on the same data: power *and* Type I error
#'
#' Simulates data once per replicate from the first scenario, then fits and
#' tests *every* supplied scenario on that same dataset. Because the models see
#' identical data, differences in their rejection rates isolate the effect of
#' the analysis choice. This is the analogue of powerlmm's `sim_formula`, raised
#' to a planning question: **which analysis plan controls Type I error AND has
#' acceptable power under your plausible data-generating process?**
#'
#' With `calibrate = TRUE` (default), each candidate model is evaluated twice on
#' shared data: once under the scenario's alternative (giving **power**) and once
#' under the null --- the focal effect set to zero in the data-generating
#' scenario --- giving its **Type I error rate** and a calibration verdict. The
#' recommended plan is the best-powered model that still controls Type I error.
#'
#' All scenarios must share the same data-generating process (design and
#' assumptions); they should differ only in their analysis model (formula /
#' random-effects structure / test). The first scenario's `simulate_fun` drives
#' data generation.
#'
#' @param scenarios A named list of `mp_scenario` objects.
#' @param nsim Positive integer number of simulations (per pass).
#' @param alpha Significance threshold (default 0.05).
#' @param seed Optional seed for reproducibility.
#' @param conf_level Confidence level for the per-model intervals.
#' @param failure_policy How to treat failed fits (see [mp_power()]).
#' @param calibrate Also estimate each model's Type I error under the null
#'   (default `TRUE`); doubles the simulation cost.
#' @return An object of class `mp_model_comparison` with a `results` data frame
#'   (one row per model: `power`, `conf_low`, `conf_high`; and, when
#'   `calibrate`, `type1`, `type1_low`, `type1_high`, `calibration`, `valid`;
#'   plus `failure_rate`, `n_effective`, `nsim`) and `$recommended`, the
#'   best-powered Type-I-valid model.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   d <- mp_design(list(subject = 30), trials_per_cell = 8)
#'   a <- mp_assumptions(
#'     fixed_effects = list("(Intercept)" = 0, condition = 0.5),
#'     random_effects = list(subject = list(intercept_sd = 0.5,
#'                                           slopes = list(condition = 0.8))),
#'     residual_sd = 1
#'   )
#'   maximal <- mp_scenario_lme4(y ~ condition + (1 + condition | subject), d, a)
#'   reduced <- mp_scenario_lme4(y ~ condition + (1 | subject), d, a)
#'   mp_compare_models(list(maximal = maximal, reduced = reduced),
#'                     nsim = 100, seed = 1)
#' }
#' }
mp_compare_models <- function(scenarios,
                              nsim,
                              alpha = 0.05,
                              seed = NULL,
                              conf_level = 0.95,
                              failure_policy = c("count_as_nondetect", "exclude"),
                              calibrate = TRUE) {
  failure_policy <- match.arg(failure_policy)
  if (inherits(scenarios, "mp_scenario") || !is.list(scenarios) ||
      length(scenarios) < 1L || is.null(names(scenarios)) ||
      any(names(scenarios) == "") ||
      !all(vapply(scenarios, inherits, logical(1), "mp_scenario"))) {
    .stop("`scenarios` must be a named list of mp_scenario objects.")
  }
  .assert_is_pos_int(nsim, "nsim")

  base <- scenarios[[1]]
  if (is.null(base$engine$simulate_fun)) {
    .stop("The first scenario must provide a `simulate_fun` (it generates the shared data).")
  }
  nm <- names(scenarios)

  alt <- .mp_compare_pass(scenarios, base, nsim, alpha, seed)
  null_pass <- NULL
  if (isTRUE(calibrate)) {
    null_base <- mp_sesoi(base, effect = 0, term = .mp_scenario_term(base))
    null_pass <- .mp_compare_pass(scenarios, null_base, nsim, alpha, seed)
  }

  rows <- lapply(seq_along(scenarios), function(j) {
    p <- .mp_compare_rate(alt$sig[, j], alt$failed[, j], nsim, conf_level, failure_policy)
    row <- data.frame(model = nm[[j]], power = p$rate,
                      conf_low = p$ci[[1]], conf_high = p$ci[[2]],
                      stringsAsFactors = FALSE)
    if (isTRUE(calibrate)) {
      t1 <- .mp_compare_rate(null_pass$sig[, j], null_pass$failed[, j], nsim, conf_level, failure_policy)
      verdict <- if (!anyNA(t1$ci) && t1$ci[[1]] > alpha) {
        "anti-conservative"
      } else if (!anyNA(t1$ci) && t1$ci[[2]] < alpha) {
        "conservative"
      } else {
        "well-calibrated"
      }
      row$type1 <- t1$rate
      row$type1_low <- t1$ci[[1]]
      row$type1_high <- t1$ci[[2]]
      row$calibration <- verdict
      row$valid <- !identical(verdict, "anti-conservative")
    }
    row$failure_rate <- p$nf / nsim
    row$n_effective <- p$n_eff
    row$nsim <- nsim
    row
  })
  results <- do.call(rbind, rows)

  recommended <- NA_character_
  if (isTRUE(calibrate)) {
    ok <- results[results$valid %in% TRUE & !is.na(results$power), , drop = FALSE]
    if (nrow(ok) > 0L) recommended <- ok$model[which.max(ok$power)]
  }

  out <- list(
    results = results,
    alpha = alpha,
    nsim = nsim,
    failure_policy = failure_policy,
    calibrate = calibrate,
    recommended = recommended
  )
  class(out) <- "mp_model_comparison"
  out
}

# One comparison pass: simulate from `base` once per replicate, fit + test each
# candidate scenario on that shared data. Returns per-replicate significance and
# failure matrices (rows = replicates, cols = models).
.mp_compare_pass <- function(scenarios, base, nsim, alpha, seed) {
  nm <- names(scenarios)
  rep_seeds <- .rep_seeds(seed, nsim)
  takes_seed <- "seed" %in% names(formals(base$engine$simulate_fun))
  sig <- matrix(FALSE, nrow = nsim, ncol = length(scenarios), dimnames = list(NULL, nm))
  failed <- matrix(FALSE, nrow = nsim, ncol = length(scenarios), dimnames = list(NULL, nm))

  for (i in seq_len(nsim)) {
    si <- rep_seeds[[i]]
    dat <- tryCatch(
      .with_seed(si, if (takes_seed) base$engine$simulate_fun(base, seed = si) else base$engine$simulate_fun(base)),
      error = function(e) NULL
    )
    if (is.null(dat) || !is.data.frame(dat)) {
      failed[i, ] <- TRUE
      next
    }
    for (j in seq_along(scenarios)) {
      s <- scenarios[[j]]
      p <- tryCatch(
        suppressWarnings(s$engine$test_fun(s$engine$fit_fun(dat, s), s)$p_value),
        error = function(e) NA_real_
      )
      if (is.na(p)) failed[i, j] <- TRUE else if (p < alpha) sig[i, j] <- TRUE
    }
  }
  list(sig = sig, failed = failed)
}

# Rejection rate + Clopper-Pearson CI for one model's column, honoring the
# failure policy.
.mp_compare_rate <- function(sig_col, failed_col, nsim, conf_level, failure_policy) {
  nf <- sum(failed_col)
  n_eff <- if (failure_policy == "exclude") nsim - nf else nsim
  x <- sum(sig_col)
  rate <- if (n_eff > 0L) x / n_eff else NA_real_
  list(rate = rate, ci = .mp_power_ci(x, n_eff, conf_level, "clopper-pearson"),
       n_eff = n_eff, nf = nf)
}

#' @export
print.mp_model_comparison <- function(x, ...) {
  cat("<mp_model_comparison>\n")
  cat(sprintf("  nsim: %d (per pass), alpha: %g\n", x$nsim, x$alpha))
  print(x$results, row.names = FALSE)
  if (isTRUE(x$calibrate)) {
    if (!is.na(x$recommended)) {
      cat(sprintf("  recommended plan (Type-I-valid, best power): %s\n", x$recommended))
    } else {
      cat("  ! No candidate model controls Type I error at this design; none recommended.\n")
    }
  }
  invisible(x)
}

#' @export
summary.mp_model_comparison <- function(object, ...) {
  object$results
}
