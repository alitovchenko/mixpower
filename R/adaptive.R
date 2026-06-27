#' Stopping rule for adaptive power simulation
#'
#' Specifies when [mp_power_adaptive()] should stop adding simulation batches.
#' Stopping is triggered (after at least `min_nsim` replicates) when *either*:
#'
#' * **precision**: the power confidence interval's half-width is `<=`
#'   `ci_halfwidth`, or
#' * **decision**: a `target` power is given and the interval lies entirely
#'   above or entirely below it (the accept/reject decision is unambiguous).
#'
#' Otherwise it keeps adding batches until `max_nsim` is reached.
#'
#' @param ci_halfwidth Target half-width of the power CI (e.g. `0.02`), or `NULL`
#'   to use only the decision rule.
#' @param target Optional target power (in (0, 1)); enables the decision rule.
#' @param min_nsim Minimum replicates before stopping is considered (default 200).
#' @param max_nsim Maximum replicates (hard cap, default 5000).
#' @param batch Replicates added per iteration (default 200).
#' @return An object of class `mp_stop`.
#' @seealso [mp_power_adaptive()].
#' @export
#' @examples
#' mp_stop(ci_halfwidth = 0.02, max_nsim = 4000)
#' mp_stop(target = 0.8, ci_halfwidth = 0.03)
mp_stop <- function(ci_halfwidth = 0.02, target = NULL, min_nsim = 200,
                    max_nsim = 5000, batch = 200) {
  if (!is.null(ci_halfwidth)) {
    if (!is.numeric(ci_halfwidth) || length(ci_halfwidth) != 1L || is.na(ci_halfwidth) ||
        ci_halfwidth <= 0 || ci_halfwidth >= 0.5) {
      .stop("`ci_halfwidth` must be a single number in (0, 0.5), or NULL.")
    }
  }
  if (!is.null(target)) {
    if (!is.numeric(target) || length(target) != 1L || is.na(target) ||
        target <= 0 || target >= 1) {
      .stop("`target` must be a single number in (0, 1), or NULL.")
    }
  }
  if (is.null(ci_halfwidth) && is.null(target)) {
    .stop("Specify at least one stopping criterion: `ci_halfwidth` and/or `target`.")
  }
  .assert_is_pos_int(min_nsim, "min_nsim")
  .assert_is_pos_int(max_nsim, "max_nsim")
  .assert_is_pos_int(batch, "batch")
  if (min_nsim > max_nsim) .stop("`min_nsim` must be <= `max_nsim`.")

  structure(
    list(ci_halfwidth = ci_halfwidth, target = target,
         min_nsim = as.integer(min_nsim), max_nsim = as.integer(max_nsim),
         batch = as.integer(batch)),
    class = "mp_stop"
  )
}

#' Adaptive (sequential) power simulation
#'
#' Runs [mp_power()] in batches and stops as soon as the result is precise enough
#' or the accept/reject decision is clear (see [mp_stop()]), instead of relying
#' on an arbitrary fixed `nsim`. The achieved Monte Carlo precision (CI
#' half-width) and the number of replicates actually used are reported, so you
#' simulate no more than necessary.
#'
#' Because replicate seeds are deterministic (`seed + i - 1`), the result for a
#' given stopping point is reproducible and identical to a fixed `mp_power()`
#' run at the same `nsim`.
#'
#' @param scenario An `mp_scenario`.
#' @param stop An [mp_stop()] specification.
#' @param alpha,seed,conf_level,ci_method,failure_policy As in [mp_power()].
#' @param check_calibration As in [mp_power()] (calibrate-first nudge); checked
#'   once at the start.
#' @param progress Emit a per-batch progress message (default `FALSE`).
#' @return An object of class `c("mp_power_adaptive", "mp_power")` — a standard
#'   `mp_power` result plus a `$stopping` list (`converged`, `reason`,
#'   `nsim_used`, `ci_halfwidth`, `target`, `rule`).
#' @seealso [mp_stop()], [mp_power()], [mp_power_checkpoint()].
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   d <- mp_design(list(subject = 30), trials_per_cell = 6)
#'   a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.5),
#'                       random_effects = list(subject = list(intercept_sd = 0.5)),
#'                       residual_sd = 1)
#'   scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
#'   mp_power_adaptive(scn, stop = mp_stop(ci_halfwidth = 0.05, batch = 50,
#'                                         min_nsim = 50, max_nsim = 400), seed = 1)
#' }
#' }
mp_power_adaptive <- function(scenario,
                              stop = mp_stop(),
                              alpha = 0.05,
                              seed = NULL,
                              conf_level = 0.95,
                              ci_method = c("clopper-pearson", "wald"),
                              failure_policy = c("count_as_nondetect", "exclude"),
                              check_calibration = TRUE,
                              progress = FALSE) {
  .assert_class(scenario, "mp_scenario", "scenario")
  if (!inherits(stop, "mp_stop")) {
    .stop("`stop` must be an `mp_stop` object (from mp_stop()).")
  }
  ci_method <- match.arg(ci_method)
  failure_policy <- match.arg(failure_policy)

  advice <- .mp_nudge_calibration(scenario, check_calibration)
  true_effect <- .mp_true_effect(scenario)

  half_width <- function(ci) if (anyNA(ci)) NA_real_ else (ci[[2]] - ci[[1]]) / 2

  sims <- NULL
  n_done <- 0L
  converged <- FALSE
  reason <- "max_nsim"

  repeat {
    this_n <- min(stop$batch, stop$max_nsim - n_done)
    if (this_n <= 0L) break
    seed_b <- if (is.null(seed)) NULL else as.integer(seed) + n_done
    batch <- mp_power(scenario, nsim = this_n, alpha = alpha, seed = seed_b,
                      conf_level = conf_level, ci_method = ci_method,
                      failure_policy = failure_policy, keep = "minimal",
                      aggregate = "full", check_calibration = FALSE)
    sims <- if (is.null(sims)) batch$sims else rbind(sims, batch$sims)
    n_done <- n_done + this_n

    agg <- .mp_aggregate_sims(sims, scenario, alpha, seed, failure_policy, "minimal",
                              conf_level, ci_method, "full", n_done, true_effect)
    hw <- half_width(agg$ci)
    if (isTRUE(progress)) {
      message(sprintf("adaptive: n=%d power=%.3f CI=[%.3f, %.3f] half-width=%.4f",
                      n_done, agg$power, agg$ci[[1]], agg$ci[[2]], hw))
    }

    if (n_done >= stop$min_nsim) {
      precision_met <- !is.null(stop$ci_halfwidth) && !is.na(hw) && hw <= stop$ci_halfwidth
      decision_met <- !is.null(stop$target) && !anyNA(agg$ci) &&
        (agg$ci[[1]] > stop$target || agg$ci[[2]] < stop$target)
      if (precision_met || decision_met) {
        converged <- TRUE
        reason <- if (precision_met && decision_met) {
          "precision+decision"
        } else if (precision_met) {
          "precision"
        } else {
          "decision"
        }
        break
      }
    }
    if (n_done >= stop$max_nsim) break
  }

  res <- .mp_aggregate_sims(sims, scenario, alpha, seed, failure_policy, "minimal",
                            conf_level, ci_method, "full", n_done, true_effect)
  res$calibration_advice <- advice
  res$stopping <- list(
    converged = converged,
    reason = reason,
    nsim_used = n_done,
    ci_halfwidth = half_width(res$ci),
    target = stop$target,
    rule = stop
  )
  class(res) <- c("mp_power_adaptive", class(res))
  res
}

#' @export
print.mp_power_adaptive <- function(x, ...) {
  s <- x$stopping
  cat("<mp_power_adaptive>\n")
  cat(sprintf("  power: %.1f%% (%g%% CI %.1f%%, %.1f%%), MCSE %.4f\n",
              100 * x$power, 100 * x$conf_level, 100 * x$ci[[1]], 100 * x$ci[[2]], x$mcse))
  status <- if (isTRUE(s$converged)) {
    sprintf("converged (%s)", s$reason)
  } else {
    sprintf("did NOT converge; hit max_nsim = %d", s$rule$max_nsim)
  }
  cat(sprintf("  nsim used: %d -- %s\n", s$nsim_used, status))
  ch <- sprintf("  achieved CI half-width: %.4f", s$ci_halfwidth)
  if (!is.null(s$rule$ci_halfwidth)) ch <- paste0(ch, sprintf(" (target <= %.4f)", s$rule$ci_halfwidth))
  cat(ch, "\n", sep = "")
  if (!is.null(s$target)) cat(sprintf("  decision target: %g\n", s$target))
  invisible(x)
}
