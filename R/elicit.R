#' Elicit assumptions from standardized scientific quantities
#'
#' Build a coherent [mp_assumptions()] object from the quantities researchers
#' actually reason about --- intraclass correlations, a standardized effect, a
#' baseline event probability and an odds/risk ratio, and random-slope
#' variability --- instead of hand-picking raw regression coefficients and
#' standard deviations. `mp_elicit()` solves for an internally consistent
#' parameter set and prints the **implied model**, combining mlmpower-style
#' elicitation with mixpower's simulation and calibration tooling.
#'
#' For `family = "gaussian"` the total outcome variance is partitioned by the
#' per-group `icc` (each random-intercept variance is `icc * outcome_sd^2`, the
#' residual variance is the remainder), and the fixed effect is `d * outcome_sd`.
#' For `family = "binomial"` the intercept is `qlogis(baseline_prob)`, the effect
#' is `log(odds_ratio)` (a `risk_ratio` is converted given `baseline_prob`), and
#' each random-intercept SD is derived from the latent-scale `icc`
#' (`residual variance = pi^2 / 3`). A random slope, when requested, is set to
#' `slope` times the random-intercept SD.
#'
#' Predictor correlations, autocorrelation, and cluster-size distributions are
#' design choices (see [mp_design()]); missingness is [mp_missing()].
#'
#' @param family `"gaussian"` or `"binomial"`.
#' @param predictor Name of the focal predictor (default `"condition"`).
#' @param d Standardized effect (Cohen's d) for `"gaussian"`.
#' @param odds_ratio,risk_ratio Effect for `"binomial"` (give one).
#' @param baseline_prob Control-group event probability for `"binomial"`.
#' @param outcome_sd Total outcome SD for `"gaussian"` (default 1).
#' @param icc Intraclass correlation: a single value (applied to all `groups`)
#'   or a vector named by group. For `"gaussian"` the values must sum to `< 1`.
#' @param slope Random-slope SD as a fraction of the random-intercept SD: a
#'   single value or named by group (`NULL` for none).
#' @param cor Intercept-slope correlation: a single value or named by group.
#' @param groups Grouping-factor name(s) (default `"subject"`).
#' @param intercept Fixed intercept; defaults to 0 (gaussian) or
#'   `qlogis(baseline_prob)` (binomial).
#' @return An object of class `c("mp_elicitation", "mp_assumptions")` usable
#'   anywhere [mp_assumptions()] is; its `print()` shows the elicited inputs and
#'   the implied model.
#' @seealso [mp_assumptions()], [mp_d_to_beta()], [mp_icc_to_sd()].
#' @export
#' @examples
#' # Gaussian: medium effect, ICC = 0.1
#' mp_elicit("gaussian", d = 0.5, icc = 0.1)
#'
#' # Binomial: 20% baseline event rate, odds ratio 1.8, subject ICC 0.05
#' mp_elicit("binomial", baseline_prob = 0.2, odds_ratio = 1.8, icc = 0.05)
mp_elicit <- function(family = c("gaussian", "binomial"),
                      predictor = "condition",
                      d = NULL,
                      odds_ratio = NULL,
                      risk_ratio = NULL,
                      baseline_prob = NULL,
                      outcome_sd = 1,
                      icc = NULL,
                      slope = NULL,
                      cor = NULL,
                      groups = "subject",
                      intercept = NULL) {
  family <- match.arg(family)
  if (!is.character(groups) || length(groups) < 1L || anyNA(groups) || any(groups == "")) {
    .stop("`groups` must be a non-empty character vector of grouping-factor names.")
  }

  icc_v <- .mp_elicit_named(icc, groups, "icc", default = 0)
  if (any(icc_v < 0 | icc_v >= 1)) .stop("each `icc` must be in [0, 1).")
  slope_v <- .mp_elicit_named(slope, groups, "slope", default = NA_real_)
  cor_v <- .mp_elicit_named(cor, groups, "cor", default = 0)

  fixed <- list()
  random <- list()
  residual <- NULL

  if (family == "gaussian") {
    .mp_chk_num(outcome_sd, "outcome_sd", lo = 0)
    if (is.null(d)) .stop("`d` (standardized effect) is required for family = 'gaussian'.")
    .mp_chk_num(d, "d")
    if (sum(icc_v) >= 1) .stop("the sum of `icc` across groups must be < 1 for family = 'gaussian'.")
    fixed[["(Intercept)"]] <- `%||%`(intercept, 0)
    fixed[[predictor]] <- d * outcome_sd
    residual <- sqrt(1 - sum(icc_v)) * outcome_sd
    intercept_sd <- function(g) sqrt(icc_v[[g]]) * outcome_sd
  } else {
    if (is.null(baseline_prob)) .stop("`baseline_prob` is required for family = 'binomial'.")
    .mp_chk_num(baseline_prob, "baseline_prob", lo = 0, hi = 1, inclusive_hi = FALSE)
    if (baseline_prob <= 0) .stop("`baseline_prob` must be in (0, 1).")
    if (is.null(odds_ratio) && is.null(risk_ratio)) {
      .stop("Provide `odds_ratio` or `risk_ratio` for family = 'binomial'.")
    }
    or <- if (!is.null(odds_ratio)) {
      odds_ratio
    } else {
      .mp_chk_num(risk_ratio, "risk_ratio", lo = 0)
      if (risk_ratio * baseline_prob >= 1) .stop("`risk_ratio` * `baseline_prob` must be < 1.")
      risk_ratio * (1 - baseline_prob) / (1 - risk_ratio * baseline_prob)
    }
    .mp_chk_num(or, "odds_ratio", lo = 0)
    if (or <= 0) .stop("`odds_ratio` must be > 0.")
    fixed[["(Intercept)"]] <- `%||%`(intercept, stats::qlogis(baseline_prob))
    fixed[[predictor]] <- log(or)
    latent_sd <- pi / sqrt(3)
    intercept_sd <- function(g) if (icc_v[[g]] > 0) sqrt(icc_v[[g]] / (1 - icc_v[[g]])) * latent_sd else 0
  }

  for (g in groups) {
    isd <- intercept_sd(g)
    spec <- list(intercept_sd = isd)
    if (!is.na(slope_v[[g]]) && slope_v[[g]] > 0) {
      spec$slopes <- stats::setNames(list(slope_v[[g]] * isd), predictor)
      if (cor_v[[g]] != 0) spec$cor <- cor_v[[g]]
    }
    random[[g]] <- spec
  }

  asm <- mp_assumptions(fixed_effects = fixed, random_effects = random, residual_sd = residual)
  attr(asm, "elicited") <- list(
    family = family, predictor = predictor, d = d, odds_ratio = odds_ratio,
    risk_ratio = risk_ratio, baseline_prob = baseline_prob, outcome_sd = outcome_sd,
    icc = icc_v, slope = slope_v, cor = cor_v, groups = groups
  )
  class(asm) <- c("mp_elicitation", class(asm))
  asm
}

# Normalise a scalar or named vector/list of standardized inputs to a vector
# named by `groups`. A single value applies to all groups.
.mp_elicit_named <- function(x, groups, name, default) {
  out <- stats::setNames(rep(default, length(groups)), groups)
  if (is.null(x)) {
    return(out)
  }
  x <- unlist(x)
  if (is.null(names(x))) {
    if (length(x) == 1L) {
      out[seq_along(out)] <- x
      return(out)
    }
    .stop(sprintf("`%s` must be a single value or named by group.", name))
  }
  for (g in names(x)) {
    if (!g %in% groups) .stop(sprintf("`%s` names an unknown group '%s'.", name, g))
    out[[g]] <- x[[g]]
  }
  out
}

#' @export
print.mp_elicitation <- function(x, ...) {
  e <- attr(x, "elicited")
  cat(sprintf("<mp_elicitation> family = %s\n", e$family))
  cat("  elicited:\n")
  if (identical(e$family, "gaussian")) {
    cat(sprintf("    - d(%s) = %g; outcome_sd = %g\n", e$predictor, e$d, e$outcome_sd))
  } else {
    eff <- if (!is.null(e$odds_ratio)) sprintf("odds_ratio = %g", e$odds_ratio) else sprintf("risk_ratio = %g", e$risk_ratio)
    cat(sprintf("    - baseline_prob = %g; %s\n", e$baseline_prob, eff))
  }
  cat(sprintf("    - icc: %s\n", paste(sprintf("%s = %g", names(e$icc), e$icc), collapse = ", ")))
  cat("  implied model:\n")
  y <- x
  attr(y, "elicited") <- NULL
  class(y) <- "mp_assumptions"
  print(y)
  invisible(x)
}
