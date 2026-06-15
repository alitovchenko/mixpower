#' Internal inference helpers
#' @noRd

# Two-sided Wald (normal-approximation) p-value for a single fixed-effect term.
#
# Robust to the object types returned by the supported backends:
#  * merMod (lme4): `coef(summary(fit))` is a numeric matrix with an
#    "Estimate" and "Std. Error" column. NOTE: `vcov(merMod)` is an S4
#    `dpoMatrix`, and `base::diag()` on it errors ("long vectors not
#    supported"); extracting from the coefficient table avoids that entirely.
#  * glmmTMB: `summary(fit)$coefficients` is a list with a `$cond` matrix.
#
# Returns NA_real_ when the term is absent or the SE is non-finite/non-positive,
# so mp_power() records it via its failure policy rather than erroring.
.mp_wald_p_value <- function(fit, term) {
  cf <- tryCatch(stats::coef(summary(fit)), error = function(e) NULL)
  if (is.list(cf) && !is.matrix(cf) && !is.null(cf$cond)) {
    cf <- cf$cond
  }
  if (is.null(cf) || !is.matrix(cf) || !term %in% rownames(cf)) {
    return(NA_real_)
  }
  if (!all(c("Estimate", "Std. Error") %in% colnames(cf))) {
    return(NA_real_)
  }
  est <- cf[term, "Estimate"]
  se <- cf[term, "Std. Error"]
  if (!is.finite(est) || !is.finite(se) || se <= 0) {
    return(NA_real_)
  }
  z <- est / se
  2 * stats::pnorm(abs(z), lower.tail = FALSE)
}
