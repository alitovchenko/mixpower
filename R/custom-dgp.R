#' Plug a custom data-generating function into the mixpower pipeline
#'
#' Wraps a user-supplied data-generating function as a validated `simulate_fun`,
#' so any design the built-in grammar does not cover --- partially nested
#' designs, multi-arm factors, nonlinear time trends, correlated predictors, or
#' anything else --- can still flow through `mp_power()`, `mp_calibrate()`,
#' `mp_compare_models()`, and the reporting tools. The returned function is the
#' escape hatch that keeps the core engine lean while leaving the design grammar
#' open-ended.
#'
#' The user function receives the scenario (and optionally `seed`) and must
#' return a `data.frame` containing every variable in the scenario's model
#' formula (the response and predictors/grouping factors). To keep
#' [mp_calibrate()] / [mp_compare_models()] meaningful, read the focal effect
#' from `scenario$assumptions$fixed_effects` rather than hard-coding it, so the
#' null (effect = 0) can be generated when those tools set it.
#'
#' @param fun A function `function(scenario, seed = NULL)` returning a
#'   `data.frame`.
#' @param validate Validate the output each call (default `TRUE`): a data frame
#'   that contains all variables in `scenario$formula`.
#' @return A function suitable as a scenario `simulate_fun` (see [mp_scenario()]
#'   and the `simulate` argument of [mp_scenario_lme4()]).
#' @seealso [mp_scenario_lme4()], [mp_scenario()].
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   # A three-arm design (omnibus test) via a custom DGP.
#'   dgp <- function(scenario, seed = NULL) {
#'     n <- scenario$design$clusters$subject
#'     b <- scenario$assumptions$fixed_effects
#'     arm <- factor(rep(c("a", "b", "c"), length.out = n))
#'     mu <- c(a = 0, b = b$arm_b, c = b$arm_c)[as.character(arm)]
#'     data.frame(subject = factor(seq_len(n)), arm = arm,
#'                y = mu + stats::rnorm(n, sd = scenario$assumptions$residual_sd))
#'   }
#'   d <- mp_design(list(subject = 90), trials_per_cell = 1)
#'   a <- mp_assumptions(list("(Intercept)" = 0, arm_b = 0.4, arm_c = 0.7),
#'                       residual_sd = 1)
#'   scn <- mp_scenario_lme4(y ~ arm + (1 | subject), design = d, assumptions = a,
#'                           simulate = dgp, test_method = "lrt",
#'                           null_formula = y ~ 1 + (1 | subject))
#'   mp_power(scn, nsim = 20, seed = 1)
#' }
#' }
mp_custom_dgp <- function(fun, validate = TRUE) {
  if (!is.function(fun)) {
    .stop("`fun` must be a function(scenario, seed = NULL) returning a data.frame.")
  }
  takes_seed <- "seed" %in% names(formals(fun))
  force(validate)

  function(scenario, seed = NULL) {
    dat <- if (takes_seed) fun(scenario, seed = seed) else fun(scenario)
    if (isTRUE(validate)) {
      if (!is.data.frame(dat)) {
        .stop("Custom data-generating function must return a data.frame.")
      }
      req <- tryCatch(all.vars(scenario$formula), error = function(e) character(0))
      missing_cols <- setdiff(req, names(dat))
      if (length(missing_cols) > 0L) {
        .stop(sprintf(
          "Custom DGP output is missing column(s) required by the model formula: %s.",
          paste(missing_cols, collapse = ", ")
        ))
      }
    }
    dat
  }
}
