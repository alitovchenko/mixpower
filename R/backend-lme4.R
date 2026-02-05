#' lme4 backend for Gaussian linear mixed-effects models
#'
#' Provides engine functions (`simulate_fun`, `fit_fun`, `test_fun`) compatible
#' with `mp_scenario()` and `mp_power()`.
#'
#' @param outcome Name of outcome variable (default "y").
#' @param predictor Name of focal fixed-effect predictor to test (default "condition").
#' @param subject Name of subject grouping factor in simulated data (default "subject").
#' @param item Optional name of item grouping factor in simulated data (default NULL).
#' @param re_subject_intercept_sd SD of subject random intercepts.
#' @param re_item_intercept_sd Optional SD of item random intercepts.
#' @param optimizer lme4 optimizer name.
#' @param check_singular Whether to compute singular fits via `lme4::isSingular`.
#'
#' @return A named list with `simulate_fun`, `fit_fun`, and `test_fun`.
#' @export
mp_backend_lme4 <- function(outcome = "y",
                            predictor = "condition",
                            subject = "subject",
                            item = NULL,
                            re_subject_intercept_sd = 1,
                            re_item_intercept_sd = NULL,
                            optimizer = "bobyqa",
                            check_singular = TRUE) {
  if (!is.character(outcome) || length(outcome) != 1) .stop("`outcome` must be a single string.")
  if (!is.character(predictor) || length(predictor) != 1) .stop("`predictor` must be a single string.")
  if (!is.character(subject) || length(subject) != 1) .stop("`subject` must be a single string.")
  if (!is.null(item) && (!is.character(item) || length(item) != 1)) .stop("`item` must be NULL or a single string.")
  .assert_is_nonneg_num(re_subject_intercept_sd, "re_subject_intercept_sd")
  if (!is.null(re_item_intercept_sd)) .assert_is_nonneg_num(re_item_intercept_sd, "re_item_intercept_sd")

  simulate_fun <- function(scn, seed) {
    simulate_lmm_data(
      scenario = scn,
      seed = seed,
      outcome = outcome,
      predictor = predictor,
      subject = subject,
      item = item,
      re_subject_intercept_sd = re_subject_intercept_sd,
      re_item_intercept_sd = re_item_intercept_sd
    )
  }

  fit_fun <- function(dat, scn) {
    # Ensure grouping vars exist
    if (!subject %in% names(dat)) .stop(sprintf("Simulated data missing subject column '%s'.", subject))
    if (!is.null(item) && !item %in% names(dat)) .stop(sprintf("Simulated data missing item column '%s'.", item))

    # Fit lmer; fixed formula comes from scenario
    ctrl <- lme4::lmerControl(optimizer = optimizer, calc.derivs = FALSE)
    fit <- lme4::lmer(formula = scn$formula, data = dat, control = ctrl)

    # Attach singular flag so mp_power can read it without importing lme4 logic
    if (check_singular) {
      attr(fit, "singular") <- isTRUE(lme4::isSingular(fit, tol = 1e-4))
    }
    fit
  }

  test_fun <- function(fit, scn) {
    # Wald test using normal approx (fast, CRAN-safe). For LRT add later.
    cf <- lme4::fixef(fit)
    vc <- as.matrix(stats::vcov(fit))

    if (!predictor %in% names(cf)) {
      .stop(sprintf("Predictor '%s' not found among fixed effects: %s",
                    predictor, paste(names(cf), collapse = ", ")))
    }

    idx <- match(predictor, names(cf))
    est <- unname(cf[[idx]])
    se <- sqrt(vc[idx, idx])

    if (!is.finite(se) || se <= 0) {
      return(list(p_value = NA_real_, estimate = est, se = se))
    }

    z <- est / se
    p <- 2 * stats::pnorm(-abs(z))

    list(p_value = as.numeric(p), estimate = est, se = se, z = z)
  }

  list(simulate_fun = simulate_fun, fit_fun = fit_fun, test_fun = test_fun)
}

#' Convenience constructor for an lme4-based scenario
#'
#' @param formula A mixed-model formula compatible with `lme4::lmer`.
#' @param design `mp_design()`.
#' @param assumptions `mp_assumptions()`.
#' @param predictor Name of fixed effect to test (must match fixed effect column name).
#' @param subject Subject grouping factor name.
#' @param item Optional item grouping factor name.
#' @param ... Passed to `mp_backend_lme4()`.
#'
#' @return An `mp_scenario` with engine functions filled.
#' @export
mp_scenario_lme4 <- function(formula,
                             design,
                             assumptions,
                             predictor = "condition",
                             subject = "subject",
                             item = NULL,
                             ...) {
  backend <- mp_backend_lme4(
    predictor = predictor,
    subject = subject,
    item = item,
    ...
  )

  mp_scenario(
    formula = formula,
    design = design,
    assumptions = assumptions,
    test = "wald",
    simulate_fun = backend$simulate_fun,
    fit_fun = backend$fit_fun,
    test_fun = backend$test_fun
  )
}
