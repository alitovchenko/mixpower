#' Build a power scenario from a fitted lme4 model
#'
#' Turns an existing `lmer`/`glmer` fit (e.g. from pilot or published data) into
#' an `mp_scenario`, so its estimated effects and variance components inform a
#' simulation-based power analysis. New responses are simulated from the fitted
#' model with [stats::simulate()] (keeping the estimated random-effect structure
#' and residual variance), the model is refit, and the focal term is tested.
#'
#' The fixed effects used to simulate are read from the scenario's assumptions,
#' which start at the fitted coefficients. This means `mp_sensitivity()` /
#' `mp_power_curve()` can vary an effect size (e.g.
#' `fixed_effects.condition`) for data-based vs smallest-effect-of-interest
#' comparisons. The number of observations is fixed at the model's data, so
#' varying sample size is not supported through a `mp_from_fit()` scenario; use
#' the synthetic [mp_scenario_lme4()] path (or extend the data) for that.
#'
#' @param fit A fitted model of class `lmerMod`/`lmerModLmerTest` (Gaussian LMM)
#'   or `glmerMod` (binomial/Poisson/negative-binomial GLMM).
#' @param test_term Fixed-effect term to test. Defaults to the first
#'   non-intercept fixed effect.
#' @param test_method Inference method. Gaussian fits allow `"wald"` (default),
#'   `"satterthwaite"`, `"kenward-roger"`, `"lrt"`, `"pb"`; GLMM fits allow
#'   `"wald"`, `"lrt"`, `"pb"`.
#' @param null_formula Null-model formula required for `"lrt"`/`"pb"`. Defaults
#'   to the fitted formula with `test_term` removed.
#' @param pb_nsim Bootstrap replicates for `test_method = "pb"` (default 100).
#' @return An object of class `mp_scenario`.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
#'   scn <- mp_from_fit(m, test_term = "Days")
#'   mp_power(scn, nsim = 20, seed = 1)
#' }
#' }
mp_from_fit <- function(fit,
                        test_term = NULL,
                        test_method = NULL,
                        null_formula = NULL,
                        pb_nsim = 100L) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` is required for `mp_from_fit()`.", call. = FALSE)
  }
  if (!inherits(fit, c("lmerMod", "lmerModLmerTest", "glmerMod"))) {
    stop("`mp_from_fit()` currently supports lme4 fits (lmer / glmer).", call. = FALSE)
  }

  is_gaussian <- !inherits(fit, "glmerMod")
  ff <- stats::formula(fit)
  fixed <- lme4::fixef(fit)
  term <- `%||%`(test_term, .mp_default_test_term(fixed))
  if (!term %in% names(fixed)) {
    stop(sprintf("`test_term` '%s' is not a fixed effect in the model.", term), call. = FALSE)
  }

  allowed <- if (is_gaussian) .mp_lme4_methods else .mp_glmm_methods
  method <- `%||%`(test_method, "wald")
  if (is.null(null_formula) && method %in% c("lrt", "pb")) {
    null_formula <- stats::update.formula(ff, stats::as.formula(paste0(". ~ . - ", term)))
  }
  method <- .mp_resolve_test_method(method, null_formula, allowed)

  fam <- if (is_gaussian) NULL else stats::family(fit)
  reml <- isTRUE(lme4::isREML(fit))
  resp <- names(stats::model.frame(fit))[1]

  simulate_fun <- function(scenario, seed = NULL) {
    beta <- .mp_assumptions_beta(scenario$assumptions, names(fixed), fixed)
    ysim <- stats::simulate(fit, nsim = 1, newparams = list(beta = beta))[[1]]
    dat <- stats::model.frame(fit)
    dat[[resp]] <- ysim
    dat
  }

  fit_fun <- function(data, scenario) {
    newfit <- if (is_gaussian) {
      if (requireNamespace("lmerTest", quietly = TRUE)) {
        lmerTest::lmer(ff, data = data, REML = reml)
      } else {
        lme4::lmer(ff, data = data, REML = reml)
      }
    } else {
      lme4::glmer(ff, data = data, family = fam)
    }
    attr(newfit, "singular") <- tryCatch(isTRUE(lme4::isSingular(newfit, tol = 1e-4)),
                                         error = function(e) NA)
    newfit
  }

  test_fun <- function(model, scenario) {
    .mp_dispatch_test(model, scenario, term, method, null_formula, pb_nsim)
  }

  backend <- mp_backend(
    simulate_fun = simulate_fun,
    fit_fun = fit_fun,
    test_fun = test_fun,
    name = if (is_gaussian) "from_fit:lmer" else "from_fit:glmer",
    capabilities = list(
      families = if (is_gaussian) "gaussian" else fam$family,
      test_methods = allowed,
      source = "fitted_model"
    )
  )

  assumptions <- mp_assumptions(
    fixed_effects = as.list(fixed),
    random_effects = .mp_extract_varcorr(fit),
    residual_sd = if (is_gaussian) stats::sigma(fit) else NULL
  )
  design <- .mp_design_from_fit(fit)

  mp_scenario(
    formula = ff,
    design = design,
    assumptions = assumptions,
    test = list(term = term, method = method,
                null_formula = null_formula, pb_nsim = pb_nsim),
    simulate_fun = backend$simulate_fun,
    fit_fun = backend$fit_fun,
    test_fun = backend$test_fun
  )
}

# First non-intercept fixed-effect name.
.mp_default_test_term <- function(fixed) {
  nm <- setdiff(names(fixed), "(Intercept)")
  if (length(nm) == 0L) {
    .stop("Model has no non-intercept fixed effect to test; supply `test_term`.")
  }
  nm[[1]]
}

# Build the fixed-effect vector (in the model's coefficient order) from the
# scenario assumptions, defaulting to the fitted values. Lets sensitivity over
# `fixed_effects.<term>` change the simulated effect size.
.mp_assumptions_beta <- function(assumptions, beta_names, default_beta) {
  vapply(beta_names, function(nm) {
    v <- assumptions$fixed_effects[[nm]]
    if (is.null(v) || !is.numeric(v) || length(v) != 1L) {
      as.numeric(default_beta[[nm]])
    } else {
      as.numeric(v)
    }
  }, numeric(1))
}

# Random-effect SDs (intercept, first slope, intercept-slope correlation) per
# grouping factor, for the scenario's assumptions metadata.
.mp_extract_varcorr <- function(fit) {
  vc <- lme4::VarCorr(fit)
  out <- list()
  for (g in names(vc)) {
    m <- vc[[g]]
    sds <- sqrt(diag(m))
    nm <- names(sds)
    re <- list(intercept_sd = if ("(Intercept)" %in% nm) unname(sds[["(Intercept)"]]) else 0)
    slope_terms <- setdiff(nm, "(Intercept)")
    if (length(slope_terms) >= 1L) {
      st <- slope_terms[[1]]
      re$slopes <- stats::setNames(list(unname(sds[[st]])), st)
      if ("(Intercept)" %in% nm) {
        cc <- suppressWarnings(stats::cov2cor(m)[["(Intercept)", st]])
        if (is.finite(cc)) re$cor <- unname(cc)
      }
    }
    out[[g]] <- re
  }
  if (length(out) == 0L) NULL else out
}

# Descriptive design (group sizes) for a fitted model.
.mp_design_from_fit <- function(fit) {
  ng <- lme4::ngrps(fit)
  clusters <- as.list(as.integer(ng))
  names(clusters) <- names(ng)
  tpc <- max(1L, as.integer(stats::nobs(fit) %/% max(ng)))
  mp_design(clusters = clusters, trials_per_cell = tpc)
}
