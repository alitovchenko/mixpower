# A simple, assumptions-aware custom DGP (so calibration can null the effect).
simple_dgp <- function(scenario, seed = NULL) {
  n <- scenario$design$clusters$subject
  t <- scenario$design$trials_per_cell
  b <- scenario$assumptions$fixed_effects
  re_sd <- scenario$assumptions$random_effects$subject$intercept_sd
  subj <- rep(seq_len(n), each = t)
  x <- rep(c(0, 1), length.out = n * t)
  re <- stats::rnorm(n, 0, re_sd)
  y <- b$`(Intercept)` + b$condition * x + re[subj] +
    stats::rnorm(n * t, 0, scenario$assumptions$residual_sd)
  data.frame(subject = factor(subj), condition = x, y = y)
}

test_that("mp_custom_dgp validates the function and its output", {
  expect_error(mp_custom_dgp(42), "must be a function")

  d <- mp_design(list(subject = 6), trials_per_cell = 2)
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.3),
                      random_effects = list(subject = list(intercept_sd = 0.4)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)

  not_df <- mp_custom_dgp(function(scenario, seed = NULL) 1:3)
  expect_error(not_df(scn), "data.frame")

  missing_y <- mp_custom_dgp(function(scenario, seed = NULL) {
    data.frame(subject = factor(1:6), condition = rep(c(0, 1), 3))
  })
  expect_error(missing_y(scn), "missing column")
})

test_that("simulate= overrides the built-in DGP and stays reproducible", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(list(subject = 25), trials_per_cell = 6)
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.5),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d,
                          assumptions = a, simulate = simple_dgp)
  r1 <- suppressWarnings(mp_power(scn, nsim = 12, seed = 1))
  r2 <- suppressWarnings(mp_power(scn, nsim = 12, seed = 1))
  expect_equal(r1$sims$p_value, r2$sims$p_value)
  expect_gt(r1$power, 0.4)
})

test_that("a custom DGP composes with calibration (null read from assumptions)", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(list(subject = 40), trials_per_cell = 6)
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.4),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d,
                          assumptions = a, simulate = simple_dgp)
  cal <- mp_calibrate(scn, nsim = 150, seed = 2) # sets condition = 0 via mp_sesoi
  expect_lt(cal$type1, 0.12)
})

test_that("multi-arm (3-level factor) works via a custom DGP + omnibus LRT", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  dgp3 <- function(scenario, seed = NULL) {
    n <- scenario$design$clusters$subject
    t <- scenario$design$trials_per_cell
    b <- scenario$assumptions$fixed_effects
    arm_subj <- factor(rep(c("a", "b", "c"), length.out = n))
    subj <- rep(seq_len(n), each = t)
    arm <- arm_subj[subj]
    re <- stats::rnorm(n, 0, scenario$assumptions$random_effects$subject$intercept_sd)
    mu <- c(a = 0, b = b$arm_b, c = b$arm_c)[as.character(arm)]
    y <- b$`(Intercept)` + mu + re[subj] + stats::rnorm(n * t, 0, scenario$assumptions$residual_sd)
    data.frame(subject = factor(subj), arm = arm, y = y)
  }
  a <- mp_assumptions(list("(Intercept)" = 0, arm_b = 0.6, arm_c = 0.9),
                      random_effects = list(subject = list(intercept_sd = 0.4)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ arm + (1 | subject),
                          design = mp_design(list(subject = 30), trials_per_cell = 4),
                          assumptions = a, simulate = dgp3,
                          test_method = "lrt", null_formula = y ~ 1 + (1 | subject))
  res <- suppressWarnings(mp_power(scn, nsim = 30, seed = 5))
  expect_gt(res$power, 0.5)
})

test_that("correlated predictors work via a custom DGP", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  dgp_cor <- function(scenario, seed = NULL) {
    n <- scenario$design$clusters$subject
    t <- scenario$design$trials_per_cell
    b <- scenario$assumptions$fixed_effects
    x1 <- stats::rnorm(n)
    x2 <- 0.7 * x1 + sqrt(1 - 0.49) * stats::rnorm(n) # cor(x1, x2) ~ 0.7
    re <- stats::rnorm(n, 0, scenario$assumptions$random_effects$subject$intercept_sd)
    subj <- rep(seq_len(n), each = t)
    y <- b$`(Intercept)` + b$x1 * x1[subj] + b$x2 * x2[subj] + re[subj] +
      stats::rnorm(n * t, 0, scenario$assumptions$residual_sd)
    data.frame(subject = factor(subj), x1 = x1[subj], x2 = x2[subj], y = y)
  }
  a <- mp_assumptions(list("(Intercept)" = 0, x1 = 0.4, x2 = 0.3),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ x1 + x2 + (1 | subject),
                          design = mp_design(list(subject = 40), trials_per_cell = 4),
                          assumptions = a, simulate = dgp_cor, predictor = "x1")
  res <- suppressWarnings(mp_power(scn, nsim = 20, seed = 3))
  expect_true(res$power >= 0 && res$power <= 1)
})
