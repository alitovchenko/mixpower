test_that("mp_scenario_glmmtmb_lmm runs mp_power without error", {
  skip_if_not_installed("glmmTMB")

  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 3)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
    residual_sd = 1
  )
  scn <- mp_scenario_glmmtmb_lmm(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a
  )
  res <- mp_power(scn, nsim = 15, seed = 7)
  expect_true(is.numeric(res$power))
  expect_true(res$power >= 0 && res$power <= 1 || is.na(res$power))
})

test_that("glmmTMB and lme4 Gaussian give similar power on same seed (rough)", {
  skip_if_not_installed("glmmTMB")

  d <- mp_design(clusters = list(subject = 25), trials_per_cell = 3)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
    residual_sd = 1
  )
  f <- y ~ condition + (1 | subject)
  sc_l <- mp_scenario_lme4(f, design = d, assumptions = a)
  sc_t <- mp_scenario_glmmtmb_lmm(f, design = d, assumptions = a)
  pl <- mp_power(sc_l, nsim = 60, seed = 100)
  pt <- mp_power(sc_t, nsim = 60, seed = 100)
  expect_true(abs(pl$power - pt$power) < 0.25)
})
