test_that("random_effects accepts slopes and cor; validation guards them", {
  ok <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.3),
    random_effects = list(subject = list(
      intercept_sd = 0.5, slopes = list(condition = 0.3), cor = 0.2
    )),
    residual_sd = 1
  )
  expect_equal(.mp_re_slope_sd(ok, "subject", "condition"), 0.3)
  expect_equal(.mp_re_cor(ok, "subject"), 0.2)

  expect_error(
    mp_assumptions(list(condition = 0.3),
                   random_effects = list(subject = list(intercept_sd = 0.5, cor = 2))),
    "\\[-1, 1\\]"
  )
  expect_error(
    mp_assumptions(list(condition = 0.3),
                   random_effects = list(subject = list(
                     intercept_sd = 0.5,
                     slopes = list(condition = 0.3, other = 0.1)
                   ))),
    "single random slope"
  )
  expect_error(
    mp_assumptions(list(condition = 0.3),
                   random_effects = list(subject = list(
                     intercept_sd = 0.5, slopes = list(condition = -1)
                   ))),
    "non-negative"
  )
})

test_that("simulation with random slopes is reproducible", {
  skip_if_not_installed("lme4")
  d <- mp_design(clusters = list(subject = 25), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.3),
    random_effects = list(subject = list(
      intercept_sd = 0.5, slopes = list(condition = 0.4), cor = 0.1
    )),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 + condition | subject), design = d, assumptions = a)
  r1 <- mp_power(scn, nsim = 12, seed = 321)
  r2 <- mp_power(scn, nsim = 12, seed = 321)
  expect_equal(r1$sims$p_value, r2$sims$p_value)
})

test_that("omitting a present random slope inflates Type I error", {
  skip_if_not_installed("lme4")

  # Null fixed effect, but a large between-subject random slope on condition.
  d <- mp_design(clusters = list(subject = 30), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0),
    random_effects = list(subject = list(
      intercept_sd = 0.5, slopes = list(condition = 0.8)
    )),
    residual_sd = 1
  )

  scn_max <- mp_scenario_lme4(
    y ~ condition + (1 + condition | subject), design = d, assumptions = a
  )
  scn_int <- mp_scenario_lme4(
    y ~ condition + (1 | subject), design = d, assumptions = a
  )

  ti_max <- mp_power(scn_max, nsim = 80, seed = 2024)$power
  ti_int <- mp_power(scn_int, nsim = 80, seed = 2024)$power

  # The intercept-only model ignores slope variability -> anti-conservative.
  expect_gt(ti_int, ti_max)
  # The maximal model keeps Type I error near the nominal level.
  expect_lt(ti_max, 0.15)
})
