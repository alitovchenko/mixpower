test_that("gaussian elicitation solves a coherent variance partition", {
  a <- mp_elicit("gaussian", d = 0.5, icc = 0.1, outcome_sd = 2)
  expect_s3_class(a, "mp_elicitation")
  expect_s3_class(a, "mp_assumptions") # usable wherever assumptions are
  expect_equal(a$fixed_effects$condition, 0.5 * 2)
  expect_equal(a$random_effects$subject$intercept_sd, sqrt(0.1) * 2, tolerance = 1e-8)
  expect_equal(a$residual_sd, sqrt(0.9) * 2, tolerance = 1e-8)
  expect_output(print(a), "implied model")
})

test_that("multi-group ICC partitions total variance and guards the sum", {
  a <- mp_elicit("gaussian", d = 0.3,
                 icc = c(subject = 0.1, item = 0.2), groups = c("subject", "item"))
  expect_equal(a$random_effects$subject$intercept_sd, sqrt(0.1), tolerance = 1e-8)
  expect_equal(a$random_effects$item$intercept_sd, sqrt(0.2), tolerance = 1e-8)
  expect_equal(a$residual_sd, sqrt(0.7), tolerance = 1e-8)
  expect_error(
    mp_elicit("gaussian", d = 0.3, icc = c(subject = 0.6, item = 0.5),
              groups = c("subject", "item")),
    "sum of `icc`"
  )
})

test_that("binomial elicitation maps baseline/OR and latent ICC", {
  a <- mp_elicit("binomial", baseline_prob = 0.2, odds_ratio = 1.8, icc = 0.05)
  expect_equal(a$fixed_effects$`(Intercept)`, stats::qlogis(0.2), tolerance = 1e-8)
  expect_equal(a$fixed_effects$condition, log(1.8), tolerance = 1e-8)
  expect_equal(a$random_effects$subject$intercept_sd,
               sqrt(0.05 / 0.95) * pi / sqrt(3), tolerance = 1e-8)
  expect_null(a$residual_sd)

  # risk_ratio is converted to an odds ratio given the baseline.
  a2 <- mp_elicit("binomial", baseline_prob = 0.2, risk_ratio = 1.5)
  or <- 1.5 * 0.8 / (1 - 1.5 * 0.2)
  expect_equal(a2$fixed_effects$condition, log(or), tolerance = 1e-8)
})

test_that("a random slope is a fraction of the intercept SD", {
  a <- mp_elicit("gaussian", d = 0.4, icc = 0.2, slope = 0.5, cor = 0.1)
  isd <- sqrt(0.2)
  expect_equal(a$random_effects$subject$slopes$condition, 0.5 * isd, tolerance = 1e-8)
  expect_equal(a$random_effects$subject$cor, 0.1)
})

test_that("mp_elicit validates inputs", {
  expect_error(mp_elicit("gaussian"), "`d`.*required")
  expect_error(mp_elicit("binomial", odds_ratio = 2), "baseline_prob")
  expect_error(mp_elicit("binomial", baseline_prob = 0.2), "odds_ratio|risk_ratio")
  expect_error(mp_elicit("gaussian", d = 0.3, icc = 1.2), "in \\[0, 1\\)")
})

test_that("elicited gaussian assumptions recover the target d in simulation", {
  skip_on_cran()
  a <- mp_elicit("gaussian", d = 0.5, icc = 0.1, outcome_sd = 1)
  d <- mp_design(list(subject = 1600), trials_per_cell = 2,
                 predictors = list(condition = list(type = "binary", level = "between")))
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  set.seed(11)
  dat <- scn$engine$simulate_fun(scn)
  m1 <- mean(dat$y[dat$condition == 1]); m0 <- mean(dat$y[dat$condition == 0])
  pooled_sd <- sqrt(mean(c(stats::var(dat$y[dat$condition == 0]),
                           stats::var(dat$y[dat$condition == 1]))))
  # Variance partition is coherent (within-condition SD ~ outcome_sd = 1)...
  expect_lt(abs(pooled_sd - 1), 0.06)
  # ...and the standardized mean difference ~ d.
  expect_lt(abs((m1 - m0) / pooled_sd - 0.5), 0.12)
})

test_that("elicited binomial assumptions recover baseline prob and odds ratio", {
  skip_on_cran()
  a <- mp_elicit("binomial", baseline_prob = 0.2, odds_ratio = 2, icc = 0) # icc=0 -> marginal = conditional
  d <- mp_design(list(subject = 4000), trials_per_cell = 1,
                 predictors = list(condition = list(type = "binary", level = "between")))
  scn <- mp_scenario_lme4_binomial(y ~ condition + (1 | subject), design = d, assumptions = a)
  set.seed(12)
  dat <- scn$engine$simulate_fun(scn)
  p0 <- mean(dat$y[dat$condition == 0]); p1 <- mean(dat$y[dat$condition == 1])
  expect_lt(abs(p0 - 0.2), 0.03)
  emp_or <- (p1 / (1 - p1)) / (p0 / (1 - p0))
  expect_lt(abs(emp_or - 2), 0.5)
})

test_that("elicited assumptions compose into a runnable scenario", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  a <- mp_elicit("gaussian", d = 0.6, icc = 0.1)
  d <- mp_design(list(subject = 24), trials_per_cell = 6)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  res <- suppressWarnings(mp_power(scn, nsim = 30, seed = 1))
  expect_true(res$power >= 0 && res$power <= 1)
})
