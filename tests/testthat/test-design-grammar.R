test_that("residual_ar1 validates and is recorded", {
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0), residual_sd = 1, residual_ar1 = 0.5)
  expect_equal(a$residual_ar1, 0.5)
  expect_error(mp_assumptions(list(condition = 0), residual_ar1 = 1.2), "\\(-1, 1\\)")
})

test_that("AR(1) residuals have the requested within-subject autocorrelation", {
  skip_on_cran()
  d <- mp_design(list(subject = 200), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0),
    random_effects = list(subject = list(intercept_sd = 0)), # isolate the residual
    residual_sd = 1, residual_ar1 = 0.6
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  set.seed(1)
  dat <- scn$engine$simulate_fun(scn)
  sub <- as.integer(dat$subject)
  same <- sub[-1] == sub[-length(sub)] # consecutive rows within the same subject (lag 1)
  r <- stats::cor(dat$y[-1][same], dat$y[-length(dat$y)][same])
  expect_equal(r, 0.6, tolerance = 0.07)
})

test_that("cluster-level predictor is constant within each cluster (CRT)", {
  d <- mp_design(list(site = 10, subject = 6), trials_per_cell = 4,
                 nesting = c(subject = "site"),
                 predictors = list(arm = list(type = "binary", level = "cluster")))
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, arm = 0.5),
    random_effects = list(site = list(intercept_sd = 0.4),
                          subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ arm + (1 | site) + (1 | subject), design = d,
                          assumptions = a, predictor = "arm")
  dat <- scn$engine$simulate_fun(scn)
  per_site <- tapply(dat$arm, dat$site, function(z) length(unique(z)))
  expect_true(all(per_site == 1L)) # treatment constant within cluster
  expect_equal(length(unique(dat$arm)), 2L)
})

test_that("cluster-level predictor requires a nesting parent", {
  d <- mp_design(list(subject = 10), trials_per_cell = 4,
                 predictors = list(arm = list(type = "binary", level = "cluster")))
  a <- mp_assumptions(list("(Intercept)" = 0, arm = 0.5),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ arm + (1 | subject), design = d, assumptions = a, predictor = "arm")
  expect_error(scn$engine$simulate_fun(scn), "nesting")
})

test_that("unequal allocation assigns the requested proportion", {
  d <- mp_design(list(subject = 100), trials_per_cell = 2,
                 predictors = list(group = list(type = "binary", level = "between", allocation = 0.75)))
  a <- mp_assumptions(list("(Intercept)" = 0, group = 0.4),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ group + (1 | subject), design = d, assumptions = a, predictor = "group")
  dat <- scn$engine$simulate_fun(scn)
  per_subj <- tapply(dat$group, dat$subject, function(z) z[1])
  expect_equal(mean(per_subj), 0.75, tolerance = 0.001)
  expect_error(
    mp_design(list(subject = 10), trials_per_cell = 2,
              predictors = list(g = list(level = "between", allocation = 1.5))),
    "in \\(0, 1\\)"
  )
})

test_that("an AR(1) longitudinal scenario runs end-to-end and is reproducible", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(list(subject = 20), trials_per_cell = 6, predictors = list(time = "continuous"))
  a <- mp_assumptions(list("(Intercept)" = 0, time = 0.3),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1, residual_ar1 = 0.4)
  scn <- mp_scenario_lme4(y ~ time + (1 | subject), design = d, assumptions = a, predictor = "time")
  r1 <- suppressWarnings(mp_power(scn, nsim = 10, seed = 1))
  r2 <- suppressWarnings(mp_power(scn, nsim = 10, seed = 1))
  expect_equal(r1$sims$p_value, r2$sims$p_value)
})

test_that("default design path is unchanged (binary 0/1 within subject)", {
  d <- mp_design(list(subject = 6), trials_per_cell = 4)
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.3),
                      random_effects = list(subject = list(intercept_sd = 0.4)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  dat <- scn$engine$simulate_fun(scn)
  expect_equal(sort(unique(dat$condition)), c(0, 1))
  expect_equal(as.numeric(tapply(dat$condition, dat$subject, mean)), rep(0.5, 6))
})