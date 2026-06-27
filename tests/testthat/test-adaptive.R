test_that("mp_stop validates its specification", {
  expect_error(mp_stop(ci_halfwidth = 0.8), "in \\(0, 0.5\\)")
  expect_error(mp_stop(target = 1.5), "in \\(0, 1\\)")
  expect_error(mp_stop(ci_halfwidth = NULL, target = NULL), "at least one")
  expect_error(mp_stop(min_nsim = 500, max_nsim = 100), "<= `max_nsim`")
  s <- mp_stop(ci_halfwidth = 0.03, target = 0.8)
  expect_s3_class(s, "mp_stop")
})

adapt_scn <- function(effect = 0.4, n = 25) {
  d <- mp_design(list(subject = n), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = effect),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
}

test_that("adaptive stops on precision before max_nsim", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  scn <- adapt_scn()
  res <- suppressWarnings(mp_power_adaptive(
    scn, stop = mp_stop(ci_halfwidth = 0.12, batch = 40, min_nsim = 40, max_nsim = 400),
    seed = 1
  ))
  expect_s3_class(res, "mp_power_adaptive")
  expect_s3_class(res, "mp_power")
  expect_true(res$stopping$converged)
  expect_match(res$stopping$reason, "precision")
  expect_lt(res$stopping$nsim_used, 400)
  expect_lte(res$stopping$ci_halfwidth, 0.12)
  expect_output(print(res), "converged")
})

test_that("adaptive hits max_nsim when the target precision is unreachable", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  scn <- adapt_scn()
  res <- suppressWarnings(mp_power_adaptive(
    scn, stop = mp_stop(ci_halfwidth = 0.001, batch = 20, min_nsim = 20, max_nsim = 60),
    seed = 2
  ))
  expect_false(res$stopping$converged)
  expect_equal(res$stopping$reason, "max_nsim")
  expect_equal(res$stopping$nsim_used, 60L)
})

test_that("adaptive stops on a clear decision vs a target", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  scn <- adapt_scn(effect = 0.9, n = 30) # strongly powered
  res <- suppressWarnings(mp_power_adaptive(
    scn, stop = mp_stop(ci_halfwidth = NULL, target = 0.2, batch = 30,
                        min_nsim = 30, max_nsim = 300),
    seed = 3
  ))
  expect_true(res$stopping$converged)
  expect_equal(res$stopping$reason, "decision")
  # CI lies entirely above the target.
  expect_gt(res$ci[[1]], 0.2)
})

test_that("adaptive result equals a fixed mp_power run at the same nsim", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  scn <- adapt_scn()
  res <- suppressWarnings(mp_power_adaptive(
    scn, stop = mp_stop(ci_halfwidth = 0.12, batch = 40, min_nsim = 40, max_nsim = 400),
    seed = 5
  ))
  fixed <- suppressWarnings(mp_power(scn, nsim = res$stopping$nsim_used, seed = 5))
  expect_equal(res$power, fixed$power)
  expect_equal(res$sims$p_value, fixed$sims$p_value)

  # Works with the standard reporting layer.
  rt <- mp_report_table(res)
  expect_true("power_estimate" %in% names(rt))
})