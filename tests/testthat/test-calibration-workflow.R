risky_scn <- function(n = 12) {
  d <- mp_design(list(subject = n), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.4),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
}

test_that(".mp_calibration_risk flags few-cluster / complex designs", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  expect_true(.mp_calibration_risk(risky_scn(8)))                 # few clusters
  expect_true(!is.null(.mp_calibration_advice(risky_scn(8))))

  big <- risky_scn(200)
  expect_false(.mp_calibration_risk(big))                         # many clusters, intercept-only
  expect_null(.mp_calibration_advice(big))

  # Random slopes make even a larger design "risky".
  d <- mp_design(list(subject = 200), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.4),
    random_effects = list(subject = list(intercept_sd = 0.5, slopes = list(condition = 0.3))),
    residual_sd = 1
  )
  expect_true(.mp_calibration_risk(
    mp_scenario_lme4(y ~ condition + (1 + condition | subject), design = d, assumptions = a)
  ))
})

test_that("mp_power nudges (once) for risky designs and records advice", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  scn <- risky_scn(12)

  options(mixpower.calibration_nudged = FALSE)
  expect_warning(mp_power(scn, nsim = 6, seed = 1), "calibrat")

  options(mixpower.calibration_nudged = FALSE)
  res <- suppressWarnings(mp_power(scn, nsim = 6, seed = 1))
  expect_false(is.null(res$calibration_advice))
  expect_true(isTRUE(getOption("mixpower.calibration_nudged"))) # nudge fired
})

test_that("nudge is suppressed by check_calibration=FALSE or an attached calibration", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  scn <- risky_scn(12)

  options(mixpower.calibration_nudged = FALSE)
  suppressWarnings(mp_power(scn, nsim = 6, seed = 1, check_calibration = FALSE))
  expect_false(isTRUE(getOption("mixpower.calibration_nudged")))

  cal <- suppressWarnings(mp_calibrate(scn, nsim = 30, seed = 1))
  scn_cal <- mp_attach_calibration(scn, cal)
  options(mixpower.calibration_nudged = FALSE)
  res <- suppressWarnings(mp_power(scn_cal, nsim = 6, seed = 1))
  expect_false(isTRUE(getOption("mixpower.calibration_nudged")))
  expect_false(is.null(res$calibration_advice)) # advice still recorded
})

test_that("mp_plan bundles calibration + power + recommendation", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  scn <- risky_scn(12)
  plan <- suppressWarnings(mp_plan(scn, nsim = 40, calibrate_nsim = 40, seed = 2))

  expect_s3_class(plan, "mp_plan")
  expect_s3_class(plan$calibration, "mp_calibration")
  expect_s3_class(plan$power, "mp_power")
  expect_true(plan$power$power >= 0 && plan$power$power <= 1)
  expect_output(print(plan), "calibration")

  s <- summary(plan)
  expect_true(all(c("type1", "calibration", "power") %in% names(s)))
  rt <- mp_report_table(plan)
  expect_true(all(c("power_estimate", "calibration") %in% names(rt)))
})

test_that("mp_attach_calibration validates its input", {
  expect_error(mp_attach_calibration(risky_scn(10), list()), "mp_calibration")
})