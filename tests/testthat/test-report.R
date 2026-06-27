rep_scn <- function(n = 12, slope = FALSE, effect = 0.4) {
  d <- mp_design(list(subject = n), trials_per_cell = 8)
  re <- if (slope) {
    list(subject = list(intercept_sd = 0.5, slopes = list(condition = 0.8)))
  } else {
    list(subject = list(intercept_sd = 0.5))
  }
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = effect),
    random_effects = re, residual_sd = 1
  )
  mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
}

test_that("mp_report builds a sectioned report from mp_plan", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  plan <- suppressWarnings(mp_plan(rep_scn(), nsim = 40, calibrate_nsim = 40, seed = 1))
  rep <- mp_report(plan, title = "My power report")
  expect_s3_class(rep, "mp_report")
  md <- paste(rep$markdown, collapse = "\n")
  expect_match(md, "# My power report")
  for (sec in c("## Summary", "## Design and model", "## Data-generating assumptions",
                "## Type I calibration", "## Power", "## Diagnostics",
                "## Caveats and fragile claims")) {
    expect_match(md, sec, fixed = TRUE)
  }
  expect_output(print(rep), "Power")
})

test_that("mp_report flags missing calibration on a risky design", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  res <- suppressWarnings(mp_power(rep_scn(n = 8), nsim = 30, seed = 1)) # few clusters, no calibration
  rep <- mp_report(res)
  expect_true(any(grepl("No Type I calibration", rep$warnings)))
})

test_that("mp_report flags an anti-conservative (omitted-slope) design", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  # DGP has a by-subject slope; the model omits it -> inflated Type I.
  plan <- suppressWarnings(mp_plan(rep_scn(n = 30, slope = TRUE, effect = 0),
                                   nsim = 80, calibrate_nsim = 120, seed = 3))
  rep <- mp_report(plan)
  expect_equal(plan$calibration$verdict, "anti-conservative")
  expect_true(any(grepl("not trustworthy|above alpha", rep$warnings)))
})

test_that("mp_report includes adaptive stopping details", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  res <- suppressWarnings(mp_power_adaptive(
    rep_scn(n = 25), stop = mp_stop(ci_halfwidth = 0.12, batch = 40, min_nsim = 40, max_nsim = 200),
    seed = 4
  ))
  md <- paste(mp_report(res)$markdown, collapse = "\n")
  expect_match(md, "adaptive stopping", fixed = TRUE)
})

test_that("mp_write_report writes markdown", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  plan <- suppressWarnings(mp_plan(rep_scn(), nsim = 30, calibrate_nsim = 30, seed = 1))
  rep <- mp_report(plan)
  f <- tempfile(fileext = ".md")
  on.exit(unlink(f), add = TRUE)
  mp_write_report(rep, f, format = "markdown")
  expect_true(file.exists(f))
  expect_true(any(grepl("# Power analysis report", readLines(f))))
})

test_that("mp_report / mp_write_report validate inputs", {
  expect_error(mp_report(list()), "mp_power")
  expect_error(mp_write_report(list(), tempfile()), "mp_report")
})