test_that("assumptions creates a classed object", {
  result <- assumptions(mean = 0, sd = 1)
  expect_s3_class(result, "mixpower_assumptions")
})
