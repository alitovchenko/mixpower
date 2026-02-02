test_that("design creates a classed object", {
  result <- design(alpha = 0.05)
  expect_s3_class(result, "mixpower_design")
})
