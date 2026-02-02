test_that("scenario combines design and assumptions", {
  result <- scenario(design(), assumptions())
  expect_s3_class(result, "mixpower_scenario")
  expect_named(result, c("design", "assumptions"))
})
