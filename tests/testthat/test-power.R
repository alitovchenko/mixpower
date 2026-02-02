test_that("power computes proportion", {
  sims <- data.frame(p_value = c(0.01, 0.2, 0.03))
  expect_equal(power(sims, alpha = 0.05), 2 / 3)
})
