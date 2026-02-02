test_that("simulate_power returns p-values", {
  sims <- simulate_power(scenario(design(), assumptions()), nsim = 5, seed = 123)
  expect_equal(nrow(sims), 5)
  expect_true(all(sims$p_value >= 0 & sims$p_value <= 1))
})
