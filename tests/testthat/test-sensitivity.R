test_that("sensitivity aggregates power by group", {
  results <- data.frame(effect = c("a", "a", "b"), power = c(0.5, 0.7, 0.9))
  summary <- sensitivity(results, "effect")
  expect_equal(nrow(summary), 2)
})
