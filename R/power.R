#' Summarize simulated power
#' @param simulations A data.frame with a p_value column.
#' @param alpha Significance threshold.
#' @return A numeric power estimate.
#' @export
power <- function(simulations, alpha = 0.05) {
  mean(simulations$p_value < alpha, na.rm = TRUE)
}
