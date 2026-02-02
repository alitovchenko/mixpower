#' Sensitivity analysis for simulated results
#' @param results A data.frame with columns effect and power.
#' @param by Column name to aggregate by.
#' @return A data.frame of aggregated power.
#' @export
sensitivity <- function(results, by) {
  stats::aggregate(results$power, list(results[[by]]), mean)
}
