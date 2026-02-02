#' Validate that a required column exists
#' @param data A data.frame.
#' @param column Column name.
#' @return The input data.frame.
#' @export
validate_column <- function(data, column) {
  if (!column %in% names(data)) {
    stop("Missing required column: ", column, call. = FALSE)
  }
  data
}
