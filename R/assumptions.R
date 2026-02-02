#' Define model assumptions
#' @param ... Named inputs describing distributional assumptions.
#' @return An assumptions object.
#' @export
assumptions <- function(...) {
  structure(list(...), class = "mixpower_assumptions")
}
