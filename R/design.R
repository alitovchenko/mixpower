#' Define a study design
#' @param ... Named inputs describing the design.
#' @return A design object.
#' @export
design <- function(...) {
  structure(list(...), class = "mixpower_design")
}
