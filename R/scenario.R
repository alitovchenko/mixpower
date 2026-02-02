#' Combine design and assumptions into a scenario
#' @param design A design object.
#' @param assumptions An assumptions object.
#' @return A scenario object.
#' @export
scenario <- function(design, assumptions) {
  structure(
    list(design = design, assumptions = assumptions),
    class = "mixpower_scenario"
  )
}
