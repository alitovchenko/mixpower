#' Set a reproducible seed
#' @param seed Numeric seed.
#' @return The seed used.
#' @export
set_seed <- function(seed) {
  if (!is.numeric(seed) || length(seed) != 1) {
    stop("seed must be a numeric scalar", call. = FALSE)
  }
  set.seed(seed)
  seed
}
