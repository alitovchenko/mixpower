is_ci_fast <- function() identical(Sys.getenv("CI_FAST"), "true")

# TRUE only when glmmTMB is installed AND its compiled TMB matches the installed
# TMB. A mismatch (glmmTMB built against a different TMB ABI) yields unreliable
# fits, so numeric cross-engine comparisons should skip rather than fail.
glmmtmb_tmb_ok <- function() {
  if (!requireNamespace("glmmTMB", quietly = TRUE)) {
    return(FALSE)
  }
  ok <- tryCatch(
    suppressWarnings(glmmTMB:::check_dep_version(dep_pkg = "TMB")),
    error = function(e) NA
  )
  # If the internal check is unavailable (NA), don't block the test.
  isTRUE(ok) || is.na(ok)
}
