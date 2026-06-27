#' Recommend an inference method for a scenario
#'
#' Heuristic guidance on the `test_method` for a scenario, based on the number of
#' levels of the random grouping factors. Wald (normal-approximation z/t) tests
#' and, to a lesser extent, likelihood-ratio tests are known to be
#' anti-conservative when the number of clusters is small (Luke, 2017): the
#' degrees of freedom are overstated, so the test rejects too often. With few
#' clusters, a degrees-of-freedom-corrected test (Satterthwaite or Kenward-Roger,
#' for linear mixed models) or a parametric bootstrap (any family) controls Type
#' I error far better.
#'
#' This is a fast, design-based heuristic; to *measure* a specific design and
#' method, use [mp_calibrate()].
#'
#' @param scenario An `mp_scenario`.
#' @param small_clusters Threshold below which the smallest grouping factor is
#'   treated as "few clusters" (default 30).
#' @return An object of class `mp_recommendation`: a list with `method` (the
#'   scenario's current method), `n_groups` (smallest grouping-factor size),
#'   `is_lmm`, `caution` (logical), `recommended` (character vector), and
#'   `rationale`.
#' @seealso [mp_calibrate()].
#' @export
#' @examples
#' d <- mp_design(list(subject = 12), trials_per_cell = 8)
#' a <- mp_assumptions(
#'   fixed_effects = list("(Intercept)" = 0, condition = 0.4),
#'   random_effects = list(subject = list(intercept_sd = 0.5)),
#'   residual_sd = 1
#' )
#' scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
#' mp_recommend_method(scn)
mp_recommend_method <- function(scenario, small_clusters = 30L) {
  .assert_class(scenario, "mp_scenario", "scenario")

  clusters <- scenario$design$clusters
  n_groups <- if (length(clusters)) {
    min(vapply(clusters, function(v) as.integer(v[[1]]), integer(1)))
  } else {
    NA_integer_
  }

  method <- if (is.list(scenario$test)) {
    `%||%`(scenario$test$method, "wald")
  } else {
    scenario$test
  }
  is_lmm <- !is.null(scenario$assumptions$residual_sd)

  df_corrected <- c("satterthwaite", "kenward-roger")
  robust_few <- if (is_lmm) c("kenward-roger", "satterthwaite", "pb") else "pb"

  few <- !is.na(n_groups) && n_groups < small_clusters
  caution <- FALSE
  recommended <- character(0)
  rationale <- ""

  if (few && method %in% c("wald", "lrt")) {
    caution <- TRUE
    recommended <- robust_few
    rationale <- sprintf(
      paste0("Smallest grouping factor has %d levels (< %d). '%s' tests can be ",
             "anti-conservative with few clusters; prefer %s. Verify with mp_calibrate()."),
      n_groups, small_clusters, method,
      paste(sprintf("'%s'", robust_few), collapse = " or ")
    )
  } else if (few) {
    recommended <- method
    rationale <- sprintf(
      "Smallest grouping factor has %d levels; '%s' is an appropriate small-sample choice.",
      n_groups, method
    )
  } else {
    recommended <- method
    rationale <- sprintf(
      "Smallest grouping factor has %s levels; '%s' is reasonable. Confirm with mp_calibrate() if unsure.",
      ifelse(is.na(n_groups), "an unknown number of", as.character(n_groups)), method
    )
  }

  if (!is_lmm) {
    recommended <- setdiff(recommended, df_corrected)
    if (length(recommended) == 0L) recommended <- "pb"
  }

  out <- list(
    method = method,
    n_groups = n_groups,
    is_lmm = is_lmm,
    caution = caution,
    recommended = recommended,
    rationale = rationale
  )
  class(out) <- "mp_recommendation"
  out
}

# TRUE when a design is non-trivial enough that Type I error should be checked
# before trusting power: few clusters with a Wald/LRT test, or a complex
# random-effects structure (random slopes, >1 grouping factor, or nesting).
.mp_calibration_risk <- function(scenario) {
  re <- scenario$assumptions$random_effects
  has_slopes <- !is.null(re) &&
    any(vapply(re, function(g) !is.null(g$slopes) && length(g$slopes) > 0L, logical(1)))
  multi_group <- !is.null(re) && length(re) > 1L
  nested <- !is.null(scenario$design$nesting)
  rec <- tryCatch(mp_recommend_method(scenario), error = function(e) NULL)
  few_caution <- !is.null(rec) && isTRUE(rec$caution)
  has_slopes || multi_group || nested || few_caution
}

# Advice string when a design warrants a calibration check, else NULL. Used by
# mp_power() to nudge toward the calibrate-first workflow.
.mp_calibration_advice <- function(scenario) {
  if (!.mp_calibration_risk(scenario)) {
    return(NULL)
  }
  paste0(
    "This design has few clusters and/or a complex random-effects structure, ",
    "where Type I error is easy to get wrong. Check it with mp_calibrate() ",
    "(or run mp_plan(), which calibrates and powers together) and see ",
    "mp_recommend_method() for the inference method. A power estimate is only ",
    "trustworthy if the test holds its alpha."
  )
}

# Shared calibrate-first nudge for the power entry points. Returns the advice
# string (or NULL) and, when `check` is TRUE and no calibration is attached,
# warns at most once per session. Re-arm with
# `options(mixpower.calibration_nudged = FALSE)`.
.mp_nudge_calibration <- function(scenario, check) {
  advice <- .mp_calibration_advice(scenario)
  if (isTRUE(check) && !is.null(advice) && is.null(scenario$calibration)) {
    if (!isTRUE(getOption("mixpower.calibration_nudged", FALSE))) {
      warning(advice, call. = FALSE)
      options(mixpower.calibration_nudged = TRUE)
    }
  }
  advice
}

#' @export
print.mp_recommendation <- function(x, ...) {
  cat("<mp_recommendation>\n")
  cat(sprintf("  current method: %s\n", x$method))
  cat(sprintf("  smallest grouping factor: %s levels\n",
              ifelse(is.na(x$n_groups), "unknown", as.character(x$n_groups))))
  cat(sprintf("  caution: %s\n", if (isTRUE(x$caution)) "yes" else "no"))
  cat(sprintf("  recommended: %s\n", paste(x$recommended, collapse = ", ")))
  cat(sprintf("  %s\n", x$rationale))
  invisible(x)
}
