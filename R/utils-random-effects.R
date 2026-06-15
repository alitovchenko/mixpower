#' Internal helpers for the canonical random-effects specification
#' @noRd

# Canonical form stored on an `mp_assumptions` object:
#   assumptions$random_effects = list(
#     subject = list(
#       intercept_sd = <non-negative numeric scalar>,
#       slopes       = list(<predictor> = <non-negative numeric scalar>),  # optional
#       cor          = <intercept-slope correlation in [-1, 1]>            # optional
#     ),
#     item = list(intercept_sd = ...)
#   )
#
# SDs are on the linear-predictor scale (what lme4 reports). A single random
# slope per grouping factor is supported (the canonical "maximal" case for one
# within-unit factor); multiple correlated slopes are future work.
#
# The legacy `icc` field (a named list interpreted, incorrectly, as a
# random-intercept SD) is still honoured as a fallback so that code written
# against mixpower <= 0.1.0 keeps working. New code should use
# `random_effects`. Resolution order: `random_effects` first, then legacy
# `icc`, then the supplied default.

.mp_re_intercept_sd <- function(assumptions, group, default = NULL) {
  re <- assumptions$random_effects
  if (!is.null(re) && !is.null(re[[group]]) && !is.null(re[[group]]$intercept_sd)) {
    return(re[[group]]$intercept_sd)
  }
  if (!is.null(assumptions$icc) && !is.null(assumptions$icc[[group]])) {
    return(assumptions$icc[[group]])
  }
  default
}

# SD of the random slope on `predictor` for `group` (0 if none specified).
.mp_re_slope_sd <- function(assumptions, group, predictor) {
  slopes <- assumptions$random_effects[[group]]$slopes
  if (is.null(slopes) || is.null(slopes[[predictor]])) {
    return(0)
  }
  slopes[[predictor]]
}

# Intercept-slope correlation for `group` (0 if unspecified).
.mp_re_cor <- function(assumptions, group) {
  `%||%`(assumptions$random_effects[[group]]$cor, 0)
}

# Validate a user-supplied `random_effects` list. Supports `intercept_sd`,
# an optional single `slopes` entry (named by predictor), and an optional
# intercept-slope correlation `cor`.
.mp_validate_random_effects <- function(random_effects) {
  if (is.null(random_effects)) {
    return(invisible(NULL))
  }
  .assert_named_list(random_effects, "random_effects")
  for (group in names(random_effects)) {
    spec <- random_effects[[group]]
    if (!is.list(spec) || is.null(names(spec)) || any(names(spec) == "")) {
      .stop(sprintf("`random_effects$%s` must be a named list, e.g. list(intercept_sd = 0.5).", group))
    }
    unknown <- setdiff(names(spec), c("intercept_sd", "slopes", "cor"))
    if (length(unknown) > 0) {
      .stop(sprintf(
        "`random_effects$%s` has unsupported field(s): %s. Supported: intercept_sd, slopes, cor.",
        group, paste(unknown, collapse = ", ")
      ))
    }
    if (is.null(spec$intercept_sd)) {
      .stop(sprintf("`random_effects$%s` must include a numeric `intercept_sd`.", group))
    }
    .assert_is_nonneg_num(spec$intercept_sd, sprintf("random_effects$%s$intercept_sd", group))

    if (!is.null(spec$slopes)) {
      .assert_named_list(spec$slopes, sprintf("random_effects$%s$slopes", group))
      if (length(spec$slopes) > 1L) {
        .stop(sprintf(
          "`random_effects$%s$slopes` supports a single random slope; got %d. Multiple correlated slopes are not yet supported.",
          group, length(spec$slopes)
        ))
      }
      for (p in names(spec$slopes)) {
        .assert_is_nonneg_num(spec$slopes[[p]], sprintf("random_effects$%s$slopes$%s", group, p))
      }
    }

    if (!is.null(spec$cor)) {
      .assert_is_num(spec$cor, sprintf("random_effects$%s$cor", group))
      if (spec$cor < -1 || spec$cor > 1) {
        .stop(sprintf("`random_effects$%s$cor` must be in [-1, 1].", group))
      }
    }
  }
  invisible(NULL)
}

# Fold a legacy `icc` list into the canonical `random_effects` form, warning
# once per session. Returns the merged `random_effects` list.
.mp_absorb_icc <- function(random_effects, icc) {
  if (is.null(icc)) {
    return(random_effects)
  }
  if (!isTRUE(getOption("mixpower.icc_deprecation_warned", FALSE))) {
    warning(
      "`icc` is deprecated and is interpreted as the random-intercept SD ",
      "(not an intraclass correlation). Use `random_effects` instead, e.g. ",
      "random_effects = list(subject = list(intercept_sd = 0.5)).",
      call. = FALSE
    )
    options(mixpower.icc_deprecation_warned = TRUE)
  }
  if (is.null(random_effects)) random_effects <- list()
  for (group in names(icc)) {
    if (is.null(random_effects[[group]]$intercept_sd)) {
      random_effects[[group]] <- list(intercept_sd = icc[[group]])
    }
  }
  random_effects
}
