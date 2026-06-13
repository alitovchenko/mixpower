#' Internal helpers for the canonical random-effects specification
#' @noRd

# Canonical form stored on an `mp_assumptions` object:
#   assumptions$random_effects = list(
#     subject = list(intercept_sd = <non-negative numeric scalar>),
#     item    = list(intercept_sd = <non-negative numeric scalar>)
#   )
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

# Validate a user-supplied `random_effects` list. Phase A supports random
# intercepts only; `slope`/`cor` are reserved for a later phase and rejected
# now so users are never silently misled into thinking slopes are simulated.
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
    unknown <- setdiff(names(spec), "intercept_sd")
    if (length(unknown) > 0) {
      .stop(sprintf(
        "`random_effects$%s` has unsupported field(s): %s. Phase A supports `intercept_sd` only.",
        group, paste(unknown, collapse = ", ")
      ))
    }
    if (is.null(spec$intercept_sd)) {
      .stop(sprintf("`random_effects$%s` must include a numeric `intercept_sd`.", group))
    }
    .assert_is_nonneg_num(spec$intercept_sd, sprintf("random_effects$%s$intercept_sd", group))
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
