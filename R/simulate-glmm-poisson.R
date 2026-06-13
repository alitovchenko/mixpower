#' Simulate count outcome data for a Poisson GLMM with random intercepts
#' @param scenario An `mp_scenario` object.
#' @param predictor Predictor column name.
#' @param subject Subject ID column name.
#' @param outcome Outcome column name.
#' @param item Optional item ID column name.
#' @return A data.frame with outcome and predictors.
#' @export
simulate_glmm_poisson_data <- function(scenario,
                                       predictor = "condition",
                                       subject = "subject",
                                       outcome = "y",
                                       item = NULL) {
  design <- scenario$design
  assumptions <- scenario$assumptions

  n_subjects <- design$clusters$subject
  n_trials <- design$trials_per_cell

  if (is.null(n_subjects) || is.null(n_trials)) {
    stop("`design` must include `clusters$subject` and `trials_per_cell`.", call. = FALSE)
  }

  n <- n_subjects * n_trials
  x <- rep(c(0, 1), length.out = n)

  beta0 <- `%||%`(assumptions$fixed_effects[["(Intercept)"]], 0)
  beta1 <- `%||%`(assumptions$fixed_effects[[predictor]], 0)

  sd_subject <- .mp_re_intercept_sd(assumptions, subject, default = 0)
  re_subject <- if (sd_subject > 0) {
    stats::rnorm(n_subjects, mean = 0, sd = sd_subject)
  } else {
    rep(0, n_subjects)
  }

  subject_id <- rep(seq_len(n_subjects), each = n_trials)
  eta <- beta0 + beta1 * x + re_subject[subject_id]

  sd_item <- if (!is.null(item)) .mp_re_intercept_sd(assumptions, item, default = 0) else 0
  if (!is.null(item) && sd_item > 0) {
    n_items <- `%||%`(design$clusters$item, n_trials)
    item_id <- rep(seq_len(n_items), length.out = n)
    re_item <- stats::rnorm(n_items, mean = 0, sd = sd_item)
    eta <- eta + re_item[item_id]
  } else {
    item_id <- NULL
  }

  mu <- exp(eta)
  y <- stats::rpois(n, lambda = mu)

  dat <- data.frame(
    y = y,
    condition = x,
    subject = subject_id,
    stringsAsFactors = FALSE
  )

  if (!is.null(item_id)) {
    dat[[item]] <- item_id
  }

  names(dat)[names(dat) == "condition"] <- predictor
  names(dat)[names(dat) == "subject"] <- subject
  names(dat)[names(dat) == "y"] <- outcome

  dat
}
