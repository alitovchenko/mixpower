#' Unified data-generating engine for mixed-model power simulation
#' @noRd

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Draw correlated random effects for one grouping factor.
#
# Returns list(intercept = <numeric n_levels>, slope = <numeric n_levels> | NULL).
# - intercept-only: one rnorm() draw (matches the historical behaviour).
# - intercept + slope: bivariate normal via Cholesky (no MASS/mvtnorm dep).
.mp_draw_group_re <- function(n_levels, intercept_sd, slope_sd = 0, cor = 0) {
  has_int <- !is.null(intercept_sd) && intercept_sd > 0
  has_slope <- !is.null(slope_sd) && slope_sd > 0

  if (!has_slope) {
    b0 <- if (has_int) stats::rnorm(n_levels, 0, intercept_sd) else rep(0, n_levels)
    return(list(intercept = b0, slope = NULL))
  }
  if (!has_int) {
    return(list(intercept = rep(0, n_levels),
                slope = stats::rnorm(n_levels, 0, slope_sd)))
  }

  rho <- max(min(cor, 1), -1)
  cov_is <- rho * intercept_sd * slope_sd
  sigma <- matrix(c(intercept_sd^2, cov_is, cov_is, slope_sd^2), nrow = 2)
  r <- tryCatch(chol(sigma), error = function(e) NULL)
  if (is.null(r)) {
    # Perfect (|rho| = 1) or numerically singular: draw as a linear combination.
    z <- stats::rnorm(n_levels)
    return(list(intercept = intercept_sd * z,
                slope = sign(if (rho == 0) 1 else rho) * slope_sd * z))
  }
  z <- matrix(stats::rnorm(2 * n_levels), nrow = n_levels, ncol = 2)
  b <- z %*% r # cov(b) = r'r = sigma
  list(intercept = b[, 1], slope = b[, 2])
}

# Family-specific response given the linear predictor `eta`.
.mp_family_outcome <- function(family, eta, assumptions, theta = NULL) {
  n <- length(eta)
  switch(family,
    gaussian = {
      resid_sd <- `%||%`(assumptions$residual_sd, 1)
      .assert_is_nonneg_num(resid_sd, "residual_sd")
      eta + stats::rnorm(n, mean = 0, sd = resid_sd)
    },
    binomial = {
      p <- pmin(pmax(stats::plogis(eta), 1e-6), 1 - 1e-6)
      stats::rbinom(n, size = 1, prob = p)
    },
    poisson = stats::rpois(n, lambda = exp(eta)),
    nbinom = {
      th <- `%||%`(theta, `%||%`(assumptions$theta, 1))
      if (!is.numeric(th) || length(th) != 1L || is.na(th) || th <= 0) {
        .stop("`theta` must be a positive numeric value.")
      }
      stats::rnbinom(n, size = th, mu = exp(eta))
    },
    .stop(sprintf("Unsupported family: %s", family))
  )
}

# Single data-generating process shared by every backend. Builds a balanced
# within-subject design, adds fixed effects, correlated random effects
# (intercept + optional slope on `predictor`) per grouping factor, and the
# family-specific response.
#
# `intercept_default` is the random-intercept SD used when neither
# `random_effects` nor `icc` specify one (1 for Gaussian so a `(1 | g)` term is
# not degenerate; 0 for GLMMs). `intercept_overrides` lets a backend force a
# group's intercept SD (used by the deprecated explicit `simulate_lmm_data`
# arguments).
.mp_simulate_mixed <- function(scenario,
                               family = c("gaussian", "binomial", "poisson", "nbinom"),
                               predictor = "condition",
                               subject = "subject",
                               outcome = "y",
                               item = NULL,
                               intercept_default = 0,
                               intercept_overrides = list(),
                               theta = NULL) {
  family <- match.arg(family)
  .assert_class(scenario, "mp_scenario", "scenario")

  des <- scenario$design
  asm <- scenario$assumptions

  n_subject <- des$clusters[[subject]]
  trials <- des$trials_per_cell
  if (is.null(n_subject) || is.null(trials)) {
    .stop(sprintf("Design must include clusters$%s and trials_per_cell.", subject))
  }
  total_n <- n_subject * trials

  beta <- asm$fixed_effects[[predictor]]
  if (is.null(beta) || !is.numeric(beta) || length(beta) != 1L || is.na(beta)) {
    .stop(sprintf("Assumptions must include a numeric scalar fixed_effects$%s.", predictor))
  }
  beta0 <- `%||%`(asm$fixed_effects[["(Intercept)"]], 0)

  # Balanced within-subject predictor so random slopes are estimable.
  subject_id <- rep(seq_len(n_subject), each = trials)
  x <- rep(rep(c(0, 1), length.out = trials), times = n_subject)

  dat <- data.frame(.id = subject_id, stringsAsFactors = FALSE)
  names(dat) <- subject
  dat[[predictor]] <- x

  eta <- beta0 + beta * x

  re_sub <- .mp_draw_group_re(
    n_subject,
    intercept_sd = `%||%`(intercept_overrides[[subject]],
                          .mp_re_intercept_sd(asm, subject, default = intercept_default)),
    slope_sd = .mp_re_slope_sd(asm, subject, predictor),
    cor = .mp_re_cor(asm, subject)
  )
  eta <- eta + re_sub$intercept[subject_id]
  if (!is.null(re_sub$slope)) eta <- eta + re_sub$slope[subject_id] * x

  if (!is.null(item)) {
    n_item <- des$clusters[[item]]
    if (is.null(n_item)) {
      .stop(sprintf("Design must include clusters$%s when item is specified.", item))
    }
    item_id <- rep(seq_len(n_item), length.out = total_n)
    re_item <- .mp_draw_group_re(
      n_item,
      intercept_sd = `%||%`(intercept_overrides[[item]],
                            .mp_re_intercept_sd(asm, item, default = intercept_default)),
      slope_sd = .mp_re_slope_sd(asm, item, predictor),
      cor = .mp_re_cor(asm, item)
    )
    eta <- eta + re_item$intercept[item_id]
    if (!is.null(re_item$slope)) eta <- eta + re_item$slope[item_id] * x
    dat[[item]] <- factor(item_id)
  }

  dat[[outcome]] <- .mp_family_outcome(family, eta, asm, theta)
  dat[[subject]] <- factor(dat[[subject]])
  dat
}
