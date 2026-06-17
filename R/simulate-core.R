#' Unified data-generating engine for mixed-model power simulation
#' @noRd

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Draw a block of correlated random effects for one grouping factor.
#
# `sds` is a named vector of standard deviations over the declared terms
# (`(Intercept)`, then slope predictors); `R` is the matching correlation
# matrix. Returns an `n_levels` x `n_active_terms` matrix (columns named by the
# terms with positive SD), or NULL when no term has positive SD.
#
# A single active term reduces to one rnorm() draw (matching the historical
# intercept-only behaviour); multiple terms use a multivariate normal via the
# Cholesky factor (or an eigen square root for singular targets), so no
# MASS/mvtnorm dependency is needed.
.mp_draw_re_block <- function(n_levels, sds, R) {
  active <- which(sds > 0)
  if (length(active) == 0L) {
    return(NULL)
  }
  sds_a <- sds[active]
  terms_a <- names(sds_a)

  if (length(active) == 1L) {
    b <- stats::rnorm(n_levels, 0, sds_a[[1]])
    return(matrix(b, ncol = 1L, dimnames = list(NULL, terms_a)))
  }

  R_a <- R[active, active, drop = FALSE]
  d <- diag(sds_a, nrow = length(sds_a))
  sigma <- d %*% R_a %*% d
  z <- matrix(stats::rnorm(length(active) * n_levels), nrow = n_levels, ncol = length(active))
  r <- tryCatch(chol(sigma), error = function(e) NULL)
  b <- if (is.null(r)) {
    # Singular target (e.g. |cor| = 1): symmetric square root via eigen.
    e <- eigen(sigma, symmetric = TRUE)
    z %*% (e$vectors %*% diag(sqrt(pmax(e$values, 0)), nrow = length(e$values)) %*% t(e$vectors))
  } else {
    z %*% r # cov(b) = r'r = sigma
  }
  colnames(b) <- terms_a
  b
}

# Add a grouping factor's random-effect contribution to the linear predictor.
.mp_add_group_re <- function(assumptions, group, id, X, n_levels,
                             intercept_default, intercept_override) {
  spec <- .mp_re_block_spec(
    assumptions, group,
    available = colnames(X),
    intercept_default = intercept_default,
    intercept_override = intercept_override
  )
  b <- .mp_draw_re_block(n_levels, spec$sds, spec$R)
  if (is.null(b)) {
    return(0)
  }
  contrib <- numeric(length(id))
  for (tm in colnames(b)) {
    draws <- b[id, tm]
    contrib <- contrib + if (tm == "(Intercept)") draws else draws * X[, tm]
  }
  contrib
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

# Single data-generating process shared by every backend. Each non-intercept
# entry of `fixed_effects` becomes a balanced within-subject predictor column;
# with several predictors they are crossed via a binary counter (a full
# factorial when `trials_per_cell` is a multiple of 2^(#predictors)), which
# keeps a single predictor at the historical 0,1,0,1,... pattern. Correlated
# random effects (intercept + any declared slopes) are added per grouping
# factor before the family-specific response is drawn.
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

  fe <- asm$fixed_effects
  predictors <- setdiff(names(fe), "(Intercept)")
  if (!(predictor %in% predictors)) {
    .stop(sprintf("Assumptions must include a numeric scalar fixed_effects$%s.", predictor))
  }
  for (p in predictors) {
    v <- fe[[p]]
    if (!is.numeric(v) || length(v) != 1L || is.na(v)) {
      .stop(sprintf("`fixed_effects$%s` must be a numeric scalar.", p))
    }
  }
  beta0 <- `%||%`(fe[["(Intercept)"]], 0)

  subject_id <- rep(seq_len(n_subject), each = trials)
  pos <- rep(seq_len(trials) - 1L, times = n_subject) # 0-based within-subject index

  # Balanced predictors via a binary counter over the within-subject index.
  X <- matrix(0, nrow = total_n, ncol = length(predictors),
              dimnames = list(NULL, predictors))
  for (j in seq_along(predictors)) {
    X[, j] <- as.numeric(bitwAnd(bitwShiftR(pos, j - 1L), 1L))
  }

  dat <- data.frame(.id = subject_id, stringsAsFactors = FALSE)
  names(dat) <- subject
  for (p in predictors) dat[[p]] <- X[, p]

  beta_vec <- vapply(predictors, function(p) as.numeric(fe[[p]]), numeric(1))
  eta <- beta0 + as.vector(X %*% beta_vec)

  eta <- eta + .mp_add_group_re(
    asm, subject, subject_id, X, n_subject,
    intercept_default = intercept_default,
    intercept_override = intercept_overrides[[subject]]
  )

  if (!is.null(item)) {
    n_item <- des$clusters[[item]]
    if (is.null(n_item)) {
      .stop(sprintf("Design must include clusters$%s when item is specified.", item))
    }
    item_id <- rep(seq_len(n_item), length.out = total_n)
    eta <- eta + .mp_add_group_re(
      asm, item, item_id, X, n_item,
      intercept_default = intercept_default,
      intercept_override = intercept_overrides[[item]]
    )
    dat[[item]] <- factor(item_id)
  }

  dat[[outcome]] <- .mp_family_outcome(family, eta, asm, theta)
  dat[[subject]] <- factor(dat[[subject]])
  dat
}
