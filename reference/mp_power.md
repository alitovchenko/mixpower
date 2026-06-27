# Simulation-based power estimation (engine-agnostic core)

`mp_power()` runs repeated simulations under a scenario and estimates
power for the scenario's test decision rule (typically p \< alpha).

## Usage

``` r
mp_power(
  scenario,
  nsim,
  alpha = 0.05,
  seed = NULL,
  failure_policy = c("count_as_nondetect", "exclude"),
  keep = c("minimal", "fits", "data"),
  conf_level = 0.95,
  ci_method = c("clopper-pearson", "wald"),
  aggregate = c("full", "streaming"),
  check_calibration = TRUE
)
```

## Arguments

- scenario:

  An `mp_scenario`.

- nsim:

  Positive integer number of simulations.

- alpha:

  Significance threshold for a detection (default 0.05).

- seed:

  Optional seed for reproducibility.

- failure_policy:

  How to treat failed fits / missing p-values:

  - `"count_as_nondetect"` (default): failures count as non-detections.

  - `"exclude"`: drop failures from the denominator (always reported).

- keep:

  What to store:

  - `"minimal"`: only per-sim summary rows.

  - `"fits"`: also store fit objects (may be large).

  - `"data"`: also store simulated data (can be very large).

- conf_level:

  Confidence level for the power interval (default 0.95).

- ci_method:

  Power CI type: `"clopper-pearson"` (default, exact binomial) or
  `"wald"` (normal approximation).

- aggregate:

  `"full"` (default) stores every replicate in `sims`. `"streaming"`
  accumulates counts only (lower memory); requires `keep = "minimal"`
  and returns an empty `sims` data frame with the same column names as
  the full run.

- check_calibration:

  If `TRUE` (default), warn once per session when the design is risky
  for Type I error (few clusters and/or complex random effects) and no
  calibration is on record, nudging toward
  [`mp_calibrate()`](mp_calibrate.md) / [`mp_plan()`](mp_plan.md). The
  advice is always available as `$calibration_advice` on the result. Set
  `FALSE` to silence (used internally by parameter sweeps).

## Value

An object of class `mp_power`.

## Details

In Phase 4 core, the scenario must provide engine functions:
`simulate_fun`, `fit_fun`, and `test_fun`. Later phases will supply
defaults based on specific backends (e.g., lme4).

Diagnostics include Type S (wrong-sign) and Type M (exaggeration-ratio)
errors among significant replicates (Gelman & Carlin, 2014), computed
when the tested term's true effect is known and non-zero and the backend
reports an estimate.

## Examples

``` r
# A tiny toy engine (not mixed models) just to demonstrate the workflow:
d <- mp_design(list(subject = 30), trials_per_cell = 1)
a <- mp_assumptions(list(condition = 0.3), residual_sd = 1)

sim_fun <- function(scn, seed) {
  n <- scn$design$clusters$subject
  x <- stats::rbinom(n, 1, 0.5)
  y <- scn$assumptions$fixed_effects$condition * x +
    stats::rnorm(n, sd = scn$assumptions$residual_sd)
  data.frame(y = y, condition = x)
}
fit_fun <- function(dat, scn) stats::lm(scn$formula, data = dat)
test_fun <- function(fit, scn) {
  sm <- summary(fit)
  p <- sm$coefficients["condition", "Pr(>|t|)"]
  list(p_value = as.numeric(p))
}

s <- mp_scenario(
  y ~ condition, d, a,
  simulate_fun = sim_fun,
  fit_fun = fit_fun,
  test_fun = test_fun
)
res <- mp_power(s, nsim = 50, seed = 1)
summary(res)
#> $power
#> [1] 0.06
#> 
#> $mcse
#> [1] 0.03358571
#> 
#> $ci
#> [1] 0.01254859 0.16548195
#> 
#> $ci_method
#> [1] "clopper-pearson"
#> 
#> $diagnostics
#> $diagnostics$fail_rate
#> [1] 0
#> 
#> $diagnostics$singular_rate
#> [1] 0
#> 
#> $diagnostics$type_s
#> [1] NA
#> 
#> $diagnostics$type_m
#> [1] NA
#> 
#> 
#> $nsim
#> [1] 50
#> 
#> $alpha
#> [1] 0.05
#> 
#> $failure_policy
#> [1] "count_as_nondetect"
#> 
#> $conf_level
#> [1] 0.95
#> 
```
