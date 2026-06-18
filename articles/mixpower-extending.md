# Extending mixpower with custom backends

This vignette describes the **backend contract** used by
[`mp_power()`](../reference/mp_power.md) so you can plug in custom
simulators, fitters, and tests without modifying the core loop.

## The three-function contract

An `mp_scenario` stores an `engine` with:

1.  **`simulate_fun(scenario, seed = NULL)`** — returns a `data.frame`
    of simulated data.  
    [`mp_power()`](../reference/mp_power.md) passes `seed` when the
    function accepts it (see [`?mp_power`](../reference/mp_power.md)).

2.  **`fit_fun(data, scenario)`** — returns a fitted model object.

3.  **`test_fun(fit, scenario)`** — returns
    `list(p_value = <scalar>)`.  
    Use `NA_real_` for `p_value` when the test cannot be computed.

[`mp_power()`](../reference/mp_power.md) records `fit_ok`, optional
`singular` from `attr(fit, "singular")`, and aggregates power using
`alpha` and `failure_policy`.

## Building a validated backend with `mp_backend()`

Use [`mp_backend()`](../reference/mp_backend.md) to wrap the three
functions with a name and optional metadata.  
[`validate_mp_backend()`](../reference/validate_mp_backend.md) checks
formal argument names before you run simulations:

``` r
library(mixpower)

sim_fun <- function(scenario, seed = NULL) {
  n <- scenario$design$clusters$subject
  x <- stats::rbinom(n, 1, 0.5)
  y <- scenario$assumptions$fixed_effects$condition * x +
    stats::rnorm(n, sd = scenario$assumptions$residual_sd)
  data.frame(y = y, condition = x)
}
fit_fun <- function(data, scenario) stats::lm(scenario$formula, data = data)
test_fun <- function(fit, scenario) {
  sm <- summary(fit)
  p <- sm$coefficients["condition", "Pr(>|t|)"]
  list(p_value = as.numeric(p))
}

eng <- mp_backend(sim_fun, fit_fun, test_fun, name = "toy_lm")
eng
#> <mp_backend>
#>   name: toy_lm 
#>   simulate_fun: list(scenario = ) ...
```

## Using `mp_scenario()` and `mp_power()`

``` r
d <- mp_design(list(subject = 25), trials_per_cell = 1)
a <- mp_assumptions(list(`(Intercept)` = 0, condition = 0.2), residual_sd = 1)
scn <- mp_scenario(
  y ~ condition, d, a,
  test = "custom",
  simulate_fun = eng$simulate_fun,
  fit_fun = eng$fit_fun,
  test_fun = eng$test_fun
)
mp_power(scn, nsim = 12, seed = 1)
#> <mp_power>
#>   nsim: 12
#>   alpha: 0.05
#>   failure_policy: count_as_nondetect
#>   power: 0.000
#>   mcse: 0.000
#>   95% CI (clopper-pearson): [0.000, 0.265]
#>   diagnostics:
#>     - fail_rate: 0.000
#>     - singular_rate: 0.000
#>     - type_s: NA
#>     - type_m: NA
```

## Built-in backends

- **lme4**: [`mp_backend_lme4()`](../reference/mp_backend_lme4.md),
  [`mp_backend_lme4_binomial()`](../reference/mp_backend_lme4_binomial.md),
  etc., return `mp_backend` objects.  
  Scenario helpers
  ([`mp_scenario_lme4()`](../reference/mp_scenario_lme4.md), …) copy the
  three functions into the scenario.

- **glmmTMB** (optional):
  [`mp_backend_glmmtmb()`](../reference/mp_backend_glmmtmb.md) when
  package `glmmTMB` is installed — see
  [`?mp_backend_glmmtmb`](../reference/mp_backend_glmmtmb.md).

## Parallel grids

Sensitivity and power-curve grids can be evaluated in parallel
**outside** [`mp_power()`](../reference/mp_power.md)
(e.g. [`mp_sensitivity_parallel()`](../reference/mp_sensitivity_parallel.md),
[`mp_power_curve_parallel()`](../reference/mp_power_curve_parallel.md))
using per-cell seeds `seed + cell_index - 1L` for reproducibility.

## Further reading

- [`?mp_backend`](../reference/mp_backend.md),
  [`?validate_mp_backend`](../reference/validate_mp_backend.md),
  [`?mp_scenario`](../reference/mp_scenario.md),
  [`?mp_power`](../reference/mp_power.md)
- Vignette `mixpower-intro` for Wald vs LRT and design-first workflows.
