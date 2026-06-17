# Compare analysis models on the same simulated data

Simulates data once per replicate from the first scenario, then fits and
tests *every* supplied scenario on that same dataset. This is the
analogue of powerlmm's `sim_formula`: because the models see identical
data, differences in their rejection rates isolate the effect of the
analysis choice. Use it to study power across competing specifications,
or to expose Type I inflation from a misspecified model (e.g. dropping a
random slope that is present in the data-generating process).

## Usage

``` r
mp_compare_models(
  scenarios,
  nsim,
  alpha = 0.05,
  seed = NULL,
  conf_level = 0.95,
  failure_policy = c("count_as_nondetect", "exclude")
)
```

## Arguments

- scenarios:

  A named list of `mp_scenario` objects.

- nsim:

  Positive integer number of simulations.

- alpha:

  Significance threshold (default 0.05).

- seed:

  Optional seed for reproducibility.

- conf_level:

  Confidence level for the per-model power intervals.

- failure_policy:

  How to treat failed fits (see [`mp_power()`](mp_power.md)).

## Value

An object of class `mp_model_comparison` with a `results` data frame
(one row per model: `power`, `conf_low`, `conf_high`, `failure_rate`,
`n_effective`, `nsim`).

## Details

All scenarios must share the same data-generating process (design and
assumptions); they should differ only in their analysis model (formula /
random-effects structure / test). The first scenario's `simulate_fun`
drives data generation.

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  d <- mp_design(list(subject = 30), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0),
    random_effects = list(subject = list(intercept_sd = 0.5,
                                          slopes = list(condition = 0.8))),
    residual_sd = 1
  )
  maximal <- mp_scenario_lme4(y ~ condition + (1 + condition | subject), d, a)
  reduced <- mp_scenario_lme4(y ~ condition + (1 | subject), d, a)
  mp_compare_models(list(maximal = maximal, reduced = reduced),
                    nsim = 50, seed = 1)
}
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> <mp_model_comparison>
#>   nsim: 50, alpha: 0.05
#>    model power   conf_low conf_high failure_rate n_effective nsim
#>  maximal  0.06 0.01254859 0.1654819            0          50   50
#>  reduced  0.16 0.07170077 0.2911263            0          50   50
# }
```
