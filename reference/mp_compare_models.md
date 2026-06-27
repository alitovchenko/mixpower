# Compare analysis models on the same data: power *and* Type I error

Simulates data once per replicate from the first scenario, then fits and
tests *every* supplied scenario on that same dataset. Because the models
see identical data, differences in their rejection rates isolate the
effect of the analysis choice. This is the analogue of powerlmm's
`sim_formula`, raised to a planning question: **which analysis plan
controls Type I error AND has acceptable power under your plausible
data-generating process?**

## Usage

``` r
mp_compare_models(
  scenarios,
  nsim,
  alpha = 0.05,
  seed = NULL,
  conf_level = 0.95,
  failure_policy = c("count_as_nondetect", "exclude"),
  calibrate = TRUE
)
```

## Arguments

- scenarios:

  A named list of `mp_scenario` objects.

- nsim:

  Positive integer number of simulations (per pass).

- alpha:

  Significance threshold (default 0.05).

- seed:

  Optional seed for reproducibility.

- conf_level:

  Confidence level for the per-model intervals.

- failure_policy:

  How to treat failed fits (see [`mp_power()`](mp_power.md)).

- calibrate:

  Also estimate each model's Type I error under the null (default
  `TRUE`); doubles the simulation cost.

## Value

An object of class `mp_model_comparison` with a `results` data frame
(one row per model: `power`, `conf_low`, `conf_high`; and, when
`calibrate`, `type1`, `type1_low`, `type1_high`, `calibration`, `valid`;
plus `failure_rate`, `n_effective`, `nsim`) and `$recommended`, the
best-powered Type-I-valid model.

## Details

With `calibrate = TRUE` (default), each candidate model is evaluated
twice on shared data: once under the scenario's alternative (giving
**power**) and once under the null — the focal effect set to zero in the
data-generating scenario — giving its **Type I error rate** and a
calibration verdict. The recommended plan is the best-powered model that
still controls Type I error.

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
    fixed_effects = list("(Intercept)" = 0, condition = 0.5),
    random_effects = list(subject = list(intercept_sd = 0.5,
                                          slopes = list(condition = 0.8))),
    residual_sd = 1
  )
  maximal <- mp_scenario_lme4(y ~ condition + (1 + condition | subject), d, a)
  reduced <- mp_scenario_lme4(y ~ condition + (1 | subject), d, a)
  mp_compare_models(list(maximal = maximal, reduced = reduced),
                    nsim = 100, seed = 1)
}
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> <mp_model_comparison>
#>   nsim: 100 (per pass), alpha: 0.05
#>    model power  conf_low conf_high type1  type1_low type1_high
#>  maximal  0.77 0.6751413 0.8482684  0.06 0.02233489  0.1260299
#>  reduced  0.86 0.7762720 0.9212946  0.14 0.07870540  0.2237280
#>        calibration valid failure_rate n_effective nsim
#>    well-calibrated  TRUE            0         100  100
#>  anti-conservative FALSE            0         100  100
#>   recommended plan (Type-I-valid, best power): maximal
# }
```
