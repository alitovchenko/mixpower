# Plan power the calibrate-first way

The recommended end-to-end workflow: **calibrate the null, choose an
inference method, then estimate power, and report both**. `mp_plan()`
runs [`mp_calibrate()`](mp_calibrate.md) and [`mp_power()`](mp_power.md)
on the same scenario and bundles them with an
[`mp_recommend_method()`](mp_recommend_method.md) suggestion, so power
is never reported without the Type I error check that tells you whether
it is trustworthy.

## Usage

``` r
mp_plan(
  scenario,
  nsim = 1000,
  calibrate_nsim = nsim,
  alpha = 0.05,
  seed = NULL,
  conf_level = 0.95,
  ci_method = c("clopper-pearson", "wald"),
  failure_policy = c("count_as_nondetect", "exclude")
)
```

## Arguments

- scenario:

  An `mp_scenario`.

- nsim:

  Simulations for the power estimate (default 1000).

- calibrate_nsim:

  Simulations for the null calibration (default `nsim`).

- alpha:

  Significance level (default 0.05).

- seed:

  Optional seed for reproducibility.

- conf_level:

  Confidence level for intervals (default 0.95).

- ci_method:

  Power CI type (see [`mp_power()`](mp_power.md)).

- failure_policy:

  Failure policy (see [`mp_power()`](mp_power.md)).

## Value

An object of class `mp_plan`: a list with `calibration`, `power`,
`recommendation`, `scenario`, and `alpha`.

## See also

[`mp_calibrate()`](mp_calibrate.md), [`mp_power()`](mp_power.md),
[`mp_recommend_method()`](mp_recommend_method.md).

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  d <- mp_design(list(subject = 12), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.4),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  mp_plan(scn, nsim = 100, seed = 1)
}
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> <mp_plan>
#>   alpha: 0.05
#>   1. calibration (null): Type I = 0.0800 (95% CI 0.0352, 0.1516) -> well-calibrated
#>   2. method: wald (consider kenward-roger, satterthwaite, pb)
#>   3. power: 45.0% (95% CI 35.0%, 55.3%)
# }
```
