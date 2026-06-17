# Check the Type I error calibration of a scenario's test

Runs the scenario under the null hypothesis — the focal fixed effect set
to zero — and estimates the empirical Type I error rate, i.e. the
proportion of replicates in which the (false) effect is declared
significant. A trustworthy test rejects at approximately `alpha`. The
estimate is compared to `alpha` using an exact (Clopper-Pearson)
interval, giving a verdict:

## Usage

``` r
mp_calibrate(
  scenario,
  term = NULL,
  nsim = 1000,
  alpha = 0.05,
  seed = NULL,
  conf_level = 0.95,
  failure_policy = c("count_as_nondetect", "exclude")
)
```

## Arguments

- scenario:

  An `mp_scenario`.

- term:

  Focal fixed-effect term to null out. Defaults to the scenario's test
  term (or the first non-intercept fixed effect).

- nsim:

  Number of null simulations (default 1000; Type I estimation needs more
  replicates than a power point estimate for a tight interval).

- alpha:

  Nominal significance level being checked (default 0.05).

- seed:

  Optional seed for reproducibility.

- conf_level:

  Confidence level for the Type I interval (default 0.95).

- failure_policy:

  Passed to [`mp_power()`](mp_power.md).

## Value

An object of class `mp_calibration`: a list with `term`, `alpha`,
`type1` (empirical Type I rate), `ci`, `conf_level`, `mcse`, `nsim`,
`verdict`, and the underlying `power_result`.

## Details

- `"well-calibrated"`: the interval contains `alpha`.

- `"anti-conservative"`: the interval lies entirely above `alpha` (the
  test rejects too often — e.g. a Wald test with few clusters, or a
  model that omits a random slope that is actually present in the
  data-generating process). Power computed with such a test is not
  trustworthy.

- `"conservative"`: the interval lies entirely below `alpha`.

This is the recommended sanity check to run before trusting a power
number: if a design/analysis combination does not control Type I error,
its power is meaningless.

## See also

[`mp_recommend_method()`](mp_recommend_method.md),
[`mp_power()`](mp_power.md).

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  d <- mp_design(list(subject = 40), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.4),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  mp_calibrate(scn, nsim = 200, seed = 1)
}
#> <mp_calibration>
#>   term:    condition (true effect set to 0)
#>   nominal alpha: 0.05
#>   empirical Type I: 0.0550  (95% CI 0.0278, 0.0963)
#>   verdict: well-calibrated
# }
```
