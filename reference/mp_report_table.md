# Publication-ready summary table for power results

Returns a flat data frame with power estimate, CI, failure/singularity
rates, and effective simulation counts. Works with
[mp_power](mp_power.md), [mp_sensitivity](mp_sensitivity.md),
[mp_power_curve](mp_power_curve.md), or the result of
[`mp_bundle_results()`](mp_bundle_results.md) (uses the bundled result).

## Usage

``` r
mp_report_table(x, ...)
```

## Arguments

- x:

  An object of class `mp_power`, `mp_sensitivity`, `mp_power_curve`,
  `mp_calibration`, or from
  [`mp_bundle_results()`](mp_bundle_results.md).

- ...:

  Unused; reserved for future arguments.

## Value

A data frame: for `mp_power` one row; for `mp_calibration` a one-row
Type I summary; for sensitivity/curve one row per grid cell with
parameter column(s), `power_estimate`, `ci_low`, `ci_high`,
`failure_rate`, `singular_rate`, `n_effective`, `nsim`.
