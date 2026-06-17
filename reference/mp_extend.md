# Scale a fitted-model scenario's sample size up or down

Sets target level counts for one or more grouping factors of a scenario
built with [`mp_from_fit()`](mp_from_fit.md), so power can be evaluated
at a sample size different from the pilot's. This is the analogue of
`simr::extend()`: levels are cloned from the pilot's within-level
structure with fresh ids, and fresh random effects are drawn from the
fitted covariance, so the extended data represent a larger (or smaller)
sample from the same population.

## Usage

``` r
mp_extend(scenario, ...)
```

## Arguments

- scenario:

  An `mp_scenario` created by [`mp_from_fit()`](mp_from_fit.md).

- ...:

  Named target level counts, e.g. `Subject = 60`.

## Value

The scenario with its `extend` targets set.

## Details

Pair with [`mp_power()`](mp_power.md) for a single N, or with
[`mp_power_curve()`](mp_power_curve.md) /
[`mp_solve_sample_size()`](mp_solve_sample_size.md) using the
`extend.<group>` key for a curve over N.

## See also

[`mp_from_fit()`](mp_from_fit.md).

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  scn <- mp_from_fit(m, test_term = "Days")
  big <- mp_extend(scn, Subject = 40)
  mp_power(big, nsim = 20, seed = 1)
}
#> <mp_power>
#>   nsim: 20
#>   alpha: 0.05
#>   failure_policy: count_as_nondetect
#>   power: 1.000
#>   mcse: 0.000
#>   95% CI (clopper-pearson): [0.832, 1.000]
#>   diagnostics:
#>     - fail_rate: 0.000
#>     - singular_rate: 0.000
#>     - type_s: 0.000
#>     - type_m: 0.99
# }
```
