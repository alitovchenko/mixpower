# Validation: is your power number trustworthy?

A power estimate is only as trustworthy as the test it is built on. If a
design/analysis combination does not control its Type I error rate, a
high “power” number is meaningless — the test is just rejecting too
often, under the null as well as the alternative. mixpower makes this
check a first-class step.

``` r
library(mixpower)
```

## Calibrate: does the test hold its alpha?

[`mp_calibrate()`](../reference/mp_calibrate.md) runs the scenario under
the null (the focal effect set to zero) and estimates the empirical Type
I error rate, with an exact interval and a verdict. For a correctly
specified model it should sit near `alpha`.

``` r
d <- mp_design(clusters = list(subject = 35), trials_per_cell = 8)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
  random_effects = list(subject = list(intercept_sd = 0.5)),
  residual_sd = 1
)
scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)

mp_calibrate(scn, nsim = 200, seed = 11)
#> <mp_calibration>
#>   term:    condition (true effect set to 0)
#>   nominal alpha: 0.05
#>   empirical Type I: 0.0850  (95% CI 0.0503, 0.1326)
#>   verdict: anti-conservative
#>   -> This test rejects too often under the null; its power is not
#>      trustworthy. Try a df-corrected or bootstrap test, or a model
#>      that matches the data-generating random-effects structure.
```

### Catching a misspecified model

The classic trap: the data have a by-subject random slope, but the
analysis model omits it. This inflates Type I error (Barr et al., 2013).
[`mp_calibrate()`](../reference/mp_calibrate.md) flags it.

``` r
a_slope <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
  random_effects = list(subject = list(
    intercept_sd = 0.5, slopes = list(condition = 0.8)
  )),
  residual_sd = 1
)
# Data have the slope; the fitted model (1 | subject) ignores it.
scn_mis <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a_slope)

mp_calibrate(scn_mis, nsim = 200, seed = 7)
#> <mp_calibration>
#>   term:    condition (true effect set to 0)
#>   nominal alpha: 0.05
#>   empirical Type I: 0.1700  (95% CI 0.1207, 0.2294)
#>   verdict: anti-conservative
#>   -> This test rejects too often under the null; its power is not
#>      trustworthy. Try a df-corrected or bootstrap test, or a model
#>      that matches the data-generating random-effects structure.
```

The verdict turns to `anti-conservative`: any power computed from this
model would be inflated. The fix is to fit the maximal model
`(1 + condition | subject)`, which mixpower simulates and tests
directly.

## Recommend: which inference method?

Wald (z/t) tests are anti-conservative when the number of clusters is
small, because their degrees of freedom are overstated (Luke, 2017).
[`mp_recommend_method()`](../reference/mp_recommend_method.md) gives
fast, design-based guidance.

``` r
scn_few <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = mp_design(list(subject = 12), trials_per_cell = 8),
  assumptions = a
)
mp_recommend_method(scn_few)
#> <mp_recommendation>
#>   current method: wald
#>   smallest grouping factor: 12 levels
#>   caution: yes
#>   recommended: kenward-roger, satterthwaite, pb
#>   Smallest grouping factor has 12 levels (< 30). 'wald' tests can be anti-conservative with few clusters; prefer 'kenward-roger' or 'satterthwaite' or 'pb'. Verify with mp_calibrate().
```

With few clusters it steers you toward a degrees-of-freedom-corrected
test (Satterthwaite or Kenward-Roger for LMMs) or a parametric
bootstrap. You can then *measure* the improvement with
[`mp_calibrate()`](../reference/mp_calibrate.md):

``` r
scn_kr <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = mp_design(list(subject = 12), trials_per_cell = 8),
  assumptions = a, test_method = "kenward-roger"
)
mp_calibrate(scn_kr, nsim = 150, seed = 3)$type1
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> [1] 0.06666667
```

## Reporting

Calibration results drop into the same reporting layer as power results:

``` r
mp_report_table(mp_calibrate(scn, nsim = 150, seed = 1))
#>        term alpha      type1     ci_low   ci_high         verdict nsim
#> 1 condition  0.05 0.07333333 0.03717461 0.1274242 well-calibrated  150
```

Run [`mp_calibrate()`](../reference/mp_calibrate.md) whenever you change
the design size or the analysis model. A trustworthy power analysis
reports both the power and the calibration that backs it.
