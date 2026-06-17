# Build a power scenario from a fitted lme4 model

Turns an existing `lmer`/`glmer` fit (e.g. from pilot or published data)
into an `mp_scenario`, so its estimated effects and variance components
inform a simulation-based power analysis. New responses are simulated
from the fitted model with
[`stats::simulate()`](https://rdrr.io/r/stats/simulate.html) (keeping
the estimated random-effect structure and residual variance), the model
is refit, and the focal term is tested.

## Usage

``` r
mp_from_fit(
  fit,
  test_term = NULL,
  test_method = NULL,
  null_formula = NULL,
  pb_nsim = 100L,
  extend = NULL
)
```

## Arguments

- fit:

  A fitted model of class `lmerMod`/`lmerModLmerTest` (Gaussian LMM) or
  `glmerMod` (binomial/Poisson/negative-binomial GLMM).

- test_term:

  Fixed-effect term to test. Defaults to the first non-intercept fixed
  effect.

- test_method:

  Inference method. Gaussian fits allow `"wald"` (default),
  `"satterthwaite"`, `"kenward-roger"`, `"lrt"`, `"pb"`; GLMM fits allow
  `"wald"`, `"lrt"`, `"pb"`.

- null_formula:

  Null-model formula required for `"lrt"`/`"pb"`. Defaults to the fitted
  formula with `test_term` removed.

- pb_nsim:

  Bootstrap replicates for `test_method = "pb"` (default 100).

- extend:

  Optional named list of target level counts per grouping factor (e.g.
  `list(Subject = 60)`) used to scale the pilot's sample size up or
  down. Levels are cloned from the pilot's structure with fresh ids and
  fresh random effects drawn from the fitted covariance. See
  [`mp_extend()`](mp_extend.md) and the `extend.<group>` sensitivity key
  for power curves over N.

## Value

An object of class `mp_scenario`.

## Details

The fixed effects used to simulate are read from the scenario's
assumptions, which start at the fitted coefficients. This means
[`mp_sensitivity()`](mp_sensitivity.md) /
[`mp_power_curve()`](mp_power_curve.md) can vary an effect size (e.g.
`fixed_effects.condition`) for data-based vs smallest-effect-of-interest
comparisons. Sample size can be scaled up or down from the pilot with
[`mp_extend()`](mp_extend.md) (or the `extend.<group>` sensitivity key),
which clones the pilot's structure with fresh levels and fresh random
effects.

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  scn <- mp_from_fit(m, test_term = "Days")
  mp_power(scn, nsim = 20, seed = 1)
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
#>     - type_m: 1.00
# }
```
