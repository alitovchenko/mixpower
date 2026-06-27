# Elicit assumptions from standardized scientific quantities

Build a coherent [`mp_assumptions()`](mp_assumptions.md) object from the
quantities researchers actually reason about — intraclass correlations,
a standardized effect, a baseline event probability and an odds/risk
ratio, and random-slope variability — instead of hand-picking raw
regression coefficients and standard deviations. `mp_elicit()` solves
for an internally consistent parameter set and prints the **implied
model**, combining mlmpower-style elicitation with mixpower's simulation
and calibration tooling.

## Usage

``` r
mp_elicit(
  family = c("gaussian", "binomial"),
  predictor = "condition",
  d = NULL,
  odds_ratio = NULL,
  risk_ratio = NULL,
  baseline_prob = NULL,
  outcome_sd = 1,
  icc = NULL,
  slope = NULL,
  cor = NULL,
  groups = "subject",
  intercept = NULL
)
```

## Arguments

- family:

  `"gaussian"` or `"binomial"`.

- predictor:

  Name of the focal predictor (default `"condition"`).

- d:

  Standardized effect (Cohen's d) for `"gaussian"`.

- odds_ratio, risk_ratio:

  Effect for `"binomial"` (give one).

- baseline_prob:

  Control-group event probability for `"binomial"`.

- outcome_sd:

  Total outcome SD for `"gaussian"` (default 1).

- icc:

  Intraclass correlation: a single value (applied to all `groups`) or a
  vector named by group. For `"gaussian"` the values must sum to `< 1`.

- slope:

  Random-slope SD as a fraction of the random-intercept SD: a single
  value or named by group (`NULL` for none).

- cor:

  Intercept-slope correlation: a single value or named by group.

- groups:

  Grouping-factor name(s) (default `"subject"`).

- intercept:

  Fixed intercept; defaults to 0 (gaussian) or `qlogis(baseline_prob)`
  (binomial).

## Value

An object of class `c("mp_elicitation", "mp_assumptions")` usable
anywhere [`mp_assumptions()`](mp_assumptions.md) is; its
[`print()`](https://rdrr.io/r/base/print.html) shows the elicited inputs
and the implied model.

## Details

For `family = "gaussian"` the total outcome variance is partitioned by
the per-group `icc` (each random-intercept variance is
`icc * outcome_sd^2`, the residual variance is the remainder), and the
fixed effect is `d * outcome_sd`. For `family = "binomial"` the
intercept is `qlogis(baseline_prob)`, the effect is `log(odds_ratio)` (a
`risk_ratio` is converted given `baseline_prob`), and each
random-intercept SD is derived from the latent-scale `icc`
(`residual variance = pi^2 / 3`). A random slope, when requested, is set
to `slope` times the random-intercept SD.

Predictor correlations, autocorrelation, and cluster-size distributions
are design choices (see [`mp_design()`](mp_design.md)); missingness is
[`mp_missing()`](mp_missing.md).

## See also

[`mp_assumptions()`](mp_assumptions.md),
[`mp_d_to_beta()`](effect_size.md), [`mp_icc_to_sd()`](effect_size.md).

## Examples

``` r
# Gaussian: medium effect, ICC = 0.1
mp_elicit("gaussian", d = 0.5, icc = 0.1)
#> <mp_elicitation> family = gaussian
#>   elicited:
#>     - d(condition) = 0.5; outcome_sd = 1
#>     - icc: subject = 0.1
#>   implied model:
#> <mp_assumptions>
#>   fixed_effects:
#>     - (Intercept): 0
#>     - condition: 0.5
#>   random_effects (SD on linear predictor):
#>     - subject: intercept_sd = 0.316228 
#>   residual_sd: 0.948683

# Binomial: 20% baseline event rate, odds ratio 1.8, subject ICC 0.05
mp_elicit("binomial", baseline_prob = 0.2, odds_ratio = 1.8, icc = 0.05)
#> <mp_elicitation> family = binomial
#>   elicited:
#>     - baseline_prob = 0.2; odds_ratio = 1.8
#>     - icc: subject = 0.05
#>   implied model:
#> <mp_assumptions>
#>   fixed_effects:
#>     - (Intercept): -1.38629
#>     - condition: 0.587787
#>   random_effects (SD on linear predictor):
#>     - subject: intercept_sd = 0.416114 
```
