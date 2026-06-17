# Quick power run for a single LMM design

One-call wrapper that builds design, assumptions, scenario (lme4 LMM),
and runs [`mp_power()`](mp_power.md). Intended for the common case: one
fixed effect, one random intercept. All arguments are explicit; pass
further options to [`mp_power()`](mp_power.md) via `...` (e.g.
`failure_policy`, `conf_level`, `keep`).

## Usage

``` r
mp_quick_power(
  formula,
  clusters,
  trials_per_cell = 1,
  fixed_effects,
  residual_sd,
  nsim,
  alpha = 0.05,
  seed = NULL,
  random_effects = NULL,
  icc = NULL,
  test_method = c("wald", "lrt"),
  null_formula = NULL,
  predictor = "condition",
  subject = "subject",
  outcome = "y",
  item = NULL,
  ...
)
```

## Arguments

- formula:

  Model formula (e.g. `y ~ condition + (1 | subject)`).

- clusters:

  Named list of cluster sizes (e.g. `list(subject = 40)`).

- trials_per_cell:

  Number of observations per cell (default 1).

- fixed_effects:

  Named list of effect sizes (e.g. `list(condition = 0.3)`). Include
  intercept as `(Intercept)` if needed.

- residual_sd:

  Residual standard deviation.

- nsim:

  Number of simulations.

- alpha:

  Significance level (default 0.05).

- seed:

  Optional seed for reproducibility.

- random_effects:

  Optional named list of random-effect sizes, e.g.
  `list(subject = list(intercept_sd = 0.5))`. See
  [`mp_assumptions()`](mp_assumptions.md).

- icc:

  Deprecated; interpreted as a random-intercept SD. Use
  `random_effects`.

- test_method:

  `"wald"` (default) or `"lrt"`.

- null_formula:

  Required when `test_method = "lrt"` (e.g. `y ~ 1 + (1 | subject)`).

- predictor:

  Predictor column name (default `"condition"`).

- subject:

  Subject ID column name (default `"subject"`).

- outcome:

  Outcome column name (default `"y"`).

- item:

  Optional item ID column name.

- ...:

  Arguments passed to [`mp_power()`](mp_power.md) (e.g.
  `failure_policy`, `conf_level`, `keep`).

## Value

The result of [`mp_power()`](mp_power.md) (object of class `mp_power`).

## Examples

``` r
mp_quick_power(
  y ~ condition + (1 | subject),
  clusters = list(subject = 40),
  trials_per_cell = 8,
  fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
  residual_sd = 1,
  nsim = 50,
  seed = 123
)
#> <mp_power>
#>   nsim: 50
#>   alpha: 0.05
#>   failure_policy: count_as_nondetect
#>   power: 0.760
#>   mcse: 0.060
#>   95% CI (clopper-pearson): [0.618, 0.869]
#>   diagnostics:
#>     - fail_rate: 0.000
#>     - singular_rate: 0.000
#>     - type_s: 0.000
#>     - type_m: 1.13
```
