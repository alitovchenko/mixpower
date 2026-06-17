# Build a glmmTMB backend for Gaussian LMM scenarios

Fits with the **glmmTMB** function `glmmTMB()` (Gaussian). Useful for
comparing simulation-based power to
[`mp_backend_lme4()`](mp_backend_lme4.md) and for workflows that later
extend to families supported by glmmTMB but not lme4.

## Usage

``` r
mp_backend_glmmtmb(
  predictor = "condition",
  subject = "subject",
  outcome = "y",
  item = NULL,
  test_method = c("wald", "lrt"),
  null_formula = NULL
)
```

## Arguments

- predictor:

  Predictor column name.

- subject:

  Subject ID column name.

- outcome:

  Outcome column name.

- item:

  Optional item ID column name.

- test_method:

  Inference method: `"wald"` (normal-approximation z test, the fast
  default), `"satterthwaite"` or `"kenward-roger"` (df-corrected t tests
  via lmerTest/pbkrtest; recommended for small samples), `"lrt"`
  (likelihood-ratio test), or `"pb"` (parametric-bootstrap LRT via
  pbkrtest).

- null_formula:

  Null-model formula required for `"lrt"` and `"pb"`.

## Value

An object of class `mp_backend`.
