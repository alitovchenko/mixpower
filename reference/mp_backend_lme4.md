# Build an lme4 backend for MixPower scenarios

Build an lme4 backend for MixPower scenarios

## Usage

``` r
mp_backend_lme4(
  predictor = "condition",
  subject = "subject",
  outcome = "y",
  item = NULL,
  test_method = c("wald", "lrt", "satterthwaite", "kenward-roger", "pb"),
  null_formula = NULL,
  pb_nsim = 100L
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

- pb_nsim:

  Bootstrap replicates for `test_method = "pb"` (default 100). Note this
  multiplies cost: each power replicate refits the model `pb_nsim`
  times.

## Value

A list containing `simulate_fun`, `fit_fun`, and `test_fun`.
