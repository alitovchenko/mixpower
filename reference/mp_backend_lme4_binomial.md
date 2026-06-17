# Build an lme4 backend for binomial GLMM scenarios

Build an lme4 backend for binomial GLMM scenarios

## Usage

``` r
mp_backend_lme4_binomial(
  predictor = "condition",
  subject = "subject",
  outcome = "y",
  item = NULL,
  test_method = c("wald", "lrt", "pb"),
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

  Inference method: `"wald"` (default), `"lrt"`, or `"pb"`
  (parametric-bootstrap LRT via pbkrtest).

- null_formula:

  Null-model formula required for `"lrt"` and `"pb"`.

- pb_nsim:

  Bootstrap replicates for `test_method = "pb"` (default 100).

## Value

A list containing `simulate_fun`, `fit_fun`, and `test_fun`.
