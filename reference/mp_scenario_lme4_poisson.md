# Create a fully specified MixPower scenario with the Poisson lme4 backend

Create a fully specified MixPower scenario with the Poisson lme4 backend

## Usage

``` r
mp_scenario_lme4_poisson(
  formula,
  design,
  assumptions,
  predictor = "condition",
  subject = "subject",
  outcome = "y",
  item = NULL,
  test_term = predictor,
  test_method = c("wald", "lrt", "pb"),
  null_formula = NULL,
  pb_nsim = 100L
)
```

## Arguments

- formula:

  Model formula.

- design:

  A `mp_design` object.

- assumptions:

  A `mp_assumptions` object.

- predictor:

  Predictor column name.

- subject:

  Subject ID column name.

- outcome:

  Outcome column name.

- item:

  Optional item ID column name.

- test_term:

  Optional explicit term to test. Defaults to `predictor`.

- test_method:

  Inference method: `"wald"` (default), `"lrt"`, or `"pb"`.

- null_formula:

  Null-model formula required for `"lrt"` and `"pb"`.

- pb_nsim:

  Bootstrap replicates for `test_method = "pb"` (default 100).

## Value

An object of class `mp_scenario`.
