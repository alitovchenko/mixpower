# Create a fully specified MixPower scenario with the lme4 backend

Create a fully specified MixPower scenario with the lme4 backend

## Usage

``` r
mp_scenario_lme4(
  formula,
  design,
  assumptions,
  predictor = "condition",
  subject = "subject",
  outcome = "y",
  item = NULL,
  test_term = predictor,
  test_method = c("wald", "lrt", "satterthwaite", "kenward-roger", "pb"),
  null_formula = NULL,
  pb_nsim = 100L,
  contrast = NULL,
  simulate = NULL
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

  Term to test. A single fixed effect (default `predictor`), or a
  character vector of terms for an omnibus / multi-degree-of-freedom
  test (joint Wald for `"wald"`; for `"lrt"`/`"pb"` the `null_formula`
  defines the joint test).

- test_method:

  Inference method: `"wald"` (default), `"satterthwaite"`,
  `"kenward-roger"`, `"lrt"`, or `"pb"`. See
  [`mp_backend_lme4()`](mp_backend_lme4.md).

- null_formula:

  Null-model formula required for `"lrt"` and `"pb"`.

- pb_nsim:

  Bootstrap replicates for `test_method = "pb"` (default 100).

- contrast:

  Optional named numeric vector of fixed-effect weights defining a
  linear contrast `L'beta` to test (e.g. weights from `emmeans`). When
  supplied it overrides `test_term`/`test_method` with a Wald test of
  the contrast.

- simulate:

  Optional custom data-generating function
  `function(scenario, seed = NULL)` returning a data frame; overrides
  the built-in simulator (keeping the lme4 fit/test) for designs the
  grammar does not cover natively. Wrapped and validated by
  [`mp_custom_dgp()`](mp_custom_dgp.md).

## Value

An object of class `mp_scenario`.
