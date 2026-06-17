# Gaussian LMM scenario using glmmTMB

Same data-generating process as
[`mp_scenario_lme4()`](mp_scenario_lme4.md) but fits with **glmmTMB**
(`glmmTMB()`).

## Usage

``` r
mp_scenario_glmmtmb_lmm(
  formula,
  design,
  assumptions,
  predictor = "condition",
  subject = "subject",
  outcome = "y",
  item = NULL,
  test_term = predictor,
  test_method = c("wald", "lrt"),
  null_formula = NULL
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
