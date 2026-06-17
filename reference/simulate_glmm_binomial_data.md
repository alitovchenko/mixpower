# Simulate binary outcome data for a GLMM with random effects

Thin wrapper over the shared simulation engine (`.mp_simulate_mixed`)
with a logit link and Bernoulli response. Random-effect sizes (intercept
and optional slope on `predictor`) come from
`scenario$assumptions$random_effects`.

## Usage

``` r
simulate_glmm_binomial_data(
  scenario,
  predictor = "condition",
  subject = "subject",
  outcome = "y",
  item = NULL
)
```

## Arguments

- scenario:

  An `mp_scenario` object.

- predictor:

  Predictor column name.

- subject:

  Subject ID column name.

- outcome:

  Outcome column name.

- item:

  Optional item ID column name.

## Value

A data.frame with outcome and predictors.
