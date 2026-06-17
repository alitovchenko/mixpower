# Create a power-analysis scenario

A scenario combines: (1) a design, (2) assumptions, (3) a model
specification, and (4) an analysis engine.

## Usage

``` r
mp_scenario(
  formula,
  design,
  assumptions,
  test = c("wald", "lrt", "custom"),
  simulate_fun = NULL,
  fit_fun = NULL,
  test_fun = NULL,
  notes = NULL
)
```

## Arguments

- formula:

  A model formula (stored for later backends).

- design:

  An `mp_design`.

- assumptions:

  An `mp_assumptions`.

- test:

  Character string or list identifying the test type (metadata).

- simulate_fun:

  Function or NULL.

- fit_fun:

  Function or NULL.

- test_fun:

  Function or NULL.

- notes:

  Optional free text.

## Value

An object of class `mp_scenario`.

## Details

In Phase 1, the engine is *pluggable* via three functions:

- `simulate_fun(scenario, seed)` returns a data.frame

- `fit_fun(data, scenario)` returns a fit object

- `test_fun(fit, scenario)` returns a list with at least `p_value`
  (numeric scalar)

This allows [`mp_power()`](mp_power.md) to run before selecting a
specific backend (e.g., lme4).

## Examples

``` r
d <- mp_design(list(subject = 20), trials_per_cell = 5)
a <- mp_assumptions(list(condition = 0.3), residual_sd = 1)
s <- mp_scenario(y ~ condition, d, a, test = "wald")
s
#> <mp_scenario>
#>   formula: y ~ condition
#>   test: wald
#>   engine:
#>     - simulate_fun: NULL
#>     - fit_fun: NULL
#>     - test_fun: NULL
```
