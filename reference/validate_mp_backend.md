# Validate a MixPower backend

Checks that `simulate_fun`, `fit_fun`, and `test_fun` are functions and
that their formal arguments are compatible with what
[`mp_power()`](mp_power.md) and `.run_one_rep()` invoke. Does not run
simulations.

## Usage

``` r
validate_mp_backend(engine)
```

## Arguments

- engine:

  A `mp_backend` object or a plain list with `simulate_fun`, `fit_fun`,
  and `test_fun`.

## Value

Invisibly `TRUE` if valid; otherwise throws an error.
