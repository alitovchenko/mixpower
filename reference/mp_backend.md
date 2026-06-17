# MixPower backend contract

A **backend** is a list with three functions used by
[`mp_power()`](mp_power.md):

- `simulate_fun`:

  First argument receives the `mp_scenario` (often named `scenario`);
  optional `seed`. Must return a `data.frame`.

- `fit_fun`:

  Two arguments: simulated `data.frame`, then the `mp_scenario` (names
  may differ; [`mp_power()`](mp_power.md) passes them positionally).

- `test_fun`:

  Two arguments: fitted model, then the `mp_scenario`; returns
  `list(p_value = <numeric scalar>)`.

## Usage

``` r
mp_backend(
  simulate_fun,
  fit_fun,
  test_fun,
  name = "custom",
  version = NULL,
  notes = NULL,
  capabilities = NULL
)
```

## Arguments

- simulate_fun:

  A data.frame-generating simulator (see Details).

- fit_fun:

  Model fitter (see Details).

- test_fun:

  Extracts a scalar p-value (see Details).

- name:

  Short label for printing and manifests (default `"custom"`).

- version:

  Optional character version string for the backend implementation.

- notes:

  Optional longer notes.

- capabilities:

  Optional named list of flags (documentation only), e.g.
  `list(families = c("gaussian"), supports_lrt = TRUE)`.

## Value

An object of class `c("mp_backend", "list")` with the components above.

## Details

Use `mp_backend()` to build a validated object of class `mp_backend`.
Custom backends can also be plain lists with those three names;
[`validate_mp_backend()`](validate_mp_backend.md) checks the contract
without requiring the class.
