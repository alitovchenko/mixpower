# Run power sensitivity analysis over a parameter grid

Run power sensitivity analysis over a parameter grid

## Usage

``` r
mp_sensitivity(
  scenario,
  vary,
  nsim = 100,
  alpha = 0.05,
  seed = NULL,
  failure_policy = c("count_as_nondetect", "exclude"),
  conf_level = 0.95
)
```

## Arguments

- scenario:

  A base `mp_scenario` object.

- vary:

  Named list of vectors. Names are dotted paths such as
  `"fixed_effects.condition"` or `"clusters.subject"`.

- nsim:

  Number of simulations for each grid cell.

- alpha:

  Significance threshold.

- seed:

  Optional seed for reproducible cell-wise execution.

- failure_policy:

  Failure policy passed to [`mp_power()`](mp_power.md).

- conf_level:

  Confidence level passed to [`mp_power()`](mp_power.md).

## Value

An object of class `mp_sensitivity`.
