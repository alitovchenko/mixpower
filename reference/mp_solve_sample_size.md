# Solve for minimum sample size achieving target power

Evaluates power on a user-supplied grid of values for one parameter
(e.g. cluster size) via [`mp_power_curve()`](mp_power_curve.md), then
returns the smallest grid value whose power estimate meets or exceeds
the target. Diagnostics (failure rate, singular rate, n_effective) are
exposed in the returned `results` table.

## Usage

``` r
mp_solve_sample_size(
  scenario,
  parameter,
  grid,
  target_power = 0.8,
  nsim = 100,
  alpha = 0.05,
  seed = NULL,
  failure_policy = c("count_as_nondetect", "exclude"),
  conf_level = 0.95
)
```

## Arguments

- scenario:

  An `mp_scenario`.

- parameter:

  Dotted path of the single parameter to vary (e.g.
  `"clusters.subject"`).

- grid:

  Numeric vector of candidate values. Use
  [`mp_grid_sample_size()`](mp_grid_sample_size.md) to build a grid from
  bounds and either `length.out` or `by`.

- target_power:

  Target power threshold (default 0.8).

- nsim:

  Number of simulations per grid point (default 100).

- alpha:

  Significance level (default 0.05).

- seed:

  Optional seed for reproducibility.

- failure_policy:

  How to treat failed fits: `"count_as_nondetect"` or `"exclude"`.

- conf_level:

  Confidence level for power intervals (default 0.95).

## Value

A list with `target_power`, `parameter`, `solution` (numeric: minimum
grid value achieving target power, or `NA` if none), and `results` (data
frame with estimate, failure_rate, singular_rate, n_effective, etc., per
grid point).
