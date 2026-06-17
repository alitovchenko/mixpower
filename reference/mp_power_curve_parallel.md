# Parallel power curve evaluation

Evaluates power over a one-parameter grid by running
[`mp_power()`](mp_power.md) for each grid cell in parallel. Uses
explicit per-cell seeds (seed + cell_index - 1L) so results are
deterministic and match serial [`mp_power_curve()`](mp_power_curve.md)
for the same seed. Does not modify [`mp_power()`](mp_power.md);
parallelization is at the scenario-grid level only.

## Usage

``` r
mp_power_curve_parallel(
  scenario,
  vary,
  workers = 2L,
  nsim = 100,
  alpha = 0.05,
  seed = NULL,
  failure_policy = c("count_as_nondetect", "exclude"),
  conf_level = 0.95,
  progress = FALSE,
  ...
)
```

## Arguments

- scenario:

  An `mp_scenario`.

- vary:

  Named list with exactly one parameter (e.g. `clusters.subject`).

- workers:

  Number of parallel workers (default 2).

- nsim:

  Number of simulations per grid point (default 100).

- alpha:

  Significance level (default 0.05).

- seed:

  Optional base seed; each cell gets `seed + cell_index - 1L`.

- failure_policy:

  How to treat failed fits: `"count_as_nondetect"` or `"exclude"`.

- conf_level:

  Confidence level for power intervals (default 0.95).

- progress:

  If `TRUE`, run serially with a progress bar; if `FALSE`, run in
  parallel.

- ...:

  Unused; reserved for future arguments.

## Value

An object of class `mp_power_curve` (same structure as
[`mp_power_curve()`](mp_power_curve.md)).

## Note

Parallel execution requires the parallel package (base R) and that
mixpower is installed (e.g.
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html) or
[`devtools::install()`](https://devtools.r-lib.org/reference/install.html))
so that workers can load it.
