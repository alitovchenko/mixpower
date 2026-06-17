# Parallel sensitivity analysis over a parameter grid

Like [`mp_sensitivity()`](mp_sensitivity.md), but evaluates each grid
cell in parallel (or with a progress bar when `progress = TRUE`). Uses
per-cell seeds `seed + cell_index - 1L` to match a serial ordering
convention. Does not modify [`mp_power()`](mp_power.md).

## Usage

``` r
mp_sensitivity_parallel(
  scenario,
  vary,
  nsim = 100,
  alpha = 0.05,
  seed = NULL,
  failure_policy = c("count_as_nondetect", "exclude"),
  conf_level = 0.95,
  workers = 2L,
  progress = FALSE,
  checkpoint_dir = NULL,
  resume = TRUE,
  ...
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

- workers:

  Number of parallel workers when `progress = FALSE` (default 2).

- progress:

  If `TRUE`, run serially with a text progress bar.

- checkpoint_dir:

  Optional directory to save per-cell RDS results and a manifest. When
  `resume = TRUE`, existing cell files are reused if the manifest
  matches the current run. Use a path on **shared storage** if
  `workers > 1`.

- resume:

  Logical; only used when `checkpoint_dir` is set.

- ...:

  Reserved.

## Value

An object of class `mp_sensitivity` (same structure as
[`mp_sensitivity()`](mp_sensitivity.md)).

## Note

Parallel execution requires the parallel package and that mixpower can
be loaded on workers (installed package).
