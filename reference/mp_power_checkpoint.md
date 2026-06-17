# Resumable, checkpointed power simulation

Runs [`mp_power()`](mp_power.md) in batches, saving the accumulated
per-replicate results to `file` (an `.rds`) after each batch. If `file`
already exists for the same `seed`, the run resumes from where it left
off and only the remaining replicates are simulated. This makes very
large or long-running power analyses robust to interruption, and lets
you grow `nsim` later without recomputing finished replicates.

## Usage

``` r
mp_power_checkpoint(
  scenario,
  nsim,
  file,
  batch_size = 100,
  alpha = 0.05,
  seed = NULL,
  failure_policy = c("count_as_nondetect", "exclude"),
  conf_level = 0.95,
  ci_method = c("clopper-pearson", "wald"),
  progress = TRUE
)
```

## Arguments

- scenario:

  An `mp_scenario`.

- nsim:

  Total number of simulations desired.

- file:

  Path to the checkpoint `.rds` file.

- batch_size:

  Replicates per batch (default 100).

- alpha, seed, failure_policy, conf_level, ci_method:

  As in [`mp_power()`](mp_power.md). `seed` must be non-NULL for
  reproducible, resumable batches.

- progress:

  Emit a progress message after each batch (default `TRUE`).

## Value

An object of class `mp_power` for all `nsim` replicates.

## Details

Because replicate seeds are deterministic (`seed + i - 1`), the result
is identical to a single `mp_power(scenario, nsim, seed = seed)` call:
batching only changes when work happens, not what is computed.

## See also

[`mp_power()`](mp_power.md).

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  d <- mp_design(list(subject = 30), trials_per_cell = 6)
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.4),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  f <- tempfile(fileext = ".rds")
  mp_power_checkpoint(scn, nsim = 60, file = f, batch_size = 20, seed = 1)
}
#> mp_power_checkpoint: 20 / 60 replicates done
#> mp_power_checkpoint: 40 / 60 replicates done
#> mp_power_checkpoint: 60 / 60 replicates done
#> <mp_power>
#>   nsim: 60
#>   alpha: 0.05
#>   failure_policy: count_as_nondetect
#>   power: 0.700
#>   mcse: 0.059
#>   95% CI (clopper-pearson): [0.568, 0.812]
#>   diagnostics:
#>     - fail_rate: 0.000
#>     - singular_rate: 0.000
#>     - type_s: 0.000
#>     - type_m: 1.19
# }
```
