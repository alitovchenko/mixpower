# Adaptive (sequential) power simulation

Runs [`mp_power()`](mp_power.md) in batches and stops as soon as the
result is precise enough or the accept/reject decision is clear (see
[`mp_stop()`](mp_stop.md)), instead of relying on an arbitrary fixed
`nsim`. The achieved Monte Carlo precision (CI half-width) and the
number of replicates actually used are reported, so you simulate no more
than necessary.

## Usage

``` r
mp_power_adaptive(
  scenario,
  stop = mp_stop(),
  alpha = 0.05,
  seed = NULL,
  conf_level = 0.95,
  ci_method = c("clopper-pearson", "wald"),
  failure_policy = c("count_as_nondetect", "exclude"),
  check_calibration = TRUE,
  progress = FALSE
)
```

## Arguments

- scenario:

  An `mp_scenario`.

- stop:

  An [`mp_stop()`](mp_stop.md) specification.

- alpha, seed, conf_level, ci_method, failure_policy:

  As in [`mp_power()`](mp_power.md).

- check_calibration:

  As in [`mp_power()`](mp_power.md) (calibrate-first nudge); checked
  once at the start.

- progress:

  Emit a per-batch progress message (default `FALSE`).

## Value

An object of class `c("mp_power_adaptive", "mp_power")` — a standard
`mp_power` result plus a `$stopping` list (`converged`, `reason`,
`nsim_used`, `ci_halfwidth`, `target`, `rule`).

## Details

Because replicate seeds are deterministic (`seed + i - 1`), the result
for a given stopping point is reproducible and identical to a fixed
[`mp_power()`](mp_power.md) run at the same `nsim`.

## See also

[`mp_stop()`](mp_stop.md), [`mp_power()`](mp_power.md),
[`mp_power_checkpoint()`](mp_power_checkpoint.md).

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  d <- mp_design(list(subject = 30), trials_per_cell = 6)
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.5),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  mp_power_adaptive(scn, stop = mp_stop(ci_halfwidth = 0.05, batch = 50,
                                        min_nsim = 50, max_nsim = 400), seed = 1)
}
#> boundary (singular) fit: see help('isSingular')
#> <mp_power_adaptive>
#>   power: 88.0% (95% CI 82.7%, 92.2%), MCSE 0.0230
#>   nsim used: 200 -- converged (precision)
#>   achieved CI half-width: 0.0474 (target <= 0.0500)
# }
```
