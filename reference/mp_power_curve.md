# Power curve for a single design/assumption parameter

Runs [`mp_power()`](mp_power.md) across a one-dimensional grid of values
for one parameter (e.g. cluster size) via
[`mp_sensitivity()`](mp_sensitivity.md). Results include power estimates
and per-grid-point diagnostics: failure rate, singular rate, and
effective N.

## Usage

``` r
mp_power_curve(
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

  An `mp_scenario`.

- vary:

  Named list with a single key (e.g. `clusters.subject`).

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

An object of class `mp_power_curve` with components `vary`, `grid`,
`results` (estimate, mcse, conf_low, conf_high, failure_rate,
singular_rate, n_effective, nsim, plus the varying parameter column),
`alpha`, `failure_policy`, and `conf_level`.
