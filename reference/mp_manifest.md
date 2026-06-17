# Reproducibility manifest for power analyses

Captures scenario fingerprint, seed strategy, session info, timestamp,
and optional git SHA so results can be reproduced or audited. Output is
a plain list (and one-row data frame via
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)) suitable
for saving alongside results.

## Usage

``` r
mp_manifest(scenario, seed = NULL, session = TRUE)
```

## Arguments

- scenario:

  An `mp_scenario` object (used for digest).

- seed:

  The seed value used (or `NULL`). Stored as-is; strategy is inferred as
  `"fixed"` if non-null else `"none"`.

- session:

  Include full
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) (default
  `TRUE`). If `FALSE`, only R version and mixpower version are stored.

## Value

A list with components: `scenario_digest`, `seed`, `seed_strategy`,
`timestamp`, `r_version`, `mixpower_version`, `session_info` (if
requested), `git_sha` (if in a git repo). Use
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on the
list for a single-row table (list components become columns where
possible).
