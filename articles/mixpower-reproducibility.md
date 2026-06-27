# Reproducibility workflow

This vignette shows how to make power analyses reproducible: capture a
manifest (scenario fingerprint, seed, session info), bundle results with
metadata and labels, export to CSV/JSON, and use the manifest to
regenerate the same outputs.

``` r
library(mixpower)
```

## Run analysis and capture manifest

Run your analysis as usual and create a manifest from the same scenario
and seed you used. The manifest records a scenario digest (so you can
verify the same design/assumptions later), seed strategy, R and mixpower
versions, optional session info, and git SHA when in a repo.

``` r
d <- mp_design(clusters = list(subject = 30), trials_per_cell = 4)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
  residual_sd = 1,
  random_effects = list(subject = list(intercept_sd = 0.1))
)
scn <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "wald"
)

seed <- 123
res <- mp_power(scn, nsim = 12, seed = seed)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
manifest <- mp_manifest(scn, seed = seed, session = FALSE)
manifest
#> <mp_manifest>
#>   scenario_digest: 20466ce7e4ab4946 ...
#>   seed:123 (fixed)
#>   timestamp: 2026-06-27 01:28:20 UTC 
#>   r_version: 4.6.1 
#>   mixpower_version: 1.1.1 
#>   git_sha: 15f2f17
```

## Bundle results and export

Combine the result, manifest, and optional labels into a single bundle.
Then write a publication-ready table (and, for JSON, manifest and
labels) to disk.

``` r
bundle <- mp_bundle_results(
  res,
  manifest,
  study_id = "power_2024_01",
  analyst = "analyst",
  notes = "Initial power run for condition effect"
)
bundle
#> <mp_bundle>
#>   result: mp_power 
#>   study_id: power_2024_01 
#>   analyst: analyst 
#> <mp_manifest>
#>   scenario_digest: 20466ce7e4ab4946 ...
#>   seed:123 (fixed)
#>   timestamp: 2026-06-27 01:28:20 UTC 
#>   r_version: 4.6.1 
#>   mixpower_version: 1.1.1 
#>   git_sha: 15f2f17
```

``` r
tab <- mp_report_table(bundle)
tab
#>   power_estimate     ci_low   ci_high failure_rate singular_rate type_s
#> 1      0.1666667 0.02086253 0.4841377            0          0.25      0
#>     type_m n_effective nsim
#> 1 1.713297          12   12
```

``` r
mp_write_results(bundle, "power_results.csv", format = "csv", row.names = FALSE)
mp_write_results(bundle, "power_results.json", format = "json")
```

(Export is skipped in the vignette to avoid writing to the user’s
working directory.)

## Regenerating from manifest and seed

To reproduce the same run later:

1.  Restore the same scenario (design, assumptions, formula, test). The
    manifest’s `scenario_digest` is a fingerprint of formula, design,
    assumptions, and test; you can recompute it with
    `mp_manifest(scn, session = FALSE)$scenario_digest` and compare to
    the stored value.
2.  Use the same seed from the manifest:
    `mp_power(scn, nsim = 20, seed = manifest$seed)`.
3.  Use the same `nsim`, `alpha`, and `failure_policy` as in the
    original run (store these in your notes or in the bundle labels if
    needed).

Example: re-run with the stored seed and confirm the power estimate
matches.

``` r
res2 <- mp_power(scn, nsim = 12, seed = manifest$seed)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
all.equal(res$power, res2$power)
#> [1] TRUE
```

## One-row manifest for saving

You can flatten the manifest to a one-row data frame (e.g. to append to
a log) by building it from the list, omitting the long `session_info` if
desired:

``` r
m <- mp_manifest(scn, seed = 123, session = FALSE)
df_row <- data.frame(
  scenario_digest = m$scenario_digest,
  seed = m$seed,
  seed_strategy = m$seed_strategy,
  timestamp = m$timestamp,
  r_version = m$r_version,
  mixpower_version = m$mixpower_version,
  git_sha = m$git_sha,
  stringsAsFactors = FALSE
)
df_row
#>                                                    scenario_digest seed
#> 1 20466ce7e4ab49465c228d4ba24cd7c28e91a0b10f372a31584d17ac18f5325d  123
#>   seed_strategy               timestamp r_version mixpower_version
#> 1         fixed 2026-06-27 01:28:20 UTC     4.6.1            1.1.1
#>                                    git_sha
#> 1 15f2f1720c75224bafef9f0dc7a22547dabc9429
```
