# Build a full "trust report" for a power analysis

Assembles a single, reviewer-ready report from a power result: the
design, model, data-generating assumptions, inference method, Type I
calibration, power estimate with interval and Monte Carlo precision,
diagnostics (failure/singular-fit rates, Type S/M), missing-data
assumptions, reproducibility manifest (software versions, seed), and —
importantly — explicit warnings about fragile or unsupported claims. The
report is Markdown, so it drops straight into methods sections, grants,
and registered reports.

## Usage

``` r
mp_report(x, title = "Power analysis report", manifest = TRUE)
```

## Arguments

- x:

  An `mp_power` (incl. `mp_power_adaptive`) or [`mp_plan()`](mp_plan.md)
  result.

- title:

  Report title.

- manifest:

  Include a reproducibility manifest (default `TRUE`).

## Value

An object of class `mp_report` (a list with `markdown`, plus the parts:
`scenario`, `power`, `calibration`, `recommendation`, `warnings`,
`manifest`). Its [`print()`](https://rdrr.io/r/base/print.html) method
renders the Markdown.

## Details

This grows [`mp_methods_text()`](mp_methods_text.md) and the
reproducibility helpers into one artifact; pass the richer
[`mp_plan()`](mp_plan.md) result to include the calibration section
automatically.

## See also

[`mp_plan()`](mp_plan.md), [`mp_methods_text()`](mp_methods_text.md),
[`mp_write_report()`](mp_write_report.md).

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  d <- mp_design(list(subject = 12), trials_per_cell = 8)
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.4),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  mp_report(mp_plan(scn, nsim = 100, seed = 1))
}
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> # Power analysis report
#> 
#> _Generated 2026-06-27 03:26 UTC with mixpower 1.2.0._
#> 
#> ## Summary
#> 
#> A simulation-based power analysis was conducted using the mixpower R package (version 1.2.0). Data were simulated under the model y ~ condition + (1 | subject), with 12 subject (8 observations each). The focal effect (condition) was set to 0.4 and tested with a Wald z/t test at alpha = 0.05. Across 100 simulated datasets, estimated power was 45.0% (95% Clopper-Pearson confidence interval 35.0% to 55.3%). Among significant replicates the average exaggeration ratio (Type M) was 1.48.
#> 
#> ## Design and model
#> 
#> - model: `y ~ condition + (1 | subject)` (Gaussian linear mixed model)
#> - clusters: 12 subject; observations: 8 per subject
#> - test: wald on `condition`
#> 
#> ## Data-generating assumptions
#> 
#> - fixed effects: (Intercept) = 0, condition = 0.4
#> - random effects (SD on the linear-predictor scale):
#>     - subject: intercept_sd = 0.5
#> - residual SD: 1
#> 
#> ## Type I calibration
#> 
#> - empirical Type I at alpha = 0.05: **0.0800** (95% CI 0.0352, 0.1516) -> **well-calibrated**
#> 
#> - inference-method guidance: current `wald` (consider kenward-roger, satterthwaite, pb)
#> 
#> ## Power
#> 
#> - estimate: **45.0%** (95% clopper-pearson CI 35.0%, 55.3%)
#> - Monte Carlo SE: 0.0497; replicates: 100
#> 
#> ## Diagnostics
#> 
#> - model-fit failure rate: 0.0%
#> - singular-fit rate: 3.0%
#> - Type S (wrong sign): 0.0%
#> - Type M (exaggeration ratio): 1.48
#> 
#> ## Reproducibility
#> 
#> - seed: 1 (fixed)
#> - R 4.6.1, mixpower 1.2.0
#> - scenario digest: b0bc697328329bad
#> - git: 69cfbc82dd
#> 
#> ## Caveats and fragile claims
#> 
#> - The chosen inference method may be anti-conservative for this cluster count; consider kenward-roger, satterthwaite, pb.
#> - Statistically significant estimates exaggerate the effect by ~1.5x (Type M); the design is underpowered for unbiased estimation.
#> - The power estimate is imprecise (wide confidence interval); increase nsim or use mp_power_adaptive().
#> 
#> 
# }
```
