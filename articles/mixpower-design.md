# Designing studies

This vignette outlines how to capture study designs with
[`mp_design()`](../reference/mp_design.md) and assumptions with
[`mp_assumptions()`](../reference/mp_assumptions.md).

``` r
library(mixpower)
```

``` r
design <- mp_design(
  clusters = list(subject = 40),
  trials_per_cell = 5,
  notes = "Baseline study layout"
)

assumptions <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
  residual_sd = 1,
  random_effects = list(subject = list(intercept_sd = 0.1))
)

design
#> <mp_design>
#>   clusters:
#>     - subject: 40
#>   trials_per_cell: 5
#>   notes: Baseline study layout
assumptions
#> <mp_assumptions>
#>   fixed_effects:
#>     - (Intercept): 0
#>     - condition: 0.3
#>   random_effects (SD on linear predictor):
#>     - subject: intercept_sd = 0.1 
#>   residual_sd: 1
```
