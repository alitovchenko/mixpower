# Diagnostics and sensitivity

This vignette shows how to assess simulation uncertainty and summarize
sensitivity analyses with
[`mp_sensitivity()`](../reference/mp_sensitivity.md).

``` r
library(mixpower)
```

``` r
d <- mp_design(clusters = list(subject = 20), trials_per_cell = 4)
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

sens <- mp_sensitivity(
  scn,
  vary = list(`fixed_effects.condition` = c(0.2, 0.4, 0.6)),
  nsim = 10,
  seed = 1
)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')

sens$results[, c("estimate", "mcse", "failure_rate", "singular_rate")]
#>   estimate       mcse failure_rate singular_rate
#> 1      0.1 0.09486833            0           0.3
#> 2      0.1 0.09486833            0           0.4
#> 3      0.6 0.15491933            0           0.4
```
