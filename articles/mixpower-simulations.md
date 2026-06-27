# Running simulations

This vignette describes the [`mp_power()`](../reference/mp_power.md)
workflow and how to summarize results with
[`summary()`](https://rdrr.io/r/base/summary.html).

``` r
library(mixpower)
```

``` r
d <- mp_design(clusters = list(subject = 20), trials_per_cell = 4)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
  residual_sd = 1,
  random_effects = list(subject = list(intercept_sd = 0.1))
)

scn <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "wald"
)

res <- mp_power(scn, nsim = 10, seed = 42)
#> Warning: This design has few clusters and/or a complex random-effects
#> structure, where Type I error is easy to get wrong. Check it with
#> mp_calibrate() (or run mp_plan(), which calibrates and powers together) and see
#> mp_recommend_method() for the inference method. A power estimate is only
#> trustworthy if the test holds its alpha.
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
summary(res)
#> $power
#> [1] 0.3
#> 
#> $mcse
#> [1] 0.1449138
#> 
#> $ci
#> [1] 0.06673951 0.65245285
#> 
#> $ci_method
#> [1] "clopper-pearson"
#> 
#> $diagnostics
#> $diagnostics$fail_rate
#> [1] 0
#> 
#> $diagnostics$singular_rate
#> [1] 0.5
#> 
#> $diagnostics$type_s
#> [1] 0
#> 
#> $diagnostics$type_m
#> [1] 1.562924
#> 
#> 
#> $nsim
#> [1] 10
#> 
#> $alpha
#> [1] 0.05
#> 
#> $failure_policy
#> [1] "count_as_nondetect"
#> 
#> $conf_level
#> [1] 0.95
```
