# mixpower intro

``` r
library(mixpower)
```

mixpower provides simulation-based power analysis for Gaussian linear
mixed-effects models.

## Inference options: Wald vs LRT

MixPower supports two inferential paths in the `lme4` backend:

- `"wald"` (default): coefficient z-test from fixed-effect estimate and
  its SE.
- `"lrt"`: likelihood-ratio test between explicit null and full models.

For LRT, you must provide `null_formula` explicitly. This avoids hidden
model-reduction rules and keeps assumptions transparent.

``` r
d <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
  residual_sd = 1,
  random_effects = list(subject = list(intercept_sd = 0.1))
)

scn_wald <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "wald"
)

scn_lrt <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "lrt",
  null_formula = y ~ 1 + (1 | subject)
)

vary_spec <- list(`clusters.subject` = c(30, 50, 80))

sens_wald <- mp_sensitivity(
  scn_wald,
  vary = vary_spec,
  nsim = 50,
  seed = 123
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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')

sens_lrt <- mp_sensitivity(
  scn_lrt,
  vary = vary_spec,
  nsim = 50,
  seed = 123
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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')

comparison <- rbind(
  transform(sens_wald$results, method = "wald"),
  transform(sens_lrt$results, method = "lrt")
)

comparison[, c(
  "method", "clusters.subject", "estimate", "mcse",
  "conf_low", "conf_high", "failure_rate", "singular_rate"
)]
#>   method clusters.subject estimate       mcse  conf_low conf_high failure_rate
#> 1   wald               30     0.78 0.05858327 0.6403881 0.8847342            0
#> 2   wald               50     0.98 0.01979899 0.8935305 0.9994938            0
#> 3   wald               80     1.00 0.00000000 0.9288783 1.0000000            0
#> 4    lrt               30     0.78 0.05858327 0.6403881 0.8847342            0
#> 5    lrt               50     0.98 0.01979899 0.8935305 0.9994938            0
#> 6    lrt               80     1.00 0.00000000 0.9288783 1.0000000            0
#>   singular_rate
#> 1          0.38
#> 2          0.48
#> 3          0.34
#> 4          0.38
#> 5          0.48
#> 6          0.34

wald_dat <- comparison[comparison$method == "wald", ]
lrt_dat  <- comparison[comparison$method == "lrt", ]

plot(
  wald_dat$`clusters.subject`, wald_dat$estimate,
  type = "b", pch = 16, lty = 1,
  ylim = c(0, 1),
  xlab = "clusters.subject",
  ylab = "Power estimate",
  col = "steelblue"
)
lines(
  lrt_dat$`clusters.subject`, lrt_dat$estimate,
  type = "b", pch = 17, lty = 2,
  col = "firebrick"
)
legend(
  "bottomright",
  legend = c("Wald", "LRT"),
  col = c("steelblue", "firebrick"),
  lty = c(1, 2), pch = c(16, 17), bty = "n"
)
```

![](mixpower-intro_files/figure-html/unnamed-chunk-2-1.png)

Interpretation notes:

- Differences in `estimate` reflect inferential method sensitivity under
  the same DGP.
- Compare `failure_rate` and `singular_rate` alongside power; higher
  failures can depress usable inference.
- Keep `nsim` modest in examples, and increase in final study planning
  runs.
- Prefer explicit `null_formula` documentation in reports for LRT
  reproducibility.

## Binary outcomes (binomial GLMM)

MixPower supports binomial GLMM power analysis via the
[`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) backend. This
keeps design assumptions explicit and diagnostics visible.

Extremely large effect sizes can lead to quasi-separation. In those
cases, [`glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) may warn or
return unstable estimates. MixPower records these as failed simulations
(via `NA` p-values), preserving transparency.

``` r
d <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.5),
  residual_sd = 1,
  random_effects = list(subject = list(intercept_sd = 0.4))
)

scn_bin <- mp_scenario_lme4_binomial(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "wald"
)

res_bin <- mp_power(scn_bin, nsim = 50, seed = 123)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
summary(res_bin)
#> $power
#> [1] 0.62
#> 
#> $mcse
#> [1] 0.06864401
#> 
#> $ci
#> [1] 0.4717492 0.7534989
#> 
#> $ci_method
#> [1] "clopper-pearson"
#> 
#> $diagnostics
#> $diagnostics$fail_rate
#> [1] 0
#> 
#> $diagnostics$singular_rate
#> [1] 0.14
#> 
#> $diagnostics$type_s
#> [1] 0
#> 
#> $diagnostics$type_m
#> [1] 1.338776
#> 
#> 
#> $nsim
#> [1] 50
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

## Sensitivity curve for binomial outcomes

``` r
sens_bin <- mp_sensitivity(
  scn_bin,
  vary = list(`fixed_effects.condition` = c(0.2, 0.4, 0.6)),
  nsim = 50,
  seed = 123
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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')

plot(sens_bin)
```

![](mixpower-intro_files/figure-html/binomial-sensitivity-1.png)

``` r
sens_bin$results
#>   fixed_effects.condition estimate       mcse   conf_low conf_high failure_rate
#> 1                     0.2     0.16 0.05184593 0.07170077 0.2911263            0
#> 2                     0.4     0.42 0.06979971 0.28188224 0.5679396            0
#> 3                     0.6     0.84 0.05184593 0.70887369 0.9282992            0
#>   singular_rate n_effective nsim
#> 1          0.12          50   50
#> 2          0.10          50   50
#> 3          0.12          50   50

# Inspect failure_rate and singular_rate alongside power.
# Increase nsim for final study reporting.
```

## Count outcomes (Poisson vs Negative Binomial)

MixPower supports count outcomes through Poisson and Negative Binomial
GLMMs. Poisson is appropriate when variance ≈ mean; NB handles
over-dispersion.

``` r
d <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
  residual_sd = 1,
  random_effects = list(subject = list(intercept_sd = 0.3))
)

scn_pois <- mp_scenario_lme4_poisson(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "wald"
)

res_pois <- mp_power(scn_pois, nsim = 50, seed = 123)
summary(res_pois)
#> $power
#> [1] 0.96
#> 
#> $mcse
#> [1] 0.02771281
#> 
#> $ci
#> [1] 0.8628624 0.9951186
#> 
#> $ci_method
#> [1] "clopper-pearson"
#> 
#> $diagnostics
#> $diagnostics$fail_rate
#> [1] 0
#> 
#> $diagnostics$singular_rate
#> [1] 0
#> 
#> $diagnostics$type_s
#> [1] 0
#> 
#> $diagnostics$type_m
#> [1] 1.008971
#> 
#> 
#> $nsim
#> [1] 50
#> 
#> $alpha
#> [1] 0.05
#> 
#> $failure_policy
#> [1] "count_as_nondetect"
#> 
#> $conf_level
#> [1] 0.95

a_nb <- a
a_nb$theta <- 1.5  # NB dispersion (size parameter)

scn_nb <- mp_scenario_lme4_nb(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a_nb,
  test_method = "wald"
)

res_nb <- mp_power(scn_nb, nsim = 50, seed = 123)
#> boundary (singular) fit: see help('isSingular')
#> 
#> boundary (singular) fit: see help('isSingular')
summary(res_nb)
#> $power
#> [1] 0.84
#> 
#> $mcse
#> [1] 0.05184593
#> 
#> $ci
#> [1] 0.7088737 0.9282992
#> 
#> $ci_method
#> [1] "clopper-pearson"
#> 
#> $diagnostics
#> $diagnostics$fail_rate
#> [1] 0
#> 
#> $diagnostics$singular_rate
#> [1] 0.04
#> 
#> $diagnostics$type_s
#> [1] 0
#> 
#> $diagnostics$type_m
#> [1] 1.187583
#> 
#> 
#> $nsim
#> [1] 50
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

## Sensitivity curves (count outcomes)

``` r
sens_pois <- mp_sensitivity(
  scn_pois,
  vary = list(`fixed_effects.condition` = c(0.2, 0.4, 0.6)),
  nsim = 50,
  seed = 123
)

sens_nb <- mp_sensitivity(
  scn_nb,
  vary = list(`fixed_effects.condition` = c(0.2, 0.4, 0.6)),
  nsim = 50,
  seed = 123
)
#> boundary (singular) fit: see help('isSingular')
#> 
#> boundary (singular) fit: see help('isSingular')
#> 
#> boundary (singular) fit: see help('isSingular')

plot(sens_pois)
```

![](mixpower-intro_files/figure-html/count-sensitivity-1.png)

``` r
plot(sens_nb)
```

![](mixpower-intro_files/figure-html/count-sensitivity-2.png)

``` r

# Compare power estimates and failure/singularity rates across Poisson vs NB.
# Overdispersion can reduce power relative to Poisson.
# Increase nsim for final reports.
```
