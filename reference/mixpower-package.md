# Simulation-Based Power Analysis for Mixed-Effects Models

**mixpower** is a simulation-based toolkit for power and sample-size
analysis for linear and generalized linear mixed-effects models (LMMs
and GLMMs). It is design-first (no pilot data required), supports
Gaussian, binomial, Poisson, and negative binomial families via
**lme4**; Wald and likelihood-ratio tests; multi-parameter sensitivity
grids; power curves and minimum sample-size solvers; parallel evaluation
with deterministic seeds; and full reproducibility (manifests, result
bundling, export to CSV/JSON). Every run reports diagnostics (failure
rate, singular-fit rate, effective N).

## Details

Typical workflow: (1) define a design with [`mp_design()`](mp_design.md)
(cluster sizes, trials per cell) and effect-size assumptions with
[`mp_assumptions()`](mp_assumptions.md); (2) build a scenario with a
backend constructor (e.g. [`mp_scenario_lme4()`](mp_scenario_lme4.md)
for Gaussian LMM); (3) run [`mp_power()`](mp_power.md) for a single
power estimate, [`mp_sensitivity()`](mp_sensitivity.md) to vary
parameters, or [`mp_power_curve()`](mp_power_curve.md) /
[`mp_solve_sample_size()`](mp_solve_sample_size.md) for curves and
sample-size. Use a fixed `seed` for reproducibility. Failure and
singular-fit rates are always reported and never suppressed.

## Getting started

After loading the package, define a design and assumptions, build an
lme4 scenario, then call [`mp_power()`](mp_power.md). See Examples below
for a minimal runnable workflow.

## Function overview

**Design and assumptions:** [`mp_design()`](mp_design.md),
[`mp_assumptions()`](mp_assumptions.md).

**Scenarios (LMM/GLMM):** [`mp_scenario()`](mp_scenario.md),
[`mp_scenario_lme4()`](mp_scenario_lme4.md),
[`mp_scenario_lme4_binomial()`](mp_scenario_lme4_binomial.md),
[`mp_scenario_lme4_poisson()`](mp_scenario_lme4_poisson.md),
[`mp_scenario_lme4_nb()`](mp_scenario_lme4_nb.md).

**Power and sensitivity:** [`mp_power()`](mp_power.md) (optional
`aggregate = "streaming"`), [`mp_sensitivity()`](mp_sensitivity.md),
[`mp_sensitivity_parallel()`](mp_sensitivity_parallel.md),
[`mp_power_curve()`](mp_power_curve.md),
[`mp_power_curve_parallel()`](mp_power_curve_parallel.md),
[`mp_solve_sample_size()`](mp_solve_sample_size.md),
[`mp_grid_sample_size()`](mp_grid_sample_size.md),
[`mp_quick_power()`](mp_quick_power.md).

**Backends and simulators:** [`mp_backend()`](mp_backend.md),
[`validate_mp_backend()`](validate_mp_backend.md),
[`mp_backend_lme4()`](mp_backend_lme4.md),
[`mp_backend_lme4_binomial()`](mp_backend_lme4_binomial.md),
[`mp_backend_lme4_poisson()`](mp_backend_lme4_poisson.md),
[`mp_backend_lme4_nb()`](mp_backend_lme4_nb.md),
[`mp_backend_glmmtmb()`](mp_backend_glmmtmb.md),
[`mp_scenario_glmmtmb_lmm()`](mp_scenario_glmmtmb_lmm.md),
[`simulate_glmm_binomial_data()`](simulate_glmm_binomial_data.md),
[`simulate_glmm_poisson_data()`](simulate_glmm_poisson_data.md),
[`simulate_glmm_nb_data()`](simulate_glmm_nb_data.md).

**Optional tidyverse (Suggests):**
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
and
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods.

**Reproducibility and reporting:** [`mp_manifest()`](mp_manifest.md),
[`mp_bundle_results()`](mp_bundle_results.md),
[`mp_report_table()`](mp_report_table.md),
[`mp_write_results()`](mp_write_results.md).

**Plotting:** [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
for `mp_power_curve` and `mp_sensitivity` objects.

## Getting the most out of mixpower

- Use a fixed `seed` (e.g. `seed = 123`) so runs are reproducible.

- Check `failure_rate` and `singular_rate` in results; investigate if
  high.

- For nested model comparison use LRT with an explicit `null_formula`.

- Use [`mp_power_curve()`](mp_power_curve.md) or
  [`mp_solve_sample_size()`](mp_solve_sample_size.md) to choose sample
  size.

- Vignettes give step-by-step guides:
  [`vignette("mixpower-intro", package = "mixpower")`](../articles/mixpower-intro.md),
  [`vignette("mixpower-design", package = "mixpower")`](../articles/mixpower-design.md),
  [`vignette("mixpower-simulations", package = "mixpower")`](../articles/mixpower-simulations.md),
  [`vignette("mixpower-diagnostics", package = "mixpower")`](../articles/mixpower-diagnostics.md),
  [`vignette("mixpower-reproducibility", package = "mixpower")`](../articles/mixpower-reproducibility.md),
  [`vignette("mixpower-extending", package = "mixpower")`](../articles/mixpower-extending.md).

## References

Bates D, Maechler M, Bolker B, Walker S (2015). "Fitting Linear
Mixed-Effects Models Using lme4." *Journal of Statistical Software*,
67(1), 1–48.
[doi:10.18637/jss.v067.i01](https://doi.org/10.18637/jss.v067.i01) .

Green P, MacLeod CJ (2016). "SIMR: an R package for power analysis of
generalized linear mixed models by simulation." *Methods in Ecology and
Evolution*, 7(4), 493–498.
[doi:10.1111/2041-210X.12504](https://doi.org/10.1111/2041-210X.12504) .

## See also

Entry points: [`mp_design()`](mp_design.md),
[`mp_power()`](mp_power.md), [`mp_quick_power()`](mp_quick_power.md).
Vignettes: `vignette(package = "mixpower")`.

## Author

**Maintainer**: Alex Litovchenko <al4877@columbia.edu>

## Examples

``` r
# Minimal workflow: design -> assumptions -> scenario -> power
d <- mp_design(clusters = list(subject = 30), trials_per_cell = 4)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
  residual_sd = 1
)
scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
res <- mp_power(scn, nsim = 50, seed = 123)
summary(res)
#> $power
#> [1] 0.32
#> 
#> $mcse
#> [1] 0.06596969
#> 
#> $ci
#> [1] 0.1952042 0.4669938
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
#> [1] 1.71327
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
#> 
```
