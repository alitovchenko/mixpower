# Generate a methods paragraph for a power analysis

Produces a ready-to-edit prose description of a simulation-based power
analysis from an [`mp_power()`](mp_power.md) result: the design, model,
effect tested, inference method, number of simulations, and the
estimated power with its interval. Intended to seed the "Power analysis"
paragraph of a methods section.

## Usage

``` r
mp_methods_text(result, software = TRUE)
```

## Arguments

- result:

  An `mp_power` object (from [`mp_power()`](mp_power.md)).

- software:

  Include a sentence naming the mixpower package and version (default
  `TRUE`).

## Value

A length-1 character string with class `mp_methods_text` (its
[`print()`](https://rdrr.io/r/base/print.html) method word-wraps the
paragraph).

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  d <- mp_design(list(subject = 30), trials_per_cell = 6)
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.4),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  mp_methods_text(mp_power(scn, nsim = 50, seed = 1))
}
#> A simulation-based power analysis was conducted using the mixpower R package
#> (version 1.1.1). Data were simulated under the model y ~ condition + (1 |
#> subject), with 30 subject (6 observations each). The focal effect (condition)
#> was set to 0.4 and tested with a Wald z/t test at alpha = 0.05. Across 50
#> simulated datasets, estimated power was 70.0% (95% Clopper-Pearson confidence
#> interval 55.4% to 82.1%). Among significant replicates the average exaggeration
#> ratio (Type M) was 1.20.
# }
```
