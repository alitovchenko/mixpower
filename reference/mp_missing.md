# Add a missing-data / dropout mechanism to a scenario

Wraps a scenario so that, on every replicate, observations are deleted
from the simulated data before the model is fit. This lets a power
analysis reflect realistic incomplete data (Gallop & Liu, 2017;
Magnusson, 2018). Three mechanisms are supported:

## Usage

``` r
mp_missing(
  scenario,
  mechanism = c("mcar", "mar", "dropout"),
  prob = NULL,
  on = NULL,
  slope = 0,
  time = NULL,
  dropout = NULL,
  subject = "subject"
)
```

## Arguments

- scenario:

  An `mp_scenario` (any backend).

- mechanism:

  One of `"mcar"`, `"mar"`, `"dropout"`.

- prob:

  Baseline deletion probability for `"mcar"`/`"mar"`.

- on:

  Name of the observed column the `"mar"` probability depends on.

- slope:

  Logit-scale slope for `"mar"` (default 0).

- time:

  Name of the within-subject ordering column for `"dropout"`.

- dropout:

  For `"dropout"`: a numeric vector of cumulative dropout proportions
  per ordered timepoint, or `list(shape=, scale=)` for Weibull.

- subject:

  Subject grouping column (default `"subject"`).

## Value

The scenario with its simulator wrapped to apply missingness.

## Details

- `"mcar"`: each observation is deleted independently with probability
  `prob` (missing completely at random).

- `"mar"`: each observation is deleted with a probability that depends
  on an *observed* column `on` through a logistic model
  `plogis(qlogis(prob) + slope * on)` (missing at random).

- `"dropout"`: monotone longitudinal dropout along `time` within each
  subject — once a subject drops out it contributes no later
  observations. The dropout pattern is given either by `dropout` as a
  vector of cumulative dropout proportions (one per ordered timepoint)
  or as `list(shape =, scale =)` for a Weibull dropout time on the
  `time` scale.

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  d <- mp_design(list(subject = 30), trials_per_cell = 6,
                 predictors = list(time = "continuous"))
  a <- mp_assumptions(list("(Intercept)" = 0, time = 0.4),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ time + (1 | subject), design = d,
                          assumptions = a, predictor = "time")
  scn_drop <- mp_missing(scn, "dropout", time = "time",
                         dropout = c(0, 0.1, 0.2, 0.35, 0.5, 0.6))
  mp_power(scn_drop, nsim = 20, seed = 1)
}
#> <mp_power>
#>   nsim: 20
#>   alpha: 0.05
#>   failure_policy: count_as_nondetect
#>   power: 1.000
#>   mcse: 0.000
#>   95% CI (clopper-pearson): [0.832, 1.000]
#>   diagnostics:
#>     - fail_rate: 0.000
#>     - singular_rate: 0.000
#>     - type_s: 0.000
#>     - type_m: 0.99
# }
```
