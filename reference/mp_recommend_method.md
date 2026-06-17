# Recommend an inference method for a scenario

Heuristic guidance on the `test_method` for a scenario, based on the
number of levels of the random grouping factors. Wald
(normal-approximation z/t) tests and, to a lesser extent,
likelihood-ratio tests are known to be anti-conservative when the number
of clusters is small (Luke, 2017): the degrees of freedom are
overstated, so the test rejects too often. With few clusters, a
degrees-of-freedom-corrected test (Satterthwaite or Kenward-Roger, for
linear mixed models) or a parametric bootstrap (any family) controls
Type I error far better.

## Usage

``` r
mp_recommend_method(scenario, small_clusters = 30L)
```

## Arguments

- scenario:

  An `mp_scenario`.

- small_clusters:

  Threshold below which the smallest grouping factor is treated as "few
  clusters" (default 30).

## Value

An object of class `mp_recommendation`: a list with `method` (the
scenario's current method), `n_groups` (smallest grouping-factor size),
`is_lmm`, `caution` (logical), `recommended` (character vector), and
`rationale`.

## Details

This is a fast, design-based heuristic; to *measure* a specific design
and method, use [`mp_calibrate()`](mp_calibrate.md).

## See also

[`mp_calibrate()`](mp_calibrate.md).

## Examples

``` r
d <- mp_design(list(subject = 12), trials_per_cell = 8)
a <- mp_assumptions(
  fixed_effects = list("(Intercept)" = 0, condition = 0.4),
  random_effects = list(subject = list(intercept_sd = 0.5)),
  residual_sd = 1
)
scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
mp_recommend_method(scn)
#> <mp_recommendation>
#>   current method: wald
#>   smallest grouping factor: 12 levels
#>   caution: yes
#>   recommended: kenward-roger, satterthwaite, pb
#>   Smallest grouping factor has 12 levels (< 30). 'wald' tests can be anti-conservative with few clusters; prefer 'kenward-roger' or 'satterthwaite' or 'pb'. Verify with mp_calibrate().
```
