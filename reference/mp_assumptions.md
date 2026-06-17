# Create modeling assumptions for simulation-based power

Assumptions encode effect sizes and the variance components used to
simulate data. Random-effect sizes are given as standard deviations on
the linear predictor (the scale lme4 reports), via `random_effects`.

## Usage

``` r
mp_assumptions(
  fixed_effects,
  random_effects = NULL,
  icc = NULL,
  residual_sd = NULL,
  notes = NULL
)
```

## Arguments

- fixed_effects:

  Named list of numeric values (e.g.,
  `list("(Intercept)" = 0, condition = 0.4)`).

- random_effects:

  Optional named list keyed by grouping factor. Each element is a named
  list with `intercept_sd` (the random-intercept SD on the
  linear-predictor scale) and, optionally, `slopes` (a named list of
  random-slope SDs, one per predictor) and `cor`. For example,
  `list(subject = list(intercept_sd = 0.5, slopes = list(condition = 0.3), cor = 0.2))`
  encodes a correlated random intercept and slope, i.e.
  `(1 + condition | subject)`. With several slopes, `cor` may be a
  single scalar (applied to every pair of terms) or a full correlation
  matrix over `c("(Intercept)", names(slopes))`; each fixed effect named
  in `fixed_effects` also becomes a balanced design predictor.

- icc:

  Deprecated. Previously documented as an intraclass correlation but
  used as a random-intercept SD. If supplied it is interpreted as
  `intercept_sd` and folded into `random_effects` with a warning. Use
  `random_effects` instead.

- residual_sd:

  Optional non-negative numeric residual SD (Gaussian).

- notes:

  Optional free text.

## Value

An object of class `mp_assumptions`.

## Examples

``` r
a <- mp_assumptions(
  fixed_effects = list("(Intercept)" = 0, condition = 0.4),
  random_effects = list(subject = list(intercept_sd = 0.5)),
  residual_sd = 1
)
a
#> <mp_assumptions>
#>   fixed_effects:
#>     - (Intercept): 0
#>     - condition: 0.4
#>   random_effects (SD on linear predictor):
#>     - subject: intercept_sd = 0.5 
#>   residual_sd: 1
```
