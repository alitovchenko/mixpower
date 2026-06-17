# Set a smallest effect size of interest (SESOI) on a scenario

Convenience helper for planning power around a *smallest effect size of
interest* rather than a single point estimate. It returns a copy of
`scenario` with the focal fixed effect replaced, either by an explicit
value or by scaling the current assumed effect (e.g. a conservative 15%
reduction, `multiplier = 0.85`). This is the recommended way to avoid
overoptimistic power based on a possibly inflated pilot/published effect
(Anderson, Kelley & Maxwell, 2017; Kumle, Vo & Draschkow, 2021).

## Usage

``` r
mp_sesoi(scenario, multiplier = 0.85, effect = NULL, term = NULL)
```

## Arguments

- scenario:

  An `mp_scenario` object.

- multiplier:

  Numeric factor applied to the current fixed effect when `effect` is
  not supplied (default `0.85`, a 15% reduction).

- effect:

  Optional explicit SESOI on the model's coefficient scale. When
  supplied it overrides `multiplier`. May be a numeric scalar or an
  [`mp_safeguard_effect()`](mp_safeguard_effect.md) result.

- term:

  Fixed-effect term to modify. Defaults to the scenario's test term (or
  the first non-intercept fixed effect).

## Value

A modified `mp_scenario` object.

## See also

[`mp_safeguard_effect()`](mp_safeguard_effect.md) for a data-driven
conservative effect.

## Examples

``` r
d <- mp_design(list(subject = 30), trials_per_cell = 8)
a <- mp_assumptions(
  fixed_effects = list("(Intercept)" = 0, condition = 0.5),
  random_effects = list(subject = list(intercept_sd = 0.5)),
  residual_sd = 1
)
scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
# Power for an effect 15% smaller than assumed:
scn_sesoi <- mp_sesoi(scn, multiplier = 0.85)
scn_sesoi$assumptions$fixed_effects$condition
#> [1] 0.425
```
