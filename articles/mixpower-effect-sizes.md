# Effect sizes: maximal models, fitted pilots, and safeguard power

The hardest part of a power analysis is rarely the simulation — it is
choosing a defensible effect size and a realistic random-effects
structure. This vignette ties together four `mixpower` features that
address exactly that:

- **maximal models** with one or more correlated random slopes,
- **[`mp_from_fit()`](../reference/mp_from_fit.md)** to drive power from
  an existing (pilot or published) fit,
- **[`mp_sesoi()`](../reference/mp_sesoi.md)** to plan around a
  *smallest effect size of interest*, and
- **[`mp_safeguard_effect()`](../reference/mp_safeguard_effect.md)** for
  an uncertainty-aware, conservative effect,

and closes with how to read the **Type S / Type M** diagnostics that
[`mp_power()`](../reference/mp_power.md) reports for free.

``` r
library(mixpower)
```

## Maximal models: correlated random slopes

A within-participant manipulation usually varies *within* every subject,
so the “maximal” random-effects structure includes a random slope for
it. Ignoring a real random slope inflates the Type I error rate, so
simulating from — and fitting — the maximal model gives honest power.

`random_effects` takes a per-group `intercept_sd`, an optional named
list of `slopes`, and an optional correlation `cor` (a scalar applied to
every pair of terms, or a full correlation matrix). Each fixed effect
you name also becomes a balanced design predictor, so several effects
are crossed as a factorial.

``` r
d <- mp_design(clusters = list(subject = 20), trials_per_cell = 6)

a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, x1 = 0.5, x2 = 0.3),
  random_effects = list(subject = list(
    intercept_sd = 0.4,
    slopes = list(x1 = 0.3, x2 = 0.3),
    cor = 0.1
  )),
  residual_sd = 1
)
a
#> <mp_assumptions>
#>   fixed_effects:
#>     - (Intercept): 0
#>     - x1: 0.5
#>     - x2: 0.3
#>   random_effects (SD on linear predictor):
#>     - subject: intercept_sd = 0.4, slope_sd(x1 = 0.3, x2 = 0.3), cor = 0.1 
#>   residual_sd: 1
```

``` r
scn_max <- mp_scenario_lme4(
  y ~ x1 + x2 + (1 + x1 + x2 | subject),
  design = d,
  assumptions = a,
  predictor = "x1"
)

mp_power(scn_max, nsim = 15, seed = 2024)$power
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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> [1] 0.7333333
```

For a single within-subject factor the same machinery reduces to the
familiar `(1 + condition | subject)` form with one slope and an
intercept-slope correlation.

## Power from a fitted pilot model

When you have a pilot or published model,
[`mp_from_fit()`](../reference/mp_from_fit.md) turns it into a scenario:
new responses are simulated from the fit (keeping its estimated variance
components), the model is refit, and the focal term is tested. The fixed
effects start at the fitted coefficients but can be varied for planning.

``` r
m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
scn_fit <- mp_from_fit(m, test_term = "Days")
scn_fit$assumptions
#> <mp_assumptions>
#>   fixed_effects:
#>     - (Intercept): 251.405
#>     - Days: 10.4673
#>   random_effects (SD on linear predictor):
#>     - Subject: intercept_sd = 24.7407, slope_sd(Days = 5.92214), cor = 0.0655512 
#>   residual_sd: 25.5918
```

``` r
mp_power(scn_fit, nsim = 15, seed = 1)$power
#> [1] 1
```

## Planning around a smallest effect size of interest

Pilot and published effects are often optimistic. A robust habit is to
plan power for the *smallest effect you would still care about*.
[`mp_sesoi()`](../reference/mp_sesoi.md) returns a copy of a scenario
with the focal effect replaced — either explicitly, or by scaling the
assumed effect (the default `multiplier = 0.85` is a conservative 15%
reduction).

``` r
scn_sesoi <- mp_sesoi(scn_fit, multiplier = 0.85)
c(
  full  = scn_fit$assumptions$fixed_effects$Days,
  sesoi = scn_sesoi$assumptions$fixed_effects$Days
)
#>      full     sesoi 
#> 10.467286  8.897193
```

Sweeping the multiplier shows how quickly power erodes as the effect of
interest shrinks — the whole point of planning for a SESOI rather than
the (large) pilot estimate:

``` r
mults <- c(1, 0.5, 0.3, 0.2)
powers <- vapply(
  mults,
  function(mult) mp_power(mp_sesoi(scn_fit, multiplier = mult), nsim = 15, seed = 1)$power,
  numeric(1)
)
data.frame(multiplier = mults, effect = mults * scn_fit$assumptions$fixed_effects$Days, power = powers)
#>   multiplier    effect     power
#> 1        1.0 10.467286 1.0000000
#> 2        0.5  5.233643 0.8000000
#> 3        0.3  3.140186 0.4666667
#> 4        0.2  2.093457 0.3333333
```

## Safeguard power

A *safeguard* effect (Perugini, Gallucci & Costantini, 2014) goes one
step further: instead of a point estimate, it uses the
confidence-interval bound nearest zero, so sampling uncertainty in the
pilot is propagated into the plan.
[`mp_safeguard_effect()`](../reference/mp_safeguard_effect.md) computes
it; feed the result straight into
[`mp_sesoi()`](../reference/mp_sesoi.md).

``` r
sg <- mp_safeguard_effect(m, term = "Days", conf_level = 0.90)
sg
#> <mp_safeguard>
#>   term:        Days
#>   estimate:    10.4673 (se 1.54579)
#>   90% CI:     [7.92469, 13.0099]
#>   safeguard:   7.92469
```

``` r
scn_safe <- mp_sesoi(scn_fit, effect = sg)
mp_power(scn_safe, nsim = 15, seed = 1)$power
#> [1] 1
```

The safeguard effect (the lower confidence bound) is smaller than the
point estimate. Here the `sleepstudy` `Days` effect is large enough that
even its conservative bound stays well powered; for a borderline effect,
that same buffer is exactly what stops an over-optimistic pilot from
promising power it cannot deliver — compare it against the multiplier
sweep above.

## Reading Type S and Type M errors

Every [`mp_power()`](../reference/mp_power.md) run reports two
diagnostics that go beyond the power figure (Gelman & Carlin, 2014):

- **Type S** (sign): among significant replicates, the fraction with the
  *wrong* sign.
- **Type M** (magnitude): among significant replicates, the average
  ratio of the estimated to the true effect — the *exaggeration* factor.

At low power, significant estimates are systematically inflated (Type M
well above 1); at high power they are well calibrated.

``` r
under <- mp_power(mp_sesoi(scn_fit, multiplier = 0.25), nsim = 25, seed = 7)
summary(under)
#> $power
#> [1] 0.44
#> 
#> $mcse
#> [1] 0.09927739
#> 
#> $ci
#> [1] 0.2440237 0.6507184
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
#> [1] 1.595777
#> 
#> 
#> $nsim
#> [1] 25
#> 
#> $alpha
#> [1] 0.05
#> 
#> $failure_policy
#> [1] "count_as_nondetect"
#> 
#> $conf_level
#> [1] 0.95
under$diagnostics$type_m
#> [1] 1.595777
```

A Type M near 1 means significant estimates are about right; a value
like 1.5 means published “hits” from this design would overstate the
effect by ~50%. This is why planning for a SESOI or safeguard effect —
and powering well — matters for replicable findings.
