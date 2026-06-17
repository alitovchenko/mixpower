# Safeguard (confidence-bound) effect size from a fitted model

Computes a *safeguard* effect for power analysis: the bound of a
confidence interval for a fitted effect that lies closest to zero
(Perugini, Gallucci & Costantini, 2014). Planning power around this
conservative, uncertainty-aware value protects against the optimism of
using a noisy pilot point estimate.

## Usage

``` r
mp_safeguard_effect(fit, term = NULL, conf_level = 0.9)
```

## Arguments

- fit:

  A fitted `lmer`/`glmer` model.

- term:

  Fixed-effect term. Defaults to the first non-intercept effect.

- conf_level:

  Two-sided confidence level for the interval (default `0.90`). Lower
  values are less conservative.

## Value

An object of class `mp_safeguard`: a list with `term`, `estimate`, `se`,
`conf_level`, the interval (`lower`, `upper`), and the `safeguard` bound
(the interval limit nearest zero, in the direction of the estimate).

## Details

The interval is the Wald (normal-approximation) interval from the fitted
coefficient and its standard error. Pair the result with
[`mp_from_fit()`](mp_from_fit.md) and [`mp_sesoi()`](mp_sesoi.md) to run
a safeguard-power simulation.

## See also

[`mp_sesoi()`](mp_sesoi.md), [`mp_from_fit()`](mp_from_fit.md).

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  sg <- mp_safeguard_effect(m, term = "Days", conf_level = 0.90)
  sg
}
#> <mp_safeguard>
#>   term:        Days
#>   estimate:    10.4673 (se 1.54579)
#>   90% CI:     [7.92469, 13.0099]
#>   safeguard:   7.92469
# }
```
