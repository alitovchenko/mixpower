# ggplot2 diagnostic plot for sensitivity or power curve

Requires ggplot2. Intended as an optional alternative to base
[`plot.mp_sensitivity()`](plot.mp_sensitivity.md) /
[`plot.mp_power_curve()`](plot.mp_power_curve.md).

## Usage

``` r
# S3 method for class 'mp_sensitivity'
autoplot(
  object,
  ...,
  y = c("estimate", "failure_rate", "singular_rate", "n_effective")
)
```

## Arguments

- object:

  An `mp_sensitivity` or `mp_power_curve` object.

- ...:

  Unused; reserved for consistency with ggplot2 generics.

- y:

  For sensitivity/curve objects: same as
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) —
  `"estimate"`, `"failure_rate"`, `"singular_rate"`, or `"n_effective"`.

## Value

A ggplot2 object.
