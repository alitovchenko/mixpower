# Plot a power curve

Plot a power curve

## Usage

``` r
# S3 method for class 'mp_power_curve'
plot(x, y = c("estimate", "failure_rate", "singular_rate", "n_effective"), ...)
```

## Arguments

- x:

  An `mp_power_curve` object.

- y:

  What to plot on the y-axis: `"estimate"` (power), `"failure_rate"`,
  `"singular_rate"`, or `"n_effective"`.

- ...:

  Arguments passed to
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly returns the plotted data.
