# Plot a sensitivity analysis

For one varying parameter: line plot with optional CI segments when
`y = "estimate"`. For two varying parameters: heatmap. More than two
parameters is not supported.

## Usage

``` r
# S3 method for class 'mp_sensitivity'
plot(x, y = c("estimate", "failure_rate", "singular_rate", "n_effective"), ...)
```

## Arguments

- x:

  An `mp_sensitivity` object.

- y:

  What to plot: `"estimate"` (power), `"failure_rate"`,
  `"singular_rate"`, or `"n_effective"`.

- ...:

  Additional graphical arguments passed to
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html)
  (1D) or [`graphics::image()`](https://rdrr.io/r/graphics/image.html)
  (2D).

## Value

Invisibly returns the plotted data (1D: ordered data frame; 2D: matrix).
