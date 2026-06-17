# Plot the p-value distribution of a power analysis

A histogram of the per-replicate p-values from
[`mp_power()`](mp_power.md), with the `alpha` threshold marked. The
shaded area to the left of `alpha` is the estimated power. Requires a
run with `aggregate = "full"` (the default), which retains per-replicate
p-values.

## Usage

``` r
# S3 method for class 'mp_power'
plot(x, ...)
```

## Arguments

- x:

  An `mp_power` object.

- ...:

  Passed to [`graphics::hist()`](https://rdrr.io/r/graphics/hist.html).

## Value

Invisibly, the p-values plotted.
