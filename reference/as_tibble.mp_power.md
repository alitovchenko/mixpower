# Coerce mixpower results to a tibble

Requires the tibble package (installed with the tidyverse).

## Usage

``` r
# S3 method for class 'mp_power'
as_tibble(x, ...)
```

## Arguments

- x:

  An `mp_power`, `mp_sensitivity`, or `mp_power_curve` object.

- ...:

  Passed to
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  on the underlying data frame.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).
