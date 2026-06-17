# Create a grid of values for sample-size search

Returns a numeric vector suitable for
[`mp_solve_sample_size()`](mp_solve_sample_size.md)'s `grid` argument.
Specify either the number of points (`length.out`) or the step size
(`by`); bounds are always explicit (`from`, `to`).

## Usage

``` r
mp_grid_sample_size(from, to, length.out = NULL, by = NULL)
```

## Arguments

- from:

  Lower bound (inclusive).

- to:

  Upper bound (inclusive).

- length.out:

  Number of points (optional). Uses
  [`seq()`](https://rdrr.io/r/base/seq.html) with `length.out`; `from`
  and `to` are the first and last values.

- by:

  Step size (optional). Uses [`seq()`](https://rdrr.io/r/base/seq.html)
  with `by`; sequence runs from `from` to `to` in steps of `by`.

## Value

A numeric vector. For integer cluster sizes, use
`round(mp_grid_sample_size(...))` or pass `by` as an integer (e.g.
`by = 10`).

## Examples

``` r
mp_grid_sample_size(20, 100, length.out = 9)
#> [1]  20  30  40  50  60  70  80  90 100
mp_grid_sample_size(20, 100, by = 10)
#> [1]  20  30  40  50  60  70  80  90 100
```
