# Write results or bundle to CSV or JSON

Writes the report table (and for bundles, manifest/labels) to file. CSV
writes the publication-ready table only; JSON writes report table plus
manifest and labels when `x` is an `mp_bundle`.

## Usage

``` r
mp_write_results(x, file, format = c("csv", "json"), ...)
```

## Arguments

- x:

  An object from [`mp_bundle_results()`](mp_bundle_results.md), or
  `mp_power`, `mp_sensitivity`, or `mp_power_curve`.

- file:

  Path to output file (extension need not match format).

- format:

  `"csv"` or `"json"`.

- ...:

  For CSV, arguments passed to
  [`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html) (e.g.
  `row.names = FALSE`).

## Value

Invisibly the path `file`.
