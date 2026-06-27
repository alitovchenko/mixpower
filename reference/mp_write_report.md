# Write a power-analysis report to a file

Writes an [`mp_report()`](mp_report.md) to disk as Markdown (default) or
HTML. HTML rendering uses `rmarkdown`/`pandoc` when available; otherwise
use `"markdown"` (which is easily converted to HTML/PDF by any Markdown
tool).

## Usage

``` r
mp_write_report(report, file, format = c("markdown", "html"))
```

## Arguments

- report:

  An `mp_report` object.

- file:

  Output file path.

- format:

  `"markdown"` (default) or `"html"`.

## Value

Invisibly, `file`.

## See also

[`mp_report()`](mp_report.md).
