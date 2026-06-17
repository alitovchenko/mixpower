# Contributing

## Coding style

- Use base R style with 2-space indentation.
- Prefer explicit namespaces (e.g.,
  [`stats::lm`](https://rdrr.io/r/stats/lm.html)).
- Keep functions focused and documented with roxygen2 tags.

## Testing

- Add or update tests for new functionality under `tests/testthat/`.
- Prefer fast, deterministic tests.

## Local checks

- [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
- `R CMD check --as-cran`

## Maintainer approval

All changes require review and approval from the package maintainer
before merge.
