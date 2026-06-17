# Create a study design specification

`mp_design()` encodes how data will be collected: cluster sizes and
repeated measurements. It does not encode effect sizes or analysis
decisions.

## Usage

``` r
mp_design(
  clusters,
  trials_per_cell = 1,
  predictors = NULL,
  nesting = NULL,
  notes = NULL
)
```

## Arguments

- clusters:

  Named list of positive integers. Example:
  `list(subject = 50, item = 30)`. For a nested (three-level) design, a
  nested factor's count is interpreted as the number of units *per
  parent* (see `nesting`).

- trials_per_cell:

  Number of repeated observations per subject. A single positive integer
  (balanced), or a positive-integer vector recycled across subjects for
  an unbalanced design.

- predictors:

  Optional named list giving the design type of each predictor (each
  non-intercept fixed effect). Each entry is either a string (`"binary"`
  or `"continuous"`) or a list with `type` and `level` (`"within"` or
  `"between"`). Unspecified predictors default to a balanced
  within-subject binary factor. Example:
  `list(time = list(type = "continuous", level = "within"), group = "binary")`.

- nesting:

  Optional named character vector mapping a child grouping factor to its
  parent, e.g. `c(subject = "site")` for subjects nested in sites. The
  parent must also appear in `clusters`.

- notes:

  Optional free text.

## Value

An object of class `mp_design`.

## Examples

``` r
d <- mp_design(clusters = list(subject = 40), trials_per_cell = 10)
d
#> <mp_design>
#>   clusters:
#>     - subject: 40
#>   trials_per_cell: 10

# Three-level: 8 sites, 5 subjects per site, 4 trials each.
d3 <- mp_design(
  clusters = list(site = 8, subject = 5),
  trials_per_cell = 4,
  nesting = c(subject = "site")
)
```
