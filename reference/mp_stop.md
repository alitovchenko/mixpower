# Stopping rule for adaptive power simulation

Specifies when [`mp_power_adaptive()`](mp_power_adaptive.md) should stop
adding simulation batches. Stopping is triggered (after at least
`min_nsim` replicates) when *either*:

## Usage

``` r
mp_stop(
  ci_halfwidth = 0.02,
  target = NULL,
  min_nsim = 200,
  max_nsim = 5000,
  batch = 200
)
```

## Arguments

- ci_halfwidth:

  Target half-width of the power CI (e.g. `0.02`), or `NULL` to use only
  the decision rule.

- target:

  Optional target power (in (0, 1)); enables the decision rule.

- min_nsim:

  Minimum replicates before stopping is considered (default 200).

- max_nsim:

  Maximum replicates (hard cap, default 5000).

- batch:

  Replicates added per iteration (default 200).

## Value

An object of class `mp_stop`.

## Details

- **precision**: the power confidence interval's half-width is `<=`
  `ci_halfwidth`, or

- **decision**: a `target` power is given and the interval lies entirely
  above or entirely below it (the accept/reject decision is
  unambiguous).

Otherwise it keeps adding batches until `max_nsim` is reached.

## See also

[`mp_power_adaptive()`](mp_power_adaptive.md).

## Examples

``` r
mp_stop(ci_halfwidth = 0.02, max_nsim = 4000)
#> $ci_halfwidth
#> [1] 0.02
#> 
#> $target
#> NULL
#> 
#> $min_nsim
#> [1] 200
#> 
#> $max_nsim
#> [1] 4000
#> 
#> $batch
#> [1] 200
#> 
#> attr(,"class")
#> [1] "mp_stop"
mp_stop(target = 0.8, ci_halfwidth = 0.03)
#> $ci_halfwidth
#> [1] 0.03
#> 
#> $target
#> [1] 0.8
#> 
#> $min_nsim
#> [1] 200
#> 
#> $max_nsim
#> [1] 5000
#> 
#> $batch
#> [1] 200
#> 
#> attr(,"class")
#> [1] "mp_stop"
```
