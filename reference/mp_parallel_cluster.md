# Create a PSOCK cluster with mixpower loaded on workers

Caller is responsible for
[`parallel::stopCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
(typically via `on.exit(..., add = TRUE)`).

## Usage

``` r
mp_parallel_cluster(workers)
```

## Arguments

- workers:

  Number of workers (integer \>= 1).

## Value

A cluster object from
[`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html).
