# ADR 0002: Standardize simulation outputs as data frames

## Context

Power analysis results need to be easy to summarize, plot, and aggregate. Users
expect data-frame outputs that integrate with base R and tidy tools.

## Decision

Return `data.frame` objects from simulation and summary helpers.

## Alternatives

- Custom S3 classes with specialized print methods.
- Tibbles with tidyverse dependencies.

## Consequences

- Base R compatibility stays strong and dependencies remain minimal.
- Downstream users can convert to tibbles when needed.

## Examples

- `simulate_power()` returns columns `simulation` and `p_value`.
- `summarize_simulations()` returns a one-row summary data frame.
