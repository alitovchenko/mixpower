# Effect-size converters for eliciting assumptions

Helpers that translate standardized or published effect sizes into the
raw coefficients and variance components that
[`mp_assumptions()`](mp_assumptions.md) expects, so you do not have to
hand-pick regression coefficients. They compose directly:

## Usage

``` r
mp_d_to_beta(d, sd = 1)

mp_beta_to_d(beta, sd = 1)

mp_r2_to_beta(r2, sd_resid = 1, predictor_sd = 1)

mp_beta_to_r2(beta, sd_resid = 1, predictor_sd = 1)

mp_icc_to_sd(icc, sd_resid = 1)

mp_sd_to_icc(sd, sd_resid = 1)

mp_or_to_logodds(or)

mp_logodds_to_or(beta)

mp_t_to_beta(t, se)

mp_f_to_beta(f, se)
```

## Arguments

- d:

  Cohen's d (standardized mean difference).

- sd:

  Standard deviation defining the standardization (e.g. the total
  outcome SD `sqrt(tau^2 + sigma^2)`).

- beta:

  A raw coefficient.

- r2:

  Target (partial) proportion of variance explained, in `[0, 1)`.

- sd_resid:

  Residual standard deviation.

- predictor_sd:

  Standard deviation of the predictor (default 1).

- icc:

  Target intraclass correlation, in `[0, 1)`.

- or:

  Odds ratio (\> 0).

- t:

  A t statistic.

- se:

  Standard error of the coefficient.

- f:

  An F statistic with one numerator degree of freedom.

## Value

A numeric scalar.

## Details

    mp_assumptions(
      fixed_effects = list("(Intercept)" = 0, condition = mp_d_to_beta(0.5, sd = 1.12)),
      random_effects = list(subject = list(intercept_sd = mp_icc_to_sd(0.1, 1))),
      residual_sd = 1
    )

- `mp_d_to_beta()` / `mp_beta_to_d()`: Cohen's d for a 0/1 predictor is
  the mean difference divided by `sd`, so the coefficient is `d * sd`.

- `mp_r2_to_beta()` / `mp_beta_to_r2()`: for a predictor with SD
  `predictor_sd`, a target (partial) variance-explained `r2` against
  residual SD `sd_resid` implies
  `beta = sqrt(r2/(1-r2)) * sd_resid / predictor_sd`.

- `mp_icc_to_sd()` / `mp_sd_to_icc()`: an intraclass correlation `icc`
  with residual SD `sd_resid` implies a random-intercept SD
  `sqrt(icc/(1-icc)) * sd_resid`.

- `mp_or_to_logodds()` / `mp_logodds_to_or()`: a binomial GLMM
  coefficient is the log odds ratio.

- `mp_t_to_beta()` / `mp_f_to_beta()`: recover a coefficient from a
  published t (or one-numerator-df F) statistic and its standard error.

## Examples

``` r
mp_d_to_beta(0.5, sd = 1.12)
#> [1] 0.56
mp_icc_to_sd(0.1, sd_resid = 1)
#> [1] 0.3333333
mp_r2_to_beta(0.02, sd_resid = 1)
#> [1] 0.1428571
mp_or_to_logodds(1.5)
#> [1] 0.4054651
```
