# mixpower 0.4.0

## Correctness

- Fixed a defect where likelihood-ratio tests (`test_method = "lrt"`) returned
  `NA` for every replicate (hence power 0) in the simulation loop: the null
  model was refit with `update()`, which could not resolve the simulated data
  in that scope. Both models are now refit explicitly from the fitted model's
  own data frame. This affected the lme4 (Gaussian and GLMM) and glmmTMB
  backends.

## Features

- New small-sample inference methods for the Gaussian lme4 backend:
  `test_method = "satterthwaite"` and `"kenward-roger"` (df-corrected t tests
  via lmerTest/pbkrtest), recommended over Wald-z when the number of groups is
  modest. Kenward-Roger is refit with REML internally as it requires.
- New `test_method = "pb"` (parametric-bootstrap LRT via pbkrtest) for the
  lme4 LMM and GLMM backends, with a `pb_nsim` control (default 100). It is
  exact-er for small samples but costly: each power replicate refits the model
  `pb_nsim` times.
- Inference is now centralized in one dispatcher shared by all backends, so the
  test logic is defined once rather than duplicated per family.

# mixpower 0.3.0

## Features

- **Random slopes.** `random_effects` now accepts a random slope and an
  intercept-slope correlation per grouping factor, e.g.
  `random_effects = list(subject = list(intercept_sd = 0.5,
  slopes = list(condition = 0.3), cor = 0.2))`, simulating `(1 + condition |
  subject)`. This is essential for correctly-sized power: omitting a present
  random slope inflates Type I error (Barr et al., 2013).

## Internal

- Unified data-generating engine. The Gaussian, binomial, Poisson, and
  negative-binomial simulators now share a single formula-agnostic core
  (`.mp_simulate_mixed`) that builds a balanced within-subject design, adds
  fixed effects and correlated random effects (intercept + optional slope, via
  a base-R Cholesky draw with no new dependency), and applies the family
  response. The four `simulate_*` functions are thin wrappers, so random-effect
  handling and slopes are defined once rather than duplicated per family.

# mixpower 0.2.0

## Correctness

- Fixed a severe defect where every lme4-family Wald test errored internally
  (`base::diag()` on the S4 `dpoMatrix` from `vcov(merMod)`), so `mp_power()`
  silently returned 0 for all lme4 Gaussian/binomial/Poisson/NB designs. The
  standard error is now read from the fitted-model coefficient table. Power is
  correct (≈1 at a strongly-powered design; ≈alpha under the null).
- Fixed a defect where the Gaussian backends ignored the random-intercept SD:
  `mp_scenario_lme4()` / `mp_scenario_glmmtmb_lmm()` always simulated a subject
  SD of 1, so `mp_sensitivity()` / `mp_power_curve()` over a random-effect size
  had no effect. Random-effect SDs now flow from `mp_assumptions()` for every
  family (Gaussian, binomial, Poisson, negative binomial).

## API

- `mp_assumptions()` gains `random_effects`, the correctly-named specification
  of random-effect sizes as standard deviations on the linear-predictor scale,
  e.g. `random_effects = list(subject = list(intercept_sd = 0.5))`.
- `icc` is deprecated. It was documented as an intraclass correlation but used
  as a random-intercept SD; it is now interpreted as `intercept_sd`, folded into
  `random_effects`, and warns once per session. The previous `[0, 1)` constraint
  (which wrongly blocked realistic SDs > 1) is removed.
- `mp_sensitivity()` / `mp_power_curve()` can vary
  `random_effects.<group>.intercept_sd`.

## Features (since 0.1.0)

- New glmmTMB backend (`mp_backend_glmmtmb()`, `mp_scenario_glmmtmb_lmm()`).
- Formalised backend contract (`mp_backend()`, `validate_mp_backend()`).
- Parallel sensitivity and power curves; streaming `mp_power()`.
- Sample-size solver (`mp_solve_sample_size()`) and power curves
  (`mp_power_curve()`).
- Reproducibility layer: manifests, result bundling, CSV/JSON export.
- Tidy S3 integration (`as_tibble()`, `autoplot()`) and `mp_quick_power()`.

# mixpower 0.1.0

- Initial release.
- Supports simulation-based power analysis for Gaussian linear mixed-effects models.
- Includes diagnostics for convergence and simulation uncertainty.
