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
