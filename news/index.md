# Changelog

## mixpower 1.2.0

### Calibration-first workflow

- [`mp_plan()`](../reference/mp_plan.md) runs the recommended end-to-end
  workflow — calibrate the null, recommend an inference method, and
  estimate power — and reports them together, so a power estimate is
  never shown without the Type I error check that says whether it is
  trustworthy.
- [`mp_power()`](../reference/mp_power.md) now nudges (once per session)
  toward [`mp_calibrate()`](../reference/mp_calibrate.md) /
  [`mp_plan()`](../reference/mp_plan.md) when run on a risky design (few
  clusters or complex random effects) with no calibration on record. The
  advice is always available as `$calibration_advice`; pass
  `check_calibration = FALSE` to silence (internal parameter sweeps
  already do).
- [`mp_attach_calibration()`](../reference/mp_attach_calibration.md)
  records a calibration result on a scenario.
- [`mp_report_table()`](../reference/mp_report_table.md) gains an
  `mp_plan` method.
- DESCRIPTION now lists the CRAN URL.

### Adaptive Monte Carlo stopping

- [`mp_power_adaptive()`](../reference/mp_power_adaptive.md) runs
  [`mp_power()`](../reference/mp_power.md) in batches and stops as soon
  as the power interval is precise enough (`ci_halfwidth`) or the
  accept/reject decision vs a `target` is unambiguous, instead of
  relying on an arbitrary fixed `nsim`. It reports the achieved Monte
  Carlo precision and the number of replicates actually used, and the
  result is identical to a fixed
  [`mp_power()`](../reference/mp_power.md) run at that `nsim`.
- [`mp_stop()`](../reference/mp_stop.md) specifies the stopping rule
  (`ci_halfwidth`, `target`, `min_nsim`, `max_nsim`, `batch`).

### Trust report

- [`mp_report()`](../reference/mp_report.md) assembles a reviewer-ready
  Markdown report from a power (or
  [`mp_plan()`](../reference/mp_plan.md)) result: design, model,
  data-generating assumptions, Type I calibration, inference-method
  guidance, power with interval and Monte Carlo precision, diagnostics
  (failure/singular rates, Type S/M), missing-data assumptions, a
  reproducibility manifest, and explicit warnings about fragile or
  unsupported claims.
  [`mp_write_report()`](../reference/mp_write_report.md) writes Markdown
  (or HTML via rmarkdown/pandoc).

### Model comparison: validity *and* power

- [`mp_compare_models()`](../reference/mp_compare_models.md) now reports
  each candidate analysis model’s **Type I error** (estimated under the
  null on the same simulated data) alongside its power, with a
  calibration verdict per model and a **recommended plan** (the
  best-powered model that still controls Type I error). This answers the
  planning question “which analysis is both valid and well powered?” —
  useful for preregistration and grant methods sections. Set
  `calibrate = FALSE` for power only.

## mixpower 1.1.1

CRAN release: 2026-06-25

- Reduce overall `R CMD check` time well under CRAN’s budget: vignettes
  now use small toy designs and few iterations, and the heavy
  simulation/correctness tests are marked `skip_on_cran()` (they still
  run in continuous integration and the lme4 path is exercised by
  examples). No user-facing changes.

## mixpower 1.1.0

A large feature release focused on credibility and design realism.

### Credibility

- [`mp_calibrate()`](../reference/mp_calibrate.md) estimates the
  empirical Type I error rate under the null and returns a verdict
  (well-calibrated / anti-conservative / conservative), flagging
  analyses (e.g. an omitted random slope, or Wald with few clusters)
  whose power would not be trustworthy.
- [`mp_recommend_method()`](../reference/mp_recommend_method.md) gives
  design-based inference-method guidance.

### Designs and data

- [`mp_design()`](../reference/mp_design.md) gains continuous and
  between-subject predictors (`predictors=`), three-level nested
  grouping factors (`nesting=`), and unbalanced within-subject sample
  sizes (vector `trials_per_cell`).
- [`mp_missing()`](../reference/mp_missing.md) adds missing-data /
  dropout mechanisms: MCAR, MAR (logit on an observed column), and
  monotone longitudinal dropout (per-timepoint proportions or a Weibull
  dropout time).

### Sample size and effect sizes

- [`mp_extend()`](../reference/mp_extend.md) and the `extend.<group>`
  sensitivity key scale a [`mp_from_fit()`](../reference/mp_from_fit.md)
  scenario’s sample size up or down (simr `extend()` analogue).
- Effect-size converters
  ([`mp_d_to_beta()`](../reference/effect_size.md),
  [`mp_r2_to_beta()`](../reference/effect_size.md),
  [`mp_icc_to_sd()`](../reference/effect_size.md),
  [`mp_or_to_logodds()`](../reference/effect_size.md),
  [`mp_t_to_beta()`](../reference/effect_size.md),
  [`mp_f_to_beta()`](../reference/effect_size.md), and inverses)
  translate standardized/published effects into model coefficients.

### Analysis flexibility

- Omnibus joint-Wald tests of several coefficients
  (`test_term = c(...)`), custom linear contrasts (`contrast=`), and
  [`mp_compare_models()`](../reference/mp_compare_models.md), which fits
  multiple analysis models to the same simulated data to expose Type I
  inflation from misspecification.

### Performance and reporting

- [`mp_power_checkpoint()`](../reference/mp_power_checkpoint.md) runs
  power simulations in resumable, persisted batches.
- [`mp_methods_text()`](../reference/mp_methods_text.md) generates a
  methods-section paragraph;
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on an
  `mp_power` object shows the p-value distribution;
  [`mp_report_table()`](../reference/mp_report_table.md) accepts
  calibration results.

## mixpower 1.0.0

First stable release and initial CRAN submission. Consolidates the 0.x
series into a documented, tested baseline: simulation-based power and
sample-size analysis for Gaussian, binomial, Poisson, and
negative-binomial mixed models via ‘lme4’; Wald / LRT / Satterthwaite /
Kenward-Roger / parametric-bootstrap tests; multiple correlated random
slopes; [`mp_from_fit()`](../reference/mp_from_fit.md); sensitivity
grids, power curves, and sample-size solvers; SESOI / safeguard helpers;
exact power intervals and Type S/M diagnostics; parallel evaluation and
reproducible result bundling.

## mixpower 0.7.0

### Features

- **Multiple correlated random slopes.** `random_effects` now accepts
  several `slopes` per grouping factor, with a correlation `cor` given
  either as a scalar (applied to every pair of terms) or as a full
  correlation matrix over `c("(Intercept)", names(slopes))`. Each fixed
  effect named in `fixed_effects` becomes a balanced design predictor,
  so multi-factor maximal models such as `(1 + x1 + x2 | subject)`
  simulate correctly. The simulator draws the random-effect block from a
  single multivariate normal (Cholesky / eigen), with no new
  dependencies; single-slope behaviour is unchanged.
- **[`mp_sesoi()`](../reference/mp_sesoi.md)** returns a scenario whose
  focal effect is replaced by a smallest effect size of interest —
  either an explicit value or a scaled version of the assumed effect
  (default `multiplier = 0.85`, a 15% reduction).
- **[`mp_safeguard_effect()`](../reference/mp_safeguard_effect.md)**
  derives a conservative, uncertainty-aware effect from a fitted model:
  the confidence-interval bound nearest zero (Perugini, Gallucci &
  Costantini, 2014). Pipe it into `mp_sesoi(effect = ...)` for a
  safeguard-power analysis.
- [`mp_from_fit()`](../reference/mp_from_fit.md) now records the full
  random-effect structure (every slope and the complete correlation
  matrix) in the scenario assumptions.
- [`mp_sensitivity()`](../reference/mp_sensitivity.md) /
  [`mp_power_curve()`](../reference/mp_power_curve.md) can vary
  `random_effects.<g>.cor` and `random_effects.<g>.slopes.<predictor>`
  in addition to `intercept_sd`.
- New vignette, *Effect sizes: maximal models, fitted pilots, and
  safeguard power*, tying these features together with Type S / Type M
  interpretation.

## mixpower 0.6.0

### Features

- [`mp_from_fit()`](../reference/mp_from_fit.md) builds a power scenario
  from an existing `lmer`/`glmer` fit (pilot or published model), the
  workflow `simr` is known for. New responses are simulated from the
  fitted model via
  [`stats::simulate()`](https://rdrr.io/r/stats/simulate.html)
  (preserving the estimated random-effect structure and residual
  variance), refit, and tested. The fixed effects come from the scenario
  assumptions (starting at the fitted coefficients), so
  [`mp_sensitivity()`](../reference/mp_sensitivity.md) /
  [`mp_power_curve()`](../reference/mp_power_curve.md) can vary an
  effect size for data-based vs smallest-effect-of-interest comparisons.
  All of mixpower’s inference methods, exact CIs, and Type S/M
  diagnostics apply.

## mixpower 0.5.0

### Features

- Exact **Clopper-Pearson** confidence intervals for the power estimate
  are now the default (`mp_power(ci_method = "clopper-pearson")`);
  `"wald"` remains available. The previous Wald interval mis-behaved
  near power 0 or 1 (e.g. returning `[0, 0]`).
- **Type S (sign) and Type M (magnitude/exaggeration) error rates**
  (Gelman & Carlin, 2014) are reported in
  [`mp_power()`](../reference/mp_power.md) diagnostics and
  [`mp_report_table()`](../reference/mp_report_table.md), computed among
  significant replicates when the tested term’s true effect is known and
  non-zero. Type M \> 1 flags that statistically significant estimates
  exaggerate the effect (acute under low power).
- Backends now also return the fixed-effect estimate per replicate (used
  for Type S/M); custom engines can supply `estimate` in their
  `test_fun` output.

## mixpower 0.4.0

### Correctness

- Fixed a defect where likelihood-ratio tests (`test_method = "lrt"`)
  returned `NA` for every replicate (hence power 0) in the simulation
  loop: the null model was refit with
  [`update()`](https://rdrr.io/r/stats/update.html), which could not
  resolve the simulated data in that scope. Both models are now refit
  explicitly from the fitted model’s own data frame. This affected the
  lme4 (Gaussian and GLMM) and glmmTMB backends.

### Features

- New small-sample inference methods for the Gaussian lme4 backend:
  `test_method = "satterthwaite"` and `"kenward-roger"` (df-corrected t
  tests via lmerTest/pbkrtest), recommended over Wald-z when the number
  of groups is modest. Kenward-Roger is refit with REML internally as it
  requires.
- New `test_method = "pb"` (parametric-bootstrap LRT via pbkrtest) for
  the lme4 LMM and GLMM backends, with a `pb_nsim` control (default
  100). It is exact-er for small samples but costly: each power
  replicate refits the model `pb_nsim` times.
- Inference is now centralized in one dispatcher shared by all backends,
  so the test logic is defined once rather than duplicated per family.

## mixpower 0.3.0

### Features

- **Random slopes.** `random_effects` now accepts a random slope and an
  intercept-slope correlation per grouping factor, e.g.
  `random_effects = list(subject = list(intercept_sd = 0.5, slopes = list(condition = 0.3), cor = 0.2))`,
  simulating `(1 + condition | subject)`. This is essential for
  correctly-sized power: omitting a present random slope inflates Type I
  error (Barr et al., 2013).

### Internal

- Unified data-generating engine. The Gaussian, binomial, Poisson, and
  negative-binomial simulators now share a single formula-agnostic core
  (`.mp_simulate_mixed`) that builds a balanced within-subject design,
  adds fixed effects and correlated random effects (intercept + optional
  slope, via a base-R Cholesky draw with no new dependency), and applies
  the family response. The four `simulate_*` functions are thin
  wrappers, so random-effect handling and slopes are defined once rather
  than duplicated per family.

## mixpower 0.2.0

### Correctness

- Fixed a severe defect where every lme4-family Wald test errored
  internally ([`base::diag()`](https://rdrr.io/r/base/diag.html) on the
  S4 `dpoMatrix` from `vcov(merMod)`), so
  [`mp_power()`](../reference/mp_power.md) silently returned 0 for all
  lme4 Gaussian/binomial/Poisson/NB designs. The standard error is now
  read from the fitted-model coefficient table. Power is correct (≈1 at
  a strongly-powered design; ≈alpha under the null).
- Fixed a defect where the Gaussian backends ignored the
  random-intercept SD:
  [`mp_scenario_lme4()`](../reference/mp_scenario_lme4.md) /
  [`mp_scenario_glmmtmb_lmm()`](../reference/mp_scenario_glmmtmb_lmm.md)
  always simulated a subject SD of 1, so
  [`mp_sensitivity()`](../reference/mp_sensitivity.md) /
  [`mp_power_curve()`](../reference/mp_power_curve.md) over a
  random-effect size had no effect. Random-effect SDs now flow from
  [`mp_assumptions()`](../reference/mp_assumptions.md) for every family
  (Gaussian, binomial, Poisson, negative binomial).

### API

- [`mp_assumptions()`](../reference/mp_assumptions.md) gains
  `random_effects`, the correctly-named specification of random-effect
  sizes as standard deviations on the linear-predictor scale,
  e.g. `random_effects = list(subject = list(intercept_sd = 0.5))`.
- `icc` is deprecated. It was documented as an intraclass correlation
  but used as a random-intercept SD; it is now interpreted as
  `intercept_sd`, folded into `random_effects`, and warns once per
  session. The previous `[0, 1)` constraint (which wrongly blocked
  realistic SDs \> 1) is removed.
- [`mp_sensitivity()`](../reference/mp_sensitivity.md) /
  [`mp_power_curve()`](../reference/mp_power_curve.md) can vary
  `random_effects.<group>.intercept_sd`.

### Features (since 0.1.0)

- New glmmTMB backend
  ([`mp_backend_glmmtmb()`](../reference/mp_backend_glmmtmb.md),
  [`mp_scenario_glmmtmb_lmm()`](../reference/mp_scenario_glmmtmb_lmm.md)).
- Formalised backend contract
  ([`mp_backend()`](../reference/mp_backend.md),
  [`validate_mp_backend()`](../reference/validate_mp_backend.md)).
- Parallel sensitivity and power curves; streaming
  [`mp_power()`](../reference/mp_power.md).
- Sample-size solver
  ([`mp_solve_sample_size()`](../reference/mp_solve_sample_size.md)) and
  power curves ([`mp_power_curve()`](../reference/mp_power_curve.md)).
- Reproducibility layer: manifests, result bundling, CSV/JSON export.
- Tidy S3 integration (`as_tibble()`, `autoplot()`) and
  [`mp_quick_power()`](../reference/mp_quick_power.md).

## mixpower 0.1.0

CRAN release: 2026-02-12

- Initial release.
- Supports simulation-based power analysis for Gaussian linear
  mixed-effects models.
- Includes diagnostics for convergence and simulation uncertainty.
