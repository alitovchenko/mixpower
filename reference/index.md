# Package index

## Core workflow

Specify a design, assumptions, and scenario, then estimate power.

- [`mp_design()`](mp_design.md) : Create a study design specification
- [`mp_assumptions()`](mp_assumptions.md) : Create modeling assumptions
  for simulation-based power
- [`mp_scenario()`](mp_scenario.md) : Create a power-analysis scenario
- [`mp_power()`](mp_power.md) : Simulation-based power estimation
  (engine-agnostic core)
- [`mp_quick_power()`](mp_quick_power.md) : Quick power run for a single
  LMM design

## Model backends

Family- and engine-specific scenario/backend constructors.

- [`mp_scenario_glmmtmb_lmm()`](mp_scenario_glmmtmb_lmm.md) : Gaussian
  LMM scenario using glmmTMB
- [`mp_scenario_lme4()`](mp_scenario_lme4.md) : Create a fully specified
  MixPower scenario with the lme4 backend
- [`mp_scenario_lme4_binomial()`](mp_scenario_lme4_binomial.md) : Create
  a fully specified MixPower scenario with the binomial lme4 backend
- [`mp_scenario_lme4_nb()`](mp_scenario_lme4_nb.md) : Create a fully
  specified MixPower scenario with the NB lme4 backend
- [`mp_scenario_lme4_poisson()`](mp_scenario_lme4_poisson.md) : Create a
  fully specified MixPower scenario with the Poisson lme4 backend
- [`mp_backend()`](mp_backend.md) : MixPower backend contract
- [`mp_backend_glmmtmb()`](mp_backend_glmmtmb.md) : Build a glmmTMB
  backend for Gaussian LMM scenarios
- [`mp_backend_lme4()`](mp_backend_lme4.md) : Build an lme4 backend for
  MixPower scenarios
- [`mp_backend_lme4_binomial()`](mp_backend_lme4_binomial.md) : Build an
  lme4 backend for binomial GLMM scenarios
- [`mp_backend_lme4_nb()`](mp_backend_lme4_nb.md) : Build an lme4
  backend for Negative Binomial GLMM scenarios
- [`mp_backend_lme4_poisson()`](mp_backend_lme4_poisson.md) : Build an
  lme4 backend for Poisson GLMM scenarios
- [`validate_mp_backend()`](validate_mp_backend.md) : Validate a
  MixPower backend

## From a fitted pilot model

- [`mp_from_fit()`](mp_from_fit.md) : Build a power scenario from a
  fitted lme4 model
- [`mp_extend()`](mp_extend.md) : Scale a fitted-model scenario's sample
  size up or down

## Credibility and validation

- [`mp_plan()`](mp_plan.md) : Plan power the calibrate-first way
- [`mp_calibrate()`](mp_calibrate.md) : Check the Type I error
  calibration of a scenario's test
- [`mp_attach_calibration()`](mp_attach_calibration.md) : Record a
  calibration result on a scenario
- [`mp_recommend_method()`](mp_recommend_method.md) : Recommend an
  inference method for a scenario

## Effect sizes

Smallest-effect-of-interest, safeguard, and standardized converters.

- [`mp_sesoi()`](mp_sesoi.md) : Set a smallest effect size of interest
  (SESOI) on a scenario
- [`mp_safeguard_effect()`](mp_safeguard_effect.md) : Safeguard
  (confidence-bound) effect size from a fitted model
- [`mp_d_to_beta()`](effect_size.md) [`mp_beta_to_d()`](effect_size.md)
  [`mp_r2_to_beta()`](effect_size.md)
  [`mp_beta_to_r2()`](effect_size.md) [`mp_icc_to_sd()`](effect_size.md)
  [`mp_sd_to_icc()`](effect_size.md)
  [`mp_or_to_logodds()`](effect_size.md)
  [`mp_logodds_to_or()`](effect_size.md)
  [`mp_t_to_beta()`](effect_size.md) [`mp_f_to_beta()`](effect_size.md)
  : Effect-size converters for eliciting assumptions

## Missing data and dropout

- [`mp_missing()`](mp_missing.md) : Add a missing-data / dropout
  mechanism to a scenario

## Planning and analysis flexibility

Sensitivity, power curves, sample-size solvers, model comparison.

- [`mp_sensitivity()`](mp_sensitivity.md) : Run power sensitivity
  analysis over a parameter grid
- [`mp_power_curve()`](mp_power_curve.md) : Power curve for a single
  design/assumption parameter
- [`mp_solve_sample_size()`](mp_solve_sample_size.md) : Solve for
  minimum sample size achieving target power
- [`mp_grid_sample_size()`](mp_grid_sample_size.md) : Create a grid of
  values for sample-size search
- [`mp_compare_models()`](mp_compare_models.md) : Compare analysis
  models on the same simulated data
- [`mp_power_checkpoint()`](mp_power_checkpoint.md) : Resumable,
  checkpointed power simulation
- [`mp_power_adaptive()`](mp_power_adaptive.md) : Adaptive (sequential)
  power simulation
- [`mp_stop()`](mp_stop.md) : Stopping rule for adaptive power
  simulation
- [`mp_power_curve_parallel()`](mp_power_curve_parallel.md) : Parallel
  power curve evaluation
- [`mp_sensitivity_parallel()`](mp_sensitivity_parallel.md) : Parallel
  sensitivity analysis over a parameter grid
- [`run_parallel()`](run_parallel.md) : Placeholder for parallel
  execution
- [`plot(`*`<mp_power_curve>`*`)`](plot.mp_power_curve.md) : Plot a
  power curve
- [`plot(`*`<mp_sensitivity>`*`)`](plot.mp_sensitivity.md) : Plot a
  sensitivity analysis
- [`autoplot(`*`<mp_sensitivity>`*`)`](autoplot.mp_sensitivity.md) :
  ggplot2 diagnostic plot for sensitivity or power curve

## Reporting and reproducibility

- [`mp_report_table()`](mp_report_table.md) : Publication-ready summary
  table for power results
- [`mp_methods_text()`](mp_methods_text.md) : Generate a methods
  paragraph for a power analysis
- [`plot(`*`<mp_power>`*`)`](plot.mp_power.md) : Plot the p-value
  distribution of a power analysis
- [`as_tibble(`*`<mp_power>`*`)`](as_tibble.mp_power.md) : Coerce
  mixpower results to a tibble
- [`mp_manifest()`](mp_manifest.md) : Reproducibility manifest for power
  analyses
- [`mp_bundle_results()`](mp_bundle_results.md) : Bundle results with
  manifest and optional labels
- [`mp_write_results()`](mp_write_results.md) : Write results or bundle
  to CSV or JSON

## Lower-level and legacy helpers

- [`mixpower-package`](mixpower-package.md)
  [`mixpower`](mixpower-package.md) : Simulation-Based Power Analysis
  for Mixed-Effects Models
- [`mp_sensitivity_cell_run()`](mp_sensitivity_cell_run.md) : Run one
  sensitivity grid cell (for serial and parallel workers)
- [`simulate_power()`](simulate_power.md) : Run a simple
  simulation-based power study
- [`summarize_simulations()`](summarize_simulations.md) : Summarize
  simulation outputs
- [`fit_model()`](fit_model.md) : Fit a model for a single simulated
  dataset
- [`test_effect()`](test_effect.md) : Extract a test statistic for a
  model term
- [`simulate_glmm_binomial_data()`](simulate_glmm_binomial_data.md) :
  Simulate binary outcome data for a GLMM with random effects
- [`simulate_glmm_poisson_data()`](simulate_glmm_poisson_data.md) :
  Simulate count outcome data for a Poisson GLMM with random effects
- [`simulate_glmm_nb_data()`](simulate_glmm_nb_data.md) : Simulate count
  outcome data for a Negative Binomial GLMM with random effects
- [`plot_power()`](plot_power.md) : Plot power results
