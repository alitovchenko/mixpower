# Roadmap

## Shipped (0.1.x)

- **Core:** `mp_design`, `mp_assumptions`, `mp_scenario`, `mp_power` (full and streaming aggregate), engine-agnostic replicate loop.
- **lme4 backends:** Gaussian LMM, binomial / Poisson / negative binomial GLMM; Wald and LRT with explicit `null_formula`.
- **Formal backend contract:** `mp_backend()`, `validate_mp_backend()`; built-in backends return `mp_backend` objects.
- **Second backend (optional dep):** `mp_backend_glmmtmb()`, `mp_scenario_glmmtmb_lmm()` (Gaussian via glmmTMB).
- **Grids:** `mp_sensitivity`, `mp_sensitivity_parallel` (per-cell seeds, optional checkpoint/resume), `mp_power_curve`, `mp_power_curve_parallel`, `mp_solve_sample_size`, `mp_grid_sample_size`.
- **Parallel utilities:** `mp_parallel_cluster()` (PSOCK + load mixpower on workers).
- **Reproducibility:** `mp_manifest`, `mp_bundle_results`, `mp_report_table`, `mp_write_results`.
- **Optional tidy layer (Suggests):** `tibble::as_tibble()` methods, `ggplot2::autoplot()` for sensitivity / power curves.
- **Docs:** vignettes (intro, design, simulations, diagnostics, reproducibility, extending); package help `?mixpower`.

## Near term

- Extend glmmTMB scenarios (zero-inflation, beta, additional families).
- Shared parallel helper reuse across curve and sensitivity (ongoing refinement).
- Checkpoint/resume polish (atomic manifest writes, clearer mismatch recovery).

## Mid term

- Optional `brms` / Stan backend (Suggests-only, heavy).
- Precision-targeted `nsim` (stop when MCSE below threshold).
- Benchmark vignette vs `simr` / analytic cases.

## Long term

- Optional GUI for interactive design exploration.
- Broader hierarchical designs (attrition, missingness, crossed random slopes templates).
