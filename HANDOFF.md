# mixpower Handoff

Single handoff for a new agent (or maintainer) so development can
continue without prior context. Covers what the package contains, issues
solved during development, and best practices for coding, building, and
pushing.

------------------------------------------------------------------------

## 1. What mixpower contains

**Purpose:** Simulation-based power and sample-size analysis for linear
and generalized linear mixed-effects models (LMMs and GLMMs).
Design-first (no pilot data required); explicit seeds and full
diagnostics.

**Repository:** <https://github.com/alitovchenko/mixpower>  
**Version:** 0.1.0 (tag `v0.1.0` released).

### Core API

- **Design / assumptions:** `mp_design(clusters, trials_per_cell)`,
  `mp_assumptions(fixed_effects, residual_sd, icc)`.
- **Scenarios:**
  `mp_scenario(formula, design, assumptions, test, simulate_fun, fit_fun, test_fun)`
  for custom engines; convenience constructors below.
- **Power:**
  `mp_power(scenario, nsim, alpha, seed, failure_policy, conf_level, keep)`
  — single design, returns power + MCSE + CI + diagnostics
  (failure_rate, singular_rate).
- **Sensitivity:** `mp_sensitivity(scenario, vary, nsim, ...)` — grid
  over one or more parameters; returns results table with estimate,
  conf_low/high, failure_rate, singular_rate, n_effective per cell.
- **Power curve:** `mp_power_curve(scenario, vary, ...)` — one-parameter
  sensitivity; [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  (1D line or 2D heatmap when two vary params),
  [`summary()`](https://rdrr.io/r/base/summary.html).
- **Parallel curve:**
  `mp_power_curve_parallel(scenario, vary, workers, ..., progress)` —
  same as curve with per-cell seeds; parallel when `progress = FALSE`,
  serial with progress bar when `progress = TRUE`.
- **Sample-size solver:**
  `mp_solve_sample_size(scenario, parameter, grid, target_power, ...)` —
  minimum grid value achieving target power. Helper:
  `mp_grid_sample_size(from, to, length.out = NULL, by = NULL)` to build
  the grid.
- **Quick power (LMM):**
  `mp_quick_power(formula, clusters, trials_per_cell, fixed_effects, residual_sd, nsim, ...)`
  — composes design, assumptions, `mp_scenario_lme4`, and `mp_power`;
  `...` passed to [`mp_power()`](reference/mp_power.md) only.

### Backends (lme4)

- **Gaussian LMM:** [`mp_backend_lme4()`](reference/mp_backend_lme4.md),
  [`mp_scenario_lme4()`](reference/mp_scenario_lme4.md).
- **Binomial GLMM:**
  [`mp_scenario_lme4_binomial()`](reference/mp_scenario_lme4_binomial.md),
  [`simulate_glmm_binomial_data()`](reference/simulate_glmm_binomial_data.md).
- **Poisson GLMM:**
  [`mp_scenario_lme4_poisson()`](reference/mp_scenario_lme4_poisson.md),
  [`simulate_glmm_poisson_data()`](reference/simulate_glmm_poisson_data.md).
- **Negative Binomial GLMM:**
  [`mp_scenario_lme4_nb()`](reference/mp_scenario_lme4_nb.md),
  [`simulate_glmm_nb_data()`](reference/simulate_glmm_nb_data.md)
  (assumptions can include `theta`).

All lme4 scenarios support `test_method = "wald"` or `"lrt"`; LRT
requires explicit `null_formula`.

### Reproducibility and reporting

- **Manifest:** `mp_manifest(scenario, seed, session)` — scenario digest
  (formula/design/assumptions/test only), seed strategy, R/mixpower
  versions, timestamp, optional git SHA and sessionInfo.
- **Bundle:**
  `mp_bundle_results(result, manifest, study_id, analyst, notes)` —
  result + manifest + labels; `result` is `mp_power`, `mp_sensitivity`,
  or `mp_power_curve`.
- **Report table:** `mp_report_table(x)` — flat data frame
  (power_estimate, ci_low/high, failure_rate, singular_rate,
  n_effective, nsim); works on power/sensitivity/curve or bundle.
- **Export:** `mp_write_results(x, file, format = "csv" | "json", ...)`
  — CSV = report table; JSON = report + manifest + labels for bundles.
  JSON requires `jsonlite` (Suggests).

### Conventions

- Internal helpers: `.assert_*`, `.stop`, `%||%` (do not export).
- Failures are never silently suppressed; diagnostics (failure_rate,
  singular_rate, n_effective) are always exposed.
- [`mp_power()`](reference/mp_power.md) is backend-agnostic; all
  inference/backend logic lives in scenario constructors and backend
  helpers.

------------------------------------------------------------------------

## 2. Issues solved during development

- **Malformed DESCRIPTION / <Authors@R>:** Use a single-line
  `Authors@R: person(...)` and a final newline. Avoid multi-line or
  malformed [`person()`](https://rdrr.io/r/utils/person.html).
- **CRAN NOTE (citEntry):** Use
  [`bibentry()`](https://rdrr.io/r/utils/bibentry.html) in
  `inst/CITATION`, not
  [`citEntry()`](https://rdrr.io/r/utils/citEntry.html).
- **Pkgdown:** `docs/` not built → run
  `pkgdown::clean_site(force = TRUE)` before build. “No package called
  ‘mixpower’” → build with `install = TRUE`. CI uses explicit build +
  deploy steps.
- **Vignettes:** All vignettes have executable chunks and real examples
  (design, simulations, diagnostics, inference, GLMMs, reproducibility).
  No placeholders.
- **LRT in lme4:** `test_method = "lrt"` with explicit `null_formula`;
  scenario constructors enforce it when LRT is selected.
- **Rd cross-references:** Do not `\link{mp_bundle}` (no such topic);
  link to the function that creates the object,
  e.g. `\link{mp_bundle_results()}`.
- **Namespace / visible binding:** Use explicit namespaces for
  base/utils when needed
  (e.g. [`utils::capture.output`](https://rdrr.io/r/utils/capture.output.html))
  so `R CMD check` does not report “no visible global function
  definition.”
- **Scenario digest determinism:**
  [`mp_manifest()`](reference/mp_manifest.md) digests only formula,
  design, assumptions, and test (not engine closures) so the digest is
  stable across runs.
- **Parallel tests:** Skip when the cluster cannot be created or when
  mixpower cannot be loaded on workers (e.g. sandbox or run from
  source). Use `skip_if_not(can_parallel)` after a tryCatch that creates
  a small cluster and loads the package.
- **Contributor list / Co-authored-by:** Do not add
  `Co-authored-by: Cursor <cursoragent@cursor.com>` (or any cursoragent
  identity) to commit messages. If present, rewrite history to remove it
  and force-push so only the human appears as contributor. Prefer
  disabling any IDE setting that auto-adds a co-author.

------------------------------------------------------------------------

## 3. Best practices for coding, building, and pushing

### Coding

- **Do not change [`mp_power()`](reference/mp_power.md) internals** for
  new features. Add wrappers or callers (e.g. power curve, solver,
  parallel curve) that call [`mp_power()`](reference/mp_power.md) and/or
  [`mp_sensitivity()`](reference/mp_sensitivity.md).
- **Explicit arguments:** Prefer explicit parameters with documented
  defaults. No hidden magic (e.g. no auto-adjusting nsim inside core
  functions). Advisory helpers (e.g. nsim guidance) should be separate
  or doc-only.
- **Diagnostics:** Keep failure_rate, singular_rate, n_effective exposed
  in all relevant results; do not hide or drop them.
- **Seeds:** Use explicit per-cell seeds (e.g. `seed + cell_index - 1L`)
  so serial and parallel runs match for the same seed.
- **Pass-through:** Wrappers that accept `...` should pass them only to
  the intended function (e.g. `mp_quick_power` passes `...` to
  [`mp_power()`](reference/mp_power.md) only) and document that.

### Building

- **Roxygen:** After adding or changing exported functions, run
  [`roxygen2::roxygenise()`](https://roxygen2.r-lib.org/reference/roxygenize.html)
  to update NAMESPACE and man.
- **Tests:**
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
  must pass. Expect 0 errors, 0 warnings; benign “boundary (singular)
  fit” in output is acceptable.
- **CRAN check:** `devtools::check(args = '--as-cran')`. Target: 0
  errors, 0 warnings, 0 notes (or only benign NOTEs such as “New
  submission” or HTML tidy).
- **.Rbuildignore:** Exclude `.git`, `.github`, and non-package files
  (see existing [.Rbuildignore](.Rbuildignore)) so the source tarball is
  clean. Do not ship `downloads/`, `cran-comments.md`, or duplicate
  license/docs that CRAN does not expect in the tarball.

### Pushing and commits

- **No Cursor/cursoragent in history:** Never add
  `Co-authored-by: Cursor` or cursoragent to commit messages. Before
  pushing, run `git log -1 --format="%B"` and remove any such line. If
  it was already pushed, rewrite history
  (`git filter-branch -f --msg-filter 'sed "/^Co-authored-by: Cursor <cursoragent@cursor.com>$/d"' main`)
  then `git push --force-with-lease origin main`. Ensure the human is
  the only author/contributor if the repo should list only them.
- **Force-push:** Use `--force-with-lease` when overwriting remote
  history (e.g. after a history rewrite).
- **Remote:** Repo is typically connected via SSH
  (`git@github.com:alitovchenko/mixpower.git`). Push to `main`;
  `gh-pages` is used for pkgdown and is updated by CI.

### Pre-push checklist

- [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
  passes  
- `devtools::check(args = '--as-cran')` passes (or only acceptable
  NOTEs)  
- [`roxygen2::roxygenise()`](https://roxygen2.r-lib.org/reference/roxygenize.html)
  has been run  
- Commit message has no `Co-authored-by: Cursor` or cursoragent  
- README/vignettes reflect the current API if you changed it

------------------------------------------------------------------------

## 4. CI and artifacts

- **Workflows** (`.github/workflows/`): R-CMD-check (multi-OS/R), tests
  (quick test on PRs), coverage (covr/codecov), pkgdown (clean, build
  with install, deploy to gh-pages). Helper:
  `tests/testthat/helper-ci.R` with `CI_FAST` hook.
- **CRAN:** Tarball can be built with `R CMD build .`; keep
  `cran-comments.md` updated for submission. References in DESCRIPTION
  use the form `authors (year) "Title" <doi:...>`.

------------------------------------------------------------------------

## 5. Where to continue

- Add families (e.g. Gamma) via new simulator/backend pairs.
- Extend design utilities (e.g. beyond subject/item random intercepts).
- Optional: `mp_scenario_from_fit()` to build a scenario from an lme4
  fit (pilot-data path); keep design-first as the primary workflow.
- pkgdown theming via `_pkgdown.yml` if desired.

This handoff plus the in-repo docs (README, vignettes,
[docs/ROADMAP.md](docs/ROADMAP.md)) give enough context to continue
development without prior mixpower-specific knowledge.
