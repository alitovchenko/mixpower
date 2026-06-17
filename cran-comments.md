## Submission

This is a new package submission (version 1.0.0).

mixpower provides simulation-based power and sample-size analysis for linear and
generalized linear mixed-effects models (Gaussian, binomial, Poisson, and
negative-binomial families) fitted with 'lme4'. There are no reverse
dependencies.

## Test environments

- local macOS, R release
- GitHub Actions: ubuntu-latest (release, devel, oldrel-1), macOS-latest,
  windows-latest
- win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* The note is the standard "New submission" note.

## Notes

* Examples and tests that rely on suggested packages ('glmmTMB', 'lmerTest',
  'pbkrtest') are guarded with `requireNamespace()` / `skip_if_not_installed()`.
* A cross-engine test that compares 'glmmTMB' and 'lme4' fits is skipped unless
  the installed 'glmmTMB' is confirmed compatible with the installed 'TMB' ABI,
  to avoid spurious failures on misconfigured environments.
