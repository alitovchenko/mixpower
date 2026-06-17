---
title: "mixpower: Simulation-Based Power Analysis for Mixed-Effects Models in R"
tags:
  - R
  - statistics
  - power analysis
  - mixed-effects models
  - simulation
authors:
  - name: Alex Litovchenko
    affiliation: 1
affiliations:
  - name: Columbia University
    index: 1
date: 7 February 2026
bibliography: paper.bib
---

# Summary

`mixpower` is an R package for simulation-based power and sample-size analysis
of linear and generalized linear mixed-effects models (LMMs and GLMMs). Given a
study design, a set of modeling assumptions, and an analysis specification, it
repeatedly simulates data, refits the model, applies an inferential test, and
estimates the probability of detecting an effect. It supports Gaussian,
binomial, Poisson, and negative-binomial families via `lme4` [@lme4]; Wald,
likelihood-ratio, Satterthwaite, Kenward-Roger, and parametric-bootstrap tests;
designs with crossed and nested random effects, continuous and between-subject
predictors, and unbalanced or incomplete data; and power analyses driven either
from declared parameters or directly from a fitted pilot model.

# Statement of need

Mixed-effects models are ubiquitous, but their power is rarely tractable
analytically, so simulation is the standard approach [@green2016]. Existing R
tools each cover part of the problem: `simr` [@green2016] simulates from a
fitted model and scales sample size; `mixedpower` [@kumle2021] popularized the
data-based and smallest-effect-of-interest workflows; `mlmpower` elicits effects
through variance-explained measures; and `powerlmm` provides analytical power
for longitudinal designs with dropout. `mixpower` unifies these capabilities
behind one engine and adds an emphasis on *credibility*: every analysis can be
checked for Type I error calibration, and each run reports exact
(Clopper-Pearson) confidence intervals, Monte Carlo standard errors, and Type S
(sign) and Type M (magnitude) error rates [@gelman2014].

# Key features

- **Engine-agnostic core** with a documented backend contract; `lme4` and
  `glmmTMB` backends ship with the package.
- **Inference methods**: Wald, likelihood-ratio, Satterthwaite and
  Kenward-Roger (via `lmerTest`/`pbkrtest`), and parametric bootstrap; omnibus
  joint tests and custom linear contrasts.
- **Credibility tools**: `mp_calibrate()` estimates the empirical Type I error
  under the null and flags anti-conservative analyses; `mp_recommend_method()`
  gives design-based method guidance.
- **Realistic designs**: correlated random slopes, three-level nesting,
  continuous/between-subject predictors, unbalanced sample sizes, and
  missing-data/dropout mechanisms (`mp_missing()`).
- **Pilot-driven analysis**: `mp_from_fit()` builds a scenario from a fitted
  model, and `mp_extend()` scales its sample size up or down.
- **Effect-size elicitation**: converters from Cohen's *d*, partial $R^2$, ICC,
  odds ratios, and published *t*/*F* statistics to model coefficients.
- **Planning and reproducibility**: sensitivity grids, power curves, sample-size
  solvers, resumable checkpointed runs, reproducibility manifests, result
  bundling, CSV/JSON export, and an auto-generated methods paragraph.

# Acknowledgements

We thank the developers of `lme4`, `lmerTest`, and `pbkrtest`, on which
`mixpower` builds.

# References
