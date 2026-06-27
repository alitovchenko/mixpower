#' Build a full "trust report" for a power analysis
#'
#' Assembles a single, reviewer-ready report from a power result: the design,
#' model, data-generating assumptions, inference method, Type I calibration,
#' power estimate with interval and Monte Carlo precision, diagnostics
#' (failure/singular-fit rates, Type S/M), missing-data assumptions,
#' reproducibility manifest (software versions, seed), and --- importantly ---
#' explicit warnings about fragile or unsupported claims. The report is Markdown,
#' so it drops straight into methods sections, grants, and registered reports.
#'
#' This grows [mp_methods_text()] and the reproducibility helpers into one
#' artifact; pass the richer [mp_plan()] result to include the calibration
#' section automatically.
#'
#' @param x An `mp_power` (incl. `mp_power_adaptive`) or [mp_plan()] result.
#' @param title Report title.
#' @param manifest Include a reproducibility manifest (default `TRUE`).
#' @return An object of class `mp_report` (a list with `markdown`, plus the
#'   parts: `scenario`, `power`, `calibration`, `recommendation`, `warnings`,
#'   `manifest`). Its `print()` method renders the Markdown.
#' @seealso [mp_plan()], [mp_methods_text()], [mp_write_report()].
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   d <- mp_design(list(subject = 12), trials_per_cell = 8)
#'   a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.4),
#'                       random_effects = list(subject = list(intercept_sd = 0.5)),
#'                       residual_sd = 1)
#'   scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
#'   mp_report(mp_plan(scn, nsim = 100, seed = 1))
#' }
#' }
mp_report <- function(x, title = "Power analysis report", manifest = TRUE) {
  if (inherits(x, "mp_plan")) {
    power <- x$power
    calibration <- x$calibration
    recommendation <- x$recommendation
    scenario <- x$scenario
  } else if (inherits(x, "mp_power")) {
    power <- x
    scenario <- x$scenario
    calibration <- scenario$calibration
    recommendation <- tryCatch(mp_recommend_method(scenario), error = function(e) NULL)
  } else {
    .stop("`x` must be an `mp_power` (or `mp_plan`) result.")
  }

  manifest_obj <- if (isTRUE(manifest)) {
    tryCatch(mp_manifest(scenario, seed = power$seed, session = FALSE), error = function(e) NULL)
  } else {
    NULL
  }
  warnings <- .mp_report_warnings(power, calibration, recommendation, scenario)
  md <- .mp_report_markdown(power, calibration, recommendation, scenario,
                            title, manifest_obj, warnings)

  out <- list(
    title = title,
    scenario = scenario,
    power = power,
    calibration = calibration,
    recommendation = recommendation,
    warnings = warnings,
    manifest = manifest_obj,
    markdown = md
  )
  class(out) <- "mp_report"
  out
}

#' @export
print.mp_report <- function(x, ...) {
  cat(x$markdown, sep = "\n")
  cat("\n")
  invisible(x)
}

#' Write a power-analysis report to a file
#'
#' Writes an [mp_report()] to disk as Markdown (default) or HTML. HTML rendering
#' uses `rmarkdown`/`pandoc` when available; otherwise use `"markdown"` (which is
#' easily converted to HTML/PDF by any Markdown tool).
#'
#' @param report An `mp_report` object.
#' @param file Output file path.
#' @param format `"markdown"` (default) or `"html"`.
#' @return Invisibly, `file`.
#' @seealso [mp_report()].
#' @export
mp_write_report <- function(report, file, format = c("markdown", "html")) {
  if (!inherits(report, "mp_report")) {
    .stop("`report` must be an `mp_report` object (from mp_report()).")
  }
  format <- match.arg(format)
  if (!is.character(file) || length(file) != 1L) .stop("`file` must be a single path.")

  if (format == "markdown") {
    writeLines(report$markdown, file)
    return(invisible(file))
  }

  if (!requireNamespace("rmarkdown", quietly = TRUE) || !rmarkdown::pandoc_available()) {
    .stop("HTML output requires 'rmarkdown' and pandoc. Use format = 'markdown' instead.")
  }
  tmp <- tempfile(fileext = ".md")
  writeLines(report$markdown, tmp)
  on.exit(unlink(tmp), add = TRUE)
  rmarkdown::pandoc_convert(input = normalizePath(tmp), to = "html",
                            output = normalizePath(file, mustWork = FALSE))
  invisible(file)
}

# --- internal builders -------------------------------------------------------

.mp_fmt_re_lines <- function(re) {
  if (is.null(re) || length(re) == 0L) {
    return("- random effects: (none)")
  }
  lines <- "- random effects (SD on the linear-predictor scale):"
  for (g in names(re)) {
    spec <- re[[g]]
    s <- sprintf("    - %s: intercept_sd = %g", g, `%||%`(spec$intercept_sd, 0))
    if (!is.null(spec$slopes) && length(spec$slopes) > 0L) {
      sl <- paste(vapply(names(spec$slopes),
                         function(p) sprintf("%s = %g", p, spec$slopes[[p]]), character(1)),
                  collapse = ", ")
      s <- paste0(s, sprintf("; slope_sd(%s)", sl))
    }
    if (!is.null(spec$cor)) {
      s <- paste0(s, if (is.matrix(spec$cor)) "; cor = <matrix>" else sprintf("; cor = %g", spec$cor))
    }
    lines <- c(lines, s)
  }
  lines
}

.mp_report_markdown <- function(power, calibration, recommendation, scenario,
                                title, manifest_obj, warnings) {
  asm <- scenario$assumptions
  des <- scenario$design
  test <- scenario$test
  pct <- function(p) if (is.na(p)) "NA" else sprintf("%.1f%%", 100 * p)

  md <- c(
    sprintf("# %s", title),
    "",
    sprintf("_Generated %s with mixpower %s._",
            format(Sys.time(), "%Y-%m-%d %H:%M %Z"), as.character(utils::packageVersion("mixpower"))),
    ""
  )

  # Summary paragraph (reuse mp_methods_text on the power result).
  summary_txt <- tryCatch(unclass(mp_methods_text(power)), error = function(e) NULL)
  if (!is.null(summary_txt)) md <- c(md, "## Summary", "", summary_txt, "")

  # Design + model
  md <- c(md, "## Design and model", "")
  clusters <- des$clusters
  cl <- paste(vapply(names(clusters), function(nm) {
    suffix <- if (!is.null(des$nesting) && nm %in% names(des$nesting)) sprintf(" (per %s)", des$nesting[[nm]]) else ""
    sprintf("%d %s%s", clusters[[nm]], nm, suffix)
  }, character(1)), collapse = ", ")
  tpc <- des$trials_per_cell
  tpc_txt <- if (length(tpc) == 1L) sprintf("%d per subject", tpc) else sprintf("[%s] per subject (unbalanced)", paste(tpc, collapse = ", "))
  family <- if (!is.null(asm$residual_sd)) "Gaussian linear mixed model" else "generalized linear mixed model"
  method <- if (is.list(test)) `%||%`(test$method, "wald") else as.character(test)
  term <- if (is.list(test)) paste(test$term, collapse = ", ") else NA_character_
  md <- c(md,
          sprintf("- model: `%s` (%s)", paste(deparse(scenario$formula), collapse = " "), family),
          sprintf("- clusters: %s; observations: %s", cl, tpc_txt),
          if (!is.null(des$predictors)) sprintf("- predictors: %s", paste(names(des$predictors), collapse = ", ")) else NULL,
          sprintf("- test: %s on `%s`", method, term),
          if (is.list(test) && !is.null(test$contrast)) "- contrast: custom linear contrast" else NULL,
          "")

  # Assumptions / DGP
  md <- c(md, "## Data-generating assumptions", "")
  fe <- paste(vapply(names(asm$fixed_effects),
                     function(nm) sprintf("%s = %g", nm, asm$fixed_effects[[nm]]), character(1)),
              collapse = ", ")
  md <- c(md, sprintf("- fixed effects: %s", fe))
  md <- c(md, .mp_fmt_re_lines(asm$random_effects))
  if (!is.null(asm$residual_sd)) md <- c(md, sprintf("- residual SD: %g", asm$residual_sd))
  if (!is.null(asm$theta)) md <- c(md, sprintf("- negative-binomial theta: %g", asm$theta))
  if (!is.null(scenario$missing)) {
    md <- c(md, sprintf("- missing data: mechanism = %s", scenario$missing$mechanism))
  }
  md <- c(md, "")

  # Calibration
  md <- c(md, "## Type I calibration", "")
  if (!is.null(calibration)) {
    md <- c(md, sprintf("- empirical Type I at alpha = %g: **%.4f** (%g%% CI %.4f, %.4f) -> **%s**",
                        calibration$alpha, calibration$type1, 100 * calibration$conf_level,
                        calibration$ci[[1]], calibration$ci[[2]], calibration$verdict), "")
  } else {
    md <- c(md, "- not run. Power is not yet verified to control Type I error; run `mp_calibrate()` or `mp_plan()`.", "")
  }
  if (!is.null(recommendation)) {
    extra <- if (isTRUE(recommendation$caution)) sprintf(" (consider %s)", paste(recommendation$recommended, collapse = ", ")) else ""
    md <- c(md, sprintf("- inference-method guidance: current `%s`%s", recommendation$method, extra), "")
  }

  # Power
  md <- c(md, "## Power", "")
  md <- c(md, sprintf("- estimate: **%s** (%g%% %s CI %s, %s)", pct(power$power),
                      100 * power$conf_level, power$ci_method, pct(power$ci[[1]]), pct(power$ci[[2]])),
          sprintf("- Monte Carlo SE: %.4f; replicates: %d", `%||%`(power$mcse, NA_real_), power$nsim))
  if (!is.null(power$stopping)) {
    s <- power$stopping
    md <- c(md, sprintf("- adaptive stopping: %s after %d replicates (CI half-width %.4f)",
                        if (isTRUE(s$converged)) sprintf("converged (%s)", s$reason) else "hit max_nsim",
                        s$nsim_used, s$ci_halfwidth))
  }
  md <- c(md, "")

  # Diagnostics
  dg <- power$diagnostics
  md <- c(md, "## Diagnostics", "",
          sprintf("- model-fit failure rate: %s", pct(`%||%`(dg$fail_rate, NA_real_))),
          sprintf("- singular-fit rate: %s", pct(`%||%`(dg$singular_rate, NA_real_))),
          sprintf("- Type S (wrong sign): %s", if (is.null(dg$type_s) || is.na(dg$type_s)) "NA" else pct(dg$type_s)),
          sprintf("- Type M (exaggeration ratio): %s", if (is.null(dg$type_m) || is.na(dg$type_m)) "NA" else sprintf("%.2f", dg$type_m)),
          "")

  # Reproducibility
  if (!is.null(manifest_obj)) {
    md <- c(md, "## Reproducibility", "",
            sprintf("- seed: %s (%s)", `%||%`(as.character(manifest_obj$seed), "none"), manifest_obj$seed_strategy),
            sprintf("- R %s, mixpower %s", manifest_obj$r_version, manifest_obj$mixpower_version),
            sprintf("- scenario digest: %s", if (is.na(manifest_obj$scenario_digest)) "NA" else substr(manifest_obj$scenario_digest, 1, 16)),
            if (!is.na(manifest_obj$git_sha)) sprintf("- git: %s", substr(manifest_obj$git_sha, 1, 10)) else NULL,
            "")
  }

  # Warnings / fragile claims
  md <- c(md, "## Caveats and fragile claims", "")
  if (length(warnings) == 0L) {
    md <- c(md, "- None flagged.", "")
  } else {
    md <- c(md, paste0("- ", warnings), "")
  }

  md
}

# Scan for fragile/unsupported claims; returns a character vector of caveats.
.mp_report_warnings <- function(power, calibration, recommendation, scenario) {
  w <- character(0)
  dg <- power$diagnostics

  if (is.na(power$power)) {
    w <- c(w, "Power could not be estimated (no usable replicates); check the failure rate.")
  }
  if (is.null(calibration)) {
    if (.mp_calibration_risk(scenario)) {
      w <- c(w, "No Type I calibration was run, and this is a risky design (few clusters and/or complex random effects): the power estimate is NOT yet verified to be trustworthy. Run mp_calibrate() / mp_plan().")
    } else {
      w <- c(w, "No Type I calibration was run; consider mp_calibrate() to confirm the test controls its alpha.")
    }
  } else if (identical(calibration$verdict, "anti-conservative")) {
    w <- c(w, sprintf("Calibration found Type I error above alpha (%.3f); the power estimate is NOT trustworthy until the model/method is fixed.", calibration$type1))
  }
  if (!is.null(recommendation) && isTRUE(recommendation$caution)) {
    w <- c(w, sprintf("The chosen inference method may be anti-conservative for this cluster count; consider %s.", paste(recommendation$recommended, collapse = ", ")))
  }
  if (!is.null(dg$fail_rate) && !is.na(dg$fail_rate) && dg$fail_rate > 0.1) {
    w <- c(w, sprintf("High model-fit failure rate (%.0f%%); the power estimate may be biased.", 100 * dg$fail_rate))
  }
  if (!is.null(dg$singular_rate) && !is.na(dg$singular_rate) && dg$singular_rate > 0.2) {
    w <- c(w, sprintf("High singular-fit rate (%.0f%%); the random-effects structure may be too complex for this design.", 100 * dg$singular_rate))
  }
  if (!is.null(dg$type_m) && !is.na(dg$type_m) && dg$type_m > 1.3) {
    w <- c(w, sprintf("Statistically significant estimates exaggerate the effect by ~%.1fx (Type M); the design is underpowered for unbiased estimation.", dg$type_m))
  }
  if (!is.null(power$ci) && !anyNA(power$ci) && (power$ci[[2]] - power$ci[[1]]) / 2 > 0.1) {
    w <- c(w, "The power estimate is imprecise (wide confidence interval); increase nsim or use mp_power_adaptive().")
  }
  if (!is.null(scenario$missing)) {
    w <- c(w, sprintf("A '%s' missing-data mechanism is applied; power reflects incomplete data, conditional on that mechanism being correct.", scenario$missing$mechanism))
  }
  w
}
