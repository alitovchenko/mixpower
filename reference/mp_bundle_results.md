# Bundle results with manifest and optional labels

Combines a single result object ([mp_power](mp_power.md),
[mp_sensitivity](mp_sensitivity.md), or
[mp_power_curve](mp_power_curve.md)), a reproducibility manifest, and
optional user labels into one object. Diagnostics and result structure
are retained unchanged.

## Usage

``` r
mp_bundle_results(
  result,
  manifest,
  study_id = NULL,
  analyst = NULL,
  notes = NULL
)
```

## Arguments

- result:

  An object of class `mp_power`, `mp_sensitivity`, or `mp_power_curve`.

- manifest:

  An `mp_manifest` object (from [`mp_manifest()`](mp_manifest.md)).

- study_id:

  Optional character; study or run identifier.

- analyst:

  Optional character; analyst name or ID.

- notes:

  Optional character; free-form notes.

## Value

An object of class `mp_bundle` with components `result`, `manifest`, and
`labels` (list with `study_id`, `analyst`, `notes`).
