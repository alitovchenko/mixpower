# Record a calibration result on a scenario

Attaches an [`mp_calibrate()`](mp_calibrate.md) result to a scenario so
that [`mp_power()`](mp_power.md) knows the Type I error has been checked
(and will not emit its calibrate-first nudge). The calibration is stored
on the scenario and travels with it.

## Usage

``` r
mp_attach_calibration(scenario, calibration)
```

## Arguments

- scenario:

  An `mp_scenario`.

- calibration:

  An `mp_calibration` object (from [`mp_calibrate()`](mp_calibrate.md)).

## Value

The scenario with `calibration` attached.

## See also

[`mp_calibrate()`](mp_calibrate.md), [`mp_plan()`](mp_plan.md).
