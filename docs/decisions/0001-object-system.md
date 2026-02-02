# ADR 0001: Use S3 for core objects

## Context

mixpower needs lightweight objects to represent study designs, assumptions, and
scenarios. These objects must be easy to serialize, simple to extend, and familiar
to R users who build simulation pipelines.

## Decision

Use S3 classes for `mixpower_design`, `mixpower_assumptions`, and
`mixpower_scenario`.

## Alternatives

- S4 classes with formal slots and validity methods.
- Reference classes or R6 objects to encapsulate state.

## Consequences

- S3 keeps constructors minimal and keeps user-facing types simple.
- S3 is easier to extend by downstream packages without requiring class
  definitions.
- We forego formal slot validation and stricter type checking from S4.

## Examples

- `design(alpha = 0.05)` returns a list tagged with class `mixpower_design`.
- `scenario(design(), assumptions())` composes two S3 objects.

## S3 vs S4 rationale

S3 offers lower ceremony and aligns with tidy workflows. S4 would add more
structure but increases boilerplate for a small API surface. If the object model
grows to require strict validation or method dispatch across many classes, we can
revisit S4 later.
