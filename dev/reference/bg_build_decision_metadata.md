# Build a decision metadata list from action and payload fields

Build a decision metadata list from action and payload fields

## Usage

``` r
bg_build_decision_metadata(action, payload, extra = list())
```

## Arguments

- action:

  An action list with `action_id`.

- payload:

  A payload list with optional provenance fields.

- extra:

  Additional fields to merge into the metadata.

## Value

A filtered list of non-NULL metadata entries.
