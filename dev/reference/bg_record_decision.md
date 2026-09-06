# Record an explicit decision

Record an explicit decision

## Usage

``` r
bg_record_decision(
  project,
  scope,
  prompt,
  choice,
  alternatives = NULL,
  rationale = NULL,
  refs = NULL,
  evidence = NULL,
  kind = "note",
  metadata = list()
)
```

## Arguments

- project:

  A `bg_handle`.

- scope:

  Scope of the decision (e.g. 'project', 'node:node_id').

- prompt:

  The question/context.

- choice:

  The decision made.

- alternatives:

  Optional alternatives not chosen.

- rationale:

  Required rationale string.

- refs:

  Optional references.

- evidence:

  Optional node IDs providing evidence.

- kind:

  Decision kind, typically "note" or "gate_answer".

- metadata:

  Optional decision metadata.

## Value

The generated `bg_decision_record`.
