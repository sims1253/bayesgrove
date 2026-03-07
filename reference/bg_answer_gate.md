# Answer a decision gate

Answer a decision gate

## Usage

``` r
bg_answer_gate(
  project,
  id,
  choice,
  rationale = NULL,
  refs = NULL,
  evidence = NULL
)
```

## Arguments

- project:

  A `bg_handle`.

- id:

  The gate ID to answer.

- choice:

  The chosen option.

- rationale:

  Required string explaining the choice.

- refs:

  Optional list of references.

- evidence:

  Optional vector of node IDs providing evidence.

## Value

A `bg_decision_record`.
