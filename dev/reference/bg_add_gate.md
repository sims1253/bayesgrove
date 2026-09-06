# Add a decision gate to an edge

Add a decision gate to an edge

## Usage

``` r
bg_add_gate(
  project,
  from,
  to,
  prompt,
  alternatives,
  refs = NULL,
  metadata = list()
)
```

## Arguments

- project:

  A `bg_handle`.

- from:

  Upstream node ID.

- to:

  Downstream node ID.

- prompt:

  The question presented to the user.

- alternatives:

  Character vector of valid choices.

- refs:

  Optional list of references.

- metadata:

  Optional metadata.

## Value

A gate specification list containing the gate ID, edge ID, prompt,
options, refs, and metadata.
