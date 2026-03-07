# Add a decision gate to an edge

Add a decision gate to an edge

## Usage

``` r
bg_add_gate(project, from, to, prompt, options, refs = NULL, metadata = list())
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

- options:

  Character vector of valid options.

- refs:

  Optional list of references.

- metadata:

  Optional metadata.

## Value

The generated `bg_pending_gate` record.
