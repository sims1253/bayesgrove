# List pending decision gates

List pending decision gates

## Usage

``` r
bg_pending_gates(project, graph = NULL)
```

## Arguments

- project:

  A `bg_handle`.

- graph:

  Optional already-loaded raw graph, to avoid a redundant read when the
  caller already has one for the same command cycle.

## Value

A list of denormalized pending gates with edge context.
