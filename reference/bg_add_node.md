# Add a node to the BayesGrove project graph

Add a node to the BayesGrove project graph

## Usage

``` r
bg_add_node(
  project,
  kind,
  label = NULL,
  params = list(),
  inputs = NULL,
  metadata = list()
)
```

## Arguments

- project:

  A `bg_handle`.

- kind:

  The node kind string.

- label:

  Optional label.

- params:

  Named list of parameters.

- inputs:

  Optional character vector of upstream node IDs to connect.

- metadata:

  Optional metadata list.

## Value

The generated node ID.
