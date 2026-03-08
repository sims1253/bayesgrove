# Register a node kind in the bayesgrove runtime registry

Node kinds have a structural component (stored in the graph) and a
runtime component (the executor, stored in the handle).

## Usage

``` r
bg_register_node_kind(
  project,
  kind,
  input_contract = NULL,
  output_type = NULL,
  param_schema = NULL,
  executor = NULL
)
```

## Arguments

- project:

  A `bg_handle`.

- kind:

  The name of the node kind.

- input_contract:

  Optional input contract for the structural graph.

- output_type:

  Optional output type string.

- param_schema:

  Optional parameter schema.

- executor:

  Optional R function to execute nodes of this kind.
