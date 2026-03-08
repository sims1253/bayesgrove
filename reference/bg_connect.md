# Connect two nodes in the bayesgrove project graph

Connect two nodes in the bayesgrove project graph

## Usage

``` r
bg_connect(project, from, to, edge_type = "data", metadata = list())
```

## Arguments

- project:

  A `bg_handle`.

- from:

  The upstream node ID.

- to:

  The downstream node ID.

- edge_type:

  The type of edge (default: "data").

- metadata:

  Optional metadata list.

## Value

The generated edge ID.
