# Read project graph

Read project graph

## Usage

``` r
bg_read_graph(project)
```

## Arguments

- project:

  A `bg_handle`

## Value

A validated graph with class `dagriculture_graph`. Uses payload-level
validation and manual class coercion until dagriculture exposes a native
JSON deserializer.
