# BayesGrove Project Handle

A reference-semantic handle for an open BayesGrove project.

## Usage

``` r
bg_handle(
  project_id,
  path,
  readonly = FALSE,
  closed = FALSE,
  loaded_graph_version = 0L,
  lock_token = NA_character_,
  registries = list(),
  metadata = list()
)
```

## Arguments

- project_id:

  Unique project identifier.

- path:

  File path to the project root.

- readonly:

  Logical, whether the handle is readonly.

- closed:

  Logical, whether the handle is closed.

- loaded_graph_version:

  Integer, current loaded graph version.

- lock_token:

  Character or NA, current lock token.

- registries:

  List of runtime registries.

- metadata:

  List of project metadata.
