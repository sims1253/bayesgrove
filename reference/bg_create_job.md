# Create a new job record

Create a new job record

## Usage

``` r
bg_create_job(project, run_id, node_id, backend = "local")
```

## Arguments

- project:

  A `bg_handle`.

- run_id:

  The run this job belongs to.

- node_id:

  The node this job executes.

- backend:

  The backend identifier.

## Value

The generated job record.
