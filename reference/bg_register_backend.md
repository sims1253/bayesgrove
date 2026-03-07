# Register a backend plugin

Backends provide domain-specific compilation and fitting implementations
(e.g. cmdstanr, brms).

## Usage

``` r
bg_register_backend(project, name, backend_impl)
```

## Arguments

- project:

  A `bg_handle`.

- name:

  The name of the backend.

- backend_impl:

  A list containing the backend interface methods.
