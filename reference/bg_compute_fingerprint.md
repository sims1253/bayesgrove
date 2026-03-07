# Compute the cache fingerprint for a node

Compute the cache fingerprint for a node

## Usage

``` r
bg_compute_fingerprint(
  project,
  node_id,
  upstream_fingerprints = list(),
  environment_manifest = list()
)
```

## Arguments

- project:

  A `bg_handle`.

- node_id:

  The ID of the node to fingerprint.

- upstream_fingerprints:

  A named list mapping upstream node IDs to their computed fingerprints.

- environment_manifest:

  Optional list of package versions.

## Value

A SHA-256 fingerprint string.
