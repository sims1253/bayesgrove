# Compute the cache fingerprint for a node

The fingerprint is a SHA-256 over a deterministic JSON payload
combining: the serialization format version, the node kind, normalized
params, ordered upstream fingerprints, an environment manifest (R and
key package versions), the registered executor, and any backend
signature.

## Usage

``` r
bg_compute_fingerprint(
  project,
  node_id,
  upstream_fingerprints = list(),
  environment_manifest = list(),
  graph = NULL
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

  Optional list of package versions. When empty, a default manifest (R +
  bayesgrove version) is used so that reinstalling a new package version
  invalidates caches.

- graph:

  Optional pre-read graph. If `NULL` the graph is read from disk.

## Value

A SHA-256 fingerprint string.
