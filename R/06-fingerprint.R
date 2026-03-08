#' Compute the cache fingerprint for a node
#'
#' @param project A `bg_handle`.
#' @param node_id The ID of the node to fingerprint.
#' @param upstream_fingerprints A named list mapping upstream node IDs to their computed fingerprints.
#' @param environment_manifest Optional list of package versions.
#' @param graph Optional pre-read graph. If `NULL` the graph is read from disk.
#'
#' @return A SHA-256 fingerprint string.
#' @export
bg_compute_fingerprint <- function(
  project,
  node_id,
  upstream_fingerprints = list(),
  environment_manifest = list(),
  graph = NULL
) {
  S7::check_is_S7(project, bg_handle)

  if (is.null(graph)) {
    graph <- bg_read_graph(project)
  }
  if (!node_id %in% names(graph$nodes)) {
    cli::cli_abort("Node {.val {node_id}} not found in graph.")
  }

  node <- graph$nodes[[node_id]]

  # 1. Kind + Normalized params
  # sort keys to ensure deterministic JSON serialization
  if (length(node$params) > 0 && !is.null(names(node$params))) {
    sorted_params <- node$params[order(names(node$params))]
  } else {
    sorted_params <- node$params
  }
  params_json <- jsonlite::toJSON(
    sorted_params,
    auto_unbox = TRUE,
    null = "null"
  )

  # 2. Upstream fingerprints
  # Need to ensure they are sorted by the input edge to maintain determinism
  upstream_edges <- bg_dagri_order_edges(bg_dagri_incoming_edges(
    graph,
    node_id
  ))

  ordered_upstream_hashes <- character(0)
  for (e in upstream_edges) {
    from_id <- e$from
    if (!from_id %in% names(upstream_fingerprints)) {
      cli::cli_abort(
        "Missing upstream fingerprint for node {.val {from_id}} required by {.val {node_id}}."
      )
    }
    ordered_upstream_hashes <- c(
      ordered_upstream_hashes,
      paste0(e$type, ":", upstream_fingerprints[[from_id]])
    )
  }

  # 3. Environment/Dependencies (for now just hash the manifest list)
  if (
    length(environment_manifest) > 0 && !is.null(names(environment_manifest))
  ) {
    sorted_env <- environment_manifest[order(names(environment_manifest))]
  } else {
    sorted_env <- environment_manifest
  }
  env_json <- jsonlite::toJSON(sorted_env, auto_unbox = TRUE, null = "null")

  # 4. Backend signatures (if it's a compile or fit node, check registries)
  # We extract backend from params if present
  backend_name <- node$params$backend %||% "default"
  backend_hash <- ""

  if (!is.null(project@registries$backends[[backend_name]])) {
    backend <- project@registries$backends[[backend_name]]
    if (is.function(backend$backend_runtime_signature)) {
      sig <- backend$backend_runtime_signature(node$params)
      # We only hash fingerprint_fields, not compatibility_fields
      if (!is.null(sig$fingerprint_fields)) {
        sorted_sig <- sig$fingerprint_fields[order(names(
          sig$fingerprint_fields
        ))]
        backend_hash <- jsonlite::toJSON(
          sorted_sig,
          auto_unbox = TRUE,
          null = "null"
        )
      }
    }

    # Optional backend source hash (e.g. Stan program text)
    if (
      is.function(backend$backend_source_hash) &&
        node$kind %in% c("compile", "model_spec")
    ) {
      backend_hash <- paste0(
        backend_hash,
        "|source:",
        backend$backend_source_hash(node$params)
      )
    }
  }

  # 5. Serialization format version
  format_version <- "1"

  # Combine all components deterministically
  components <- list(
    format = format_version,
    kind = node$kind,
    params = as.character(params_json),
    upstreams = ordered_upstream_hashes,
    env = as.character(env_json),
    backend = backend_hash
  )

  payload <- jsonlite::toJSON(components, auto_unbox = TRUE, null = "null")

  # Return xxhash32 for MVP, but SHA-256 is the target
  # We use digest::digest with algo = "sha256"
  sprintf("sha256:%s", digest::digest(payload, algo = "sha256"))
}
