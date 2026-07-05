#' Compute the cache fingerprint for a node
#'
#' The fingerprint is a SHA-256 over a deterministic JSON payload combining:
#' the serialization format version, the node kind, normalized params, ordered
#' upstream fingerprints, an environment manifest (R and key package
#' versions), the registered executor, and any backend signature.
#'
#' @param project A `bg_handle`.
#' @param node_id The ID of the node to fingerprint.
#' @param upstream_fingerprints A named list mapping upstream node IDs to their computed fingerprints.
#' @param environment_manifest Optional list of package versions. When empty,
#'   a default manifest (R + bayesgrove version) is used so that reinstalling
#'   a new package version invalidates caches.
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

  # 1. Kind + Normalized params (sort keys for deterministic JSON)
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

  # 2. Upstream fingerprints, ordered by input edge for determinism
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

  # 3. Environment manifest. A caller-supplied manifest (from bg_plan) is
  #    already resolved; an empty manifest falls back to the default so that
  #    direct bg_compute_fingerprint() calls still pin R + bayesgrove version.
  manifest <- bg_resolve_environment_manifest(environment_manifest)
  env_json <- jsonlite::toJSON(manifest, auto_unbox = TRUE, null = "null")

  # 4. Executor. Resolution order:
  #    a. built-in executor id ("builtin:<id>") -> hash id + package version
  #       (stable across reinstalls of the same version, changes on upgrade).
  #    b. user-registered executor function -> hash deparsed source body.
  #    c. no executor registered -> empty string (planning without executors
  #       must still fingerprint, e.g. read-only inspection).
  executor_hash <- bg_executor_fingerprint_component(project, node)

  # 5. Stan-program source hash. For nodes carrying a `stan_file` param, hash
  #    the file CONTENTS (not the path) so editing the model program invalidates
  #    the cache even at a fixed path. Applied by param presence, so custom
  #    kinds carrying `stan_file` are covered; returns "" for nodes without it.
  source_hash <- bg_source_hash_component(node)

  # 6. Serialization format version. Bumping invalidates all prior caches.
  format_version <- "2"

  # Combine all components deterministically
  components <- list(
    format = format_version,
    kind = node$kind,
    params = as.character(params_json),
    upstreams = ordered_upstream_hashes,
    env = as.character(env_json),
    executor = executor_hash,
    source = source_hash
  )

  payload <- jsonlite::toJSON(components, auto_unbox = TRUE, null = "null")

  sprintf("sha256:%s", digest::digest(payload, algo = "sha256"))
}

#' @keywords internal
#' @noRd
bg_default_environment_manifest <- function() {
  manifest <- list(
    r = as.character(getRversion()),
    bayesgrove = as.character(utils::packageVersion("bayesgrove"))
  )

  # Fold in backend package versions for fit-capable backends when installed,
  # so a cmdstanr/brms upgrade invalidates caches.
  if (requireNamespace("cmdstanr", quietly = TRUE)) {
    manifest$cmdstanr <- as.character(utils::packageVersion("cmdstanr"))
  }
  if (requireNamespace("brms", quietly = TRUE)) {
    manifest$brms <- as.character(utils::packageVersion("brms"))
  }

  manifest
}

#' Resolve the environment manifest used in fingerprinting.
#'
#' A non-empty caller manifest is treated as already resolved (bg_plan builds
#' it once per plan and passes it in), so no default is recomputed per node.
#' An empty manifest falls back to the default so that direct
#' bg_compute_fingerprint() calls still pin the R and bayesgrove versions.
#' @keywords internal
#' @noRd
bg_resolve_environment_manifest <- function(environment_manifest) {
  if (length(environment_manifest) > 0) {
    return(environment_manifest)
  }

  bg_default_environment_manifest()
}

#' Compute the executor component of a node fingerprint.
#'
#' Returns a string that changes when the executor body changes (user
#' executors) or when the built-in executor version changes (built-ins).
#' Planning without a registered executor hashes the empty string.
#' @keywords internal
#' @noRd
bg_executor_fingerprint_component <- function(project, node) {
  kind_reg <- project@registries$node_kinds[[node$kind]] %||% NULL

  if (is.null(kind_reg)) {
    return("")
  }

  # Built-in executors store an executor_ref of the form "builtin:<id>" and
  # are fingerprinted by id + package version (stable across reinstalls of
  # the same version, invalidating on upgrade). The built-in registry is
  # established in Phase 4; until then this branch is forward-compatible.
  executor_ref <- kind_reg$executor_ref %||% NULL
  if (!is.null(executor_ref) && startsWith(executor_ref, "builtin:")) {
    version <- as.character(utils::packageVersion("bayesgrove"))
    return(digest::digest(
      paste0(executor_ref, "@", version),
      algo = "sha256"
    ))
  }

  executor <- kind_reg$executor %||% NULL
  if (!is.function(executor)) {
    return("")
  }

  digest::digest(
    paste(deparse(executor), collapse = "\n"),
    algo = "sha256"
  )
}

#' Compute the source-code hash component for a node.
#'
#' For any node carrying a `stan_file` param pointing at an existing file, hash
#' the file CONTENTS (not the path): editing the model program must invalidate
#' the cache even when the path is unchanged. Applied by param presence rather
#' than by kind name, so custom node kinds carrying `stan_file` are covered.
#' When the param names a missing file, returns the literal `"missing_stan_file"`
#' so planning still works and the eventual executor error is the user-facing
#' signal. Returns `""` for nodes without a `stan_file` param (the formula is
#' already hashed via params, the brms version via the environment manifest, and
#' codegen drift between brms versions is covered by the version pin).
#' @keywords internal
#' @noRd
bg_source_hash_component <- function(node) {
  stan_file <- node$params$stan_file %||% NULL
  if (
    is.null(stan_file) || !is.character(stan_file) || length(stan_file) != 1L
  ) {
    return("")
  }

  if (!file.exists(stan_file)) {
    return("missing_stan_file")
  }

  digest::digest(file = stan_file, algo = "sha256")
}
