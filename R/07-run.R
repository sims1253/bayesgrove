#' Check if an artifact exists in cache
#' @keywords internal
bg_check_artifact <- function(project, fingerprint) {
  # Strip prefix for directory path
  hash <- sub("^sha256:", "", fingerprint)
  prefix <- substr(hash, 1, 2)

  index_path <- file.path(project@path, ".bayesgrove", "cache", "index.json")
  if (!file.exists(index_path)) {
    return(NULL)
  }

  idx <- jsonlite::read_json(index_path)
  if (!is.null(idx[[fingerprint]])) {
    return(idx[[fingerprint]]$artifact_ref)
  }

  NULL
}

#' Store an artifact in cache
#' @keywords internal
bg_store_artifact <- function(project, node_id, fingerprint, result) {
  # Write object to temp file to get its hash
  tmp <- tempfile()
  saveRDS(result, tmp)

  file_hash <- digest::digest(file = tmp, algo = "sha256")
  prefix <- substr(file_hash, 1, 2)

  cas_dir <- file.path(project@path, ".bayesgrove", "cache", "sha256", prefix)
  dir.create(cas_dir, recursive = TRUE, showWarnings = FALSE)

  artifact_ref <- sprintf("cas:sha256:%s", file_hash)
  dest_path <- file.path(cas_dir, paste0(file_hash, ".rds"))

  if (!file.exists(dest_path)) {
    file.rename(tmp, dest_path)
  } else {
    unlink(tmp)
  }

  # Update index
  index_path <- file.path(project@path, ".bayesgrove", "cache", "index.json")
  if (file.exists(index_path)) {
    idx <- jsonlite::read_json(index_path)
  } else {
    idx <- list()
  }

  idx[[fingerprint]] <- list(
    artifact_ref = artifact_ref,
    node_id = node_id,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  jsonlite::write_json(idx, index_path, auto_unbox = TRUE, pretty = TRUE)

  artifact_ref
}

#' Fetch an artifact from cache
#' @keywords internal
bg_fetch_artifact <- function(project, ref) {
  if (!startsWith(ref, "cas:sha256:")) {
    cli::cli_abort("Invalid artifact ref format: {.val {ref}}")
  }

  hash <- sub("^cas:sha256:", "", ref)
  prefix <- substr(hash, 1, 2)

  path <- file.path(
    project@path,
    ".bayesgrove",
    "cache",
    "sha256",
    prefix,
    paste0(hash, ".rds")
  )

  if (!file.exists(path)) {
    cli::cli_abort("Artifact file missing at {.path {path}}")
  }

  readRDS(path)
}

#' Create an execution plan
#'
#' @param project A `bg_handle`.
#' @param targets Optional character vector of target node IDs.
#' @param mode Execution mode: 'sync' or 'async'.
#'
#' @return A `bg_run_plan` list.
#' @export
bg_plan <- function(project, targets = NULL, mode = c("sync", "async")) {
  mode <- match.arg(mode)
  S7::check_is_S7(project, bg_handle)

  graph <- bg_read_graph(project)
  graph <- dagriculture::dagri_recompute_state(graph)
  graph_plan <- dagriculture::dagri_plan(graph, targets)

  # Forward propagate fingerprints to determine cache hits
  fingerprints <- list()
  artifact_refs <- list()
  cache_hits <- character(0)
  missing_results <- character(0)
  input_bindings <- list()

  for (node_id in graph_plan$topo_order) {
    # If the node is blocked or a pending gate blocks it, we stop planning this path
    # (dagri_plan eligible only contains ready unblocked nodes, but we iterate topo_order)

    # We only compute for structurally eligible, but what about downstream of cache hits?
    # A structurally blocked node (e.g. missing upstream) is not eligible.
    # But if upstream was a cache hit, the node IS eligible.

    # Let's collect upstream fingerprints and artifact bindings
    upstream_edges <- Filter(function(e) e$to == node_id, graph$edges)
    up_fps <- list()
    node_bindings <- list()
    can_plan <- TRUE

    for (e in upstream_edges) {
      if (is.null(fingerprints[[e$from]])) {
        can_plan <- FALSE
        break
      }
      up_fps[[e$from]] <- fingerprints[[e$from]]

      # We also need to bind the actual artifacts for execution
      if (is.null(artifact_refs[[e$from]])) {
        can_plan <- FALSE
        break
      }

      node_bindings[[length(node_bindings) + 1]] <- list(
        edge_id = e$id,
        from_node_id = e$from,
        edge_type = e$type,
        artifact_ref = artifact_refs[[e$from]]
      )
    }

    if (!can_plan) {
      next
    }

    # Compute fingerprint
    fp <- bg_compute_fingerprint(
      project,
      node_id,
      upstream_fingerprints = up_fps
    )
    fingerprints[[node_id]] <- fp

    input_bindings[[node_id]] <- node_bindings

    # Check cache
    # In the MVP we check our dummy metadata cache
    cached_ref <- if (!is.null(project@metadata$mvp_cache[[fp]])) {
      project@metadata$mvp_cache[[fp]]
    } else {
      bg_check_artifact(project, fp)
    }

    if (!is.null(cached_ref)) {
      cache_hits <- c(cache_hits, node_id)
      artifact_refs[[node_id]] <- cached_ref
    } else {
      if (node_id %in% graph_plan$eligible) {
        missing_results <- c(missing_results, node_id)
      }
      # If it's missing, downstream cannot be fully planned yet.
      # They will just not have an artifact_ref.
    }
  }

  to_execute <- intersect(missing_results, graph_plan$eligible)

  list(
    graph_plan = graph_plan,
    targets = targets %||% graph_plan$targets,
    eligible = graph_plan$eligible,
    blocked = graph_plan$blocked,
    cache_hits = cache_hits,
    missing_results = missing_results,
    to_execute = to_execute,
    input_bindings = input_bindings,
    mode = mode,
    metadata = list(
      fingerprints = fingerprints
    )
  )
}

#' Run a project workflow
#'
#' @param project A `bg_handle`.
#' @param targets Optional character vector of target node IDs.
#' @param mode Execution mode: 'sync' or 'async'.
#'
#' @return A `bg_run_handle` list.
#' @export
bg_run <- function(project, targets = NULL, mode = c("sync", "async")) {
  mode <- match.arg(mode)
  S7::check_is_S7(project, bg_handle)

  if (mode == "async") {
    cli::cli_abort("Async mode not yet implemented for MVP Phase 1.")
  }

  plan <- bg_plan(project, targets, mode = "sync")
  graph <- bg_read_graph(project)

  run_id <- sprintf("run_%s", digest::digest(runif(1), algo = "xxhash32"))
  job_ids <- character(0)
  num_executed <- 0L

  cli::cli_inform(
    "Starting run {.val {run_id}} with {length(plan$to_execute)} node{?s} to execute."
  )

  # Synchronous execution
  for (node_id in plan$graph_plan$topo_order) {
    if (!node_id %in% plan$to_execute) {
      next
    }

    cli::cli_inform("Running node {.val {node_id}}...")
    num_executed <- num_executed + 1L

    node <- graph$nodes[[node_id]]
    kind_reg <- project@registries$node_kinds[[node$kind]]

    if (is.null(kind_reg) || is.null(kind_reg$executor)) {
      cli::cli_abort("No executor registered for node kind {.val {node$kind}}.")
    }

    # Resolve input bindings
    bindings <- plan$input_bindings[[node_id]]
    resolved_inputs <- list()
    for (b in bindings) {
      resolved_inputs[[b$from_node_id]] <- bg_fetch_artifact(
        project,
        b$artifact_ref
      )
    }

    # Execute
    result <- tryCatch(
      {
        kind_reg$executor(node, resolved_inputs)
      },
      error = function(e) {
        cli::cli_abort(
          "Execution failed for node {.val {node_id}}: {e$message}"
        )
      }
    )

    # Store artifact
    fp <- plan$metadata$fingerprints[[node_id]]
    ref <- bg_store_artifact(project, node_id, fp, result)

    # We must replan to update downstream artifact bindings in case they become eligible
    # For a pure synchronous loop, updating our local artifact_refs would suffice,
    # but replanning is safer for branching.
    # For MVP, we'll just re-call bg_plan to get the next step.
    plan <- bg_plan(project, targets, mode = "sync")
  }

  list(
    run_id = run_id,
    status = if (length(plan$to_execute) == 0) "succeeded" else "partial",
    mode = mode,
    targets = plan$targets,
    job_ids = job_ids,
    submitted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    summary = list(total_executed = num_executed),
    error = NULL,
    metadata = list()
  )
}
