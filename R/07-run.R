#' Check if an artifact exists in cache
#' @keywords internal
#' @export
bg_check_artifact <- function(project, fingerprint, node_id = NULL) {
  idx <- bg_read_artifact_index(project)
  entry <- idx[[fingerprint]] %||% NULL
  if (is.null(entry) || !identical(entry$status, "active")) {
    return(NULL)
  }

  if (!is.null(node_id)) {
    binding <- bg_artifact_binding(idx, fingerprint, node_id)
    if (!is.null(binding)) {
      return(
        if (identical(binding$status, "active")) entry$artifact_ref else NULL
      )
    }
  }

  entry$artifact_ref
}

#' Store an artifact in cache
#' @keywords internal
#' @export
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

  bg_modify_artifact_index(project, {
    idx <- bg_read_artifact_index(project)
    superseded <- bg_supersede_artifacts_for_node(
      idx,
      node_id,
      except_fingerprint = fingerprint
    )
    idx <- superseded$index

    entry <- bg_normalize_artifact_entry(
      fingerprint,
      idx[[fingerprint]] %||% list()
    )
    created_at <- entry$created_at %||% bg_now_timestamp()
    entry$artifact_ref <- artifact_ref
    entry$created_at <- created_at
    entry$bindings[[node_id]] <- list(
      node_id = node_id,
      status = "active",
      updated_at = bg_now_timestamp()
    )
    entry <- bg_refresh_artifact_entry(entry)
    idx[[fingerprint]] <- entry

    bg_write_artifact_index(project, idx)
  })

  artifact_ref
}

#' Fetch an artifact from cache
#' @keywords internal
#' @export
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

bg_read_artifact_index <- function(project) {
  index_path <- bg_artifact_index_path(project)
  if (!file.exists(index_path)) {
    return(list())
  }

  bg_normalize_artifact_index(jsonlite::read_json(
    index_path,
    simplifyVector = FALSE
  ))
}

#' @keywords internal
bg_normalize_execution_result <- function(result) {
  if (!is.list(result) || is.null(names(result))) {
    return(list(artifact = result, summaries = list(), metadata = list()))
  }

  if (!"summaries" %in% names(result)) {
    return(list(artifact = result, summaries = list(), metadata = list()))
  }

  artifact <- result$result %||% result$artifact %||% result$value
  if (is.null(artifact) && !is.null(result$artifacts)) {
    artifact <- result$artifacts
  }
  if (is.null(artifact)) {
    artifact <- result[setdiff(
      names(result),
      c("summaries", "metadata", "status", "execution_fingerprint")
    )]
  }

  list(
    artifact = artifact,
    summaries = result$summaries %||% list(),
    metadata = result$metadata %||% list()
  )
}

#' Create an execution plan
#'
#' @param project A `bg_handle`.
#' @param targets Optional character vector of target node IDs.
#' @param external_holds Optional named list mapping node ids to external hold
#'   reasons. Held nodes remain distinct from structural blockers.
#' @param mode Execution mode: 'sync' or 'async'.
#' @param include_inactive Whether to keep retired or disabled nodes in the
#'   planning graph. Defaults to `FALSE`.
#'
#' @return A `bg_run_plan` list.
#' @export
bg_plan <- function(
  project,
  targets = NULL,
  external_holds = list(),
  mode = c("sync", "async"),
  include_inactive = FALSE
) {
  mode <- match.arg(mode)
  S7::check_is_S7(project, bg_handle)

  full_graph <- bg_read_graph(project)
  inactive_node_ids <- if (isTRUE(include_inactive)) {
    character()
  } else {
    bg_inactive_node_ids(project, graph = full_graph)
  }
  if (!is.null(targets) && length(targets) > 0 && !isTRUE(include_inactive)) {
    inactive_targets <- intersect(targets, inactive_node_ids)
    if (length(inactive_targets) > 0) {
      cli::cli_abort(
        "Cannot plan retired or disabled nodes: {.val {inactive_targets}}."
      )
    }
  }

  graph <- if (isTRUE(include_inactive)) {
    full_graph
  } else {
    bg_active_graph(project, graph = full_graph)
  }
  graph <- dagriculture::dagri_recompute_state(graph)
  graph_plan <- dagriculture::dagri_plan(
    graph,
    targets,
    external_holds = external_holds
  )

  # Forward propagate fingerprints to determine cache hits
  fingerprints <- list()
  artifact_refs <- list()
  cache_hits <- character(0)
  missing_results <- character(0)
  input_bindings <- list()

  for (node_id in graph_plan$topo_order) {
    upstream_edges <- Filter(function(e) e$to == node_id, graph$edges)
    up_fps <- list()
    can_fingerprint <- TRUE

    for (e in upstream_edges) {
      if (is.null(fingerprints[[e$from]])) {
        can_fingerprint <- FALSE
        break
      }
      up_fps[[e$from]] <- fingerprints[[e$from]]
    }

    if (!can_fingerprint) {
      next
    }

    fingerprints[[node_id]] <- bg_compute_fingerprint(
      project,
      node_id,
      upstream_fingerprints = up_fps
    )
  }

  for (node_id in graph_plan$topo_order) {
    upstream_edges <- Filter(function(e) e$to == node_id, graph$edges)
    node_bindings <- list()
    can_plan <- TRUE

    for (e in upstream_edges) {
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

    fp <- fingerprints[[node_id]] %||% NULL
    if (is.null(fp)) {
      next
    }
    fingerprints[[node_id]] <- fp

    input_bindings[[node_id]] <- node_bindings

    cached_ref <- bg_check_artifact(project, fp, node_id = node_id)

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

  held_nodes <- names(graph_plan$external_blocked %||% list())
  to_execute <- setdiff(
    intersect(missing_results, graph_plan$eligible),
    held_nodes
  )

  list(
    graph_plan = graph_plan,
    targets = targets %||% graph_plan$targets,
    eligible = graph_plan$eligible,
    blocked = graph_plan$blocked,
    external_blocked = graph_plan$external_blocked,
    held_by_policy = graph_plan$external_blocked,
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
#' @param backend For async mode, the backend to use ('auto', 'callr', 'mirai').
#'
#' @return A `bg_run_handle` list.
#' @export
bg_run <- function(
  project,
  targets = NULL,
  mode = c("sync", "async"),
  backend = c("auto", "callr", "mirai")
) {
  mode <- match.arg(mode)
  backend <- match.arg(backend)
  S7::check_is_S7(project, bg_handle)

  if (bg_workflow_paused(project)) {
    cli::cli_abort(
      "Workflow is paused. Call {.fn bg_resume} before dispatching new work."
    )
  }

  if (mode == "async") {
    return(bg_submit(
      project,
      targets = targets,
      mode = "async",
      backend = backend
    ))
  }
  external_holds <- bg_workflow_external_holds(project)
  plan <- bg_plan(
    project,
    targets,
    external_holds = external_holds,
    mode = "sync"
  )
  graph <- bg_read_graph(project)

  run_id <- sprintf("run_%s", digest::digest(runif(1), algo = "xxhash32"))
  job_ids <- character(0)
  num_executed <- 0L
  run_started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

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
    job <- bg_create_job(project, run_id, node_id, backend = "sync")
    job_ids <- c(job_ids, job$job_id)
    bg_update_job(
      project,
      job$job_id,
      status = "running",
      started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )

    node <- graph$nodes[[node_id]]
    kind_reg <- project@registries$node_kinds[[node$kind]]

    if (is.null(kind_reg) || is.null(kind_reg$executor)) {
      bg_update_job(
        project,
        job$job_id,
        status = "failed",
        error = list(
          message = sprintf(
            "No executor registered for node kind %s.",
            node$kind
          )
        ),
        finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )
      res <- list(
        run_id = run_id,
        status = "failed",
        mode = mode,
        targets = plan$targets,
        job_ids = job_ids,
        submitted_at = run_started_at,
        started_at = run_started_at,
        finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        summary = list(total_executed = num_executed - 1L),
        error = list(
          message = sprintf(
            "No executor registered for node kind %s.",
            node$kind
          )
        ),
        metadata = list(
          held_by_policy = plan$held_by_policy %||% list()
        )
      )
      class(res) <- "bg_run_handle"
      return(res)
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
    execution_result <- tryCatch(
      {
        execution <- kind_reg$executor(node, resolved_inputs)
        normalized <- bg_normalize_execution_result(execution)

        fp <- plan$metadata$fingerprints[[node_id]]
        ref <- bg_store_artifact(project, node_id, fp, normalized$artifact)
        bg_write_summaries(
          project = project,
          node_id = node_id,
          artifact_ref = ref,
          execution_fingerprint = fp,
          summaries = normalized$summaries
        )

        bg_update_job(
          project,
          job$job_id,
          status = "succeeded",
          result_ref = ref,
          finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        )

        list(ok = TRUE, ref = ref)
      },
      error = function(e) {
        bg_update_job(
          project,
          job$job_id,
          status = "failed",
          error = list(message = e$message),
          finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        )
        list(ok = FALSE, error = e)
      }
    )

    if (!isTRUE(execution_result$ok)) {
      res <- list(
        run_id = run_id,
        status = "failed",
        mode = mode,
        targets = plan$targets,
        job_ids = job_ids,
        submitted_at = run_started_at,
        started_at = run_started_at,
        finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        summary = list(total_executed = num_executed - 1L),
        error = list(message = execution_result$error$message),
        metadata = list(
          held_by_policy = plan$held_by_policy %||% list()
        )
      )
      class(res) <- "bg_run_handle"
      return(res)
    }

    external_holds <- bg_workflow_external_holds(project)
    plan <- bg_plan(
      project,
      targets,
      external_holds = external_holds,
      mode = "sync"
    )
  }

  final_status <- if (length(plan$to_execute) == 0) {
    if (length(plan$held_by_policy %||% list()) > 0) {
      "blocked"
    } else {
      "succeeded"
    }
  } else {
    "partial"
  }

  res <- list(
    run_id = run_id,
    status = final_status,
    mode = mode,
    targets = plan$targets,
    job_ids = job_ids,
    submitted_at = run_started_at,
    started_at = run_started_at,
    finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    summary = list(total_executed = num_executed),
    error = NULL,
    metadata = list(
      held_by_policy = plan$held_by_policy %||% list()
    )
  )
  class(res) <- "bg_run_handle"
  res
}
