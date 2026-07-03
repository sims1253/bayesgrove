#' Check if an artifact exists in cache
#'
#' @return The artifact reference string if found and active, otherwise `NULL`.
#' @keywords internal
#' @export
bg_check_artifact <- function(
  project,
  fingerprint,
  node_id = NULL,
  artifact_index = NULL
) {
  idx <- artifact_index %||% bg_read_artifact_index(project)
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
#'
#' @return The artifact reference string (e.g. `"cas:sha256:..."`).
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
#'
#' @return The deserialized R object stored at the given reference.
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
  graph <- bg_dagri_recompute_state(graph)
  graph_plan <- bg_dagri_plan(
    graph,
    targets,
    external_holds = external_holds
  )

  # Forward propagate fingerprints to determine cache hits
  fingerprints <- list()
  artifact_refs <- list()
  artifact_index <- bg_read_artifact_index(project)
  cache_hits <- character(0)
  missing_results <- character(0)
  input_bindings <- list()

  for (node_id in graph_plan$topo_order) {
    upstream_edges <- bg_dagri_incoming_edges(graph, node_id)
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
      upstream_fingerprints = up_fps,
      graph = graph
    )
  }

  bg_derive_run_plan_state(
    project,
    list(
      graph_plan = graph_plan,
      targets = targets %||% graph_plan$targets,
      eligible = graph_plan$eligible,
      blocked = graph_plan$blocked,
      external_blocked = graph_plan$external_blocked,
      held_by_policy = graph_plan$external_blocked,
      cache_hits = cache_hits,
      missing_results = missing_results,
      to_execute = character(),
      input_bindings = input_bindings,
      mode = mode,
      metadata = list(
        fingerprints = fingerprints,
        artifact_refs = artifact_refs,
        artifact_index = artifact_index
      )
    ),
    graph,
    external_holds = graph_plan$external_blocked
  )
}

#' @keywords internal
bg_derive_run_plan_state <- function(
  project,
  plan,
  graph,
  external_holds = plan$external_blocked %||% list()
) {
  artifact_index <- plan$metadata$artifact_index %||% list()
  fingerprints <- plan$metadata$fingerprints %||% list()
  eligible <- plan$graph_plan$eligible %||% character()
  artifact_refs <- list()
  cache_hits <- character()
  missing_results <- character()
  input_bindings <- list()

  for (node_id in plan$graph_plan$topo_order %||% character()) {
    upstream_edges <- bg_dagri_incoming_edges(graph, node_id)
    node_bindings <- list()
    can_plan <- TRUE

    for (e in upstream_edges) {
      upstream_ref <- artifact_refs[[e$from]] %||% NULL
      if (is.null(upstream_ref)) {
        can_plan <- FALSE
        break
      }

      node_bindings[[length(node_bindings) + 1]] <- list(
        edge_id = e$id,
        from_node_id = e$from,
        edge_type = e$type,
        artifact_ref = upstream_ref
      )
    }

    if (!can_plan) {
      next
    }

    fingerprint <- fingerprints[[node_id]] %||% NULL
    if (is.null(fingerprint)) {
      next
    }

    input_bindings[[node_id]] <- node_bindings

    cached_ref <- bg_check_artifact(
      project,
      fingerprint,
      node_id = node_id,
      artifact_index = artifact_index
    )

    if (!is.null(cached_ref)) {
      cache_hits <- c(cache_hits, node_id)
      artifact_refs[[node_id]] <- cached_ref
    } else if (node_id %in% eligible) {
      missing_results <- c(missing_results, node_id)
    }
  }

  external_blocked <- bg_run_plan_external_holds(plan, external_holds)
  held_nodes <- names(external_blocked %||% list())

  utils::modifyList(
    plan,
    list(
      graph_plan = utils::modifyList(
        plan$graph_plan,
        list(external_blocked = external_blocked)
      ),
      targets = plan$targets %||% plan$graph_plan$targets,
      eligible = eligible,
      blocked = plan$graph_plan$blocked,
      external_blocked = external_blocked,
      held_by_policy = external_blocked,
      cache_hits = cache_hits,
      missing_results = missing_results,
      to_execute = setdiff(intersect(missing_results, eligible), held_nodes),
      input_bindings = input_bindings,
      metadata = list(
        artifact_refs = artifact_refs,
        artifact_index = artifact_index
      )
    )
  )
}

#' @keywords internal
bg_run_plan_external_holds <- function(plan, external_holds = list()) {
  external_holds <- external_holds %||% list()
  topo_order <- plan$graph_plan$topo_order %||% character()
  hold_ids <- intersect(topo_order, names(external_holds))

  if (length(hold_ids) == 0) {
    return(list())
  }

  stats::setNames(
    lapply(hold_ids, function(node_id) external_holds[[node_id]]),
    hold_ids
  )
}

#' @keywords internal
bg_run_plan_record_artifact <- function(plan, node_id, artifact_ref) {
  fingerprint <- plan$metadata$fingerprints[[node_id]] %||% NULL
  if (is.null(fingerprint)) {
    cli::cli_abort(
      "Cannot refresh the run plan for node {.val {node_id}} without a fingerprint."
    )
  }

  artifact_index <- plan$metadata$artifact_index %||% list()
  superseded <- bg_supersede_artifacts_for_node(
    artifact_index,
    node_id,
    except_fingerprint = fingerprint
  )
  artifact_index <- superseded$index

  entry <- bg_normalize_artifact_entry(
    fingerprint,
    artifact_index[[fingerprint]] %||% list()
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
  artifact_index[[fingerprint]] <- entry

  plan$metadata$artifact_index <- artifact_index
  plan
}

#' @keywords internal
bg_refresh_run_plan <- function(
  project,
  plan,
  graph,
  external_holds = plan$external_blocked %||% list()
) {
  bg_derive_run_plan_state(
    project,
    plan,
    graph,
    external_holds = external_holds
  )
}

#' Run a project workflow
#'
#' @param project A `bg_handle`.
#' @param targets Optional character vector of target node IDs.
#' @param mode Execution mode: 'sync' or 'async'.
#' @param backend For async mode, the backend to use (`"auto"` or `"mirai"`).
#'
#' @return A `bg_run_handle` list.
#' @export
bg_run <- function(
  project,
  targets = NULL,
  mode = c("sync", "async"),
  backend = c("auto", "mirai")
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
  graph <- bg_dagri_recompute_state(
    bg_active_graph(project, graph = bg_read_graph(project))
  )
  workflow_plan <- bg_plan(project, mode = "sync")
  external_holds <- bg_workflow_external_holds(project, plan = workflow_plan)
  plan <- if (is.null(targets) || length(targets) == 0) {
    bg_refresh_run_plan(
      project,
      workflow_plan,
      graph,
      external_holds = external_holds
    )
  } else {
    bg_plan(
      project,
      targets,
      external_holds = external_holds,
      mode = "sync"
    )
  }

  run_id <- bg_new_id("run")
  job_ids <- character(0)
  num_executed <- 0L
  run_started_at <- bg_now_timestamp()

  bg_cli_inform(
    "Starting run {.val {run_id}} with {length(plan$to_execute)} node{?s} to execute."
  )

  # Synchronous execution
  for (node_id in plan$graph_plan$topo_order) {
    if (!node_id %in% plan$to_execute) {
      next
    }

    bg_cli_inform("Running node {.val {node_id}}...")
    num_executed <- num_executed + 1L
    job <- bg_create_job(project, run_id, node_id, backend = "sync")
    job_ids <- c(job_ids, job$job_id)
    bg_update_job(
      project,
      job$job_id,
      status = "running",
      started_at = bg_now_timestamp()
    )

    node <- graph$nodes[[node_id]]

    execution_result <- bg_execute_node(
      handle = project,
      node_id = node_id,
      fingerprint = plan$metadata$fingerprints[[node_id]],
      input_bindings = plan$input_bindings[[node_id]],
      graph = graph,
      job_id = job$job_id
    )

    if (!isTRUE(execution_result$ok)) {
      return(bg_build_run_handle(
        run_id = run_id,
        status = "failed",
        mode = mode,
        targets = plan$targets,
        job_ids = job_ids,
        submitted_at = run_started_at,
        started_at = run_started_at,
        finished_at = bg_now_timestamp(),
        summary = list(total_executed = num_executed - 1L),
        error = list(message = execution_result$error$message),
        metadata = list(
          held_by_policy = plan$held_by_policy %||% list()
        )
      ))
    }

    plan <- bg_run_plan_record_artifact(plan, node_id, execution_result$ref)
    external_holds <- bg_workflow_external_holds(project, plan = plan)
    plan <- bg_refresh_run_plan(
      project,
      plan,
      graph,
      external_holds = external_holds
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

  bg_build_run_handle(
    run_id = run_id,
    status = final_status,
    mode = mode,
    targets = plan$targets,
    job_ids = job_ids,
    submitted_at = run_started_at,
    started_at = run_started_at,
    finished_at = bg_now_timestamp(),
    summary = list(total_executed = num_executed),
    error = NULL,
    metadata = list(
      held_by_policy = plan$held_by_policy %||% list()
    )
  )
}

#' @keywords internal
bg_build_run_handle <- function(
  run_id,
  status,
  mode,
  targets,
  job_ids,
  submitted_at = NULL,
  started_at = NULL,
  finished_at = NULL,
  summary = list(),
  error = NULL,
  metadata = list()
) {
  res <- list(
    run_id = run_id,
    status = status,
    mode = mode,
    targets = targets %||% character(0),
    job_ids = job_ids %||% character(0),
    submitted_at = submitted_at,
    started_at = started_at,
    finished_at = finished_at,
    summary = summary,
    error = error,
    metadata = metadata
  )
  class(res) <- "bg_run_handle"
  res
}
