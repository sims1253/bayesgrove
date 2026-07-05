#' Check if an artifact exists in cache
#'
#' @return The artifact reference string if found and active, otherwise `NULL`.
#' @keywords internal
#' @noRd
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
#' Writes the blob to the content-addressed store and binds it in the artifact
#' index (superseding prior bindings for the node). Thin wrapper over
#' [bg_store_cas_blob] (the idempotent, atomic blob write) and
#' [bg_bind_artifact_index] (the locked index update) so callers that want both
#' at once — the sequential execution path — get the historical behavior. The
#' parallel path splits them: a worker writes the blob; the main process binds
#' the index (single-writer invariant).
#' @return The artifact reference string (e.g. `"cas:sha256:..."`).
#' @keywords internal
#' @noRd
bg_store_artifact <- function(project, node_id, fingerprint, result) {
  artifact_ref <- bg_store_cas_blob(project, result)
  bg_bind_artifact_index(project, node_id, fingerprint, artifact_ref)
  artifact_ref
}

#' Bind an artifact ref in the index, superseding prior node bindings.
#'
#' The index-writing half of [bg_store_artifact], split out so the parallel
#' scheduler can have workers write blobs (idempotent, atomic) while the main
#' process — the single writer of the index — performs this binding after a
#' wave resolves. Runs under the artifact-index file lock.
#' @return The artifact reference string (invisibly).
#' @keywords internal
#' @noRd
bg_bind_artifact_index <- function(
  project,
  node_id,
  fingerprint,
  artifact_ref
) {
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

  invisible(artifact_ref)
}

#' Store an R object in the content-addressed blob store, return its ref.
#'
#' Writes `object` to the CAS under `cas:sha256:<hash>` and returns the ref
#' without touching the artifact index (no node binding, no supersession).
#' Used by `bg_store_artifact` (which then records the binding) and by
#' `bg_set_node_data` (which only needs a stable, content-addressed ref to
#' point a node param at).
#' @return The artifact reference string (e.g. `"cas:sha256:..."`).
#' @keywords internal
#' @noRd
bg_store_cas_blob <- function(project, object) {
  cache_root <- file.path(project@path, ".bayesgrove", "cache", "sha256")
  dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)

  # Write the temp RDS inside the destination CAS root so file.rename() never
  # crosses filesystem boundaries (mirrors bg_write_json_atomic).
  tmp <- tempfile(tmpdir = cache_root, fileext = ".rds.tmp")
  saveRDS(object, tmp)
  # Ensure the temp is cleaned up on every exit path (successful rename moves
  # it so this is a no-op there; cache hits and errors need the cleanup).
  on.exit(
    if (file.exists(tmp)) {
      unlink(tmp)
    },
    add = TRUE
  )

  file_hash <- digest::digest(file = tmp, algo = "sha256")
  prefix <- substr(file_hash, 1, 2)

  cas_dir <- file.path(cache_root, prefix)
  dir.create(cas_dir, recursive = TRUE, showWarnings = FALSE)

  artifact_ref <- sprintf("cas:sha256:%s", file_hash)
  dest_path <- file.path(cas_dir, paste0(file_hash, ".rds"))

  if (!file.exists(dest_path)) {
    renamed <- file.rename(tmp, dest_path)
    if (!renamed) {
      # Cross-device or permission failure: fall back to copy + unlink,
      # then verify the destination is readable.
      copied <- file.copy(tmp, dest_path, overwrite = FALSE)
      if (!copied || !file.exists(dest_path)) {
        cli::cli_abort("Failed to store artifact at {.path {dest_path}}.")
      }
      unlink(tmp)
    }
  }

  artifact_ref
}

#' Fetch an artifact from cache
#'
#' @return The deserialized R object stored at the given reference.
#' @keywords internal
#' @noRd
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
#' @param include_inactive Whether to keep retired or disabled nodes in the
#'   planning graph. Defaults to `FALSE`.
#'
#' @return A `bg_run_plan` list.
#' @export
bg_plan <- function(
  project,
  targets = NULL,
  external_holds = list(),
  include_inactive = FALSE
) {
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
  artifact_index <- bg_read_artifact_index(project)

  # Compute the environment manifest once per plan, not per node.
  environment_manifest <- bg_default_environment_manifest()

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
      environment_manifest = environment_manifest,
      graph = graph
    )
  }

  # bg_derive_run_plan_state recomputes cache_hits, missing_results,
  # to_execute, input_bindings, and artifact_refs from the graph plan and
  # fingerprint map, so the skeleton passes only what it actually consumes.
  bg_derive_run_plan_state(
    project,
    list(
      graph_plan = graph_plan,
      targets = targets %||% graph_plan$targets,
      metadata = list(
        fingerprints = fingerprints,
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
#' Executes the workflow, driving every node that is ready and unheld to
#' completion. Execution proceeds in topological **waves**: each wave is the set
#' of nodes whose upstream inputs are already available, so the nodes within a
#' wave are mutually independent. With `parallel = "auto"` (the default) and
#' active [mirai::daemons] set, a wave is dispatched concurrently; otherwise it
#' runs sequentially in topological order. Either way the protocol holds and the
#' pause flag are evaluated at wave boundaries, so a hold discovered from a fresh
#' summary in one wave takes effect before the next wave is dispatched.
#'
#' @param project A `bg_handle`.
#' @param targets Optional character vector of target node IDs.
#' @param parallel One of `"auto"`, `"never"`, `"always"`. `"auto"` (default)
#'   dispatches waves in parallel when mirai daemons are active, otherwise falls
#'   back to sequential execution. `"never"` always runs sequentially.
#'   `"always"` requires mirai and active daemons, erroring if unavailable.
#'
#' @return A `bg_run_handle` list.
#' @export
bg_run <- function(
  project,
  targets = NULL,
  parallel = c("auto", "never", "always")
) {
  S7::check_is_S7(project, bg_handle)
  parallel <- match.arg(parallel)

  if (bg_workflow_paused(project)) {
    cli::cli_abort(
      "Workflow is paused. Call {.fn bg_resume} before dispatching new work."
    )
  }

  # Read the raw graph once and derive the recomputed active graph from it.
  # The raw graph feeds the protocol holds check (descendants must traverse
  # inactive nodes too, matching pre-Phase-6 semantics); the active graph
  # feeds the run-plan derivation. Both are paid for once, not per node.
  raw_graph <- bg_read_graph(project)
  graph <- bg_dagri_recompute_state(bg_active_graph(project, graph = raw_graph))
  workflow_plan <- bg_plan(project)
  external_holds <- bg_workflow_external_holds(
    project,
    plan = workflow_plan,
    graph = raw_graph
  )
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
      external_holds = external_holds
    )
  }

  run_id <- bg_new_id("run")
  job_ids <- character(0)
  num_executed <- 0L
  run_started_at <- bg_now_timestamp()

  # Per-run state object (Phase 6): cache summaries/decisions/jobs in memory so
  # the external-holds check between nodes does not re-read every JSONL file.
  # Trusted only within this bg_run() call; everything else reads disk.
  run_state <- new.env(parent = emptyenv())
  run_state$summaries <- bg_read_summaries(
    project,
    include_stale = TRUE,
    include_inactive = FALSE,
    predicted_fingerprints = plan$metadata$fingerprints,
    artifact_index = plan$metadata$artifact_index
  )
  run_state$decisions <- bg_read_decisions(project)
  run_state$jobs <- bg_jobs(project)

  bg_cli_inform(
    "Starting run {.val {run_id}} with {length(plan$to_execute)} node{?s} to execute."
  )

  use_parallel <- bg_parallel_active(parallel)

  # Wave-based execution: each wave is the set of to_execute nodes whose inputs
  # are already available (mutually independent). The wave is dispatched
  # sequentially here unless mirai daemons are active and parallel != "never"
  # (Step 3 wires the parallel branch). Holds and pause are checked at wave
  # boundaries, so a mid-run hold from a fresh summary applies before the next
  # wave — never mid-wave.
  while (length(wave <- bg_compute_wave(plan)) > 0) {
    if (bg_workflow_paused(project)) {
      bg_cli_inform(
        "Workflow paused mid-run after {num_executed} node{?s}; call {.fn bg_resume} then {.fn bg_run} to continue."
      )
      break
    }

    # Decide dispatch mode for this wave: parallel only when enabled AND the
    # wave has >1 node (a single-node wave gains nothing from dispatch and the
    # sequential path avoids task-construction overhead).
    wave_parallel <- use_parallel && length(wave) > 1L

    # Record jobs (queued -> running) up front so the records exist regardless
    # of dispatch mode; the executor/blob work happens next.
    wave_jobs <- list()
    for (node_id in wave) {
      bg_cli_inform("Running node {.val {node_id}}...")
      num_executed <- num_executed + 1L
      job <- bg_create_job(project, run_id, node_id)
      job_ids <- c(job_ids, job$job_id)
      bg_update_job(
        project,
        job$job_id,
        status = "running",
        started_at = bg_now_timestamp()
      )
      wave_jobs[[node_id]] <- job$job_id
    }

    # Dispatch and gather one (node_id -> execution_result) per wave node.
    results <- if (wave_parallel) {
      bg_dispatch_wave_parallel(project, wave, plan, graph)
    } else {
      lapply(wave, function(node_id) {
        bg_execute_node(
          handle = project,
          node_id = node_id,
          fingerprint = plan$metadata$fingerprints[[node_id]],
          input_bindings = plan$input_bindings[[node_id]],
          graph = graph,
          job_id = wave_jobs[[node_id]]
        )
      })
    }
    names(results) <- wave

    # Main-process finalization for any parallel result that returned only a
    # blob ref (worker wrote the blob; main binds the index, copies fit
    # outputs, writes summaries, and marks the job). Sequential results are
    # already finalized by bg_execute_node; this is a no-op for them.
    for (node_id in wave) {
      res <- results[[node_id]]
      if (isTRUE(res$worker_origin) && isTRUE(res$ok)) {
        ref <- res$ref
        bg_bind_artifact_index(
          project,
          node_id,
          plan$metadata$fingerprints[[node_id]],
          ref
        )
        # Fit-output copying needs the artifact object; refetch from the CAS
        # (the worker wrote the blob). Cheap relative to the fit itself.
        # NOTE: best-effort under parallel dispatch — a fit produced on a
        # remote daemon embedded CSV paths in the daemon's tempdir, which are
        # unreachable from this process, so bg_copy_fit_outputs silently skips
        # them. The RDS artifact itself is preserved; only the durable CSV
        # copies are dropped under parallel dispatch.
        bg_copy_fit_outputs(
          project,
          bg_fetch_artifact(project, ref),
          wave_jobs[[node_id]]
        )
        written <- bg_write_summaries(
          project = project,
          node_id = node_id,
          artifact_ref = ref,
          execution_fingerprint = plan$metadata$fingerprints[[node_id]],
          summaries = res$summaries
        )
        res$summaries <- written
        results[[node_id]] <- res
        bg_update_job(
          project,
          wave_jobs[[node_id]],
          status = "succeeded",
          result_ref = ref,
          finished_at = bg_now_timestamp()
        )
      }
    }

    # Fold summaries into run state + record artifacts into the plan. Abort the
    # run on the first failed node (mirrors the pre-wave-loop behavior).
    for (node_id in wave) {
      res <- results[[node_id]]
      if (!isTRUE(res$ok)) {
        # Ensure the job reflects the failure (parallel workers do not update
        # jobs; sequential bg_execute_node already did).
        if (isTRUE(res$worker_origin)) {
          bg_update_job(
            project,
            wave_jobs[[node_id]],
            status = "failed",
            error = list(message = res$error$message),
            finished_at = bg_now_timestamp()
          )
        }
        return(bg_build_run_handle(
          run_id = run_id,
          status = "failed",
          targets = plan$targets,
          job_ids = job_ids,
          submitted_at = run_started_at,
          started_at = run_started_at,
          finished_at = bg_now_timestamp(),
          summary = list(total_executed = num_executed - 1L),
          error = list(message = res$error$message),
          metadata = list(
            held_by_policy = plan$held_by_policy %||% list()
          )
        ))
      }

      plan <- bg_run_plan_record_artifact(plan, node_id, res$ref)

      if (!is.null(res$summaries)) {
        for (s in res$summaries) {
          s$is_fresh <- TRUE
          s$is_stale <- FALSE
          run_state$summaries[[s$summary_id]] <- s
        }
      }
    }

    # Wave boundary: refresh jobs, re-evaluate protocol holds, and refresh the
    # plan so the next wave's input_bindings populate from the artifacts just
    # recorded. Holds discovered here block nodes in the next wave.
    run_state$jobs <- bg_jobs(project)
    external_holds <- bg_workflow_external_holds(
      project,
      plan = plan,
      state = run_state,
      graph = raw_graph
    )
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

#' Run a node's executor and normalize the result (no persistence).
#'
#' Pure executor invocation shared by the sequential [bg_execute_node] path and
#' the parallel wave worker: given the node (with `$resolved$data` already
#' attached for `cas:` data refs), the resolved input artifacts, and the kind
#' registry entry, call the executor and normalize what it returns into
#' `list(artifact=, summaries=)`. Throws on executor error; callers handle
#' persistence and job-status updates.
#' @param node The graph node list (with `$resolved$data` set if applicable).
#' @param resolved_inputs Named list of fetched upstream artifacts.
#' @param kind_reg The node-kind registry entry carrying `$executor`.
#' @return `list(artifact = <object>, summaries = <list>)`.
#' @keywords internal
#' @noRd
bg_run_executor <- function(node, resolved_inputs, kind_reg) {
  execution <- kind_reg$executor(node, resolved_inputs)
  bg_normalize_execution_result(execution)
}

#' Execute a single node: resolve inputs, run executor, store artifact, update job
#'
#' @param handle A `bg_handle`.
#' @param node_id Node to execute.
#' @param fingerprint Execution fingerprint.
#' @param input_bindings Input binding list from the run plan.
#' @param graph The graph (used to look up the node).
#' @param job_id Job ID to update.
#'
#' @return A list with `ok` (logical) and either `ref` (artifact ref) or `error`.
#' @keywords internal
#' @noRd
bg_execute_node <- function(
  handle,
  node_id,
  fingerprint,
  input_bindings,
  graph,
  job_id
) {
  node <- graph$nodes[[node_id]]
  kind_reg <- handle@registries$node_kinds[[node$kind]]

  if (is.null(kind_reg) || is.null(kind_reg$executor)) {
    message <- sprintf(
      paste0(
        "No executor registered for node kind %s. ",
        "Restore persisted executors with bg_restore_executors(trust = TRUE) ",
        "or re-register the kind with bg_register_node_kind()."
      ),
      node$kind
    )
    bg_update_job(
      handle,
      job_id,
      status = "failed",
      error = list(message = message),
      finished_at = bg_now_timestamp()
    )
    return(list(
      ok = FALSE,
      error = list(message = message)
    ))
  }

  resolved_inputs <- list()
  for (b in input_bindings) {
    resolved_inputs[[b$from_node_id]] <- bg_fetch_artifact(
      handle,
      b$artifact_ref
    )
  }

  # Generic data_ref resolution: any node whose params carry a single "cas:"
  # data_ref gets the referenced object fetched from the CAS store and attached
  # as node$resolved$data before the executor runs. This keeps executors (whose
  # (node, inputs) signature cannot receive the handle) free of hidden global
  # state, e.g. bg_executor_stan_data no longer needs an active-handle global.
  data_ref <- node$params$data_ref %||% NULL
  if (
    is.character(data_ref) &&
      length(data_ref) == 1 &&
      !is.na(data_ref) &&
      startsWith(data_ref, "cas:")
  ) {
    if (is.null(node$resolved)) {
      node$resolved <- list()
    }
    node$resolved$data <- bg_fetch_artifact(handle, data_ref)
  }

  tryCatch(
    {
      normalized <- bg_run_executor(node, resolved_inputs, kind_reg)

      ref <- bg_store_artifact(
        handle,
        node_id,
        fingerprint,
        normalized$artifact
      )

      # Copy durable CSV output files for fit executors into runs/<job_id>/.
      bg_copy_fit_outputs(handle, normalized$artifact, job_id)

      written_summaries <- bg_write_summaries(
        project = handle,
        node_id = node_id,
        artifact_ref = ref,
        execution_fingerprint = fingerprint,
        summaries = normalized$summaries
      )

      bg_update_job(
        handle,
        job_id,
        status = "succeeded",
        result_ref = ref,
        finished_at = bg_now_timestamp()
      )

      list(ok = TRUE, ref = ref, summaries = written_summaries)
    },
    error = function(e) {
      bg_update_job(
        handle,
        job_id,
        status = "failed",
        error = list(message = e$message),
        finished_at = bg_now_timestamp()
      )
      list(ok = FALSE, error = e)
    }
  )
}

#' Copy durable CSV output files for fit artifacts into runs/<job_id>/.
#'
#' CmdStanMCMC objects carry the paths to their output CSVs; copying them into
#' the project's run directory preserves the raw sampler output alongside the
#' cached RDS artifact. Silently skips non-fit artifacts or missing CSVs.
#' @keywords internal
#' @noRd
bg_copy_fit_outputs <- function(handle, artifact, job_id) {
  if (!inherits(artifact, "CmdStanMCMC")) {
    return(invisible(FALSE))
  }

  csv_files <- tryCatch(artifact$output_files(), error = function(e) {
    character()
  })
  if (length(csv_files) == 0) {
    return(invisible(FALSE))
  }

  run_dir <- file.path(handle@path, ".bayesgrove", "runs", job_id)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  for (csv in csv_files) {
    if (file.exists(csv)) {
      file.copy(csv, run_dir, overwrite = TRUE)
    }
  }

  invisible(TRUE)
}

#' @keywords internal
bg_build_run_handle <- function(
  run_id,
  status,
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
