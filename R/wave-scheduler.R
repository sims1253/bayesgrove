# Wave scheduler --------------------------------------------------------------
#
# Drives bg_run() in topological waves: each wave is the set of nodes currently
# ready to execute (in plan$to_execute AND with all upstream artifacts already
# available, i.e. present in plan$input_bindings). Nodes within a wave are
# mutually independent by construction, so they can be dispatched in parallel
# (Milestone 3 Step 3); the sequential fallback executes them in topo order.
#
# Protocol holds and the pause flag are evaluated at WAVE BOUNDARIES, not
# between individual nodes within a wave. This is the documented semantic: a
# hold discovered from a fresh summary in wave N takes effect before wave N+1
# is dispatched, never mid-wave.

#' Compute the current execution wave.
#'
#' The wave is the set of nodes that are (a) scheduled for execution
#' (`plan$to_execute`) and (b) have all upstream artifact inputs resolved
#' (`plan$input_bindings[[node_id]]` is non-empty, OR the node has no upstream
#' edges — base nodes). These nodes are mutually independent by construction
#' (no edges run between them within the wave), so they may run concurrently.
#'
#' `plan$input_bindings[[node_id]]` is populated by `bg_derive_run_plan_state`
#' only when every upstream `artifact_ref` is available, so presence in
#' `names(plan$input_bindings)` is the ready signal.
#'
#' @param plan A run plan (from `bg_plan` / `bg_refresh_run_plan`).
#' @return Character vector of node IDs ready to execute now, in topological
#'   order. Empty when nothing is ready (run done, or remaining nodes are all
#'   held/blocked).
#' @keywords internal
#' @noRd
bg_compute_wave <- function(plan) {
  to_execute <- plan$to_execute %||% character()
  if (length(to_execute) == 0) {
    return(character())
  }
  ready <- names(plan$input_bindings %||% list())
  # Base nodes (no upstream edges) have an empty-but-present input_bindings
  # entry; nodes awaiting unresolved inputs are absent entirely. Intersect with
  # to_execute, then preserve topological order for deterministic execution.
  wave <- intersect(to_execute, ready)
  topo <- plan$graph_plan$topo_order %||% character()
  intersect(topo, wave)
}

#' Detect whether parallel dispatch should be used for the given mode.
#'
#' `parallel = "never"` always returns FALSE. `"auto"` returns TRUE only when
#' mirai reports active daemons (so a wave can be dispatched in parallel without
#' the caller having to wire anything up). `"always"` requires mirai daemons and
#' errors if they are unavailable. Soft dependency: returns FALSE (never/auto) or
#' errors cleanly (always) when mirai is not installed.
#'
#' @param parallel One of `"auto"`, `"never"`, `"always"`.
#' @return TRUE if the current wave should be dispatched in parallel.
#' @keywords internal
#' @noRd
bg_parallel_active <- function(parallel) {
  if (identical(parallel, "never")) {
    return(FALSE)
  }
  if (!requireNamespace("mirai", quietly = TRUE)) {
    if (identical(parallel, "always")) {
      cli::cli_abort(c(
        "{.arg parallel = \"always\"} requires the {.pkg mirai} package.",
        "i" = "Install it with {.run install.packages(\"mirai\")}, or use {.arg parallel = \"auto\"} to fall back to sequential execution when mirai is absent."
      ))
    }
    return(FALSE)
  }
  daemons_up <- bg_mirai_daemons_active()
  if (identical(parallel, "always") && !daemons_up) {
    cli::cli_abort(c(
      "{.arg parallel = \"always\"} requires active mirai daemons, but none are set.",
      "i" = "Start daemons with {.run mirai::daemons(4)} before calling {.fn bg_run}."
    ))
  }
  daemons_up
}

#' Report whether mirai daemons are currently active.
#'
#' `mirai::status()` returns a list whose `$connections` field counts active
#' daemon connections (> 0 when daemons are set). This is the modern, stable
#' check. Wrapped in tryCatch so a future API change degrades to "no daemons"
#' (sequential fallback) rather than erroring mid-run.
#' @return TRUE if at least one mirai daemon connection is active.
#' @keywords internal
#' @noRd
bg_mirai_daemons_active <- function() {
  status <- tryCatch(mirai::status(), error = function(e) NULL)
  is.list(status) &&
    is.numeric(status$connections) &&
    length(status$connections) > 0 &&
    status$connections[[1]] > 0
}

# Parallel wave dispatch -----------------------------------------------------
#
# Workers run the executor and write the artifact BLOB only (content-addressed
# writes are idempotent and atomic, so concurrent workers never corrupt the
# CAS). The main process is the single writer of the artifact INDEX, the
# summaries JSONL, and the jobs JSONL — the existing single-writer lock model
# is preserved. Each worker returns list(node_id, ref, summaries, error) and
# the main process folds the results back into the plan + run state.

#' Build a worker task bundle for one node in a wave.
#'
#' Pre-resolves everything the worker cannot reach without a live handle: the
#' fetched upstream artifacts (`resolved_inputs`) and the `node$resolved$data`
#' for `cas:` data refs. The node, kind registry entry, fingerprint, and project
#' path travel to the daemon; the worker runs `bg_run_executor` and
#' `bg_store_cas_blob` and returns the result shape.
#'
#' Built-in executors are NOT shipped across (they resolve by `builtin:` ref
#' on the daemon); user executors are prepared by
#' `bg_prepare_executor_for_ship`, which rebuilds them from persisted source
#' when available. Aborts (via that helper) when the kind has no usable
#' executor.
#' @return A list suitable as the worker payload.
#' @keywords internal
#' @noRd
bg_build_worker_task <- function(project, node_id, plan, graph, kind_reg) {
  node <- graph$nodes[[node_id]]

  resolved_inputs <- list()
  for (b in plan$input_bindings[[node_id]] %||% list()) {
    resolved_inputs[[b$from_node_id]] <- bg_fetch_artifact(
      project,
      b$artifact_ref
    )
  }

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
    node$resolved$data <- bg_fetch_artifact(project, data_ref)
  }

  executor <- bg_prepare_executor_for_ship(kind_reg, node$kind)

  list(
    node_id = node_id,
    node = node,
    resolved_inputs = resolved_inputs,
    executor = executor,
    kind_reg = kind_reg,
    fingerprint = plan$metadata$fingerprints[[node_id]],
    project_path = project@path
  )
}

#' Prepare a user executor for shipment to a mirai daemon.
#'
#' mirai serializes the function object to the daemon directly. To keep the
#' closure self-contained (so it does not capture unreachable state from the
#' caller's environment), we prefer to rebuild the function from its persisted
#' source when available — the rebuilt function's parent environment is a child
#' of globalenv, matching the `bg_restore_executors` evaluation context. When no
#' source is available, we ship the registered function as-is and warn that a
#' non-self-contained closure may not survive the daemon trip.
#'
#' `carrier::crate()` is the canonical "self-contained closure" tool, but it
#' requires the function literal to be defined INSIDE the crate() call — it
#' cannot wrap an already-built function object or an `eval(parse())` result.
#' So crating is the user's responsibility at registration time (they may
#' register a function built inside `carrier::crate()`); here we ship the
#' cleanest available form.
#' @param kind_reg The node-kind registry entry.
#' @return A list describing how the worker resolves the executor.
#' @keywords internal
#' @noRd
bg_prepare_executor_for_ship <- function(kind_reg, kind) {
  executor_ref <- kind_reg$executor_ref %||% NULL
  if (!is.null(executor_ref) && startsWith(executor_ref, "builtin:")) {
    return(list(mode = "builtin", ref = executor_ref))
  }

  fn <- kind_reg$executor
  if (!is.function(fn)) {
    cli::cli_abort(
      "Executor for kind {.val {kind}} is not a registered function; call {.fn bg_restore_executors} or {.fn bg_register_node_kind} before running in parallel."
    )
  }

  # Prefer rebuilding from persisted source so the closure's parent is a clean
  # globalenv child (no capture of the caller's runtime state). This matches
  # how bg_restore_executors evaluates source and yields a daemon-safe closure.
  src <- kind_reg$executor_source %||% NULL
  if (
    is.character(src) &&
      length(src) == 1L &&
      nzchar(src)
  ) {
    rebuilt <- tryCatch(
      eval(parse(text = src), envir = new.env(parent = globalenv())),
      error = function(e) NULL
    )
    if (is.function(rebuilt)) {
      return(list(mode = "function", fn = rebuilt, rebuilt_from_source = TRUE))
    }
  }

  # Fall back to shipping the registered function object directly. mirai
  # serializes it; warn that a non-self-contained closure may not survive.
  cli::cli_warn(
    "Shipping the {.val {kind}} executor to daemons without rebuilding from source. Make it self-contained (avoid capturing non-package objects) for robust parallel dispatch."
  )
  list(mode = "function", fn = fn, rebuilt_from_source = FALSE)
}

#' Dispatch a wave's nodes to mirai daemons in parallel and gather results.
#'
#' Builds a worker task bundle per node (pre-resolving inputs and `cas:` data
#' refs on the main process, where the live handle is available), dispatches
#' them with `purrr::map(.x, purrr::in_parallel(bg_wave_worker))` (backed by the
#' active mirai daemons), and returns one result list per node tagged
#' `$worker_origin = TRUE` so the caller knows the blob is written but the
#' index/summaries/job still need finalizing on the main process.
#'
#' @param project A `bg_handle` (main process; holds the writer lock).
#' @param wave Character vector of node IDs in this wave.
#' @param plan The current run plan.
#' @param graph The recomputed active graph.
#' @return A list of result lists (one per wave node), each carrying
#'   `$node_id`, `$ok`, and either `$ref`+`$summaries` or `$error`, plus
#'   `$worker_origin = TRUE`.
#' @keywords internal
#' @noRd
bg_dispatch_wave_parallel <- function(project, wave, plan, graph) {
  tasks <- lapply(wave, function(node_id) {
    kind_reg <- project@registries$node_kinds[[graph$nodes[[node_id]]$kind]]
    bg_build_worker_task(project, node_id, plan, graph, kind_reg)
  })
  names(tasks) <- wave

  # purrr::in_parallel() crates `.f` with carrier, so `.f` MUST be a function
  # literal defined inline (it cannot wrap an existing package function
  # object). The inline literal runs crated on the daemon in a globalenv
  # context, NOT bayesgrove's namespace, so it must reach the worker via
  # getFromNamespace rather than a bare name (which would not resolve).
  # getFromNamespace avoids the R CMD check NOTE that ::: to one's own
  # namespace would trigger, while still resolving the internal worker on the
  # daemon (bayesgrove must be installed there, as it is for any built-in
  # executor). The map blocks until all daemons return.
  raw_results <- purrr::map(
    tasks,
    purrr::in_parallel(function(task) {
      worker <- utils::getFromNamespace("bg_wave_worker", "bayesgrove")
      worker(task)
    })
  )
  lapply(wave, function(node_id) {
    res <- raw_results[[node_id]]
    res$node_id <- node_id
    res$worker_origin <- TRUE
    res
  })
}

#' The wave worker: run one node's executor and write its CAS blob.
#'
#' Runs on a mirai daemon. Loads bayesgrove, resolves the executor (built-in by
#' ref, or uses the crated user function), calls `bg_run_executor`, and writes
#' the artifact blob via `bg_store_cas_blob` (idempotent + atomic, safe under
#' concurrent writes). Does NOT touch the artifact index, summaries JSONL, or
#' jobs JSONL — the main process owns those (single-writer invariant). Returns
#' `list(node_id, ref, summaries, ok, error)`; `ref`/`summaries` are populated
#' on success, `error` on failure.
#' @param task A worker task bundle from `bg_build_worker_task`.
#' @return Result list for the main process to finalize.
#' @keywords internal
#' @noRd
bg_wave_worker <- function(task) {
  # Daemons need bayesgrove installed; require it quietly so a missing install
  # surfaces as a clean per-node failure. (The body below uses unqualified
  # calls — they resolve in bayesgrove's own namespace because this function's
  # defining environment is the package, even when the daemon looked it up via
  # getFromNamespace.)
  if (!requireNamespace("bayesgrove", quietly = TRUE)) {
    return(list(
      node_id = task$node_id,
      ok = FALSE,
      error = list(message = "bayesgrove is not installed on the daemon")
    ))
  }

  # Open a read-only handle on the worker so bg_store_cas_blob can write the
  # blob (it only needs the path). Read-only skips the writer lock, which the
  # main process already holds; the worker only writes the content-addressed
  # blob (idempotent + atomic), never the locked index/summaries/jobs files.
  project <- bg_open(task$project_path, readonly = TRUE)

  kind_reg <- task$kind_reg
  resolved_executor <- if (identical(task$executor$mode, "builtin")) {
    bg_resolve_builtin_executor(task$executor$ref)
  } else {
    task$executor$fn
  }
  kind_reg$executor <- resolved_executor
  if (is.null(kind_reg$executor) || !is.function(kind_reg$executor)) {
    return(list(
      node_id = task$node_id,
      ok = FALSE,
      error = list(
        message = sprintf(
          "Could not resolve executor on the daemon for node %s",
          task$node_id
        )
      )
    ))
  }

  tryCatch(
    {
      normalized <- bg_run_executor(
        task$node,
        task$resolved_inputs,
        kind_reg
      )
      ref <- bg_store_cas_blob(project, normalized$artifact)
      list(
        node_id = task$node_id,
        ok = TRUE,
        ref = ref,
        summaries = normalized$summaries,
        fingerprint = task$fingerprint
      )
    },
    error = function(e) {
      list(
        node_id = task$node_id,
        ok = FALSE,
        error = list(message = conditionMessage(e))
      )
    }
  )
}
