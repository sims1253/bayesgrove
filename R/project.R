#' @keywords internal
bg_project_config_path <- function(path) {
  file.path(path, ".bayesgrove", "config.json")
}

# --- Project-level single-writer lock ---------------------------------------

#' @keywords internal
bg_lock_dir <- function(path) {
  file.path(path, ".bayesgrove", "project.lock")
}

#' @keywords internal
bg_lock_info_path <- function(path) {
  file.path(bg_lock_dir(path), "holder.json")
}

#' Acquire the single-writer project lock.
#'
#' Uses an atomic `dir.create()` on `.bayesgrove/project.lock` (the same
#' primitive as `bg_with_file_lock`). On success a JSON file with pid,
#' hostname, timestamp, and a random token is written inside the lock dir and
#' the token is returned (stored on the handle). On failure the lock-holder
#' info is reported. `force = TRUE` steals the lock (stale-process takeover).
#' @keywords internal
#' @noRd
bg_acquire_lock <- function(path, force = FALSE) {
  lock_dir <- bg_lock_dir(path)
  dir.create(dirname(lock_dir), recursive = TRUE, showWarnings = FALSE)

  acquired <- dir.create(lock_dir, recursive = FALSE, showWarnings = FALSE)

  if (!acquired && isTRUE(force)) {
    unlink(lock_dir, recursive = TRUE, force = TRUE)
    acquired <- dir.create(lock_dir, recursive = FALSE, showWarnings = FALSE)
  }

  if (!acquired) {
    holder <- bg_read_lock_holder(path)
    if (isTRUE(force)) {
      # Another writer raced in and re-created the lock dir between our
      # unlink() and dir.create(); report who holds it now instead of a
      # dead-end error, since the project is validly locked, just not by us.
      cli::cli_abort(c(
        "Project at {.path {path}} was locked by another writer during the steal.",
        "i" = "Lock holder: pid {.val {holder$pid}}, host {.val {holder$hostname}}, acquired {.val {holder$acquired_at}}.",
        "i" = "Retry {.code bg_open(path, force = TRUE)} if that holder is also stale."
      ))
    }
    cli::cli_abort(c(
      "Project at {.path {path}} is locked by another writer.",
      "i" = "Lock holder: pid {.val {holder$pid}}, host {.val {holder$hostname}}, acquired {.val {holder$acquired_at}}.",
      "i" = "Open with {.code bg_open(path, force = TRUE)} to steal the lock if the holder is stale."
    ))
  }

  token <- bg_new_id("lock")
  info <- list(
    pid = Sys.getpid(),
    hostname = Sys.info()[["nodename"]] %||% "unknown",
    acquired_at = bg_now_timestamp(),
    token = token
  )
  jsonlite::write_json(info, bg_lock_info_path(path), auto_unbox = TRUE)
  token
}

#' @keywords internal
#' @noRd
bg_read_lock_holder <- function(path) {
  tryCatch(
    jsonlite::read_json(bg_lock_info_path(path), simplifyVector = FALSE),
    error = function(e) {
      list(
        pid = NA_integer_,
        hostname = "unknown",
        acquired_at = "unknown"
      )
    }
  )
}

#' Release the project lock if the handle's token matches.
#'
#' Readonly handles (NA token) skip release. A token mismatch warns and leaves
#' the lock in place (another writer has since taken over).
#' @keywords internal
#' @noRd
bg_release_lock <- function(path, token) {
  if (is.na(token)) {
    return(invisible(FALSE))
  }

  lock_dir <- bg_lock_dir(path)
  if (!dir.exists(lock_dir)) {
    return(invisible(FALSE))
  }

  holder <- bg_read_lock_holder(path)
  if (!identical(holder$token %||% NA_character_, token)) {
    # Another writer owns the lock now; do not remove it.
    return(invisible(FALSE))
  }

  unlink(lock_dir, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

#' @keywords internal
bg_empty_runtime_manifest <- function() {
  list(
    node_kinds = list(),
    summary_kinds = list()
  )
}

#' @keywords internal
bg_normalize_runtime_manifest <- function(manifest) {
  manifest <- manifest %||% list()

  list(
    node_kinds = manifest$node_kinds %||% list(),
    summary_kinds = manifest$summary_kinds %||% list()
  )
}

#' @keywords internal
bg_normalize_project_config <- function(config) {
  config <- config %||% list()
  config$workflow_packs <- bg_normalize_workflow_pack_refs(
    config$workflow_packs %||% list()
  )
  config$workflow_packs <- bg_apply_workflow_strictness_default(
    config$workflow_packs,
    config$workflow_strictness
  )
  config$runtime_manifest <- bg_normalize_runtime_manifest(
    config$runtime_manifest
  )
  config
}

#' Apply a project-level `workflow_strictness` default to pack refs.
#'
#' When a project config sets `workflow_strictness` (e.g. `"advisory"`), each
#' pack ref that does not declare its own `config$strictness` inherits it, so
#' packs without an explicit per-ref setting still downgrade blocking
#' obligations. Explicit per-ref strictness always wins.
#' @keywords internal
bg_apply_workflow_strictness_default <- function(
  pack_refs,
  default_strictness
) {
  if (
    is.null(default_strictness) ||
      !default_strictness %in% c("blocking", "advisory") ||
      length(pack_refs) == 0
  ) {
    return(pack_refs)
  }

  lapply(pack_refs, function(ref) {
    if (!is.null(ref$config$strictness)) {
      return(ref)
    }
    ref$config <- ref$config %||% list()
    ref$config$strictness <- default_strictness
    ref
  })
}

#' @keywords internal
bg_read_project_config_path <- function(path) {
  config_path <- bg_project_config_path(path)
  if (!file.exists(config_path)) {
    return(bg_normalize_project_config(list()))
  }

  bg_normalize_project_config(jsonlite::read_json(config_path))
}

#' @keywords internal
bg_write_project_config_path <- function(path, config) {
  bg_write_json_atomic(
    bg_project_config_path(path),
    bg_normalize_project_config(config)
  )
}

#' @keywords internal
#' @noRd
bg_is_valid_kind_name <- function(kind) {
  is.character(kind) && length(kind) == 1 && nzchar(kind)
}

#' Restore persisted node-kind registrations structurally (no code execution).
#'
#' On open, persisted node kinds are restored structurally only: the kind
#' name, contracts, and the stored `executor_source` / `executor_ref` are
#' kept as inert metadata. User-supplied executor source is NEVER evaluated
#' here — opening a project directory must not run project-supplied code.
#' Use [bg_restore_executors()] to opt into restoring executable executors.
#' @keywords internal
#' @noRd
bg_restore_runtime_manifest <- function(project) {
  config <- bg_read_project_config(project)
  entries <- config$runtime_manifest$node_kinds %||% list()
  summary_entries <- config$runtime_manifest$summary_kinds %||% list()

  if (length(entries) == 0 && length(summary_entries) == 0) {
    return(invisible(project))
  }

  if (is.null(project@registries$node_kinds)) {
    project@registries$node_kinds <- list()
  }

  for (entry in entries) {
    kind <- entry$kind %||% NULL
    if (!bg_is_valid_kind_name(kind)) {
      next
    }

    # Built-in executors are safe to restore by reference: they resolve
    # through the package-internal bg_builtin_executors() registry (Phase 4).
    executor_ref <- entry$executor_ref %||% NULL
    if (!is.null(executor_ref) && startsWith(executor_ref, "builtin:")) {
      executor <- bg_resolve_builtin_executor(executor_ref)
      project@registries$node_kinds[[kind]] <- list(
        name = kind,
        executor = executor,
        executor_ref = executor_ref
      )
      next
    }

    # User executors: keep the stored source as inert metadata only. The
    # executor slot stays NULL until bg_restore_executors(trust = TRUE) is
    # called explicitly.
    project@registries$node_kinds[[kind]] <- list(
      name = kind,
      executor = NULL,
      executor_source = entry$executor_source %||% NULL
    )
  }

  # Summary-kind registrations are plain data (no code), so they restore
  # fully on open — no trust gate needed.
  if (length(summary_entries) > 0) {
    if (is.null(project@registries$summary_kinds)) {
      project@registries$summary_kinds <- list()
    }
    for (entry in summary_entries) {
      kind <- entry$kind %||% NULL
      if (!bg_is_valid_kind_name(kind)) {
        next
      }
      project@registries$summary_kinds[[kind]] <- entry
    }
  }

  invisible(project)
}

#' Restore user-supplied executors from persisted source.
#'
#' Opening a project never runs project-supplied code. This function is the
#' explicit, opt-in path to restore executors stored as source text in the
#' project's runtime manifest.
#'
#' With `trust = FALSE` (the default) the stored source for each kind is
#' printed and the function aborts, so the user can review what would be
#' evaluated before proceeding. With `trust = TRUE` each source string is
#' evaluated in a child of [globalenv()] (not in the bayesgrove namespace)
#' and a warning notes that closures over the original environment are not
#' restored.
#'
#' @param project A `bg_handle`.
#' @param trust Logical. If `FALSE` (default), print sources and abort without
#'   evaluating. If `TRUE`, evaluate the sources.
#' @return Invisibly, the `project` handle with executors populated.
#' @export
bg_restore_executors <- function(project, trust = FALSE) {
  S7::check_is_S7(project, bg_handle)

  if (is.null(project@registries$node_kinds)) {
    project@registries$node_kinds <- list()
  }

  pending <- list()
  for (kind in names(project@registries$node_kinds)) {
    entry <- project@registries$node_kinds[[kind]]
    if (is.function(entry$executor)) {
      next
    }
    src <- entry$executor_source %||% NULL
    if (!is.character(src) || length(src) != 1 || !nzchar(src)) {
      next
    }
    pending[[kind]] <- src
  }

  if (length(pending) == 0) {
    return(invisible(project))
  }

  if (!isTRUE(trust)) {
    for (kind in names(pending)) {
      cli::cli_text("{.field {kind}} executor source:")
      cli::cli_code(pending[[kind]])
    }
    cli::cli_abort(c(
      "Refusing to evaluate {.val {length(pending)}} persisted executor{?s} without explicit consent.",
      "i" = "Review the source above, then call {.fn bg_restore_executors} with {.arg trust = TRUE} to restore them.",
      "i" = "Alternatively, re-register executors with {.fn bg_register_node_kind}."
    ))
  }

  cli::cli_warn(c(
    "Restoring persisted executors by evaluating stored source text.",
    "i" = "Executors are evaluated in a child of the global environment; closures over the original environment are not restored."
  ))

  target_env <- new.env(parent = globalenv())
  restored <- list()
  failures <- character(0)
  for (kind in names(pending)) {
    parsed <- tryCatch(
      eval(parse(text = pending[[kind]]), envir = target_env),
      error = function(e) e
    )
    if (inherits(parsed, "error")) {
      failures <- c(failures, sprintf("%s: %s", kind, conditionMessage(parsed)))
      next
    }
    if (!is.function(parsed)) {
      failures <- c(
        failures,
        sprintf("%s: parsed source is not a function", kind)
      )
      next
    }
    project@registries$node_kinds[[kind]]$executor <- parsed
    restored <- c(restored, kind)
  }

  if (length(failures) > 0) {
    cli::cli_warn(c(
      "Failed to restore {.val {length(failures)}} executor{?s}:",
      setNames(failures, rep("x", length(failures)))
    ))
  }

  invisible(project)
}

#' Resolve a built-in executor reference to its function.
#'
#' Built-in executors (established in Phase 4) are persisted as
#' `"builtin:<id>"` references and resolve through the package-internal
#' registry. Until that registry exists this returns NULL.
#' @keywords internal
#' @noRd
bg_resolve_builtin_executor <- function(executor_ref) {
  registry <- tryCatch(
    bg_builtin_executors(),
    error = function(e) list()
  )

  ref_id <- sub("^builtin:", "", executor_ref)
  fn <- registry[[ref_id]] %||% NULL
  if (!is.function(fn)) {
    cli::cli_warn(
      "Built-in executor {.val {executor_ref}} is not available in this version of bayesgrove."
    )
    return(NULL)
  }

  fn
}

#' Initialize a bayesgrove Project
#'
#' Creates the directory structure and initial state for a new bayesgrove project.
#'
#' @param path Directory path where the project should be initialized.
#' @param project_name Optional. Name of the project. Defaults to the basename of the path.
#' @param config Optional. Configuration list.
#' @param workflow_packs Optional list of active workflow packs. These are
#'   normalized and fixed at project initialization. The project starts empty by
#'   default. Built-in packs such as `bayesgrove.default_bayesian` provide computation review,
#'   branch-scoped fit criticism, candidate-comparison guidance, and explicit
#'   branch acceptance or rejection decisions. Optional built-in packs add
#'   prior rationale and prior predictive review
#'   (`bayesgrove.prior_workflow`), posterior predictive and SBC review
#'   (`bayesgrove.model_checks`), model-selection review with stacking weights
#'   (`bayesgrove.model_selection`), minimal causal framing
#'   (`bayesgrove.causal_minimal`), and PAD annotations
#'   (`bayesgrove.pad_scaffold`).
#'
#' @return A `bg_handle` representing the open project.
#' @export
bg_init <- function(
  path = ".",
  project_name = NULL,
  config = list(),
  workflow_packs = NULL
) {
  path <- normalizePath(path, mustWork = FALSE)

  if (is.null(project_name)) {
    project_name <- basename(path)
  }

  bg_dir <- file.path(path, ".bayesgrove")
  if (dir.exists(bg_dir)) {
    cli::cli_abort("Project already exists at {.path {path}}")
  }

  # Create the base path if it doesn't exist
  if (!dir.exists(path)) {
    created <- tryCatch(
      dir.create(path, recursive = TRUE, showWarnings = FALSE),
      error = function(e) FALSE
    )
    if (!created || !dir.exists(path)) {
      cli::cli_abort("Failed to create project directory at {.path {path}}")
    }
  }

  # Create directory structure
  dirs <- c(
    "graph",
    "decisions",
    "data_recipes",
    "runs",
    "cache",
    "checkpoints",
    "env",
    "workflow"
  )

  for (d in dirs) {
    dir.create(file.path(bg_dir, d), recursive = TRUE, showWarnings = FALSE)
  }

  # Initialize an empty dagriculture graph
  registry <- dagriculture::dagri_registry()
  graph <- dagriculture::dagri_graph(registry)

  # Save initial graph
  graph_path <- file.path(bg_dir, "graph", "graph.json")
  jsonlite::write_json(
    unclass(graph),
    graph_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    force = TRUE
  )

  project_id <- bg_new_id("proj")

  # Save config
  configured_workflow_packs <- workflow_packs
  if (is.null(configured_workflow_packs)) {
    configured_workflow_packs <- config$workflow_packs %||% list()
  }
  config$workflow_packs <- NULL
  config$runtime_manifest <- bg_normalize_runtime_manifest(
    config$runtime_manifest
  )
  full_config <- bg_normalize_project_config(utils::modifyList(
    list(
      project_id = project_id,
      project_name = project_name,
      version = as.character(utils::packageVersion("bayesgrove")),
      workflow_packs = configured_workflow_packs,
      runtime_manifest = bg_empty_runtime_manifest()
    ),
    config
  ))

  bg_write_project_config_path(path, full_config)

  handle <- bg_handle(
    project_id = project_id,
    path = path,
    readonly = FALSE,
    closed = FALSE,
    loaded_graph_version = graph$version,
    lock_token = NA_character_,
    registries = list(),
    metadata = list()
  )

  bg_write_branch_registry(
    project = handle,
    registry = bg_empty_branch_registry(handle)
  )
  bg_write_goal_registry(
    project = handle,
    registry = bg_empty_goal_registry(handle)
  )

  # Acquire the single-writer lock for the fresh project.
  token <- bg_acquire_lock(path)
  handle@lock_token <- token
  bg_register_lock_finalizer(handle)

  handle
}

#' Open a bayesgrove Project
#'
#' Opens an existing project. Opening a project never runs project-supplied
#' code: persisted node-kind executors are restored structurally only (the
#' kind name, contracts, and stored executor source kept as inert metadata).
#' To restore executable executors from the persisted source, call
#' [bg_restore_executors()] with `trust = TRUE` after opening, or
#' re-register them with [bg_register_node_kind()].
#'
#' A non-readonly open acquires a single-writer lock
#' (`.bayesgrove/project.lock`) so two writers cannot mutate a project
#' concurrently. If the lock is already held, the holder's pid/host is
#' reported; pass `force = TRUE` to steal a stale lock. Readonly opens skip
#' locking and can run alongside a writer.
#'
#' @param path Path to the project directory.
#' @param readonly Whether to open the project in read-only mode.
#' @param force Logical. If `TRUE` and the project is locked, steal the lock
#'   (use when the prior holder is known to be stale). Ignored when
#'   `readonly = TRUE`.
#'
#' @return A `bg_handle`.
#' @export
bg_open <- function(path = ".", readonly = FALSE, force = FALSE) {
  path <- normalizePath(path, mustWork = TRUE)
  bg_dir <- file.path(path, ".bayesgrove")

  if (!dir.exists(bg_dir)) {
    cli::cli_abort("No bayesgrove project found at {.path {path}}")
  }

  config <- bg_read_project_config_path(path)
  if (is.null(config$project_name)) {
    config$project_name <- basename(path)
  }

  graph_path <- file.path(bg_dir, "graph", "graph.json")
  loaded_version <- 0L
  if (file.exists(graph_path)) {
    raw_graph <- jsonlite::read_json(graph_path)
    loaded_version <- as.integer(raw_graph$version %||% 0L)
  }

  project_id <- config$project_id %||%
    sprintf("proj_%s", digest::digest(path, algo = "xxhash32"))

  lock_token <- if (!readonly) {
    bg_acquire_lock(path, force = force)
  } else {
    NA_character_
  }

  handle <- bg_handle(
    project_id = project_id,
    path = path,
    readonly = readonly,
    closed = FALSE,
    loaded_graph_version = loaded_version,
    lock_token = lock_token,
    registries = list(),
    metadata = list()
  )

  if (!readonly) {
    bg_register_lock_finalizer(handle)
  }

  bg_restore_runtime_manifest(handle)
  bg_warn_on_bundle_manifest_mismatch(handle)
  handle
}

#' Warn when an unpacked bundle was produced in a different environment.
#'
#' A project restored from [bg_bundle()] carries
#' `.bayesgrove/bundle_manifest.json` with the reproducibility manifest of the
#' machine that produced it. On open, compare the recorded R version, platform,
#' and CmdStan version against the current session and warn on mismatch — the
#' cache stays valid (fingerprints decide reruns), but the user should know the
#' environment differs before trusting bit-level reproducibility.
#' @keywords internal
#' @noRd
bg_warn_on_bundle_manifest_mismatch <- function(handle) {
  manifest_path <- file.path(
    handle@path,
    ".bayesgrove",
    "bundle_manifest.json"
  )
  if (!file.exists(manifest_path)) {
    return(invisible(FALSE))
  }

  manifest <- tryCatch(
    jsonlite::read_json(manifest_path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  recorded <- manifest$reproducibility %||% NULL
  if (is.null(recorded)) {
    return(invisible(FALSE))
  }

  current <- bg_reproducibility_manifest()
  mismatches <- character()
  compare <- function(field, label) {
    rec <- recorded[[field]] %||% NULL
    cur <- current[[field]] %||% NULL
    if (
      is.character(rec) &&
        is.character(cur) &&
        !is.na(rec) &&
        !is.na(cur) &&
        !identical(rec, cur)
    ) {
      sprintf("%s: bundle has %s, this machine has %s", label, rec, cur)
    } else {
      character()
    }
  }
  mismatches <- c(
    compare("r_version", "R version"),
    compare("platform", "Platform"),
    compare("cmdstan", "CmdStan version")
  )

  if (length(mismatches) > 0) {
    cli::cli_warn(c(
      "This project was bundled in a different environment.",
      stats::setNames(mismatches, rep("*", length(mismatches))),
      "i" = "Cached artifacts remain usable; fingerprints decide what reruns. See {.path {manifest_path}} for the full manifest."
    ))
  }
  invisible(length(mismatches) > 0)
}

#' Register a finalizer that releases the project lock on GC.
#'
#' Uses the handle's internal state environment so the finalizer fires even
#' if the handle object itself is collected without an explicit `bg_close()`.
#' @keywords internal
#' @noRd
bg_register_lock_finalizer <- function(handle) {
  lock_token <- handle@lock_token
  path <- handle@path
  reg.finalizer(
    handle@.state,
    function(state) {
      tryCatch(
        bg_release_lock(path, lock_token),
        error = function(e) NULL
      )
    },
    onexit = TRUE
  )
}

#' Close a bayesgrove Project
#'
#' Releases the single-writer lock if this handle holds it.
#'
#' @param project A `bg_handle`.
#'
#' @export
bg_close <- function(project) {
  S7::check_is_S7(project, bg_handle)

  if (project@closed) {
    return(invisible(project))
  }

  bg_release_lock(project@path, project@lock_token)
  project@lock_token <- NA_character_
  project@closed <- TRUE

  invisible(project)
}
