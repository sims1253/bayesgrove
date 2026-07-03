#' @keywords internal
bg_project_config_path <- function(path) {
  file.path(path, ".bayesgrove", "config.json")
}

#' @keywords internal
bg_empty_runtime_manifest <- function() {
  list(
    node_kinds = list()
  )
}

#' @keywords internal
bg_normalize_runtime_manifest <- function(manifest) {
  manifest <- manifest %||% list()

  list(
    node_kinds = manifest$node_kinds %||% list()
  )
}

#' @keywords internal
bg_normalize_project_config <- function(config) {
  config <- config %||% list()
  config$workflow_packs <- bg_normalize_workflow_pack_refs(
    config$workflow_packs %||% list()
  )
  config$runtime_manifest <- bg_normalize_runtime_manifest(
    config$runtime_manifest
  )
  config
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
bg_restore_runtime_manifest <- function(project) {
  config <- bg_read_project_config(project)
  entries <- config$runtime_manifest$node_kinds %||% list()

  if (length(entries) == 0) {
    return(invisible(project))
  }

  if (is.null(project@registries$node_kinds)) {
    project@registries$node_kinds <- list()
  }

  for (entry in entries) {
    kind <- entry$kind %||% NULL
    if (!is.character(kind) || length(kind) != 1 || !nzchar(kind)) {
      next
    }

    executor <- NULL
    executor_source <- entry$executor_source %||% NULL
    if (is.character(executor_source) && length(executor_source) == 1) {
      executor <- eval(
        parse(text = executor_source),
        envir = asNamespace("bayesgrove")
      )
    }

    project@registries$node_kinds[[kind]] <- list(
      name = kind,
      executor = executor
    )
  }

  invisible(project)
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
#'   default. Built-in packs such as `bayesguide.default_bayesian` provide computation review,
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
      version = "0.1.0",
      workflow_packs = configured_workflow_packs,
      runtime_manifest = bg_empty_runtime_manifest()
    ),
    config
  ))

  bg_write_project_config_path(path, full_config)

  init_handle <- bg_handle(
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
    project = init_handle,
    registry = bg_empty_branch_registry(init_handle)
  )
  bg_write_goal_registry(
    project = init_handle,
    registry = bg_empty_goal_registry(init_handle)
  )

  # Return handle
  bg_handle(
    project_id = project_id,
    path = path,
    readonly = FALSE,
    closed = FALSE,
    loaded_graph_version = graph$version,
    lock_token = NA_character_,
    registries = list(),
    metadata = list()
  )
}

#' Open a bayesgrove Project
#'
#' @param path Path to the project directory.
#' @param readonly Whether to open the project in read-only mode.
#'
#' @return A `bg_handle`.
#' @export
bg_open <- function(path = ".", readonly = FALSE) {
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

  # In a real implementation we would acquire a lock here if not readonly
  lock_token <- if (!readonly) "lock_mock" else NA_character_

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

  bg_restore_runtime_manifest(handle)
  handle
}

#' Close a bayesgrove Project
#'
#' @param project A `bg_handle`.
#'
#' @export
bg_close <- function(project) {
  S7::check_is_S7(project, bg_handle)

  if (project@closed) {
    return(invisible(project))
  }

  # Clean up mirai daemons for this project
  if (requireNamespace("mirai", quietly = TRUE)) {
    tryCatch(
      mirai::daemons(0, .compute = project@project_id),
      error = function(e) NULL
    )
  }

  # Here we would release the lock if we hold it
  project@lock_token <- NA_character_
  project@closed <- TRUE

  invisible(project)
}
