#' Initialize a BayesGrove Project
#'
#' Creates the directory structure and initial state for a new BayesGrove project.
#'
#' @param path Directory path where the project should be initialized.
#' @param project_name Optional. Name of the project. Defaults to the basename of the path.
#' @param config Optional. Configuration list.
#'
#' @return A `bg_handle` representing the open project.
#' @export
bg_init <- function(path = ".", project_name = NULL, config = list()) {
  path <- normalizePath(path, mustWork = FALSE)

  if (is.null(project_name)) {
    project_name <- basename(path)
  }

  bg_dir <- file.path(path, ".bayesgrove")
  if (dir.exists(bg_dir)) {
    cli::cli_abort("Project already exists at {.path {path}}")
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

  project_id <- sprintf("proj_%s", digest::digest(runif(1), algo = "xxhash32"))

  # Save config
  config_path <- file.path(bg_dir, "config.json")
  full_config <- utils::modifyList(
    list(
      project_id = project_id,
      project_name = project_name,
      version = "0.1.0"
    ),
    config
  )

  jsonlite::write_json(
    full_config,
    config_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

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

#' Open a BayesGrove Project
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
    cli::cli_abort("No BayesGrove project found at {.path {path}}")
  }

  config_path <- file.path(bg_dir, "config.json")
  if (file.exists(config_path)) {
    config <- jsonlite::read_json(config_path)
  } else {
    config <- list(project_name = basename(path))
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

  bg_handle(
    project_id = project_id,
    path = path,
    readonly = readonly,
    closed = FALSE,
    loaded_graph_version = loaded_version,
    lock_token = lock_token,
    registries = list(),
    metadata = list()
  )
}

#' Close a BayesGrove Project
#'
#' @param project A `bg_handle`.
#'
#' @export
bg_close <- function(project) {
  S7::check_is_S7(project, bg_handle)

  if (project@closed) {
    return(invisible(project))
  }

  # Here we would release the lock if we hold it
  project@lock_token <- NA_character_
  project@closed <- TRUE

  invisible(project)
}
