#' @importFrom rlang %||%
NULL

#' @keywords internal
bg_validate_graph_payload <- function(raw, graph_path) {
  if (!is.list(raw)) {
    cli::cli_abort(
      "Malformed graph at {.path {graph_path}}: expected a JSON object."
    )
  }

  if (is.null(raw$version) || length(raw$version) != 1L) {
    cli::cli_abort(
      "Malformed graph at {.path {graph_path}}: missing scalar `version`."
    )
  }

  if (is.null(raw$nodes) || !is.list(raw$nodes)) {
    cli::cli_abort(
      "Malformed graph at {.path {graph_path}}: `nodes` must be a list."
    )
  }

  if (is.null(raw$edges) || !is.list(raw$edges)) {
    cli::cli_abort(
      "Malformed graph at {.path {graph_path}}: `edges` must be a list."
    )
  }

  invalid_node_idx <- which(!vapply(raw$nodes, is.list, logical(1)))
  if (length(invalid_node_idx) > 0) {
    cli::cli_abort(
      "Malformed graph at {.path {graph_path}}: every node entry must be a list."
    )
  }

  invalid_edge_idx <- which(
    !vapply(
      raw$edges,
      function(edge) {
        is.list(edge) && !is.null(edge$from) && !is.null(edge$to)
      },
      logical(1)
    )
  )
  if (length(invalid_edge_idx) > 0) {
    cli::cli_abort(
      paste0(
        "Malformed graph at {.path {graph_path}}: every edge must be a list ",
        "with `from` and `to` fields."
      )
    )
  }

  invisible(TRUE)
}

#' Write project graph safely
#' @param project A `bg_handle`
#' @param graph A `dagri_graph`
#' @keywords internal
#' @export
bg_commit_graph <- function(project, graph) {
  if (project@closed) {
    cli::cli_abort("Cannot commit to a closed project.")
  }
  if (project@readonly) {
    cli::cli_abort("Cannot commit to a readonly project.")
  }

  # Basic version check
  if (graph$version <= project@loaded_graph_version) {
    cli::cli_abort(c(
      "Version conflict: attempted to commit graph version {graph$version},",
      "but handle is already at version {project@loaded_graph_version}."
    ))
  }

  bg_dir <- file.path(project@path, ".bayesgrove")
  graph_path <- file.path(bg_dir, "graph", "graph.json")
  lock_path <- paste0(graph_path, ".lock")

  bg_with_file_lock(lock_path, {
    persisted_graph <- if (file.exists(graph_path)) {
      jsonlite::read_json(graph_path, simplifyVector = FALSE)
    } else {
      NULL
    }
    persisted_version <- as.integer(persisted_graph$version %||% 0L)
    loaded_version <- as.integer(project@loaded_graph_version %||% 0L)

    if (!identical(persisted_version, loaded_version)) {
      cli::cli_abort(c(
        "Version conflict: attempted to commit graph version {graph$version},",
        "but persisted graph version is {persisted_version} and this handle last loaded version {loaded_version}."
      ))
    }

    bg_write_json_atomic(graph_path, unclass(graph), sort_keys = FALSE)

    # Update handle only after the persisted write succeeds.
    project@loaded_graph_version <- graph$version
  })

  invisible(TRUE)
}

#' Read project graph
#' @param project A `bg_handle`
#' @return A validated graph with class `dagriculture_graph`.
#'   Uses payload-level validation and manual class coercion until
#'   dagriculture exposes a native JSON deserializer.
#' @keywords internal
#' @export
bg_read_graph <- function(project) {
  bg_dir <- file.path(project@path, ".bayesgrove")
  graph_path <- file.path(bg_dir, "graph", "graph.json")

  if (!file.exists(graph_path)) {
    cli::cli_abort(
      "Graph file not found at {.path {graph_path}}."
    )
  }

  raw <- jsonlite::read_json(graph_path)
  project@loaded_graph_version <- as.integer(raw$version %||% 0L)
  bg_validate_graph_payload(raw, graph_path)
  class(raw) <- unique(c("dagriculture_graph", class(raw)))
  raw
}
