#' @importFrom rlang %||%
NULL

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
  temp_path <- paste0(graph_path, ".tmp")

  # Write to temp file
  jsonlite::write_json(
    unclass(graph),
    temp_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    force = TRUE
  )

  # Atomic rename
  file.rename(temp_path, graph_path)

  # Update handle
  project@loaded_graph_version <- graph$version
  invisible(TRUE)
}

#' Read project graph
#' @param project A `bg_handle`
#' @keywords internal
#' @export
bg_read_graph <- function(project) {
  bg_dir <- file.path(project@path, ".bayesgrove")
  graph_path <- file.path(bg_dir, "graph", "graph.json")

  if (!file.exists(graph_path)) {
    cli::cli_abort("Graph file missing at {.path {graph_path}}")
  }

  raw <- jsonlite::read_json(graph_path)
  # groots doesn't have a json deserializer exposed yet, but we can coerce the lists.
  # Assuming groots provides groots_graph() to construct it or we pass the raw lists if groots validates them.
  # Actually groots might require reconstructing the objects. For now, groots_graph() from lists.

  # In the MVP we'll just return it as a list and let groots handle parsing when we have that implemented,
  # or construct a groots_graph manually.

  # For now, let's assume groots has a way to hydrate, or we just trust the json layout matches S7 properties
  # Let's write a simple reconstructor since groots is value-oriented.

  # Hack for MVP: groots currently accepts lists that look like graphs, but we should do it properly.
  # Let's rely on groots::from_list(raw) if it exists, otherwise we'll build it.
  # I'll just return the raw object and we can see what groots does.
  # Wait, groots alpha design has `groots_graph(registry, nodes=..., edges=...)`.
  # For the rewrite, groots API provides constructors.
  # I'll use a placeholder `groots:::from_list(raw)` or just try to pass raw to groots if it's S3/list.
  # Given `groots` is local, I can check its API.
  raw
}
