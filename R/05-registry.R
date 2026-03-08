#' Register a node kind in the bayesgrove runtime registry
#'
#' Node kinds have a structural component (stored in the graph) and a runtime
#' component (the executor, stored in the handle).
#'
#' @param project A `bg_handle`.
#' @param kind The name of the node kind.
#' @param input_contract Optional input contract for the structural graph.
#' @param output_type Optional output type string.
#' @param param_schema Optional parameter schema.
#' @param executor Optional R function to execute nodes of this kind.
#'
#' @export
bg_register_node_kind <- function(
  project,
  kind,
  input_contract = NULL,
  output_type = NULL,
  param_schema = NULL,
  executor = NULL
) {
  S7::check_is_S7(project, bg_handle)

  # 1. Add structural kind to the graph
  graph <- bg_read_graph(project)

  new_kind <- dagriculture::dagri_kind(
    name = kind,
    input_contract = input_contract,
    output_type = output_type,
    param_schema = param_schema
  )

  graph$registry$kinds[[kind]] <- new_kind
  graph$version <- graph$version + 1L

  bg_commit_graph(project, graph)

  # 2. Add executor to the runtime handle registries
  if (!is.null(executor)) {
    if (!is.function(executor)) {
      cli::cli_abort("Executor must be a function.")
    }

    if (is.null(project@registries$node_kinds)) {
      project@registries$node_kinds <- list()
    }

    project@registries$node_kinds[[kind]] <- list(
      name = kind,
      executor = executor
    )
  }

  invisible(TRUE)
}

#' Register a backend plugin
#'
#' Backends provide domain-specific compilation and fitting implementations
#' (e.g. cmdstanr, brms).
#'
#' @param project A `bg_handle`.
#' @param name The name of the backend.
#' @param backend_impl A list containing the backend interface methods.
#'
#' @export
bg_register_backend <- function(project, name, backend_impl) {
  S7::check_is_S7(project, bg_handle)

  required_methods <- c(
    "backend_compile",
    "backend_fit",
    "backend_source_hash",
    "backend_runtime_signature"
  )

  missing <- setdiff(required_methods, names(backend_impl))
  if (length(missing) > 0) {
    cli::cli_abort(
      "Backend {.val {name}} is missing required methods: {.val {missing}}"
    )
  }

  if (is.null(project@registries$backends)) {
    project@registries$backends <- list()
  }

  project@registries$backends[[name]] <- backend_impl

  invisible(TRUE)
}
