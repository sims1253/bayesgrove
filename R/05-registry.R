#' @keywords internal
bg_serialize_registration_function <- function(fn) {
  paste(deparse(fn, width.cutoff = 500L), collapse = "\n")
}

#' @keywords internal
bg_node_kind_manifest_entry <- function(
  kind,
  input_contract = NULL,
  output_type = NULL,
  param_schema = NULL,
  executor = NULL
) {
  list(
    kind = kind,
    input_contract = input_contract,
    output_type = output_type,
    param_schema = param_schema,
    executor_source = if (is.null(executor)) {
      NULL
    } else {
      bg_serialize_registration_function(executor)
    }
  )
}

#' @keywords internal
bg_persist_node_kind_registration <- function(
  project,
  kind,
  input_contract = NULL,
  output_type = NULL,
  param_schema = NULL,
  executor = NULL
) {
  config <- bg_read_project_config(project)
  manifest <- config$runtime_manifest %||% bg_empty_runtime_manifest()
  manifest$node_kinds[[kind]] <- bg_node_kind_manifest_entry(
    kind = kind,
    input_contract = input_contract,
    output_type = output_type,
    param_schema = param_schema,
    executor = executor
  )
  config$runtime_manifest <- manifest
  bg_write_project_config_path(project@path, config)
  invisible(TRUE)
}

#' @keywords internal
bg_merge_workflow_pack_refs <- function(existing, incoming) {
  merged <- bg_normalize_workflow_pack_refs(existing)

  for (pack_ref in bg_normalize_workflow_pack_refs(incoming)) {
    existing_ids <- vapply(merged, `[[`, character(1), "pack_id")
    match_idx <- match(pack_ref$pack_id, existing_ids)
    if (is.na(match_idx)) {
      merged[[length(merged) + 1L]] <- pack_ref
    } else {
      merged[[match_idx]] <- pack_ref
    }
  }

  merged
}

#' Activate workflow packs for a project
#'
#' @param project A `bg_handle`.
#' @param workflow_packs One or more workflow-pack references.
#'
#' @return The normalized active workflow packs.
#' @export
bg_use_workflow_packs <- function(project, workflow_packs) {
  S7::check_is_S7(project, bg_handle)

  config <- bg_read_project_config(project)
  config$workflow_packs <- bg_merge_workflow_pack_refs(
    config$workflow_packs %||% list(),
    workflow_packs
  )
  bg_write_project_config_path(project@path, config)

  bg_workflow_packs(project)
}

#' Activate the built-in starter workflow
#'
#' @param project A `bg_handle`.
#'
#' @return The updated `bg_handle`.
#' @export
bg_use_default_workflow <- function(project) {
  S7::check_is_S7(project, bg_handle)

  bg_use_workflow_packs(project, "bayesguide.default_bayesian")

  default_node_kinds <- list(
    list(kind = "source", output_type = "data.frame"),
    list(
      kind = "fit",
      input_contract = list(data = "data.frame"),
      output_type = "fit"
    ),
    list(
      kind = "ppc",
      input_contract = list(fit = "fit"),
      output_type = "ppc"
    ),
    list(
      kind = "compare",
      input_contract = list(fits = "list"),
      output_type = "comparison"
    )
  )

  graph <- bg_read_graph(project)
  existing_kinds <- names(graph$registry$kinds %||% list())
  for (spec in default_node_kinds) {
    if (spec$kind %in% existing_kinds) {
      next
    }

    bg_register_node_kind(
      project = project,
      kind = spec$kind,
      input_contract = spec$input_contract %||% NULL,
      output_type = spec$output_type %||% NULL
    )
  }

  invisible(project)
}

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

  bg_persist_node_kind_registration(
    project = project,
    kind = kind,
    input_contract = input_contract,
    output_type = output_type,
    param_schema = param_schema,
    executor = executor
  )

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
