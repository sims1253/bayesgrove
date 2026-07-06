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
  executor = NULL,
  executor_ref = NULL
) {
  entry <- list(
    kind = kind,
    input_contract = input_contract,
    output_type = output_type,
    param_schema = param_schema
  )

  # Built-in executors persist as a reference and never as source text.
  # executor_ref is passed by backends-cmdstanr.R's builtin registration path;
  # bg_register_node_kind() (this file) never sets it.
  if (!is.null(executor_ref) && startsWith(executor_ref, "builtin:")) {
    entry$executor_ref <- executor_ref
    return(entry)
  }

  entry$executor_source <- if (is.null(executor)) {
    NULL
  } else {
    bg_serialize_registration_function(executor)
  }

  entry
}

#' @keywords internal
bg_persist_node_kind_registration <- function(
  project,
  kind,
  input_contract = NULL,
  output_type = NULL,
  param_schema = NULL,
  executor = NULL,
  executor_ref = NULL
) {
  config <- bg_read_project_config(project)
  manifest <- config$runtime_manifest %||% bg_empty_runtime_manifest()
  manifest$node_kinds[[kind]] <- bg_node_kind_manifest_entry(
    kind = kind,
    input_contract = input_contract,
    output_type = output_type,
    param_schema = param_schema,
    executor = executor,
    executor_ref = executor_ref
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

  bg_use_workflow_packs(project, "bayesgrove.default_bayesian")

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

    # Carry the serialized source on the in-memory entry too (not only in the
    # on-disk manifest), so parallel dispatch can rebuild a daemon-safe closure
    # from source in the same session the executor was registered.
    project@registries$node_kinds[[kind]] <- list(
      name = kind,
      executor = executor,
      executor_source = bg_serialize_registration_function(executor)
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
