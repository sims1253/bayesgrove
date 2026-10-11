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
  executor_ref = NULL,
  pad = NULL
) {
  entry <- list(
    kind = kind,
    input_contract = input_contract,
    output_type = output_type,
    param_schema = param_schema
  )
  # P/A/D declarations are plain data and restore on open without trust.
  if (!is.null(pad)) {
    entry$pad <- bg_pad_declaration_manifest(pad)
  }

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
  executor_ref = NULL,
  pad = NULL
) {
  config <- bg_read_project_config(project)
  manifest <- config$runtime_manifest %||% bg_empty_runtime_manifest()
  manifest$node_kinds[[kind]] <- bg_node_kind_manifest_entry(
    kind = kind,
    input_contract = input_contract,
    output_type = output_type,
    param_schema = param_schema,
    executor = executor,
    executor_ref = executor_ref,
    pad = pad
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
  bg_assert_writable(project, "activate workflow packs on")

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
#' @param executor Optional R function to execute nodes of this kind. It is
#'   called as `executor(node, inputs)`, where `inputs` is a list of upstream
#'   artifacts named by node id. It may return the artifact itself, or an
#'   envelope `list(result = , summaries = , metadata = )`. Without
#'   `summaries`, a list is treated as an envelope only when it has exactly
#'   one of `result` or `artifact`, has `metadata`, and has no other entries;
#'   any other list is stored as the artifact. A
#'   `metadata$provenance = list(consumed = , effective = )` entry reports what
#'   the execution consumed (`consumed$D`, the data input ids it used;
#'   `consumed$subject`, the input id of the fit a check evaluated;
#'   `consumed$data_files`, `list(node_id, path)` entries naming file-path
#'   inputs it read) and the settings it actually used (`effective$P`,
#'   `effective$A`). Without a report, every input counts as consumed data.
#'   File-backed data is supported only as a regular file hashed before the
#'   run at the identical path, resolved in the executing process: every
#'   consumed `D` input whose value is a character string must be such a
#'   file, every `data_files` entry must name a consumed `D` input at that
#'   path, and neither those files nor the `stan_file` source may change
#'   during execution. Otherwise the provenance records a `capture_error`
#'   and no P/A/D snapshot; inputs that were not consumed are never checked.
#' @param pad Optional P/A/D classification of the kind's parameters: a named
#'   list with character vectors `P` (model), `A` (algorithm), `D` (data),
#'   `locator` (parameters that select inputs, such as `data_input`), and
#'   `other` (parameters that don't change the result's meaning, such as
#'   diagnostic thresholds). Each parameter may appear in one category only.
#'   When a node's parameters are all classified, each execution records a
#'   P/A/D snapshot with SHA-256 digests in its provenance (see
#'   [bg_node_pad()]); an unclassified parameter means no snapshot is
#'   recorded. The declaration is stored in the runtime manifest and restored
#'   by [bg_open()]. If provenance cannot be captured at all, the succeeded
#'   job records `capture_error` instead of `provenance`.
#' @return Invisible `TRUE`, after the kind is recorded in the graph registry
#'   and the on-disk registration manifest. The executor is bound into the
#'   handle's in-memory runtime registry (and its serialized source stored in
#'   the manifest entry) only when `executor` is supplied.
#' @export
bg_register_node_kind <- function(
  project,
  kind,
  input_contract = NULL,
  output_type = NULL,
  param_schema = NULL,
  executor = NULL,
  pad = NULL
) {
  S7::check_is_S7(project, bg_handle)
  pad <- bg_normalize_pad_declaration(pad)

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

  # The declaration follows the latest registration, including clearing it.
  if (!is.null(project@registries$node_kinds[[kind]]) || !is.null(pad)) {
    if (is.null(project@registries$node_kinds)) {
      project@registries$node_kinds <- list()
    }
    entry <- project@registries$node_kinds[[kind]] %||% list(name = kind)
    entry["pad"] <- list(pad)
    project@registries$node_kinds[[kind]] <- entry
  }

  bg_persist_node_kind_registration(
    project = project,
    kind = kind,
    input_contract = input_contract,
    output_type = output_type,
    param_schema = param_schema,
    executor = executor,
    pad = pad
  )

  invisible(TRUE)
}
