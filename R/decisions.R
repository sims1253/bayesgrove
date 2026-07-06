#' Add a decision gate to an edge
#'
#' @param project A `bg_handle`.
#' @param from Upstream node ID.
#' @param to Downstream node ID.
#' @param prompt The question presented to the user.
#' @param alternatives Character vector of valid choices.
#' @param refs Optional list of references.
#' @param metadata Optional metadata.
#'
#' @return A gate specification list containing the gate ID, edge ID,
#'   prompt, options, refs, and metadata.
#' @export
bg_add_gate <- function(
  project,
  from,
  to,
  prompt,
  alternatives,
  refs = NULL,
  metadata = list()
) {
  S7::check_is_S7(project, bg_handle)

  graph <- bg_read_graph(project)

  # Find the edge between `from` and `to`
  edge_id <- NULL
  for (e in graph$edges) {
    if (e$from == from && e$to == to) {
      edge_id <- e$id
      break
    }
  }

  if (is.null(edge_id)) {
    cli::cli_abort("No edge found from {.val {from}} to {.val {to}}.")
  }

  gate_id <- bg_new_id("gate")

  # 1. Add structural gate to the graph
  graph <- dagriculture::dagri_add_gate(
    graph = graph,
    edge_id = edge_id,
    id = gate_id
  )

  bg_commit_graph(project, graph)

  # 2. Add semantic gate spec to bayesgrove storage
  gate_spec <- list(
    id = gate_id,
    edge_id = edge_id,
    prompt = prompt,
    options = as.character(alternatives),
    refs = refs %||% list(),
    created_at = bg_now_timestamp(),
    metadata = metadata
  )

  bg_modify_gate_specs(project, function(specs) {
    specs[[gate_id]] <- gate_spec
    specs
  })

  gate_spec
}

#' Answer a decision gate
#'
#' @param project A `bg_handle`.
#' @param id The gate ID to answer.
#' @param choice The chosen option.
#' @param rationale Required string explaining the choice.
#' @param refs Optional list of references.
#' @param evidence Optional vector of node IDs providing evidence.
#'
#' @return A `bg_decision_record`.
#' @export
bg_answer_gate <- function(
  project,
  id,
  choice,
  rationale = NULL,
  refs = NULL,
  evidence = NULL
) {
  S7::check_is_S7(project, bg_handle)

  bg_require_rationale(
    rationale,
    message = "Rationale is required for reproducibility."
  )

  specs <- bg_read_gate_specs(project)
  if (!id %in% names(specs)) {
    cli::cli_abort("Gate {.val {id}} not found in gate specs.")
  }

  gate_spec <- specs[[id]]
  gate_spec$options <- as.character(unlist(
    gate_spec$options,
    use.names = FALSE
  ))
  if (!choice %in% gate_spec$options) {
    cli::cli_abort(
      "Choice {.val {choice}} is not one of the valid options: {.val {gate_spec$options}}"
    )
  }

  # 1. Resolve structural gate in graph
  graph <- bg_read_graph(project)
  original_graph <- graph
  original_loaded_version <- project@loaded_graph_version
  if (!id %in% names(graph$gates)) {
    cli::cli_abort("Gate {.val {id}} not found in graph.")
  }

  edge <- graph$edges[[gate_spec$edge_id]]

  graph <- dagriculture::dagri_resolve_gate(graph, id)

  # 2. Build the decision and then commit the full transaction.
  decision <- bg_new_decision_record(
    project = project,
    scope = paste0("gate:", id),
    prompt = gate_spec$prompt,
    choice = choice,
    alternatives = as.character(setdiff(gate_spec$options, choice)),
    rationale = rationale,
    refs = refs %||% gate_spec$refs,
    evidence = evidence,
    kind = "gate_answer",
    metadata = list(
      gate_id = id,
      edge_id = gate_spec$edge_id,
      from_node_id = edge$from,
      to_node_id = edge$to,
      options = gate_spec$options,
      gate_metadata = gate_spec$metadata %||% list()
    )
  )

  tryCatch(
    {
      bg_commit_graph(project, graph)
      bg_modify_gate_specs(project, function(current_specs) {
        current_specs[[id]] <- NULL
        current_specs
      })
      bg_write_decision_record(project, decision)
    },
    error = function(e) {
      rollback_error <- tryCatch(
        {
          graph_path <- file.path(
            project@path,
            ".bayesgrove",
            "graph",
            "graph.json"
          )
          bg_write_json_atomic(graph_path, original_graph, sort_keys = FALSE)
          project@loaded_graph_version <- original_loaded_version
          # Restore only this gate entry to avoid clobbering concurrent updates.
          bg_modify_gate_specs(project, function(current_specs) {
            current_specs[[id]] <- specs[[id]] %||% NULL
            current_specs
          })
          NULL
        },
        error = function(rollback_e) rollback_e
      )

      if (!is.null(rollback_error)) {
        cli::cli_abort(c(
          "Failed to answer gate transactionally.",
          "Primary error: {e$message}",
          "Rollback error: {rollback_error$message}"
        ))
      }

      cli::cli_abort("Failed to answer gate transactionally: {e$message}")
    }
  )

  decision
}

#' List pending decision gates
#'
#' @param project A `bg_handle`.
#' @param graph Optional already-loaded raw graph, to avoid a redundant
#'   read when the caller already has one for the same command cycle.
#'
#' @return A list of denormalized pending gates with edge context.
#' @export
bg_pending_gates <- function(project, graph = NULL) {
  S7::check_is_S7(project, bg_handle)

  specs <- bg_read_gate_specs(project)
  if (length(specs) == 0) {
    return(list())
  }

  graph <- graph %||% bg_read_graph(project)
  inactive_node_ids <- bg_inactive_node_ids(project, graph = graph)

  gates <- Filter(
    function(spec) {
      edge <- graph$edges[[spec$edge_id]] %||% NULL
      if (is.null(edge)) {
        return(FALSE)
      }
      !edge$from %in% inactive_node_ids && !edge$to %in% inactive_node_ids
    },
    specs
  )

  lapply(gates, function(spec) {
    edge <- graph$edges[[spec$edge_id]]
    from_node <- graph$nodes[[edge$from]]
    to_node <- graph$nodes[[edge$to]]

    utils::modifyList(
      spec,
      list(
        from_node_id = edge$from,
        to_node_id = edge$to,
        from_label = from_node$label %||% from_node$kind,
        to_label = to_node$label %||% to_node$kind
      )
    )
  })
}

#' Record an explicit decision
#'
#' @param project A `bg_handle`.
#' @param scope Scope of the decision (e.g. 'project', 'node:node_id').
#' @param prompt The question/context.
#' @param choice The decision made.
#' @param alternatives Optional alternatives not chosen.
#' @param rationale Required rationale string.
#' @param refs Optional references.
#' @param evidence Optional node IDs providing evidence.
#' @param kind Decision kind, typically "note" or "gate_answer".
#' @param metadata Optional decision metadata.
#'
#' @return The generated `bg_decision_record`.
#' @export
bg_record_decision <- function(
  project,
  scope,
  prompt,
  choice,
  alternatives = NULL,
  rationale = NULL,
  refs = NULL,
  evidence = NULL,
  kind = "note",
  metadata = list()
) {
  S7::check_is_S7(project, bg_handle)

  bg_require_rationale(
    rationale,
    message = "Rationale is required for reproducibility."
  )

  record <- bg_new_decision_record(
    project = project,
    scope = scope,
    prompt = prompt,
    choice = choice,
    alternatives = alternatives,
    rationale = rationale,
    refs = refs,
    evidence = evidence,
    kind = kind,
    metadata = metadata
  )
  bg_write_decision_record(project, record)

  record
}

#' @keywords internal
bg_new_decision_record <- function(
  project,
  scope,
  prompt,
  choice,
  alternatives = NULL,
  rationale = NULL,
  refs = NULL,
  evidence = NULL,
  kind = "note",
  metadata = list()
) {
  decision_id <- bg_new_id("dec")

  res <- list(
    schema_name = "bg_decision_entry",
    schema_version = 1L,
    decision_id = decision_id,
    project_id = project@project_id,
    scope = scope,
    kind = kind,
    prompt = prompt,
    choice = choice,
    alternatives = alternatives %||% character(),
    rationale = rationale,
    refs = refs %||% list(),
    evidence = evidence %||% character(),
    status = "active",
    created_at = bg_now_timestamp(),
    supersedes = NULL,
    metadata = metadata %||% list()
  )
  class(res) <- "bg_decision_record"
  res
}

#' @keywords internal
bg_write_decision_record <- function(project, record) {
  log_path <- file.path(
    project@path,
    ".bayesgrove",
    "decisions",
    "decisions.jsonl"
  )
  bg_append_jsonl(log_path, record)
  bg_update_goal_registry_from_decision(project, record)

  invisible(record)
}

bg_read_decisions <- function(project) {
  log_path <- file.path(
    project@path,
    ".bayesgrove",
    "decisions",
    "decisions.jsonl"
  )

  if (!file.exists(log_path)) {
    return(list())
  }

  lines <- readLines(log_path, warn = FALSE)
  if (length(lines) == 0) {
    return(list())
  }

  decisions <- list()
  for (line in lines) {
    if (trimws(line) == "") {
      next
    }
    record <- jsonlite::fromJSON(line, simplifyVector = FALSE)
    class(record) <- "bg_decision_record"
    decisions[[record$decision_id]] <- record
  }

  decisions
}

# --- IO Helpers for Gate Specs ---

bg_read_gate_specs <- function(project) {
  spec_path <- bg_gate_specs_path(project)
  if (!file.exists(spec_path)) {
    return(list())
  }
  jsonlite::read_json(spec_path)
}

bg_write_gate_specs <- function(project, specs) {
  spec_path <- bg_gate_specs_path(project)
  if (length(specs) == 0) {
    bg_write_json_atomic(spec_path, list(), sort_keys = FALSE)
  } else {
    bg_write_json_atomic(spec_path, specs, sort_keys = FALSE)
  }
  invisible(TRUE)
}

bg_gate_specs_path <- function(project) {
  file.path(
    project@path,
    ".bayesgrove",
    "decisions",
    "gate_specs.json"
  )
}

bg_modify_gate_specs <- function(project, code, timeout = 10, poll = 0.05) {
  spec_path <- bg_gate_specs_path(project)
  lock_path <- paste0(spec_path, ".lock")

  bg_with_file_lock(
    lock_path,
    {
      specs <- bg_read_gate_specs(project)
      updated_specs <- code(specs)
      if (!is.list(updated_specs)) {
        cli::cli_abort("Gate spec modifier must return a list.")
      }
      bg_write_gate_specs(project, updated_specs)
      invisible(updated_specs)
    },
    timeout = timeout,
    poll = poll
  )
}

#' Build a decision metadata list from action and payload fields
#'
#' @param action An action list with `action_id`.
#' @param payload A payload list with optional provenance fields.
#' @param extra Additional fields to merge into the metadata.
#' @return A filtered list of non-NULL metadata entries.
#' @keywords internal
bg_build_decision_metadata <- function(action, payload, extra = list()) {
  base <- Filter(
    Negate(is.null),
    list(
      action_id = action$action_id,
      summary_ids = payload$summary_ids %||% character(),
      node_ids = payload$node_ids %||% character(),
      fit_node_ids = payload$fit_node_ids %||% character(),
      branch_ids = payload$branch_ids %||% character(),
      candidate_signature = payload$candidate_signature %||% NULL,
      comparison_signature = payload$comparison_signature %||% NULL,
      comparison_context = payload$comparison_context %||% NULL
    )
  )
  utils::modifyList(base, extra)
}

#' @keywords internal
bg_record_decision_from_action <- function(
  project,
  scope,
  prompt,
  choice,
  choice_label,
  rationale,
  decision_type,
  action,
  payload,
  extra_metadata = list()
) {
  extra_metadata$disposition <- extra_metadata$disposition %||% choice

  decision_metadata <- bg_build_decision_metadata(
    action,
    payload,
    extra = extra_metadata
  )

  bg_record_decision(
    project = project,
    scope = scope,
    prompt = prompt,
    choice = choice_label,
    rationale = rationale,
    kind = decision_type,
    metadata = decision_metadata
  )
}
