#' Add a decision gate to an edge
#'
#' @param project A `bg_handle`.
#' @param from Upstream node ID.
#' @param to Downstream node ID.
#' @param prompt The question presented to the user.
#' @param options Character vector of valid options.
#' @param refs Optional list of references.
#' @param metadata Optional metadata.
#'
#' @return The generated `bg_pending_gate` record.
#' @export
bg_add_gate <- function(
  project,
  from,
  to,
  prompt,
  options,
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

  gate_id <- sprintf("gate_%s", digest::digest(runif(1), algo = "xxhash32"))

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
    options = as.character(options),
    refs = refs %||% list(),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
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

  if (is.null(rationale) || trimws(rationale) == "") {
    cli::cli_abort("Rationale is required for reproducibility.")
  }

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
          bg_modify_gate_specs(project, function(current_specs) {
            specs
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
#'
#' @return A list of denormalized pending gates with edge context.
#' @export
bg_pending_gates <- function(project) {
  S7::check_is_S7(project, bg_handle)

  specs <- bg_read_gate_specs(project)
  if (length(specs) == 0) {
    return(list())
  }

  graph <- bg_read_graph(project)

  lapply(specs, function(spec) {
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

  if (is.null(rationale) || trimws(rationale) == "") {
    cli::cli_abort("Rationale is required for reproducibility.")
  }

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
  decision_id <- sprintf("dec_%s", digest::digest(runif(1), algo = "xxhash32"))

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
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
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
  temp_path <- paste0(spec_path, ".tmp")

  # If list is empty, write an empty object {}
  if (length(specs) == 0) {
    # Hack to force write an empty object rather than empty array
    writeLines("{}", temp_path)
  } else {
    jsonlite::write_json(
      specs,
      temp_path,
      auto_unbox = TRUE,
      pretty = TRUE,
      force = TRUE
    )
  }

  file.rename(temp_path, spec_path)
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
