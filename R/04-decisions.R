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

  specs <- bg_read_gate_specs(project)
  specs[[gate_id]] <- gate_spec
  bg_write_gate_specs(project, specs)

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
  if (!choice %in% gate_spec$options) {
    cli::cli_abort(
      "Choice {.val {choice}} is not one of the valid options: {.val {gate_spec$options}}"
    )
  }

  # 1. Resolve structural gate in graph
  graph <- bg_read_graph(project)
  if (!id %in% names(graph$gates)) {
    cli::cli_abort("Gate {.val {id}} not found in graph.")
  }

  graph <- dagriculture::dagri_resolve_gate(graph, id)
  bg_commit_graph(project, graph)

  # 2. Record the decision
  decision <- bg_record_decision(
    project = project,
    scope = paste0("gate:", id),
    prompt = gate_spec$prompt,
    choice = choice,
    rationale = rationale,
    refs = refs,
    evidence = evidence,
    kind = "gate_answer"
  )

  # Remove from pending gate specs
  specs[[id]] <- NULL
  bg_write_gate_specs(project, specs)

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
#' @param rationale Required rationale string.
#' @param refs Optional references.
#' @param evidence Optional node IDs providing evidence.
#' @param kind Decision kind, typically "note" or "gate_answer".
#'
#' @return The generated `bg_decision_record`.
#' @export
bg_record_decision <- function(
  project,
  scope,
  prompt,
  choice,
  rationale = NULL,
  refs = NULL,
  evidence = NULL,
  kind = "note"
) {
  S7::check_is_S7(project, bg_handle)

  if (is.null(rationale) || trimws(rationale) == "") {
    cli::cli_abort("Rationale is required for reproducibility.")
  }

  decision_id <- sprintf("dec_%s", digest::digest(runif(1), algo = "xxhash32"))

  record <- list(
    decision_id = decision_id,
    scope = scope,
    kind = kind,
    prompt = prompt,
    choice = choice,
    rationale = rationale,
    refs = refs %||% list(),
    evidence = evidence %||% character(),
    status = "active",
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    supersedes = NULL,
    metadata = list()
  )

  log_path <- file.path(
    project@path,
    ".bayesgrove",
    "decisions",
    "decisions.jsonl"
  )
  json_line <- jsonlite::toJSON(record, auto_unbox = TRUE)

  # Append to JSONL log
  cat(paste0(json_line, "\n"), file = log_path, append = TRUE)

  record
}

# --- IO Helpers for Gate Specs ---

bg_read_gate_specs <- function(project) {
  spec_path <- file.path(
    project@path,
    ".bayesgrove",
    "decisions",
    "gate_specs.json"
  )
  if (!file.exists(spec_path)) {
    return(list())
  }
  jsonlite::read_json(spec_path)
}

bg_write_gate_specs <- function(project, specs) {
  spec_path <- file.path(
    project@path,
    ".bayesgrove",
    "decisions",
    "gate_specs.json"
  )
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
