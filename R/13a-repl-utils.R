# REPL Utility Functions
# ----------------------
# Helper functions for the interactive REPL.

#' @keywords internal
bg_repl_state_label <- function(state) {
  switch(
    state,
    open = "Open",
    idle = "Idle",
    ready = "Ready",
    running = "Running",
    blocked = "Blocked",
    paused = "Paused",
    degraded = "Degraded",
    "Unknown"
  )
}

#' @keywords internal
bg_repl_scope_label <- function(scope) {
  if (is.null(scope) || identical(scope, "project")) {
    return("project")
  }
  if (startsWith(scope, "branch:")) {
    return(scope)
  }
  "project"
}

#' @keywords internal
bg_repl_protocol_query <- function(scope = "project") {
  if (!is.null(scope) && startsWith(scope, "branch:")) {
    return(list(
      scope = "branch",
      branch_id = scope
    ))
  }

  list(
    scope = "project",
    branch_id = NULL
  )
}

#' @keywords internal
bg_repl_coerce_param_value <- function(value) {
  numeric_value <- suppressWarnings(as.numeric(value))

  if (!is.na(numeric_value) && nzchar(value)) {
    return(numeric_value)
  }

  value
}

#' @keywords internal
bg_repl_readline <- function(prompt = "") {
  readline(prompt = prompt)
}

#' @keywords internal
bg_repl_confirm <- function(prompt = "Continue? [Y/n]: ") {
  response <- trimws(bg_repl_readline(prompt))
  identical(response, "") || identical(tolower(response), "y")
}

#' @keywords internal
bg_repl_format_value <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }

  if (is.data.frame(x)) {
    return(sprintf("<data.frame %d x %d>", nrow(x), ncol(x)))
  }

  if (is.list(x)) {
    if (length(x) == 0) {
      return("list()")
    }

    if (!is.null(names(x)) && any(nzchar(names(x)))) {
      parts <- vapply(
        names(x),
        function(name) {
          paste0(name, "=", bg_repl_format_value(x[[name]]))
        },
        character(1)
      )
      return(paste0("{", paste(parts, collapse = ", "), "}"))
    }

    parts <- vapply(x, bg_repl_format_value, character(1))
    return(paste0("[", paste(parts, collapse = ", "), "]"))
  }

  if (is.character(x)) {
    quoted <- paste0("'", x, "'")
    if (length(quoted) == 1) {
      return(quoted)
    }
    return(paste0("[", paste(quoted, collapse = ", "), "]"))
  }

  if (is.atomic(x)) {
    values <- as.character(x)
    if (length(values) == 1) {
      return(values)
    }
    return(paste0("[", paste(values, collapse = ", "), "]"))
  }

  sprintf("<%s>", class(x)[[1]])
}

#' @keywords internal
bg_repl_parse_export_args <- function(args = "") {
  input <- trimws(paste(args, collapse = " "))
  if (!nzchar(input)) {
    return(list(format = "html", path = NULL))
  }

  parts <- strsplit(input, "\\s+")[[1]]
  first <- tolower(parts[[1]])

  if (first %in% c("html", "md")) {
    path <- if (length(parts) > 1) paste(parts[-1], collapse = " ") else NULL
    if (!is.null(path) && !nzchar(trimws(path))) {
      path <- NULL
    }
    return(list(format = first, path = path))
  }

  inferred_format <- if (grepl("\\.md$", input, ignore.case = TRUE)) {
    "md"
  } else if (grepl("\\.html?$", input, ignore.case = TRUE)) {
    "html"
  } else {
    "html"
  }

  list(format = inferred_format, path = input)
}

#' @keywords internal
bg_repl_scope_targets <- function(project, scope = "project") {
  if (!startsWith(scope, "branch:")) {
    return(NULL)
  }

  bg_scope_node_ids(project, scope)
}

#' @keywords internal
bg_repl_resolve_run_targets <- function(project, scope = "project", args = "") {
  if (length(args) > 0 && nzchar(trimws(args))) {
    return(bg_repl_find_node(project, args))
  }

  bg_repl_scope_targets(project, scope)
}

#' @keywords internal
bg_repl_record_action_note <- function(
  project,
  scope,
  action,
  choice,
  rationale,
  metadata = list()
) {
  bg_record_decision(
    project = project,
    scope = scope,
    prompt = action$title %||% action$kind %||% "Workflow action",
    choice = choice,
    rationale = rationale,
    kind = "note",
    metadata = utils::modifyList(
      list(action_id = action$action_id %||% NULL),
      metadata %||% list()
    )
  )
}

#' @keywords internal
bg_repl_list_branches <- function(project) {
  branches <- bg_list_branches(project)
  if (length(branches) == 0) {
    return(character())
  }
  sort(names(branches))
}

#' @keywords internal
bg_repl_find_branch <- function(project, ref) {
  branches <- bg_read_branch_registry(project)$branches %||% list()
  if (ref %in% names(branches)) {
    return(ref)
  }

  labels <- vapply(
    branches,
    function(branch) branch$label %||% "",
    character(1)
  )
  exact <- names(labels)[labels == ref]
  if (length(exact) == 1) {
    return(exact[[1]])
  }
  if (length(exact) > 1) {
    cli::cli_abort(
      "Multiple branches match '{ref}'. Please use a more specific label or ID."
    )
  }

  node_id <- bg_repl_find_node(project, ref)
  branch_id <- bg_resolve_node_scope(project, node_id)
  if (!startsWith(branch_id, "branch:")) {
    cli::cli_abort(
      "Node {.val {node_id}} is not part of a branch. Provide a branch id or a branch node."
    )
  }

  branch_id
}

#' @keywords internal
bg_repl_find_node <- function(project, ref) {
  graph <- bg_read_graph(project)
  if (ref %in% names(graph$nodes)) {
    return(ref)
  }

  # exact label
  for (node_id in names(graph$nodes)) {
    if (identical(graph$nodes[[node_id]]$label, ref)) return(node_id)
  }

  # partial label
  matches <- character()
  for (node_id in names(graph$nodes)) {
    lbl <- graph$nodes[[node_id]]$label %||% ""
    if (grepl(ref, lbl, ignore.case = TRUE)) {
      matches <- c(matches, node_id)
    }
  }
  if (length(matches) == 1) {
    return(matches[[1]])
  }
  if (length(matches) > 1) {
    cli::cli_abort(
      "Multiple nodes match '{ref}'. Please use a more specific label or ID."
    )
  }
  cli::cli_abort("Node '{ref}' not found.")
}

#' @keywords internal
bg_repl_truncate_text <- function(text, width = 100L) {
  if (is.null(text) || !nzchar(text)) {
    return("")
  }

  if (nchar(text) <= width) {
    return(text)
  }

  paste0(substr(text, 1, width - 3L), "...")
}
