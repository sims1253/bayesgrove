# REPL Guide Functions
# --------------------
# Functions for workflow guide, actions, and dashboard state.

#' @keywords internal
bg_repl_guide_state <- function(project, scope = "project", bundle = NULL) {
  query <- bg_repl_protocol_query(scope)
  actions <- if (is.null(bundle)) {
    bg_next_actions(
      project,
      scope = query$scope,
      branch_id = query$branch_id
    )
  } else {
    resolved_scope <- bg_resolve_workflow_scope(
      project,
      query$scope,
      query$branch_id
    )
    bg_next_actions_impl(
      project,
      resolved_scope,
      plan = bundle$plan_seed,
      state = bundle$state,
      graph = bundle$raw_graph
    )
  }

  list(
    scope = scope,
    scope_label = bg_scope_label(project, scope),
    actions = actions,
    partitioned = bg_partition_protocol_by_scope(actions, project = project)
  )
}

#' @keywords internal
bg_repl_held_node_rows <- function(
  project,
  scope = "project",
  limit = 5L,
  bundle = NULL
) {
  rows <- Filter(
    function(row) identical(row$state, "held"),
    bg_repl_node_rows(project, bundle = bundle)
  )

  if (startsWith(scope, "branch:")) {
    scope_node_ids <- bg_scope_node_ids(
      project,
      scope,
      graph = bundle$raw_graph
    )
    rows <- Filter(function(row) row$id %in% scope_node_ids, rows)
  }

  if (!is.null(limit) && length(rows) > limit) {
    rows <- rows[seq_len(limit)]
  }

  rows
}

#' @keywords internal
bg_repl_decision_rows <- function(project, scope = "project", limit = 10L) {
  decisions <- if (startsWith(scope, "branch:")) {
    bg_scope_decisions(project, scope)
  } else {
    bg_read_decisions(project)
  }

  if (length(decisions) == 0) {
    return(list())
  }

  rows <- unname(decisions)
  rows <- rows[order(
    vapply(rows, function(decision) decision$created_at %||% "", character(1)),
    decreasing = TRUE
  )]

  if (!is.null(limit) && length(rows) > limit) {
    rows <- rows[seq_len(limit)]
  }

  lapply(rows, function(decision) {
    list(
      decision_id = decision$decision_id,
      kind = decision$kind %||% "note",
      scope = decision$scope %||% "project",
      prompt = decision$prompt %||% NULL,
      choice = decision$choice %||% "(no choice)",
      created_at = decision$created_at %||% "(unknown)",
      rationale = decision$rationale %||% ""
    )
  })
}

#' @keywords internal
bg_repl_lineage_rows <- function(project, scope) {
  if (!startsWith(scope, "branch:")) {
    return(list())
  }

  branch_ids <- rev(c(scope, bg_branch_lineage(project, scope)))
  lapply(seq_along(branch_ids), function(i) {
    branch_id <- branch_ids[[i]]
    list(
      branch_id = branch_id,
      label = bg_scope_label(project, branch_id),
      depth = i - 1L,
      current = identical(branch_id, scope)
    )
  })
}

#' @keywords internal
bg_repl_pending_gates <- function(project, scope = "project", graph = NULL) {
  gates <- bg_pending_gates(project, graph = graph)
  if (!startsWith(scope, "branch:")) {
    return(gates)
  }

  scope_node_ids <- bg_scope_node_ids(project, scope, graph = graph)
  Filter(
    function(gate) {
      from_node_id <- gate$from_node_id %||% NULL
      to_node_id <- gate$to_node_id %||% NULL
      from_node_id %in% scope_node_ids || to_node_id %in% scope_node_ids
    },
    gates
  )
}

#' @keywords internal
bg_repl_dashboard_state <- function(
  project,
  scope = "project",
  held_limit = 5L,
  decision_limit = 5L
) {
  # One graph+plan+holds bundle for the whole dashboard, instead of each
  # section (status, guide, held nodes, gates) re-reading and re-planning
  # independently.
  bundle <- bg_workflow_bundle(project)
  pending_gates <- bg_repl_pending_gates(
    project,
    scope,
    graph = bundle$raw_graph
  )
  list(
    scope = scope,
    scope_label = bg_scope_label(project, scope),
    status = bg_status_from_bundle(project, bundle),
    guide = bg_repl_guide_state(project, scope, bundle = bundle),
    held_nodes = bg_repl_held_node_rows(
      project,
      scope,
      limit = held_limit,
      bundle = bundle
    ),
    pending_gates = bg_repl_gate_rows(pending_gates),
    decisions = bg_repl_decision_rows(project, scope, limit = decision_limit),
    lineage = bg_repl_lineage_rows(project, scope)
  )
}

#' @keywords internal
bg_repl_print_guide_state <- function(guide_state, project) {
  scope <- guide_state$scope
  actions <- guide_state$actions
  partitioned <- guide_state$partitioned
  scope_label <- guide_state$scope_label
  cli::cli_h2("Workflow Guide")
  cli::cli_text("{cli::col_grey('Scope:')} {cli::col_cyan(scope_label)}")
  cli::cli_text("")

  if (length(actions$obligations) == 0) {
    cli::cli_alert_success("No active obligations. The workflow is healthy.")
  } else {
    cli::cli_text("{.strong Active Obligations:}")
    for (i in seq_along(actions$obligations)) {
      obl <- actions$obligations[[i]]
      style <- bg_obligation_severity_style(obl$severity)
      color <- style$color
      icon <- style$icon
      # Show scope label if different from current scope (for project-wide view)
      scope_note <- if (
        !identical(obl$scope, scope) && identical(scope, "project")
      ) {
        sprintf(" [%s]", bg_scope_label(project, obl$scope))
      } else {
        ""
      }
      cli::cli_bullets(c(
        "*" = "{color(icon)} [{i}] {color(obl$title)}{cli::col_grey(scope_note)}"
      ))
      if (!is.null(obl$explanation$why)) {
        cli::cli_bullets(c(" " = "{cli::col_grey(obl$explanation$why)}"))
      }
    }
    cli::cli_text(
      "{cli::col_grey('Type')} explain <n> {cli::col_grey('to see the rationale and references behind an obligation.')}"
    )
  }

  if (length(actions$actions) > 0) {
    cli::cli_text("")
    cli::cli_text("{.strong Suggested Actions:}")
    for (i in seq_along(actions$actions)) {
      act <- actions$actions[[i]]
      idx <- i
      # Show scope label if different from current scope (for project-wide view)
      scope_note <- if (
        !identical(act$scope, scope) && identical(scope, "project")
      ) {
        sprintf(" [%s]", bg_scope_label(project, act$scope))
      } else {
        ""
      }
      cli::cli_bullets(c(
        "*" = "[{idx}] {cli::col_cyan(act$title)}{cli::col_grey(scope_note)} {.grey [kind: {act$kind}]}"
      ))
      if (!is.null(act$explanation$why_now)) {
        cli::cli_bullets(c(" " = "{cli::col_grey(act$explanation$why_now)}"))
      }
    }
  }

  # Show summary if multiple scopes were evaluated
  if (length(partitioned$summary$scopes) > 1) {
    cli::cli_text("")
    cli::cli_text(
      "{cli::col_grey('Evaluated scopes:')} {paste(partitioned$summary$scopes, collapse = ', ')}"
    )
  }

  invisible(actions)
}

#' @keywords internal
bg_repl_print_guide <- function(project, scope = "project") {
  bg_repl_print_guide_state(bg_repl_guide_state(project, scope), project)
}

#' Print one obligation's rationale and literature references.
#'
#' Backs the REPL `explain <n>` command: `n` indexes the numbered obligation
#' list printed by `guide`. The references carried on every obligation (via
#' `bg_workflow_references`) are the teaching hook — they say WHY the
#' protocol asks, with citations to follow up on.
#' @keywords internal
#' @noRd
bg_repl_explain_obligation <- function(project, scope, idx) {
  actions <- bg_repl_guide_state(project, scope)$actions
  obligations <- actions$obligations %||% list()
  if (length(obligations) == 0) {
    cli::cli_inform("No active obligations to explain.")
    return(invisible(NULL))
  }
  if (is.na(idx) || idx < 1 || idx > length(obligations)) {
    cli::cli_abort(
      "Obligation {idx} not found. Use {.code guide} to list the {length(obligations)} active obligation{?s}."
    )
  }

  obl <- obligations[[idx]]
  style <- bg_obligation_severity_style(obl$severity %||% "advisory")
  cli::cli_h2("{style$color(style$icon)} {obl$title %||% obl$kind}")
  cli::cli_text(
    "{cli::col_grey('Kind:')} {obl$kind %||% 'unknown'}   {cli::col_grey('Severity:')} {style$color(obl$severity %||% 'advisory')}   {cli::col_grey('Scope:')} {bg_scope_label(project, obl$scope %||% 'project')}"
  )

  why <- obl$explanation$why %||% obl$description %||% NULL
  if (!is.null(why) && nzchar(why)) {
    cli::cli_text("")
    cli::cli_text("{.strong Why:} {why}")
  }

  refs <- obl$explanation$references %||% character()
  if (length(refs) > 0) {
    cli::cli_text("")
    cli::cli_text("{.strong References:}")
    for (ref in refs) {
      cli::cli_bullets(c("*" = "{ref}"))
    }
  }

  invisible(obl)
}

#' @keywords internal
bg_repl_print_actions <- function(project, scope = "project") {
  guide_state <- bg_repl_guide_state(project, scope)
  actions <- guide_state$actions

  if (length(actions$actions) == 0) {
    cli::cli_h2("Actions")
    cli::cli_inform("No suggested actions at this time.")
    return(invisible(list()))
  }

  cli::cli_h2("Actions")
  cli::cli_text(
    "{cli::col_grey('Scope:')} {cli::col_cyan(guide_state$scope_label)}"
  )
  cli::cli_text("")

  for (i in seq_along(actions$actions)) {
    act <- actions$actions[[i]]
    cli::cli_text("{.strong [{i}]} {cli::col_cyan(act$title)}")
    cli::cli_bullets(c(" " = "{cli::col_grey('Kind:')} {act$kind}"))
    cli::cli_bullets(c(" " = "{cli::col_grey('ID:')}   {act$action_id}"))

    if (!is.null(act$explanation$why_now)) {
      cli::cli_bullets(c(" " = "{cli::col_grey(act$explanation$why_now)}"))
    }

    if (!is.null(act$basis$node_ids) && length(act$basis$node_ids) > 0) {
      cli::cli_bullets(c(
        " " = "{cli::col_grey('Nodes:')} {paste(act$basis$node_ids, collapse = ', ')}"
      ))
    }

    if (!is.null(act$payload) && length(act$payload) > 0) {
      payload_str <- paste(
        vapply(
          names(act$payload),
          function(name) {
            paste0(name, " = ", bg_repl_format_value(act$payload[[name]]))
          },
          character(1)
        ),
        collapse = ",\n"
      )
      cli::cli_bullets(c(" " = "{cli::col_grey('Payload:')} {payload_str}"))
    }

    cli::cli_text("")
  }

  cli::cli_text("{cli::col_grey('Use `do <number>` to execute an action.')}")
  invisible(actions$actions)
}

#' @keywords internal
bg_repl_post_action_hint <- function(project, scope, action) {
  actions <- bg_repl_guide_state(project, scope)$actions

  n_blocking <- sum(vapply(
    actions$obligations,
    function(o) identical(o$severity, "blocking"),
    integer(1)
  ))
  n_actions <- length(actions$actions)

  hints <- character()

  if (n_blocking > 0) {
    hints <- c(hints, "guide - address blocking obligations")
  }

  if (n_actions > 0) {
    hints <- c(hints, "actions - see next suggested actions")
  }

  template_ref <- if (is.list(action)) bg_action_template_ref(action) else NULL
  kind <- if (is.list(action)) action$kind %||% NULL else action

  if (identical(template_ref, "diagnostic_check")) {
    hints <- c(hints, "run - execute the diagnostic check")
  } else if (identical(template_ref, "branch_comparison")) {
    hints <- c(hints, "run - execute the comparison")
  } else if (identical(template_ref, "branch_and_modify_fit")) {
    hints <- c(hints, "run - execute the new branch")
  } else if (identical(template_ref, "review_decision")) {
    hints <- c(hints, "run - continue execution")
  } else if (identical(kind, "branch_and_modify")) {
    hints <- c(hints, "run - execute the new branch")
  } else if (identical(kind, "create_node_from_template")) {
    hints <- c(hints, "run - execute the new node")
  } else if (identical(kind, "record_decision")) {
    hints <- c(hints, "run - continue execution")
  }

  if (length(hints) == 0) {
    hints <- "status - check workflow state"
  }

  cli::cli_text("")
  cli::cli_text("{cli::col_grey('Next:')} {paste(hints, collapse = ' | ')}")
  invisible(hints)
}

#' @keywords internal
bg_repl_action_preview_rows <- function(action) {
  list(
    title = action$title %||% action$kind %||% "Workflow action",
    kind = action$kind %||% "(unknown)",
    scope = action$scope %||% "project",
    why_now = action$explanation$why_now %||% NULL,
    node_ids = action$basis$node_ids %||% character(),
    payload = action$payload %||% list()
  )
}

#' @keywords internal
bg_repl_print_action_preview <- function(action, heading = "Action Preview") {
  preview <- bg_repl_action_preview_rows(action)
  cli::cli_h2(heading)
  cli::cli_bullets(c("*" = "{cli::col_cyan(preview$title)}"))
  cli::cli_bullets(c(" " = "{cli::col_grey('Kind:')} {preview$kind}"))

  if (startsWith(preview$scope, "branch:")) {
    cli::cli_bullets(c(" " = "{cli::col_grey('Scope:')} {preview$scope}"))
  }

  if (!is.null(preview$why_now)) {
    cli::cli_bullets(c(" " = "{cli::col_grey(preview$why_now)}"))
  }

  if (length(preview$node_ids) > 0) {
    cli::cli_bullets(c(
      " " = "{cli::col_grey('Nodes:')} {paste(preview$node_ids, collapse = ', ')}"
    ))
  }

  if (length(preview$payload) > 0) {
    # Filter out internal protocol metadata to keep the output readable
    internal_keys <- c(
      "summary_ids",
      "comparison_context",
      "comparison_signature",
      "candidate_signature",
      "fit_node_ids",
      "branch_ids",
      "source_node_id"
    )
    printable_payload <- preview$payload[setdiff(
      names(preview$payload),
      internal_keys
    )]

    if (length(printable_payload) > 0) {
      cli::cli_bullets(c(" " = "{cli::col_grey('Payload:')}"))
      for (name in names(printable_payload)) {
        val <- bg_repl_format_value(printable_payload[[name]])
        cli::cli_text("     {cli::col_cyan(name)}: {val}")
      }
    }
  }

  invisible(preview)
}

#' @keywords internal
bg_repl_print_held_nodes <- function(project, scope = "project", limit = 5L) {
  rows <- bg_repl_held_node_rows(project, scope, limit = limit)
  bg_repl_print_held_node_rows(rows)
}

#' @keywords internal
bg_repl_print_decisions <- function(project, scope = "project", limit = 10L) {
  decision_rows <- bg_repl_decision_rows(project, scope, limit = limit)
  bg_repl_print_decision_rows(decision_rows)
}

#' @keywords internal
bg_repl_print_dashboard <- function(project, scope = "project") {
  dashboard <- bg_repl_dashboard_state(project, scope)
  bg_repl_print_dashboard_state(dashboard, project)
}

#' @keywords internal
bg_repl_print_dashboard_state <- function(dashboard, project) {
  bg_repl_print_status(dashboard$status, project = project)
  cli::cli_text("")
  bg_repl_print_guide_state(dashboard$guide, project)

  if (startsWith(dashboard$scope, "branch:")) {
    cli::cli_text("")
    bg_repl_print_lineage_rows(dashboard$lineage)
  }

  if (length(dashboard$held_nodes) > 0) {
    cli::cli_text("")
    bg_repl_print_held_node_rows(dashboard$held_nodes)
  }

  if (length(dashboard$decisions) > 0) {
    cli::cli_text("")
    bg_repl_print_decision_rows(dashboard$decisions)
  }

  if (length(dashboard$pending_gates) > 0) {
    cli::cli_text("")
    bg_repl_print_gate_rows(dashboard$pending_gates)
  }

  invisible(NULL)
}

#' @keywords internal
bg_repl_print_lineage <- function(project, scope) {
  if (!startsWith(scope, "branch:")) {
    cli::cli_text(
      "{cli::col_grey('Lineage is only available in branch scope.')}"
    )
    return(invisible(NULL))
  }
  rows <- bg_repl_lineage_rows(project, scope)
  bg_repl_print_lineage_rows(rows)
}

#' @keywords internal
bg_repl_print_goal <- function(project, scope) {
  cli::cli_h2("Inferential Goal")

  if (!startsWith(scope, "branch:")) {
    cli::cli_text(
      "{cli::col_grey('Goals are branch-scoped. Switch to a branch using `scope <branch_id>`.')}"
    )
    return(invisible(NULL))
  }

  goal <- bg_get_goal(project, scope)

  if (is.null(goal)) {
    cli::cli_text(
      "{cli::col_yellow('No inferential goal set for this branch.')}"
    )
    cli::cli_text("")
    cli::cli_text(
      "{cli::col_grey('Use `goal set` to define an inferential goal.')}"
    )
    return(invisible(NULL))
  }

  cli::cli_bullets(c(
    "*" = "{cli::col_grey('Kind:')} {cli::col_cyan(goal$kind)}",
    "*" = "{cli::col_grey('Label:')} {goal$label %||% '(none)'}",
    "*" = "{cli::col_grey('Decision:')} {goal$decision_id %||% '(unknown)'}"
  ))

  invisible(goal)
}
