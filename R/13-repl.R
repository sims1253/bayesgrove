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
bg_repl_print_branches <- function(project, current_scope = "project") {
  branches <- bg_list_branches(project)
  if (length(branches) == 0) {
    cli::cli_inform("No branches found.")
    return(invisible(NULL))
  }

  cli::cli_h2("Branches")
  cli::cli_text(
    "{cli::col_grey('Current scope:')} {cli::col_cyan(bg_scope_label(project, current_scope))}"
  )
  cli::cli_text("")

  goals <- bg_read_goal_registry(project)$branch_goals %||% list()

  for (branch_id in names(branches)) {
    branch <- branches[[branch_id]]
    current_marker <- if (identical(branch_id, current_scope)) {
      cli::col_green(" (current)")
    } else {
      ""
    }

    goal <- goals[[branch_id]]
    goal_status <- if (!is.null(goal)) {
      cli::col_green(paste(cli::symbol$tick, goal$kind))
    } else {
      cli::col_red(paste(cli::symbol$cross, "no goal"))
    }
    lifecycle <- branch$lifecycle %||% "active"
    lifecycle_label <- if (identical(lifecycle, "active")) {
      cli::col_green("active")
    } else {
      cli::col_yellow(lifecycle)
    }

    label <- if (nzchar(branch$label)) branch$label else "(no label)"
    cli::cli_bullets(c(
      "*" = "{cli::col_cyan(label)}{current_marker}",
      " " = "{cli::col_grey('ID:')} {branch$branch_id}",
      " " = "{cli::col_grey('Lifecycle:')} {lifecycle_label}",
      " " = "{cli::col_grey('Goal:')} {goal_status}"
    ))
  }

  cli::cli_text("")
  cli::cli_text(
    "{cli::col_grey('Use `scope <branch_id>` to switch, `use <n>` for quick switch by number.')}"
  )

  invisible(branches)
}

#' @keywords internal
bg_repl_status_rows <- function(status, project = NULL) {
  if (
    !is.null(project) &&
      (is.null(status$cached_nodes) || is.null(status$total_nodes))
  ) {
    plan <- bg_plan(
      project,
      external_holds = bg_workflow_external_holds(project)
    )
    status$cached_nodes <- status$cached_nodes %||% length(plan$cache_hits)
    status$total_nodes <- status$total_nodes %||%
      length(plan$graph_plan$topo_order)
  }

  rows <- c(
    "Workflow state" = bg_repl_state_label(status$workflow_state %||% ""),
    "Runnable now" = as.character(status$runnable_nodes %||% 0L),
    "Cached nodes" = as.character(status$cached_nodes %||% 0L),
    "Blocked nodes" = as.character(status$blocked_nodes %||% 0L),
    "Pending gates" = as.character(status$pending_gates %||% 0L),
    "Active jobs" = as.character(status$active_jobs %||% 0L),
    "Total nodes" = as.character(status$total_nodes %||% 0L)
  )

  if (!is.null(status$health)) {
    rows <- c(rows, "Health" = as.character(status$health))
  }

  if (!is.null(status$last_run_id) && nzchar(status$last_run_id)) {
    rows <- c(rows, "Last run" = status$last_run_id)
  }

  rows
}

#' @keywords internal
bg_repl_print_status <- function(status, project = NULL) {
  state_color <- switch(
    status$workflow_state %||% "",
    "open" = cli::col_cyan,
    "idle" = cli::col_blue,
    "running" = cli::col_yellow,
    "blocked" = cli::col_red,
    "paused" = cli::col_yellow,
    "degraded" = cli::col_magenta,
    cli::col_grey
  )

  health_icon <- switch(
    status$health %||% "",
    "ok" = cli::col_green(cli::symbol$tick),
    "warning" = cli::col_yellow(cli::symbol$warning),
    "error" = cli::col_red(cli::symbol$cross),
    ""
  )

  cli::cli_h2("Status")
  cli::cli_text(
    "{health_icon} {.strong Workflow State:} {state_color(bg_repl_state_label(status$workflow_state %||% ''))}"
  )

  rows <- bg_repl_status_rows(status, project = project)

  # Skip 'Workflow state' and 'Health' as we just printed them nicer
  skip_keys <- c("Workflow state", "Health")
  for (i in seq_along(rows)) {
    label <- names(rows)[[i]]
    if (label %in% skip_keys) {
      next
    }
    value <- unname(rows[[i]])
    cli::cli_bullets(c("*" = "{.strong {label}}: {value}"))
  }

  messages <- status$messages %||% character(0)
  if (length(messages) > 0) {
    cli::cli_text("")
    cli::cli_text("{.strong Notes:}")
    for (message in messages) {
      cli::cli_bullets(c("*" = cli::col_yellow(message)))
    }
  }
}

#' @keywords internal
bg_repl_node_rows <- function(project) {
  external_holds <- bg_workflow_external_holds(project)
  plan <- bg_plan(project, external_holds = external_holds)
  graph <- dagriculture::dagri_recompute_state(bg_read_graph(project))
  jobs <- bg_jobs(project)
  active_jobs <- Filter(function(j) j$status %in% c("queued", "running"), jobs)

  job_by_node <- list()
  for (job in active_jobs) {
    if (
      is.null(job_by_node[[job$node_id]]) || identical(job$status, "running")
    ) {
      job_by_node[[job$node_id]] <- job
    }
  }

  lapply(plan$graph_plan$topo_order, function(node_id) {
    node <- graph$nodes[[node_id]]
    node_job <- job_by_node[[node_id]] %||% NULL
    lifecycle <- bg_node_lifecycle(node)

    state <- if (!is.null(node_job)) {
      node_job$status
    } else if (!bg_is_active_lifecycle(lifecycle)) {
      lifecycle
    } else if (node_id %in% plan$cache_hits) {
      "cached"
    } else if (node_id %in% plan$to_execute) {
      "ready"
    } else if (node_id %in% names(plan$held_by_policy %||% list())) {
      "held"
    } else if (node_id %in% names(plan$blocked %||% list())) {
      "blocked"
    } else {
      node$state %||% "new"
    }

    detail <- if (!is.null(node_job)) {
      sprintf("Run %s", node_job$run_id)
    } else if (!bg_is_active_lifecycle(lifecycle)) {
      sprintf("Lifecycle: %s", lifecycle)
    } else if (node_id %in% names(plan$held_by_policy %||% list())) {
      sprintf("Policy hold: %s", plan$held_by_policy[[node_id]])
    } else if (node_id %in% names(plan$blocked %||% list())) {
      sprintf("Blocked: %s", plan$blocked[[node_id]])
    } else if (node_id %in% plan$cache_hits) {
      "Cached result available"
    } else if (
      !is.null(node$block_reason) && !identical(node$block_reason, "none")
    ) {
      sprintf("Graph state: %s", node$block_reason)
    } else {
      "Awaiting execution"
    }

    list(
      id = node_id,
      label = node$label %||% "(no label)",
      kind = node$kind,
      state = state,
      detail = detail
    )
  })
}

#' @keywords internal
bg_repl_print_nodes <- function(project, scope = "project") {
  graph <- dagriculture::dagri_recompute_state(bg_read_graph(project))

  external_holds <- bg_workflow_external_holds(project)
  plan <- bg_plan(project, external_holds = external_holds)

  jobs <- bg_jobs(project)
  active_jobs <- Filter(function(j) j$status %in% c("queued", "running"), jobs)
  job_by_node <- list()
  for (job in active_jobs) {
    if (
      is.null(job_by_node[[job$node_id]]) || identical(job$status, "running")
    ) {
      job_by_node[[job$node_id]] <- job
    }
  }

  # Overlay execution state onto the structural graph state for printing
  for (node_id in names(graph$nodes)) {
    node_job <- job_by_node[[node_id]] %||% NULL
    lifecycle <- bg_node_lifecycle(graph$nodes[[node_id]])

    state <- if (!is.null(node_job)) {
      node_job$status
    } else if (!bg_is_active_lifecycle(lifecycle)) {
      lifecycle
    } else if (node_id %in% plan$cache_hits) {
      "cached"
    } else if (node_id %in% plan$to_execute) {
      "runnable"
    } else if (node_id %in% names(plan$held_by_policy %||% list())) {
      "held"
    } else if (node_id %in% names(plan$blocked %||% list())) {
      "blocked"
    } else {
      dagri_state <- graph$nodes[[node_id]]$state %||% "new"
      if (identical(dagri_state, "ready")) "pending" else dagri_state
    }

    graph$nodes[[node_id]]$state <- state
  }

  # Filter nodes by scope if in branch mode
  if (startsWith(scope, "branch:")) {
    scope_node_ids <- bg_scope_node_ids(project, scope)
    graph$nodes <- graph$nodes[scope_node_ids]
    # Keep only edges fully contained within the branch view.
    graph$edges <- Filter(
      function(e) {
        e$from %in% scope_node_ids && e$to %in% scope_node_ids
      },
      graph$edges
    )
  }

  bg_print_graph_tree(graph)
  invisible(graph)
}

#' @keywords internal
bg_repl_gate_rows <- function(gates) {
  lapply(gates, function(gate) {
    list(
      id = gate$id,
      prompt = gate$prompt,
      route = sprintf(
        "%s -> %s",
        gate$from_label %||% gate$from_node_id %||% "?",
        gate$to_label %||% gate$to_node_id %||% "?"
      ),
      options = paste(gate$options %||% character(0), collapse = ", ")
    )
  })
}

#' @keywords internal
bg_repl_print_gates <- function(gates) {
  rows <- bg_repl_gate_rows(gates)
  if (length(rows) == 0) {
    cli::cli_inform("No pending gates.")
    return(invisible(NULL))
  }

  cli::cli_h2("Pending Gates")
  for (row in rows) {
    cli::cli_bullets(c(
      "*" = "{.strong {row$prompt}}",
      " " = "{cli::col_grey('Gate ID:')} {row$id}",
      " " = "{cli::col_grey('Route:')}   {cli::col_cyan(row$route)}",
      " " = "{cli::col_grey('Options:')} {row$options}"
    ))
  }

  invisible(rows)
}

#' @keywords internal
bg_repl_job_rows <- function(project) {
  graph <- bg_read_graph(project)
  jobs <- Filter(
    function(job) job$status %in% c("queued", "running"),
    bg_jobs(project)
  )

  lapply(jobs, function(job) {
    node <- graph$nodes[[job$node_id]] %||% list()
    list(
      job_id = job$job_id,
      run_id = job$run_id,
      status = job$status,
      node_id = job$node_id,
      node_label = node$label %||% job$node_id
    )
  })
}

#' @keywords internal
bg_repl_print_jobs <- function(project) {
  rows <- bg_repl_job_rows(project)
  if (length(rows) == 0) {
    cli::cli_inform("No active jobs.")
    return(invisible(NULL))
  }

  cli::cli_h2("Active Jobs")
  for (row in rows) {
    cli::cli_bullets(c(
      "*" = "{.strong {row$node_label}} [{cli::col_yellow(row$status)}]",
      " " = "{cli::col_grey('Job ID:')} {row$job_id}",
      " " = "{cli::col_grey('Node:')}   {row$node_id}",
      " " = "{cli::col_grey('Run:')}    {row$run_id}"
    ))
  }

  invisible(rows)
}

#' @keywords internal
bg_repl_print_guide <- function(project, scope = "project") {
  query <- bg_repl_protocol_query(scope)
  actions <- bg_next_actions(
    project,
    scope = query$scope,
    branch_id = query$branch_id
  )
  partitioned <- bg_partition_protocol_by_scope(actions, project = project)

  scope_label <- bg_scope_label(project, scope)
  cli::cli_h2("Workflow Guide")
  cli::cli_text("{cli::col_grey('Scope:')} {cli::col_cyan(scope_label)}")
  cli::cli_text("")

  if (length(actions$obligations) == 0) {
    cli::cli_alert_success("No active obligations. The workflow is healthy.")
  } else {
    cli::cli_text("{.strong Active Obligations:}")
    for (obl in actions$obligations) {
      color <- if (obl$severity == "blocking") cli::col_red else cli::col_yellow
      icon <- if (obl$severity == "blocking") {
        cli::symbol$cross
      } else {
        cli::symbol$warning
      }
      # Show scope label if different from current scope (for project-wide view)
      scope_note <- if (
        !identical(obl$scope, scope) && identical(scope, "project")
      ) {
        sprintf(" [%s]", bg_scope_label(project, obl$scope))
      } else {
        ""
      }
      cli::cli_bullets(c(
        "*" = "{color(icon)} {color(obl$title)}{cli::col_grey(scope_note)}"
      ))
      if (!is.null(obl$explanation$why)) {
        cli::cli_bullets(c(" " = "{cli::col_grey(obl$explanation$why)}"))
      }
    }
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
bg_repl_print_scope <- function(project, current_scope) {
  cli::cli_h2("Current Scope")
  cli::cli_text("{cli::col_grey('Scope:')} {cli::col_cyan(current_scope)}")

  branches <- bg_repl_list_branches(project)
  if (length(branches) == 0) {
    cli::cli_text("")
    cli::cli_text("{cli::col_grey('No branches found.')}")
    return(invisible(current_scope))
  }

  cli::cli_text("")
  cli::cli_text("{.strong Available Branches:}")
  for (branch_id in branches) {
    current_marker <- if (identical(branch_id, current_scope)) {
      " <- current"
    } else {
      ""
    }
    cli::cli_bullets(c("*" = "{branch_id}{cli::col_grey(current_marker)}"))
  }

  cli::cli_text("")
  cli::cli_text("{cli::col_grey('Use `scope <branch_id>` to switch scope.')}")
  invisible(current_scope)
}

#' @keywords internal
bg_repl_print_actions <- function(project, scope = "project") {
  query <- bg_repl_protocol_query(scope)
  actions <- bg_next_actions(
    project,
    scope = query$scope,
    branch_id = query$branch_id
  )

  if (length(actions$actions) == 0) {
    cli::cli_h2("Actions")
    cli::cli_inform("No suggested actions at this time.")
    return(invisible(list()))
  }

  cli::cli_h2("Actions")
  cli::cli_text("{cli::col_grey('Scope:')} {cli::col_cyan(scope)}")
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
bg_repl_post_action_hint <- function(project, scope, action_kind) {
  # Get fresh state after action
  query <- bg_repl_protocol_query(scope)
  actions <- bg_next_actions(
    project,
    scope = query$scope,
    branch_id = query$branch_id
  )

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

  template_ref <- if (is.list(action_kind)) {
    bg_action_template_ref(action_kind)
  } else {
    NULL
  }
  kind <- if (is.list(action_kind)) action_kind$kind %||% NULL else action_kind

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
bg_repl_execute_action <- function(project, action, scope = "project") {
  kind <- action$kind
  target_scope <- action$scope %||% scope
  template_ref <- bg_action_template_ref(action)

  result <- if (!is.null(template_ref)) {
    bg_apply_template_action(project, action, target_scope)
  } else {
    switch(
      kind,
      "record_decision" = {
        bg_repl_execute_record_decision(project, action, target_scope)
      },
      "branch_and_modify" = {
        bg_repl_execute_branch_and_modify(project, action, target_scope)
      },
      "create_node_from_template" = {
        bg_repl_execute_create_node_from_template(project, action, target_scope)
      },
      {
        cli::cli_alert_warning(
          "Action kind {.val {kind}} is not yet supported for direct execution."
        )
        cli::cli_text(
          "{cli::col_grey('You can still perform this action using the low-level API.')}"
        )
        return(invisible(NULL))
      }
    )
  }

  # Provide post-action guidance
  bg_repl_post_action_hint(project, scope, action)

  invisible(result)
}

#' @keywords internal
bg_repl_execute_create_node_from_template <- function(project, action, scope) {
  bg_apply_template_action(project, action, scope)
}

#' @keywords internal
bg_repl_execute_record_decision <- function(project, action, scope) {
  payload <- action$payload
  decision_type <- payload$decision_type %||% "note"

  if (identical(bg_action_template_ref(action), "review_decision")) {
    return(bg_apply_template_action(project, action, scope))
  }

  cli::cli_h2("Record Decision")
  cli::cli_text("{cli::col_cyan(action$title)}")
  cli::cli_text("")

  prompt <- switch(
    decision_type,
    "computation_review" = "Is this computation acceptable for downstream use?",
    "fit_criticism" = "What is your fit criticism assessment for these summaries?",
    "model_comparison" = "What is your explicit model comparison decision?",
    "branch_disposition" = "Should this branch be accepted or rejected?",
    "goal_update" = "Describe the inferential goal for this branch:",
    action$title
  )

  cli::cli_text("{cli::col_yellow('Prompt:')} {prompt}")
  cli::cli_text("")

  if (decision_type == "goal_update") {
    allowed_kinds <- payload$allowed_goal_kinds %||%
      c("observable_prediction", "latent_inference")

    cli::cli_text("{.strong Goal kinds:}")
    for (i in seq_along(allowed_kinds)) {
      cli::cli_bullets(c("*" = "[{i}] {allowed_kinds[[i]]}"))
    }
    cli::cli_text("")

    kind_idx <- bg_repl_readline("Choose goal kind (number): ")
    idx <- as.integer(kind_idx)

    if (is.na(idx) || idx < 1 || idx > length(allowed_kinds)) {
      cli::cli_abort("Invalid choice.")
    }

    choice <- allowed_kinds[[idx]]

    label_input <- bg_repl_readline("Optional label (press Enter to skip): ")
    choice_label <- if (nzchar(trimws(label_input))) {
      trimws(label_input)
    } else {
      choice
    }
  } else {
    choice_input <- bg_repl_readline("Enter your decision: ")
    if (trimws(choice_input) == "") {
      cli::cli_abort("Decision choice is required.")
    }
    choice <- trimws(choice_input)
    choice_label <- choice
  }

  rationale <- bg_repl_readline("Rationale for this decision: ")
  if (trimws(rationale) == "") {
    cli::cli_abort("Rationale is required.")
  }

  if (decision_type == "goal_update" && startsWith(scope, "branch:")) {
    decision <- bg_set_goal(
      project = project,
      branch_id = scope,
      kind = choice,
      label = choice_label,
      rationale = rationale
    )
  } else {
    decision_metadata <- Filter(
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

    if (identical(decision_type, "branch_disposition")) {
      decision_metadata$disposition <- choice
    }

    decision <- bg_record_decision(
      project = project,
      scope = scope,
      prompt = prompt,
      choice = choice_label,
      rationale = rationale,
      kind = decision_type,
      metadata = decision_metadata
    )
  }

  cli::cli_alert_success("Decision recorded: {.val {decision$decision_id}}")
  invisible(decision)
}

#' @keywords internal
bg_repl_execute_branch_and_modify <- function(project, action, scope) {
  bg_apply_template_action(project, action, scope)
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

#' @keywords internal
bg_repl_set_goal_interactive <- function(project, scope) {
  if (!startsWith(scope, "branch:")) {
    cli::cli_abort("Goals can only be set for branch scopes.")
  }

  allowed_kinds <- c("observable_prediction", "latent_inference")

  cli::cli_h2("Set Inferential Goal")
  cli::cli_text("{cli::col_grey('Branch:')} {scope}")
  cli::cli_text("")

  cli::cli_text("{.strong Goal kinds:}")
  for (i in seq_along(allowed_kinds)) {
    cli::cli_bullets(c("*" = "[{i}] {allowed_kinds[[i]]}"))
  }
  cli::cli_text("")

  kind_idx <- bg_repl_readline("Choose goal kind (number): ")
  idx <- as.integer(kind_idx)

  if (is.na(idx) || idx < 1 || idx > length(allowed_kinds)) {
    cli::cli_abort("Invalid choice.")
  }

  kind <- allowed_kinds[[idx]]

  label_input <- bg_repl_readline("Optional label (press Enter to skip): ")
  label <- if (nzchar(trimws(label_input))) trimws(label_input) else kind

  rationale <- bg_repl_readline("Rationale for this goal: ")
  if (trimws(rationale) == "") {
    cli::cli_abort("Rationale is required.")
  }

  decision <- bg_set_goal(
    project = project,
    branch_id = scope,
    kind = kind,
    label = label,
    rationale = rationale
  )

  cli::cli_alert_success("Goal set: {.val {kind}}")
  invisible(decision)
}

#' @keywords internal
bg_repl_print_decisions <- function(project, scope = "project", limit = 10L) {
  cli::cli_h2("Recent Decisions")

  decisions <- if (startsWith(scope, "branch:")) {
    bg_scope_decisions(project, scope)
  } else {
    bg_read_decisions(project)
  }

  if (length(decisions) == 0) {
    cli::cli_text("{cli::col_grey('No decisions recorded.')}")
    return(invisible(list()))
  }

  # Sort by created_at descending
  decisions_list <- unname(decisions)
  decisions_list <- decisions_list[order(
    vapply(decisions_list, function(d) d$created_at %||% "", character(1)),
    decreasing = TRUE
  )]

  # Limit
  if (length(decisions_list) > limit) {
    decisions_list <- decisions_list[seq_len(limit)]
    cli::cli_text("{cli::col_grey('Showing most recent {limit} decisions.')}")
    cli::cli_text("")
  }

  for (decision in decisions_list) {
    kind_color <- switch(
      decision$kind %||% "note",
      "gate_answer" = cli::col_cyan,
      "goal_update" = cli::col_green,
      "computation_review" = cli::col_yellow,
      cli::col_grey
    )

    cli::cli_bullets(c(
      "*" = "{kind_color(decision$kind %||% 'note')}: {decision$choice %||% '(no choice)'}"
    ))
    cli::cli_bullets(c(
      " " = "{cli::col_grey('ID:')} {decision$decision_id}",
      " " = "{cli::col_grey('Scope:')} {decision$scope %||% 'project'}",
      " " = "{cli::col_grey('Created:')} {decision$created_at %||% '(unknown)'}"
    ))

    if (!is.null(decision$rationale) && nzchar(decision$rationale)) {
      # Truncate long rationales
      rationale <- decision$rationale
      if (nchar(rationale) > 100) {
        rationale <- paste0(substr(rationale, 1, 97), "...")
      }
      cli::cli_bullets(c(" " = "{cli::col_grey('Rationale:')} {rationale}"))
    }

    cli::cli_text("")
  }

  invisible(decisions_list)
}

#' @keywords internal
bg_repl_help_lines <- function() {
  c(
    "status      Show workflow state and key counts",
    "guide       Show active obligations and suggested actions",
    "actions     List suggested actions with details",
    "do <n>      Execute action by number",
    "scope       Show or set current scope (project or branch)",
    "use <n>     Quick switch to branch by number (see `branches`)",
    "goal        Show or set the inferential goal for current scope",
    "decisions   Show recent decisions for current scope",
    "branches    List all branches with their status",
    "nodes       List nodes (scope-filtered in branch mode)",
    "gates       List pending decision gates with edge context",
    "result      Inspect the cached result of a node (requires node_id or label)",
    "answer      Answer the first pending gate interactively",
    "branch      Branch from an existing node (requires node_id or label)",
    "set         Set a parameter on a node: set <node_id or label> <key>=<value>",
    "invalidate  Invalidate a node and its downstream (requires node_id or label)",
    "retire      Retire a node and downstream lineage (requires node_id or label)",
    "retire-branch Retire an entire branch (requires branch id, branch label, or branch node)",
    "run [node]  Run eligible nodes or a specific node synchronously",
    "submit      Submit eligible nodes asynchronously",
    "jobs        List active background jobs",
    "cancel      Cancel an active run (requires run_id)",
    "exit        Leave REPL"
  )
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

#' Interactive REPL for BayesGrove
#'
#' @param project A `bg_handle`.
#' @param initial_scope Optional initial scope (e.g., "project" or "branch:xxx").
#'
#' @return Invisibly returns `NULL` when the session exits.
#'
#' @details
#' `bg_repl()` provides an interactive loop for navigating workflow state,
#' reviewing protocol guidance, and applying common workflow actions without
#' manually calling the lower-level APIs.
#'
#' The REPL is scope-aware. In project scope it can surface obligations and
#' actions across the whole workflow; in branch scope it focuses on the selected
#' branch and its branch-local graph view.
#'
#' Supported commands include:
#' \itemize{
#'   \item `help`: show the command list
#'   \item `status`: print workflow state and health
#'   \item `guide`: show active obligations and suggested actions
#'   \item `actions`: list actionable protocol suggestions with payload details
#'   \item `do <n>`: execute the nth suggested action
#'   \item `scope` / `scope <scope>`: inspect or change the current scope
#'   \item `branches` and `use <n>`: inspect branches and switch by index
#'   \item `goal` / `goal set`: inspect or set a branch-scoped inferential goal
#'   \item `decisions`: show recent decisions for the current scope
#'   \item `nodes`: print the scoped execution graph
#'   \item `result <node>`: inspect a cached result and fresh summaries
#'   \item `branch <node>`: create a new branch from a node and switch to it
#'   \item `set <node> <key>=<value>`: update a node label or parameter
#'   \item `invalidate <node>`: invalidate a node and downstream cache lineage
#'   \item `retire <node>`: retire a node and its downstream lineage
#'   \item `retire-branch <branch>`: retire an entire branch from future planning
#'   \item `gates` / `answer`: inspect and answer pending structural gates
#'   \item `run` / `submit`: execute or enqueue ready work in the current scope
#'   \item `jobs` and `cancel <run_id>`: inspect or cancel background work
#'   \item `exit`, `quit`, `q`: leave the REPL
#' }
#'
#' The function must be called from an interactive R session.
#'
#' @export
bg_repl <- function(project, initial_scope = NULL) {
  S7::check_is_S7(project, bg_handle)

  if (!interactive()) {
    cli::cli_abort("bg_repl() must be run in an interactive R session.")
  }

  # Initialize current scope
  current_scope <- initial_scope %||% "project"

  cli::cli_h1("BayesGrove Interactive REPL")
  cli::cli_text("Type 'help' for commands, 'exit' to leave.")

  repeat {
    bg_reconcile_daemon_jobs(project)

    st <- bg_status(project, auto_advance = FALSE)

    # Context-aware prompt with scope
    scope_short <- if (startsWith(current_scope, "branch:")) {
      substr(current_scope, 9, nchar(current_scope))
    } else {
      "proj"
    }

    prompt_str <- switch(
      st$workflow_state,
      "open" = cli::col_cyan(sprintf("bg [%s] (open)> ", scope_short)),
      "idle" = cli::col_blue(sprintf("bg [%s] (idle)> ", scope_short)),
      "running" = cli::col_yellow(sprintf("bg [%s] (running)> ", scope_short)),
      "blocked" = cli::col_red(sprintf("bg [%s] (blocked)> ", scope_short)),
      "paused" = cli::col_yellow(sprintf("bg [%s] (paused)> ", scope_short)),
      "degraded" = cli::col_magenta(sprintf(
        "bg [%s] (degraded)> ",
        scope_short
      )),
      sprintf("bg [%s]> ", scope_short)
    )

    input <- readline(prompt = prompt_str)

    if (input %in% c("exit", "quit", "q")) {
      cli::cli_inform("Exiting REPL.")
      break
    }

    if (trimws(input) == "" || startsWith(trimws(input), "#")) {
      next
    }

    parts <- strsplit(trimws(input), "\\s+")[[1]]
    cmd <- parts[1]

    # Allow arguments with spaces if quotes are not used, just merge the rest
    arg_str <- trimws(substr(trimws(input), nchar(cmd) + 1, nchar(input)))
    arg_str <- gsub("^\"|\"$", "", arg_str) # strip surrounding quotes if present
    arg_str <- gsub("^'|'$", "", arg_str)
    args <- if (nzchar(arg_str)) arg_str else character(0)

    tryCatch(
      {
        switch(
          cmd,
          "help" = {
            cli::cli_h2("Commands")
            for (line in bg_repl_help_lines()) {
              cli::cli_bullets(c("*" = line))
            }
          },
          "status" = {
            bg_repl_print_status(st, project = project)
          },
          "scope" = {
            if (length(args) == 0 || trimws(args) == "") {
              bg_repl_print_scope(project, current_scope)
            } else {
              new_scope <- trimws(args)
              # Validate branch scope
              if (startsWith(new_scope, "branch:")) {
                branches <- bg_repl_list_branches(project)
                if (!new_scope %in% branches) {
                  cli::cli_abort("Branch {.val {new_scope}} not found.")
                }
              } else if (!identical(new_scope, "project")) {
                cli::cli_abort(
                  "Invalid scope. Use 'project' or a branch id like 'branch:xxx'."
                )
              }
              current_scope <<- new_scope
              cli::cli_alert_success(
                "Switched to scope: {.val {current_scope}}"
              )
            }
          },
          "use" = {
            if (length(args) == 0 || trimws(args) == "") {
              cli::cli_abort(
                "Provide a branch number. Use `branches` to list available branches."
              )
            }

            idx <- as.integer(trimws(args))
            if (is.na(idx) || idx < 1) {
              cli::cli_abort("Invalid branch number.")
            }

            branches <- bg_repl_list_branches(project)
            if (length(branches) == 0) {
              cli::cli_abort(
                "No branches available. Use `branch <node>` to create one."
              )
            }

            if (idx > length(branches)) {
              cli::cli_abort(
                "Branch {idx} not found. Only {length(branches)} branches available."
              )
            }

            branch_id <- branches[[idx]]
            current_scope <<- branch_id
            cli::cli_alert_success(
              "Switched to branch: {.val {bg_scope_label(project, branch_id)}}"
            )
            cli::cli_text("")
            bg_repl_print_guide(project, current_scope)
          },
          "guide" = {
            bg_repl_print_guide(project, current_scope)
          },
          "actions" = {
            bg_repl_print_actions(project, current_scope)
          },
          "do" = {
            if (length(args) == 0 || trimws(args) == "") {
              cli::cli_abort(
                "Provide an action number. Use `actions` to list available actions."
              )
            }

            idx <- as.integer(trimws(args))
            if (is.na(idx) || idx < 1) {
              cli::cli_abort("Invalid action number.")
            }

            query <- bg_repl_protocol_query(current_scope)
            actions_result <- bg_next_actions(
              project,
              scope = query$scope,
              branch_id = query$branch_id
            )

            if (length(actions_result$actions) < idx) {
              cli::cli_abort(
                "Action {idx} not found. Only {length(actions_result$actions)} actions available."
              )
            }

            action <- actions_result$actions[[idx]]
            bg_repl_execute_action(project, action, current_scope)
          },
          "goal" = {
            if (length(args) == 0 || trimws(args) == "") {
              bg_repl_print_goal(project, current_scope)
            } else {
              subcmd <- trimws(args)
              if (identical(subcmd, "set")) {
                bg_repl_set_goal_interactive(project, current_scope)
              } else {
                cli::cli_abort(
                  "Unknown goal subcommand: {.val {subcmd}}. Use `goal` or `goal set`."
                )
              }
            }
          },
          "decisions" = {
            bg_repl_print_decisions(project, current_scope)
          },
          "branches" = {
            bg_repl_print_branches(project, current_scope)
          },
          "branch" = {
            if (length(args) == 0) {
              cli::cli_abort("Provide a node_id or label to branch.")
            }
            node_id <- bg_repl_find_node(project, args)
            branch <- bg_branch(project, node_id)
            cli::cli_inform(
              c(
                "Created branch:",
                "*" = "{.val {branch$branch_id}}",
                "*" = "Source node: {.val {branch$source_node_id}}",
                "*" = "Branch root: {.val {branch$root_node_id}}",
                "*" = "Label: {branch$label %||% '(none)'}"
              )
            )
            # Auto-switch to the new branch scope
            current_scope <<- branch$branch_id
            cli::cli_alert_info("Switched to new branch scope.")
          },
          "set" = {
            parts_split <- strsplit(trimws(arg_str), " ")[[1]]
            if (length(parts_split) < 2) {
              cli::cli_abort("Usage: set <node_id or label> <key>=<value>")
            }

            # The last token is key=value
            kv_str <- parts_split[length(parts_split)]

            # Everything before the last token is the node ref
            node_str <- trimws(substr(
              arg_str,
              1,
              nchar(arg_str) - nchar(kv_str)
            ))
            node_str <- gsub("^\"|\"$", "", node_str)
            node_str <- gsub("^'|'$", "", node_str)

            node_id <- bg_repl_find_node(project, node_str)

            eq_pos <- regexpr("=", kv_str, fixed = TRUE)[[1]]
            if (eq_pos < 2 || eq_pos >= nchar(kv_str)) {
              cli::cli_abort("Parameter must be key=value format.")
            }

            key <- trimws(substr(kv_str, 1, eq_pos - 1L))
            value_text <- trimws(substr(kv_str, eq_pos + 1L, nchar(kv_str)))

            node <- bg_read_graph(project)$nodes[[node_id]]

            if (key == "label") {
              bg_update_node(project, node_id, label = value_text)
              cli::cli_alert_success(
                "Updated label to {.val {value_text}} on {.val {node_id}}."
              )
            } else {
              params <- node$params %||% list()
              val <- bg_repl_coerce_param_value(value_text)
              params[[key]] <- val
              bg_update_node(project, node_id, params = params)
              cli::cli_alert_success(
                "Updated parameter {.val {key}} to {.val {val}} on {.val {node$label %||% node_id}}."
              )
            }
          },
          "invalidate" = {
            if (length(args) == 0) {
              cli::cli_abort("Provide a node_id or label to invalidate.")
            }
            node_id <- bg_repl_find_node(project, args)
            bg_invalidate(project, node_id)
          },
          "retire" = {
            if (length(args) == 0) {
              cli::cli_abort("Provide a node_id or label to retire.")
            }
            node_id <- bg_repl_find_node(project, args)
            bg_retire_node(project, node_id, recursive = TRUE)
          },
          "retire-branch" = {
            if (length(args) == 0) {
              cli::cli_abort(
                "Provide a branch id, branch label, or branch node to retire."
              )
            }
            branch_id <- bg_repl_find_branch(project, args)
            bg_retire_branch(project, branch_id)
          },
          "nodes" = {
            bg_repl_print_nodes(project, current_scope)
          },
          "result" = {
            if (length(args) == 0) {
              cli::cli_abort("Provide a node_id or label to inspect.")
            }
            node_id <- bg_repl_find_node(project, args)

            tryCatch(
              {
                res <- bg_result(project, node_id)

                cli::cli_h2("Result Artifact: {.val {node_id}}")

                if (length(res) == 0 && is.list(res)) {
                  cli::cli_text(cli::col_grey(
                    "(Empty artifact, execution only returned summaries or metadata)"
                  ))
                } else if (is.list(res)) {
                  # Format a nice summary of the list instead of dumping it raw
                  for (nm in names(res)) {
                    val <- res[[nm]]
                    if (is.atomic(val) && length(val) == 1) {
                      cli::cli_bullets(c("*" = "{.strong {nm}}: {.val {val}}"))
                    } else if (is.data.frame(val)) {
                      cli::cli_bullets(c(
                        "*" = "{.strong {nm}}: data.frame [{nrow(val)} x {ncol(val)}]"
                      ))
                    } else {
                      cli::cli_bullets(c(
                        "*" = "{.strong {nm}}: {class(val)[1]} [{length(val)}]"
                      ))
                    }
                  }
                } else {
                  print(res)
                }

                summaries <- bg_read_summaries(project)
                node_summaries <- Filter(
                  function(s) {
                    identical(s$node_id, node_id) && isTRUE(s$is_fresh)
                  },
                  summaries
                )

                if (length(node_summaries) > 0) {
                  cli::cli_h2("Diagnostics & Summaries")
                  for (s in node_summaries) {
                    color <- switch(
                      s$severity %||% "ok",
                      "blocking" = cli::col_red,
                      "warning" = cli::col_yellow,
                      "ok" = cli::col_green,
                      cli::col_grey
                    )
                    icon <- switch(
                      s$severity %||% "ok",
                      "blocking" = cli::symbol$cross,
                      "warning" = cli::symbol$warning,
                      "ok" = cli::symbol$tick,
                      "*"
                    )
                    cli::cli_bullets(c(
                      "*" = "{color(icon)} {.strong {s$summary_kind}} [{color(s$severity %||% 'ok')}]"
                    ))
                    if (!is.null(s$metrics) && length(s$metrics) > 0) {
                      for (m_nm in names(s$metrics)) {
                        cli::cli_bullets(c(
                          " " = "  {cli::col_grey(m_nm)}: {s$metrics[[m_nm]]}"
                        ))
                      }
                    }
                  }
                }
              },
              error = function(e) {
                cli::cli_abort("Could not fetch result: {e$message}")
              }
            )
          },
          "gates" = {
            bg_repl_print_gates(bg_pending_gates(project))
          },
          "answer" = {
            gates <- bg_pending_gates(project)
            if (length(gates) == 0) {
              cli::cli_inform("No pending gates to answer.")
            } else {
              g <- gates[[1]] # For MVP, just answer the first one interactively
              cli::cli_h2("Gate: {g$id}")
              cli::cli_text("{cli::col_yellow('Prompt:')} {g$prompt}")
              from_lbl <- g$from_label %||% g$from_node_id
              to_lbl <- g$to_label %||% g$to_node_id
              cli::cli_text(
                "{cli::col_grey('Route:')} {cli::col_cyan(from_lbl)} -> {cli::col_cyan(to_lbl)}"
              )
              cli::cli_text("")
              cli::cli_text("Options:")
              for (i in seq_along(g$options)) {
                cli::cli_bullets(c("*" = "[{i}] {g$options[[i]]}"))
              }
              cli::cli_text("")

              choice_idx <- bg_repl_readline("Choose an option (number): ")
              idx <- as.integer(choice_idx)

              if (is.na(idx) || idx < 1 || idx > length(g$options)) {
                cli::cli_abort("Invalid choice.")
              }

              choice <- g$options[idx]
              rationale <- bg_repl_readline("Rationale for this decision: ")

              if (trimws(rationale) == "") {
                cli::cli_abort("Rationale is required.")
              }

              bg_answer_gate(project, g$id, choice, rationale)
              cli::cli_alert_success("Gate answered and logged.")
            }
          },
          "run" = {
            if (length(bg_pending_gates(project)) > 0) {
              cli::cli_warn("Cannot run, workflow is blocked by pending gates.")
            } else {
              bg_run(
                project,
                targets = bg_repl_resolve_run_targets(
                  project,
                  current_scope,
                  args
                ),
                mode = "sync"
              )
            }
          },
          "submit" = {
            if (length(bg_pending_gates(project)) > 0) {
              cli::cli_warn(
                "Cannot submit, workflow is blocked by pending gates."
              )
            } else {
              bg_submit(
                project,
                targets = bg_repl_scope_targets(project, current_scope)
              )
            }
          },
          "jobs" = {
            bg_repl_print_jobs(project)
          },
          "cancel" = {
            if (length(args) == 0) {
              cli::cli_abort("Provide a run_id to cancel.")
            }
            bg_cancel(project, args[1])
          },
          {
            cli::cli_warn("Unknown command: {cmd}")
          }
        )
      },
      error = function(e) {
        cli::cli_alert_danger("Error: {e$message}")
      }
    )
  }
}
