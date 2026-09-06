# REPL Display Functions
# ----------------------
# Functions for printing and formatting REPL output.

#' Shared color/icon style for an obligation's severity.
#'
#' Obligation severity is binary in practice ("blocking" or "advisory"), so
#' anything other than "blocking" gets the advisory style; this keeps
#' unrecognized/future severities from silently falling through to no style.
#' @keywords internal
#' @noRd
bg_obligation_severity_style <- function(severity) {
  if (identical(severity, "blocking")) {
    list(color = cli::col_red, icon = cli::symbol$cross)
  } else {
    list(color = cli::col_yellow, icon = cli::symbol$warning)
  }
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

  # Skip 'Workflow state' and 'Health'; they are already printed above.
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

#' Compute the graph/plan/jobs needed to render node state, reusing an
#' already-computed bundle when the caller has one for this command cycle.
#' @keywords internal
#' @noRd
bg_repl_graph_plan_jobs <- function(project, bundle = NULL) {
  if (!is.null(bundle)) {
    return(list(
      graph = bg_dagri_recompute_state(bundle$raw_graph),
      plan = bundle$plan,
      jobs = bundle$state$jobs
    ))
  }

  external_holds <- bg_workflow_external_holds(project)
  list(
    graph = bg_dagri_recompute_state(bg_read_graph(project)),
    plan = bg_plan(project, external_holds = external_holds),
    jobs = bg_jobs(project)
  )
}

#' @keywords internal
bg_repl_job_by_node <- function(jobs) {
  active_jobs <- Filter(function(j) j$status %in% c("queued", "running"), jobs)
  job_by_node <- list()
  for (job in active_jobs) {
    if (
      is.null(job_by_node[[job$node_id]]) || identical(job$status, "running")
    ) {
      job_by_node[[job$node_id]] <- job
    }
  }
  job_by_node
}

#' @keywords internal
bg_repl_node_rows <- function(project, bundle = NULL) {
  gpj <- bg_repl_graph_plan_jobs(project, bundle)
  graph <- gpj$graph
  plan <- gpj$plan
  job_by_node <- bg_repl_job_by_node(gpj$jobs)

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
bg_repl_print_nodes <- function(project, scope = "project", bundle = NULL) {
  gpj <- bg_repl_graph_plan_jobs(project, bundle)
  graph <- gpj$graph
  plan <- gpj$plan
  job_by_node <- bg_repl_job_by_node(gpj$jobs)

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
    scope_node_ids <- bg_scope_node_ids(project, scope, graph = graph)
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
      from_node_id = gate$from_node_id %||% NULL,
      to_node_id = gate$to_node_id %||% NULL,
      from_label = gate$from_label %||% gate$from_node_id %||% "?",
      to_label = gate$to_label %||% gate$to_node_id %||% "?",
      prompt = gate$prompt,
      route = sprintf(
        "%s -> %s",
        gate$from_label %||% gate$from_node_id %||% "?",
        gate$to_label %||% gate$to_node_id %||% "?"
      ),
      options = paste(gate$options %||% character(0), collapse = ", "),
      option_values = gate$options %||% character(0)
    )
  })
}

#' @keywords internal
bg_repl_print_gate_rows <- function(rows) {
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
bg_repl_print_gates <- function(gates) {
  bg_repl_print_gate_rows(bg_repl_gate_rows(gates))
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
bg_repl_print_held_node_rows <- function(rows) {
  if (length(rows) == 0) {
    return(invisible(list()))
  }

  cli::cli_h2("Held Nodes")
  for (row in rows) {
    cli::cli_bullets(c(
      "*" = "{cli::col_yellow(row$label)} [{row$kind}]",
      " " = "{cli::col_grey('Node:')} {row$id}",
      " " = "{cli::col_grey('Reason:')} {row$detail}"
    ))
  }

  invisible(rows)
}

#' @keywords internal
bg_repl_print_decision_rows <- function(decision_rows) {
  cli::cli_h2("Recent Decisions")
  if (length(decision_rows) == 0) {
    cli::cli_text("{cli::col_grey('No decisions recorded.')}")
    return(invisible(list()))
  }

  for (decision in decision_rows) {
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

    if (nzchar(decision$rationale %||% "")) {
      cli::cli_bullets(c(
        " " = "{cli::col_grey('Rationale:')} {bg_repl_truncate_text(decision$rationale)}"
      ))
    }

    cli::cli_text("")
  }

  invisible(decision_rows)
}

#' @keywords internal
bg_repl_print_lineage_rows <- function(rows) {
  if (length(rows) == 0) {
    cli::cli_text("{cli::col_grey('No branch lineage available.')}")
    return(invisible(list()))
  }

  cli::cli_h2("Branch Lineage")

  for (row in rows) {
    prefix <- strrep("  ", row$depth)
    marker <- if (isTRUE(row$current)) " (current)" else ""
    cli::cli_bullets(c(
      "*" = "{prefix}{cli::col_cyan(row$label)}{cli::col_grey(marker)}",
      " " = "{prefix}{cli::col_grey('ID:')} {row$branch_id}"
    ))
  }

  invisible(rows)
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
bg_repl_help_lines <- function() {
  c(
    "dashboard   Show full workflow dashboard (status, guide, holds, decisions, gates)",
    "status      Show workflow state and key counts",
    "guide       Show active obligations and suggested actions",
    "explain <n> Show an obligation's rationale and literature references",
    "actions     List suggested actions with details",
    "next        Preview and execute the top recommended action",
    "do <n>      Execute action by number",
    "scope       Show or set current scope (project or branch)",
    "use <n>     Quick switch to branch by number (see `branches`)",
    "goal        Show or set the inferential goal for current scope",
    "decisions   Show recent decisions for current scope",
    "branches    List all branches with their status",
    "lineage     Show the current branch and its ancestors",
    "nodes       List nodes (scope-filtered in branch mode)",
    "gates       List pending decision gates with edge context",
    "result      Inspect the cached result of a node (requires node_id or label)",
    "answer      Answer the first pending gate interactively",
    "branch      Branch from an existing node (requires node_id or label)",
    "set         Set a parameter on a node: set <node_id or label> <key>=<value>",
    "invalidate  Invalidate a node and its downstream (requires node_id or label)",
    "retire      Retire a node and downstream lineage (requires node_id or label)",
    "retire-branch Retire an entire branch (requires branch id, branch label, or branch node)",
    "run [node]  Run eligible nodes or a specific node",
    "jobs        List job records",
    "export      Export workflow report [html|md] [path]",
    "exit        Leave REPL"
  )
}
