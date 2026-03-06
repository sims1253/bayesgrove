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
  cli::cli_h2("Status")
  rows <- bg_repl_status_rows(status, project = project)
  for (i in seq_along(rows)) {
    label <- names(rows)[[i]]
    value <- unname(rows[[i]])
    cli::cli_bullets(c("*" = "{.strong {label}}: {value}"))
  }

  messages <- status$messages %||% character(0)
  if (length(messages) > 0) {
    cli::cli_text("Notes:")
    for (message in messages) {
      cli::cli_bullets(c("*" = message))
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

    state <- if (!is.null(node_job)) {
      node_job$status
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
bg_repl_print_nodes <- function(project) {
  rows <- bg_repl_node_rows(project)
  if (length(rows) == 0) {
    cli::cli_inform("No nodes in graph.")
    return(invisible(NULL))
  }

  cli::cli_h2("Nodes")
  for (row in rows) {
    cli::cli_bullets(c(
      "*" = "{.strong {row$label}} ({row$kind}) [{row$state}]",
      " " = "{row$id}",
      " " = "{row$detail}"
    ))
  }

  invisible(rows)
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
      " " = "{row$id}",
      " " = "{row$route}",
      " " = "Options: {row$options}"
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
      "*" = "{.strong {row$node_label}} [{row$status}]",
      " " = "{row$job_id}",
      " " = "Node: {row$node_id}",
      " " = "Run: {row$run_id}"
    ))
  }

  invisible(rows)
}

#' @keywords internal
bg_repl_help_lines <- function() {
  c(
    "status      Show workflow state and key counts",
    "nodes       List nodes with labels, states, and cache/hold details",
    "gates       List pending decision gates with edge context",
    "answer      Answer the first pending gate interactively",
    "branch      Branch from an existing node (requires node_id)",
    "invalidate  Invalidate a node and its downstream (requires node_id)",
    "run         Run eligible nodes synchronously",
    "submit      Submit eligible nodes asynchronously",
    "jobs        List active background jobs",
    "cancel      Cancel an active run (requires run_id)",
    "exit        Leave REPL"
  )
}

#' Interactive REPL for BayesGrove
#'
#' @param project A `bg_handle`.
#'
#' @export
bg_repl <- function(project) {
  S7::check_is_S7(project, bg_handle)

  if (!interactive()) {
    cli::cli_abort("bg_repl() must be run in an interactive R session.")
  }

  cli::cli_h1("BayesGrove Interactive REPL")
  cli::cli_text("Type 'help' for commands, 'exit' to leave.")

  repeat {
    bg_reconcile_daemon_jobs(project)

    st <- bg_status(project, auto_advance = FALSE)

    # Context-aware prompt color
    prompt_str <- switch(
      st$workflow_state,
      "open" = cli::col_cyan("bayesgrove (open)> "),
      "idle" = cli::col_blue("bayesgrove (idle)> "),
      "running" = cli::col_yellow("bayesgrove (running)> "),
      "blocked" = cli::col_red("bayesgrove (blocked)> "),
      "paused" = cli::col_yellow("bayesgrove (paused)> "),
      "degraded" = cli::col_magenta("bayesgrove (degraded)> "),
      "bayesgrove> "
    )

    input <- readline(prompt = prompt_str)

    if (input %in% c("exit", "quit", "q")) {
      cli::cli_inform("Exiting REPL.")
      break
    }

    if (trimws(input) == "") {
      next
    }

    parts <- strsplit(trimws(input), "\\s+")[[1]]
    cmd <- parts[1]
    args <- if (length(parts) > 1) parts[-1] else character(0)

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
          "branch" = {
            if (length(args) == 0) {
              cli::cli_abort("Provide a node_id to branch.")
            }
            branch <- bg_branch(project, args[1])
            cli::cli_inform(
              c(
                "Created branch:",
                "*" = "{.val {branch$branch_id}}",
                "*" = "Source node: {.val {branch$source_node_id}}",
                "*" = "Branch root: {.val {branch$root_node_id}}",
                "*" = "Label: {branch$label %||% '(none)'}"
              )
            )
          },
          "invalidate" = {
            if (length(args) == 0) {
              cli::cli_abort("Provide a node_id to invalidate.")
            }
            bg_invalidate(project, args[1])
          },
          "nodes" = {
            bg_repl_print_nodes(project)
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
              cli::cli_text("Prompt: {g$prompt}")
              cli::cli_text(
                "Route: {g$from_label %||% g$from_node_id} -> {g$to_label %||% g$to_node_id}"
              )
              cli::cli_text("Options:")
              for (i in seq_along(g$options)) {
                cli::cli_bullets(c("*" = "{i}. {g$options[[i]]}"))
              }

              choice_idx <- readline("Choose an option (number): ")
              idx <- as.integer(choice_idx)

              if (is.na(idx) || idx < 1 || idx > length(g$options)) {
                cli::cli_abort("Invalid choice.")
              }

              choice <- g$options[idx]
              rationale <- readline("Rationale for this decision: ")

              if (trimws(rationale) == "") {
                cli::cli_abort("Rationale is required.")
              }

              bg_answer_gate(project, g$id, choice, rationale)
              cli::cli_inform("Gate answered and logged.")
            }
          },
          "run" = {
            if (st$workflow_state == "blocked") {
              cli::cli_warn("Cannot run, workflow is blocked by pending gates.")
            } else {
              bg_run(project, mode = "sync")
            }
          },
          "submit" = {
            if (st$workflow_state == "blocked") {
              cli::cli_warn(
                "Cannot submit, workflow is blocked by pending gates."
              )
            } else {
              bg_submit(project)
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
