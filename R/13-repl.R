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
    prompt_str <- switch(st$workflow_state,
      "idle" = cli::col_blue("bayesgrove (idle)> "),
      "ready" = cli::col_green("bayesgrove (ready)> "),
      "running" = cli::col_yellow("bayesgrove (running)> "),
      "blocked" = cli::col_red("bayesgrove (blocked)> "),
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
        switch(cmd,
          "help" = {
            cli::cli_inform(c(
              "Commands:",
              "  status    - Show project status",
              "  nodes     - List all nodes",
              "  gates     - List pending decision gates",
              "  answer    - Interactively answer a pending gate",
              "  branch    - Branch from an existing node (requires node_id)",
              "  invalidate- Invalidate a node and its downstream (requires node_id)",
              "  run       - Run eligible nodes synchronously",
              "  submit    - Submit eligible nodes asynchronously",
              "  jobs      - List active background jobs",
              "  cancel    - Cancel an active run (requires run_id)",
              "  exit      - Leave REPL"
            ))
          },
          "status" = {
            print(st)
          },
          "branch" = {
            if (length(args) == 0) {
              cli::cli_abort("Provide a node_id to branch.")
            }
            new_id <- bg_branch(project, args[1])
            cli::cli_inform("Created branch: {.val {new_id}}")
          },
          "invalidate" = {
            if (length(args) == 0) {
              cli::cli_abort("Provide a node_id to invalidate.")
            }
            bg_invalidate(project, args[1])
          },
          "nodes" = {
            graph <- bg_read_graph(project)
            if (length(graph$nodes) == 0) {
              cli::cli_inform("No nodes in graph.")
            } else {
              for (id in names(graph$nodes)) {
                n <- graph$nodes[[id]]
                cli::cli_bullets(c("*" = "{id}: {n$kind} - {n$label %||% ''}"))
              }
            }
          },
          "gates" = {
            gates <- bg_pending_gates(project)
            if (length(gates) == 0) {
              cli::cli_inform("No pending gates.")
            } else {
              for (g in gates) {
                cli::cli_bullets(c("*" = "{g$id}: {g$prompt}"))
                cli::cli_text("  Options: {paste(g$options, collapse = ', ')}")
              }
            }
          },
          "answer" = {
            gates <- bg_pending_gates(project)
            if (length(gates) == 0) {
              cli::cli_inform("No pending gates to answer.")
            } else {
              g <- gates[[1]] # For MVP, just answer the first one interactively
              cli::cli_h2("Gate: {g$id}")
              cli::cli_text("Prompt: {g$prompt}")

              # Simple interactive choice
              cat("Options:\n")
              for (i in seq_along(g$options)) {
                cat(sprintf("  %d) %s\n", i, g$options[i]))
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
            active_jobs <- Filter(
              function(j) j$status %in% c("queued", "running"),
              bg_jobs(project)
            )
            if (length(active_jobs) == 0) {
              cli::cli_inform("No active jobs.")
            } else {
              for (j in active_jobs) {
                cli::cli_bullets(c(
                  "*" = "{j$job_id} ({j$status}): Node {j$node_id} [Run: {j$run_id}]"
                ))
              }
            }
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
