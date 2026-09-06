# REPL Main Loop
# --------------
# Interactive REPL for bayesgrove workflow management.

#' Interactive REPL for bayesgrove
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
#' In project scope, the REPL shows obligations and actions across the whole
#' workflow. In branch scope, it shows the selected branch and its graph.
#'
#' Supported commands include:
#' \itemize{
#'   \item `help`: show the command list
#'   \item `dashboard`: show workflow status, obligations, holds, and decisions
#'   \item `status`: print workflow state and health
#'   \item `guide`: show active obligations and suggested actions
#'   \item `explain <n>`: show an obligation's rationale and references
#'   \item `actions`: list actionable protocol suggestions with payload details
#'   \item `next`: preview and optionally execute the top suggested action
#'   \item `do <n>`: execute the nth suggested action
#'   \item `scope` / `scope <scope>`: inspect or change the current scope
#'   \item `branches` and `use <n>`: inspect branches and switch by index
#'   \item `goal` / `goal set`: inspect or set a branch-scoped inferential goal
#'   \item `decisions`: show recent decisions for the current scope
#'   \item `lineage`: show the current branch and its ancestors
#'   \item `nodes`: print the scoped execution graph
#'   \item `result <node>`: inspect a cached result and fresh summaries
#'   \item `branch <node>`: create a new branch from a node and switch to it
#'   \item `set <node> <key>=<value>`: update a node label or parameter
#'   \item `invalidate <node>`: invalidate a node and downstream cache lineage
#'   \item `retire <node>`: retire a node and its downstream lineage
#'   \item `retire-branch <branch>`: retire an entire branch from future planning
#'   \item `gates` / `answer`: inspect and answer pending structural gates
#'   \item `run`: execute ready work in the current scope
#'   \item `jobs`: inspect job records
#'   \item `export [html|md] [path]`: export a workflow report from the REPL
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

  # Mutable REPL state stored in an environment to avoid <<- assignments
  state <- new.env(parent = emptyenv())
  state$scope <- initial_scope %||% "project"

  cli::cli_h1("bayesgrove Interactive REPL")
  cli::cli_text("Type 'help' for commands, 'exit' to leave.")
  cli::cli_text("")

  bg_repl_print_dashboard(project, state$scope)

  repeat {
    st <- bg_status(project)

    # Context-aware prompt with scope
    scope_short <- if (startsWith(state$scope, "branch:")) {
      substr(state$scope, 9, nchar(state$scope))
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

    input <- bg_repl_readline(prompt = prompt_str)

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
          "dashboard" = {
            bg_repl_print_dashboard(project, state$scope)
          },
          "status" = {
            bg_repl_print_status(st, project = project)
          },
          "scope" = {
            if (length(args) == 0 || trimws(args) == "") {
              bg_repl_print_scope(project, state$scope)
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
              state$scope <- new_scope
              cli::cli_alert_success(
                "Switched to scope: {.val {state$scope}}"
              )
              cli::cli_text("")
              bg_repl_print_dashboard(project, state$scope)
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
            state$scope <- branch_id
            cli::cli_alert_success(
              "Switched to branch: {.val {bg_scope_label(project, branch_id)}}"
            )
            cli::cli_text("")
            bg_repl_print_dashboard(project, state$scope)
          },
          "guide" = {
            bg_repl_print_guide(project, state$scope)
          },
          "explain" = {
            if (length(args) == 0 || trimws(args) == "") {
              cli::cli_abort(
                "Provide an obligation number. Use `guide` to list obligations."
              )
            }
            idx <- as.integer(trimws(args))
            if (is.na(idx) || idx < 1) {
              cli::cli_abort("Invalid obligation number.")
            }
            bg_repl_explain_obligation(project, state$scope, idx)
          },
          "actions" = {
            bg_repl_print_actions(project, state$scope)
          },
          "next" = {
            bg_repl_execute_suggested_action(
              project,
              state$scope,
              idx = 1L,
              heading = "Next Action"
            )
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

            bg_repl_execute_suggested_action(project, state$scope, idx = idx)
          },
          "goal" = {
            if (length(args) == 0 || trimws(args) == "") {
              bg_repl_print_goal(project, state$scope)
            } else {
              subcmd <- trimws(args)
              if (identical(subcmd, "set")) {
                bg_repl_set_goal_interactive(project, state$scope)
              } else {
                cli::cli_abort(
                  "Unknown goal subcommand: {.val {subcmd}}. Use `goal` or `goal set`."
                )
              }
            }
          },
          "lineage" = {
            bg_repl_print_lineage(project, state$scope)
          },
          "export" = {
            bg_repl_export_report_command(project, args)
          },
          "decisions" = {
            bg_repl_print_decisions(project, state$scope)
          },
          "branches" = {
            bg_repl_print_branches(project, state$scope)
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
            state$scope <- branch$branch_id
            cli::cli_alert_info("Switched to new branch scope.")
            cli::cli_text("")
            bg_repl_print_dashboard(project, state$scope)
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
            bg_repl_print_nodes(project, state$scope)
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
                  # Print a summary of the list.
                  for (nm in names(res)) {
                    val <- res[[nm]]
                    if (is.atomic(val) && length(val) == 1) {
                      cli::cli_bullets(c("*" = "{.strong {nm}}: {.val {val}}"))
                    } else if (is.data.frame(val) || is.matrix(val)) {
                      cli::cli_bullets(c(
                        "*" = "{.strong {nm}}:"
                      ))
                      print(val)
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
              bg_require_rationale(rationale)

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
                  state$scope,
                  args
                )
              )
            }
          },
          "jobs" = {
            bg_repl_print_jobs(project)
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
    if (grepl(ref, lbl, ignore.case = TRUE, fixed = TRUE)) {
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
