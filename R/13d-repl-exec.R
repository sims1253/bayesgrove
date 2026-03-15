# REPL Execution Functions
# ------------------------
# Functions for executing workflow actions in the REPL.

#' @keywords internal
bg_repl_execute_suggested_action <- function(
  project,
  scope = "project",
  idx = 1L,
  heading = "Action Preview"
) {
  guide_state <- bg_repl_guide_state(project, scope)
  actions <- guide_state$actions$actions

  if (length(actions) == 0) {
    cli::cli_inform("No suggested actions at this time.")
    return(invisible(list(executed = FALSE, action = NULL)))
  }

  idx_int <- suppressWarnings(as.integer(idx))
  if (is.na(idx_int) || idx_int < 1L || idx_int > length(actions)) {
    cli::cli_abort(
      "Action {idx} not found. Only {length(actions)} actions available."
    )
  }

  action <- actions[[idx_int]]
  bg_repl_print_action_preview(action, heading = heading)
  cli::cli_text("")

  if (bg_repl_confirm("Execute this action? [Y/n]: ")) {
    result <- bg_repl_execute_action(project, action, scope)
    return(invisible(list(executed = TRUE, action = action, result = result)))
  }

  cli::cli_inform("Action cancelled.")
  invisible(list(executed = FALSE, action = action, result = NULL))
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
      "create_node_from_template" = ,
      "branch_and_modify" = {
        bg_apply_template_action(project, action, target_scope)
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
bg_repl_execute_record_decision <- function(project, action, scope) {
  payload <- action$payload
  decision_type <- payload$decision_type %||% "note"

  if (identical(bg_action_template_ref(action), "review_decision")) {
    return(bg_apply_template_action(project, action, scope))
  }

  cli::cli_h2("Record Decision")
  cli::cli_text("{cli::col_cyan(action$title)}")
  cli::cli_text("")

  prompt <- if (identical(decision_type, "goal_update")) {
    "Describe the inferential goal for this branch:"
  } else {
    payload$prompt %||% action$title %||% "Describe the decision:"
  }

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
  bg_require_rationale(rationale)

  if (decision_type == "goal_update" && startsWith(scope, "branch:")) {
    decision <- bg_set_goal(
      project = project,
      branch_id = scope,
      kind = choice,
      label = choice_label,
      rationale = rationale
    )
  } else {
    decision_metadata <- bg_build_decision_metadata(
      action,
      payload,
      extra = if (identical(decision_type, "branch_disposition")) {
        list(disposition = choice)
      } else {
        list()
      }
    )

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
  bg_require_rationale(rationale)

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
bg_repl_export_report_command <- function(project, args = "") {
  export_request <- bg_repl_parse_export_args(args)
  cli::cli_inform("Exporting workflow report...")
  report_path <- bg_export_report(
    project,
    path = export_request$path,
    format = export_request$format
  )
  cli::cli_alert_success("Report exported to: {.val {report_path}}")
  invisible(report_path)
}
