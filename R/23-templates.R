#' List built-in workflow templates
#'
#' @param template_ref Optional template ref. When provided, returns the
#'   matching descriptor or `NULL`.
#'
#' @return Either a list of built-in template descriptors keyed by
#'   `template_ref`, or a single descriptor when `template_ref` is supplied.
#'   Built-in refs are `diagnostic_check`, `branch_comparison`,
#'   `branch_and_modify_fit`, and `review_decision`.
#'
#' @details Built-in templates are intentionally small and explicit. Some
#'   create nodes directly, while others wrap narrow workflow macros such as
#'   `bg_branch_with_continuation()` or `bg_record_decision()`. The guided REPL
#'   uses the same registry to execute template-backed actions.
#' @export
bg_list_templates <- function(template_ref = NULL) {
  registry <- lapply(
    bg_builtin_template_registry(),
    bg_template_public_descriptor
  )

  if (is.null(template_ref)) {
    return(registry)
  }

  if (!is.character(template_ref) || length(template_ref) != 1) {
    cli::cli_abort("`template_ref` must be a single string.")
  }

  registry[[template_ref]] %||% NULL
}

#' @keywords internal
bg_review_decision_types <- function() {
  c(
    "computation_review",
    "fit_criticism",
    "prior_rationale",
    "prior_check_review",
    "posterior_check_review",
    "sbc_review",
    "model_comparison",
    "branch_disposition",
    "causal_question",
    "pad_annotation_review"
  )
}

#' @keywords internal
bg_builtin_template_registry <- function() {
  list(
    "diagnostic_check" = list(
      template_ref = "diagnostic_check",
      title = "Create diagnostic check",
      operation_type = "create_node",
      default_label = "Diagnostics: {source_label}",
      required_basis = list(
        action_kind = "create_node_from_template",
        source_node_id = "exactly one fit node"
      ),
      parameter_suggestions = list(
        node_kind = "check"
      ),
      description = paste0(
        "Create a downstream diagnostic check node from a single fit node."
      ),
      executor = bg_execute_template_diagnostic_check
    ),
    "branch_comparison" = list(
      template_ref = "branch_comparison",
      title = "Create comparison node",
      operation_type = "create_node",
      default_label = "Compare: {fit_labels}",
      required_basis = list(
        action_kind = "create_node_from_template",
        inputs = "two or more fit nodes"
      ),
      parameter_suggestions = list(
        node_kind = "compare"
      ),
      description = paste0(
        "Create a comparison node that takes multiple fit nodes as inputs."
      ),
      executor = bg_execute_template_branch_comparison
    ),
    "branch_and_modify_fit" = list(
      template_ref = "branch_and_modify_fit",
      title = "Branch and modify fit",
      operation_type = "workflow_macro",
      default_label = "{source_label} (revised)",
      required_basis = list(
        action_kind = "branch_and_modify",
        source_node_id = "exactly one fit node"
      ),
      parameter_suggestions = list(
        modification_hints = c("reparametrize", "adjust_tolerances"),
        continuation_kinds = c("check", "ppc")
      ),
      description = paste0(
        "Branch a fit node with continuation and apply suggested parameter ",
        "changes to the new branch root."
      ),
      executor = bg_execute_template_branch_and_modify_fit
    ),
    "review_decision" = list(
      template_ref = "review_decision",
      title = "Record review decision",
      operation_type = "record_decision",
      default_label = "{decision_type}",
      required_basis = list(
        action_kind = "record_decision",
        decision_type = bg_review_decision_types()
      ),
      parameter_suggestions = list(
        decision_types = bg_review_decision_types()
      ),
      description = paste0(
        "Record an explicit review-oriented decision tied to current summaries, ",
        "candidate fits, or branch disposition context."
      ),
      executor = bg_execute_template_review_decision
    )
  )
}

#' @keywords internal
bg_template_public_descriptor <- function(template) {
  template[setdiff(names(template), "executor")]
}

#' @keywords internal
bg_lookup_template <- function(template_ref, error = FALSE) {
  template <- bg_builtin_template_registry()[[template_ref]] %||% NULL
  if (is.null(template) && isTRUE(error)) {
    cli::cli_abort("Unknown template ref {.val {template_ref}}.")
  }
  template
}

#' @keywords internal
bg_action_template_ref <- function(action) {
  payload <- action$payload %||% list()
  template_ref <- payload$template_ref %||% NULL

  if (!is.null(template_ref)) {
    return(template_ref)
  }

  if (identical(action$kind %||% NULL, "branch_and_modify")) {
    return("branch_and_modify_fit")
  }

  decision_type <- payload$decision_type %||% NULL
  if (
    identical(action$kind %||% NULL, "record_decision") &&
      decision_type %in% bg_review_decision_types()
  ) {
    return("review_decision")
  }

  NULL
}

#' @keywords internal
bg_apply_template_action <- function(
  project,
  action,
  scope = "project",
  overrides = list(),
  interactive = TRUE
) {
  S7::check_is_S7(project, bg_handle)

  template_ref <- bg_action_template_ref(action)
  if (is.null(template_ref)) {
    cli::cli_abort(
      "Action {.val {action$kind %||% 'unknown'}} is not template-backed."
    )
  }

  template <- bg_lookup_template(template_ref, error = TRUE)
  template$executor(
    project = project,
    action = action,
    scope = scope,
    template = template,
    overrides = overrides,
    interactive = interactive
  )
}

#' @keywords internal
bg_template_resolve_source_node <- function(project, source_node_id) {
  graph <- bg_read_graph(project)
  source_node <- graph$nodes[[source_node_id]] %||% NULL

  if (is.null(source_node)) {
    cli::cli_abort("Source node {.val {source_node_id}} not found.")
  }

  list(
    graph = graph,
    node = source_node
  )
}

#' @keywords internal
bg_template_read_label <- function(prompt, default_label) {
  label_input <- bg_repl_readline(prompt)
  if (nzchar(trimws(label_input))) {
    return(trimws(label_input))
  }
  default_label
}

#' @keywords internal
bg_template_resolve_label <- function(
  prompt,
  default_label,
  overrides = list(),
  interactive = TRUE
) {
  label_override <- overrides$label %||% NULL
  if (!is.null(label_override) && nzchar(trimws(label_override))) {
    return(trimws(label_override))
  }

  if (!isTRUE(interactive)) {
    return(default_label)
  }

  bg_template_read_label(prompt, default_label)
}

#' @keywords internal
bg_template_resolve_rationale <- function(
  overrides = list(),
  interactive = TRUE,
  prompt = "Rationale for this decision: "
) {
  rationale <- overrides$rationale %||%
    if (isTRUE(interactive)) bg_repl_readline(prompt) else NULL

  bg_require_rationale(rationale)
}

#' @keywords internal
bg_template_review_prompt <- function(action, decision_type) {
  switch(
    decision_type,
    "computation_review" = "Is this computation acceptable for downstream use?",
    "fit_criticism" = "What is your fit criticism assessment for these summaries?",
    "prior_rationale" = "How do the current priors support this branch and inferential goal?",
    "prior_check_review" = "What is your review of the current prior predictive evidence?",
    "posterior_check_review" = "What is your review of the current posterior predictive evidence?",
    "sbc_review" = "What is your assessment of the current SBC evidence?",
    "model_comparison" = "What is your explicit model comparison decision?",
    "branch_disposition" = "Should this branch be accepted or rejected?",
    "causal_question" = "What causal question, estimand, or DAG review should be recorded for this branch?",
    "pad_annotation_review" = "How should this branch be described using PAD taxonomy and utility language?",
    action$title %||% "Record review decision"
  )
}

#' @keywords internal
bg_template_review_choice <- function(
  decision_type,
  overrides = list(),
  interactive = TRUE
) {
  choice_override <- overrides$choice %||% NULL
  choice_label_override <- overrides$choice_label %||% NULL

  if (!is.null(choice_override)) {
    choice_key <- tolower(trimws(choice_override))

    if (identical(decision_type, "computation_review")) {
      labels <- c(
        accept = "Accept",
        reject = "Reject",
        needs_revision = "Needs revision"
      )
      aliases <- c(
        accept = "accept",
        reject = "reject",
        needs_revision = "needs_revision",
        "needs revision" = "needs_revision"
      )
      resolved <- aliases[[choice_key]] %||% NULL
      if (is.null(resolved)) {
        cli::cli_abort(
          "Decision choice must be one of {.val {names(labels)}}."
        )
      }

      return(list(
        choice = resolved,
        choice_label = choice_label_override %||% labels[[resolved]]
      ))
    }

    if (identical(decision_type, "branch_disposition")) {
      if (!choice_key %in% c("accept", "reject")) {
        cli::cli_abort(
          "Decision choice must be one of {.val {c('accept', 'reject')}}."
        )
      }

      return(list(
        choice = choice_key,
        choice_label = choice_label_override %||% choice_key
      ))
    }

    return(list(
      choice = trimws(choice_override),
      choice_label = choice_label_override %||% trimws(choice_override)
    ))
  }

  if (!isTRUE(interactive)) {
    cli::cli_abort(
      "`choice` override is required for non-interactive review decisions."
    )
  }

  if (identical(decision_type, "computation_review")) {
    cli::cli_text("{.strong Options:}")
    cli::cli_bullets(c("*" = "[1] Accept - computation is valid for use"))
    cli::cli_bullets(c("*" = "[2] Reject - computation is not acceptable"))
    cli::cli_bullets(c("*" = "[3] Needs revision - acceptable with changes"))
    cli::cli_text("")

    idx <- as.integer(bg_repl_readline("Choose an option (1-3): "))
    if (is.na(idx) || idx < 1 || idx > 3) {
      cli::cli_abort("Invalid choice.")
    }

    return(list(
      choice = c("accept", "reject", "needs_revision")[[idx]],
      choice_label = c("Accept", "Reject", "Needs revision")[[idx]]
    ))
  }

  if (identical(decision_type, "branch_disposition")) {
    cli::cli_text("{.strong Options:}")
    cli::cli_bullets(c("*" = "[1] Accept"))
    cli::cli_bullets(c("*" = "[2] Reject"))
    cli::cli_text("")

    idx <- as.integer(bg_repl_readline("Choose an option (1-2): "))
    if (is.na(idx) || idx < 1 || idx > 2) {
      cli::cli_abort("Invalid choice.")
    }

    choice <- if (identical(idx, 1L)) "accept" else "reject"
    return(list(choice = choice, choice_label = choice))
  }

  choice_input <- bg_repl_readline("Enter your decision: ")
  if (!nzchar(trimws(choice_input))) {
    cli::cli_abort("Decision choice is required.")
  }

  choice <- trimws(choice_input)
  list(choice = choice, choice_label = choice)
}

#' @keywords internal
bg_template_review_metadata <- function(action) {
  payload <- action$payload %||% list()
  Filter(
    Negate(is.null),
    list(
      action_id = action$action_id,
      template_ref = bg_action_template_ref(action),
      summary_ids = payload$summary_ids %||% character(),
      node_ids = payload$node_ids %||% character(),
      fit_node_ids = payload$fit_node_ids %||% character(),
      branch_ids = payload$branch_ids %||% character(),
      candidate_signature = payload$candidate_signature %||% NULL,
      comparison_signature = payload$comparison_signature %||% NULL,
      comparison_context = payload$comparison_context %||% NULL
    )
  )
}

#' @keywords internal
bg_execute_template_diagnostic_check <- function(
  project,
  action,
  scope,
  template,
  overrides = list(),
  interactive = TRUE
) {
  payload <- action$payload %||% list()
  source_node_id <- payload$source_node_id %||%
    action$basis$node_ids[[1]] %||%
    payload$inputs[[1]] %||%
    NULL

  if (is.null(source_node_id)) {
    cli::cli_abort("Diagnostic check template requires `source_node_id`.")
  }

  source <- bg_template_resolve_source_node(project, source_node_id)
  source_node <- source$node
  if (!identical(source_node$kind %||% NULL, "fit")) {
    cli::cli_abort("Diagnostic check template requires a fit source node.")
  }

  default_label <- payload$default_label %||%
    paste("Diagnostics:", source_node$label %||% source_node_id)

  cli::cli_h2(template$title)
  cli::cli_text("{cli::col_cyan(action$title %||% template$title)}")
  cli::cli_text("")
  cli::cli_bullets(c(
    "*" = "{cli::col_grey('Source node:')} {source_node$label %||% source_node_id}"
  ))
  cli::cli_text("")

  label <- bg_template_resolve_label(
    "Label for diagnostic check node (press Enter for default): ",
    default_label,
    overrides = overrides,
    interactive = interactive
  )

  check_node_id <- bg_add_node(
    project = project,
    kind = payload$node_kind %||% "check",
    label = label,
    inputs = source_node_id
  )

  cli::cli_alert_success(
    "Created diagnostic check node: {.val {check_node_id}}"
  )

  decision <- bg_repl_record_action_note(
    project = project,
    scope = scope,
    action = action,
    choice = paste0("Created diagnostic check ", label),
    rationale = "Guided diagnostic-check template executed from the REPL.",
    metadata = list(
      node_id = check_node_id,
      source_node_id = source_node_id,
      template_ref = template$template_ref
    )
  )

  invisible(list(
    node_id = check_node_id,
    label = label,
    inputs = source_node_id,
    decision = decision
  ))
}

#' @keywords internal
bg_execute_template_branch_comparison <- function(
  project,
  action,
  scope,
  template,
  overrides = list(),
  interactive = TRUE
) {
  payload <- action$payload %||% list()
  input_ids <- payload$inputs %||% character()

  if (length(input_ids) < 2) {
    cli::cli_abort("Comparison requires at least two input nodes.")
  }

  graph <- bg_read_graph(project)
  input_labels <- vapply(
    input_ids,
    function(id) {
      node <- graph$nodes[[id]] %||% NULL
      if (is.null(node)) {
        cli::cli_abort("Comparison input node {.val {id}} not found.")
      }
      node$label %||% id
    },
    character(1)
  )

  cli::cli_h2(template$title)
  cli::cli_text("{cli::col_cyan(action$title %||% template$title)}")
  cli::cli_text("")
  cli::cli_text("{.strong Inputs to compare:}")
  for (i in seq_along(input_ids)) {
    cli::cli_bullets(c(
      "*" = "{cli::col_cyan(input_labels[[i]])} [{input_ids[[i]]}]"
    ))
  }
  cli::cli_text("")

  default_label <- payload$default_label %||%
    paste("Compare:", paste(input_labels, collapse = " vs "))
  label <- bg_template_resolve_label(
    "Label for comparison node (press Enter for default): ",
    default_label,
    overrides = overrides,
    interactive = interactive
  )

  compare_node_id <- bg_add_node(
    project = project,
    kind = payload$node_kind %||% "compare",
    label = label,
    inputs = input_ids
  )

  cli::cli_alert_success("Created comparison node: {.val {compare_node_id}}")
  cli::cli_bullets(c(
    "*" = "{cli::col_grey('Label:')} {label}",
    "*" = "{cli::col_grey('Inputs:')} {paste(input_labels, collapse = ', ')}"
  ))
  cli::cli_text("")
  cli::cli_text(
    "{cli::col_grey('Use `run` to execute the comparison, or `result {compare_node_id}` to inspect after execution.')}"
  )

  decision <- bg_repl_record_action_note(
    project = project,
    scope = scope,
    action = action,
    choice = paste0("Created comparison node ", label),
    rationale = "Guided comparison template executed from the REPL.",
    metadata = list(
      node_id = compare_node_id,
      template_ref = template$template_ref,
      inputs = input_ids
    )
  )

  invisible(list(
    node_id = compare_node_id,
    label = label,
    inputs = input_ids,
    decision = decision
  ))
}

#' @keywords internal
bg_execute_template_branch_and_modify_fit <- function(
  project,
  action,
  scope,
  template,
  overrides = list(),
  interactive = TRUE
) {
  payload <- action$payload %||% list()
  source_node_id <- payload$source_node_id %||% action$basis$node_ids[[1]]

  if (is.null(source_node_id)) {
    cli::cli_abort("No source node specified for branch operation.")
  }

  source <- bg_template_resolve_source_node(project, source_node_id)
  source_node <- source$node

  modification_hint <- payload$modification_hint
  parameter_suggestions <- payload$parameter_suggestions %||% list()
  continuation_kinds <- payload$continuation_kinds %||% c("check", "ppc")
  default_label <- payload$default_label %||%
    bg_revised_label(source_node)

  cli::cli_h2(template$title)
  cli::cli_text("{cli::col_cyan(action$title %||% template$title)}")
  cli::cli_text("")
  cli::cli_bullets(c(
    "*" = "{cli::col_grey('Source node:')} {source_node$label %||% source_node_id}",
    "*" = "{cli::col_grey('Kind:')} {source_node$kind}"
  ))

  if (!is.null(modification_hint)) {
    cli::cli_bullets(c(
      "*" = "{cli::col_grey('Suggested fix:')} {cli::col_yellow(modification_hint)}"
    ))
  }

  if (length(parameter_suggestions) > 0) {
    cli::cli_text("")
    cli::cli_text("{.strong Suggested parameter changes:}")
    for (param_name in names(parameter_suggestions)) {
      suggested_val <- parameter_suggestions[[param_name]]
      current_val <- source_node$params[[param_name]] %||% "(not set)"
      cli::cli_bullets(c(
        "*" = "{param_name}: {cli::col_grey(current_val)} -> {cli::col_green(suggested_val)}"
      ))
    }
  }
  cli::cli_text("")

  label <- bg_template_resolve_label(
    "Label for the new branch (press Enter for default): ",
    default_label,
    overrides = overrides,
    interactive = interactive
  )

  result <- bg_branch_with_continuation(
    project = project,
    node_id = source_node_id,
    label = label,
    continuation_kinds = continuation_kinds
  )
  branch <- result$branch
  continuation_nodes <- result$continuation_nodes
  branch_root_id <- branch$root_node_id

  bg_update_branch_metadata(
    project,
    branch$branch_id,
    list(
      goal_optional = TRUE,
      branch_purpose = "technical_revision",
      remediation_action_id = action$action_id %||% NULL,
      remediation_source_node_id = source_node_id,
      template_ref = template$template_ref
    )
  )
  branch$metadata <- utils::modifyList(
    branch$metadata %||% list(),
    list(goal_optional = TRUE, template_ref = template$template_ref)
  )

  cli::cli_alert_success("Created branch: {.val {branch$branch_id}}")
  cli::cli_bullets(c(
    "*" = "{cli::col_grey('Branch root:')} {branch_root_id}",
    "*" = "{cli::col_grey('Label:')} {label}"
  ))

  applied_params <- list()
  if (length(parameter_suggestions) > 0) {
    apply_params <- overrides$apply_parameter_suggestions %||% NULL
    if (is.null(apply_params)) {
      if (isTRUE(interactive)) {
        apply_input <- bg_repl_readline(
          "Apply suggested parameter changes? [Y/n]: "
        )
        apply_params <- !identical(tolower(trimws(apply_input)), "n")
      } else {
        apply_params <- TRUE
      }
    }
    apply_params <- isTRUE(apply_params)

    if (apply_params) {
      branch_node <- bg_read_graph(project)$nodes[[branch_root_id]]
      new_params <- utils::modifyList(
        branch_node$params %||% list(),
        parameter_suggestions
      )
      bg_update_node(project, branch_root_id, params = new_params)
      applied_params <- parameter_suggestions

      cli::cli_alert_success("Applied parameter changes to branch root.")
      for (param_name in names(parameter_suggestions)) {
        cli::cli_bullets(c(
          "*" = "{param_name} = {parameter_suggestions[[param_name]]}"
        ))
      }
    }
  }

  decision <- bg_repl_record_action_note(
    project = project,
    scope = branch$branch_id,
    action = action,
    choice = paste0("Created remediation branch ", label),
    rationale = "Guided branch-and-modify template executed from the REPL.",
    metadata = list(
      branch_id = branch$branch_id,
      branch_root_id = branch_root_id,
      source_node_id = source_node_id,
      template_ref = template$template_ref,
      applied_params = applied_params,
      continuation_node_ids = vapply(
        continuation_nodes,
        `[[`,
        character(1),
        "clone_id"
      )
    )
  )

  if (length(continuation_nodes) > 0) {
    cli::cli_text("")
    cli::cli_text("{.strong Continuation nodes:}")
    for (source_id in names(continuation_nodes)) {
      cont <- continuation_nodes[[source_id]]
      cli::cli_bullets(c(
        "*" = "{cli::col_cyan(cont$label)} [{cont$kind}]"
      ))
    }
  }

  cli::cli_text("")
  cli::cli_h3("Next Steps")
  if (length(applied_params) > 0) {
    cli::cli_bullets(c(
      "*" = "{.code run} - Execute the modified branch to test the fix"
    ))
  } else {
    cli::cli_bullets(c(
      "*" = "{.code set {branch_root_id} <key>=<value>} - Modify parameters",
      "*" = "{.code run} - Execute the branch"
    ))
  }

  goal <- bg_get_goal(project, branch$branch_id)
  if (is.null(goal) && !isTRUE(branch$metadata$goal_optional)) {
    cli::cli_bullets(c(
      "*" = "{.code scope {branch$branch_id}} - Switch to branch scope",
      "*" = "{.code goal set} - Define inferential goal for this branch"
    ))
  }

  invisible(list(
    branch = branch,
    continuation_nodes = continuation_nodes,
    decision = decision
  ))
}

#' @keywords internal
bg_execute_template_review_decision <- function(
  project,
  action,
  scope,
  template,
  overrides = list(),
  interactive = TRUE
) {
  payload <- action$payload %||% list()
  decision_type <- payload$decision_type %||% NULL

  if (is.null(decision_type)) {
    cli::cli_abort("Review decision template requires `decision_type`.")
  }

  prompt <- bg_template_review_prompt(action, decision_type)

  cli::cli_h2(template$title)
  cli::cli_text("{cli::col_cyan(action$title %||% template$title)}")
  cli::cli_text("")
  cli::cli_text("{cli::col_yellow('Prompt:')} {prompt}")
  cli::cli_text("")

  choice <- bg_template_review_choice(
    decision_type,
    overrides = overrides,
    interactive = interactive
  )
  rationale <- bg_template_resolve_rationale(
    overrides = overrides,
    interactive = interactive
  )

  decision_metadata <- bg_template_review_metadata(action)
  if (identical(decision_type, "branch_disposition")) {
    decision_metadata$disposition <- choice$choice
  }

  decision <- bg_record_decision(
    project = project,
    scope = scope,
    prompt = prompt,
    choice = choice$choice_label,
    rationale = rationale,
    kind = decision_type,
    metadata = decision_metadata
  )

  cli::cli_alert_success("Decision recorded: {.val {decision$decision_id}}")
  invisible(decision)
}
