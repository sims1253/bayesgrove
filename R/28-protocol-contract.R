#' Execute a workflow action from the protocol surface
#'
#' Resolves an action by `action_id` from the current `bg_next_actions()`
#' surface and executes it inside bayesgrove so GUI clients do not need to own
#' workflow-command semantics.
#'
#' @param project A `bg_handle`.
#' @param action_id Action identifier from `bg_next_actions()`.
#' @param overrides Optional named list of user-supplied inputs.
#'
#' @return A plain-data execution result.
#' @export
bg_execute_action <- function(project, action_id, overrides = list()) {
  S7::check_is_S7(project, bg_handle)

  if (
    !is.character(action_id) || length(action_id) != 1 || !nzchar(action_id)
  ) {
    cli::cli_abort("`action_id` must be a single non-empty string.")
  }

  if (!is.list(overrides)) {
    cli::cli_abort("`overrides` must be a named list.")
  }

  actions <- bg_next_actions(project, scope = "project")$actions %||% list()
  action <- actions[[action_id]] %||% NULL
  if (is.null(action)) {
    cli::cli_abort(
      "Action {.val {action_id}} is not present in the current protocol surface."
    )
  }

  result <- bg_execute_resolved_action(
    project = project,
    action = action,
    overrides = overrides
  )

  bg_drop_null_fields(list(
    action_id = action$action_id,
    kind = action$kind,
    scope = action$scope,
    scope_label = action$scope_label %||% bg_scope_label(project, action$scope),
    template_ref = bg_action_template_ref(action),
    result = result
  ))
}

#' Return the descriptive extension registry for a project
#'
#' Exposes Bayesgrove-owned runtime extension descriptors as a read-only,
#' protocol-facing registry for GUI consumers.
#'
#' @param project A `bg_handle`.
#'
#' @return A plain-data extension registry.
#' @export
bg_extension_registry <- function(project) {
  S7::check_is_S7(project, bg_handle)

  graph <- bg_read_graph(project)
  node_kinds <- graph$registry$kinds %||% list()
  backends <- project@registries$backends %||% list()
  workflow_packs <- bg_workflow_packs(project)
  templates <- bg_list_templates()

  bg_drop_null_fields(list(
    node_kinds = bg_protocol_named_list(stats::setNames(
      lapply(names(node_kinds), function(kind) {
        spec <- node_kinds[[kind]]
        list(
          kind = kind,
          input_contract = spec$input_contract %||% NULL,
          output_type = spec$output_type %||% NULL,
          param_schema = spec$param_schema %||% NULL,
          executor_registered = !is.null(project@registries$node_kinds[[kind]]),
          stability = "project_runtime",
          owner = "bayesgrove",
          read_only = TRUE
        )
      }),
      names(node_kinds)
    )),
    backends = bg_protocol_named_list(stats::setNames(
      lapply(names(backends), function(name) {
        backend <- backends[[name]]
        list(
          backend_id = name,
          runtime_signature = if (
            is.function(backend$backend_runtime_signature)
          ) {
            backend$backend_runtime_signature(list())
          } else {
            list()
          },
          capabilities = c("compile", "fit"),
          owner = "bayesgrove",
          read_only = TRUE
        )
      }),
      names(backends)
    )),
    workflow_packs = bg_protocol_named_list(stats::setNames(
      lapply(workflow_packs, bg_workflow_pack_public_descriptor),
      vapply(workflow_packs, `[[`, character(1), "pack_id")
    )),
    templates = bg_protocol_named_list(stats::setNames(
      lapply(templates, function(template) {
        utils::modifyList(template, list(read_only = TRUE))
      }),
      names(templates)
    )),
    policy = list(
      execution_owner = "bayesgrove",
      gui_role = "presentation_only",
      registry_mode = "descriptive",
      gui_extension_api = FALSE
    )
  ))
}

#' @keywords internal
bg_execute_resolved_action <- function(project, action, overrides = list()) {
  scope <- action$scope %||% "project"
  template_ref <- bg_action_template_ref(action)

  if (!is.null(template_ref)) {
    return(bg_apply_template_action(
      project = project,
      action = action,
      scope = scope,
      overrides = overrides,
      interactive = FALSE
    ))
  }

  if (identical(action$kind %||% NULL, "record_decision")) {
    return(bg_execute_record_decision_action(
      project = project,
      action = action,
      scope = scope,
      overrides = overrides
    ))
  }

  cli::cli_abort(
    paste(
      "Action {.val {action$action_id %||% action$kind %||% 'unknown'}} is not",
      "executable through the canonical protocol surface."
    )
  )
}

#' @keywords internal
bg_execute_record_decision_action <- function(
  project,
  action,
  scope,
  overrides = list()
) {
  payload <- action$payload %||% list()
  decision_type <- payload$decision_type %||% "note"

  prompt <- overrides$prompt %||%
    if (identical(decision_type, "goal_update")) {
      "Describe the inferential goal for this branch:"
    } else {
      payload$prompt %||% action$title %||% "Describe the decision:"
    }

  rationale <- bg_protocol_required_string(
    overrides$rationale %||% NULL,
    "`rationale` override is required for non-interactive decision actions."
  )

  if (identical(decision_type, "goal_update") && startsWith(scope, "branch:")) {
    allowed_kinds <- payload$allowed_goal_kinds %||%
      c("observable_prediction", "latent_inference")
    choice <- bg_protocol_required_string(
      overrides$choice %||% NULL,
      "`choice` override is required for goal-setting actions."
    )
    if (!choice %in% allowed_kinds) {
      cli::cli_abort(
        "Goal choice {.val {choice}} is not one of {.val {allowed_kinds}}."
      )
    }

    return(bg_set_goal(
      project = project,
      branch_id = scope,
      kind = choice,
      label = overrides$choice_label %||% choice,
      rationale = rationale,
      metadata = list(action_id = action$action_id)
    ))
  }

  choice_spec <- bg_action_choice_spec(action)
  choice <- bg_protocol_required_string(
    overrides$choice %||% NULL,
    "`choice` override is required for decision actions."
  )
  choice_resolved <- bg_action_resolve_choice_label(
    choice = choice,
    choice_label = overrides$choice_label %||% NULL,
    choice_spec = choice_spec
  )

  decision_metadata <- bg_build_decision_metadata(
    action,
    payload,
    extra = if (identical(decision_type, "branch_disposition")) {
      list(disposition = choice_resolved$value)
    } else {
      list()
    }
  )

  bg_record_decision(
    project = project,
    scope = scope,
    prompt = prompt,
    choice = choice_resolved$label,
    rationale = rationale,
    kind = decision_type,
    metadata = decision_metadata
  )
}

#' @keywords internal
bg_protocol_required_string <- function(x, message) {
  if (is.null(x) || !is.character(x) || length(x) != 1 || !nzchar(trimws(x))) {
    cli::cli_abort(message)
  }

  trimws(x)
}

#' @keywords internal
bg_action_choice_spec <- function(action) {
  payload <- action$payload %||% list()
  decision_type <- payload$decision_type %||% NULL

  if (identical(decision_type, "computation_review")) {
    return(list(
      accept = "Accept",
      reject = "Reject",
      needs_revision = "Needs revision"
    ))
  }

  if (identical(decision_type, "branch_disposition")) {
    return(list(
      accept = "accept",
      reject = "reject"
    ))
  }

  NULL
}

#' @keywords internal
bg_action_resolve_choice_label <- function(
  choice,
  choice_label = NULL,
  choice_spec = NULL
) {
  normalized_choice <- trimws(choice)

  if (is.null(choice_spec)) {
    return(list(
      value = normalized_choice,
      label = trimws(choice_label %||% normalized_choice)
    ))
  }

  aliases <- c(
    stats::setNames(names(choice_spec), names(choice_spec)),
    stats::setNames(names(choice_spec), tolower(unname(unlist(choice_spec))))
  )
  key <- tolower(normalized_choice)
  resolved <- aliases[[key]] %||% NULL
  if (is.null(resolved)) {
    cli::cli_abort(
      "Choice {.val {choice}} is not one of {.val {names(choice_spec)}}."
    )
  }

  list(
    value = resolved,
    label = trimws(choice_label %||% choice_spec[[resolved]])
  )
}

#' @keywords internal
bg_protocol_command_surface <- function() {
  registry <- bg_remote_command_registry()
  boundary <- bg_api_boundary()

  bg_protocol_named_list(stats::setNames(
    lapply(names(registry), function(command_name) {
      spec <- registry[[command_name]]
      api_row <- boundary[boundary$fn == command_name, , drop = FALSE]

      bg_drop_null_fields(list(
        command = command_name,
        classification = if (nrow(api_row) == 1) {
          api_row$classification[[1]]
        } else {
          NULL
        },
        description = if (identical(command_name, "bg_snapshot")) {
          "Return the canonical GraphSnapshot message."
        } else if (nrow(api_row) == 1) {
          api_row$note[[1]]
        } else {
          NULL
        },
        result_message_type = if (identical(command_name, "bg_snapshot")) {
          "GraphSnapshot"
        } else {
          NULL
        },
        mutates_state = isTRUE(spec$mutates_state),
        allowed_args = spec$allowed_args %||% character(),
        required_args = spec$required_args %||% character()
      ))
    }),
    names(registry)
  ))
}

#' @keywords internal
bg_workflow_pack_public_descriptor <- function(pack_ref) {
  pack <- bg_lookup_workflow_pack(pack_ref$pack_id)

  bg_drop_null_fields(list(
    pack_id = pack_ref$pack_id,
    version = pack_ref$version %||% pack$version %||% NULL,
    title = pack$title %||% pack_ref$pack_id,
    description = pack$description %||% NULL,
    stability = pack$stability %||% "experimental",
    owner = "bayesgrove",
    read_only = TRUE
  ))
}

#' @keywords internal
bg_scope_kind <- function(scope) {
  if (identical(scope, "project")) {
    return("project")
  }

  if (startsWith(scope, "branch:")) {
    return("branch")
  }

  "other"
}

#' @keywords internal
bg_protocol_scope_descriptor <- function(project, scope) {
  scope_kind <- bg_scope_kind(scope)

  descriptor <- list(
    scope = scope,
    scope_kind = scope_kind,
    scope_label = bg_scope_label(project, scope)
  )

  if (!identical(scope_kind, "branch")) {
    return(descriptor)
  }

  branches <- bg_read_branch_registry(project)$branches %||% list()
  branch <- branches[[scope]] %||% list()
  descriptor$branch_context <- bg_drop_null_fields(list(
    branch_id = scope,
    label = branch$label %||% bg_scope_label(project, scope),
    root_node_id = branch$root_node_id %||% NULL,
    source_node_id = branch$source_node_id %||% NULL,
    lifecycle = bg_lifecycle_state(branch$metadata %||% list()),
    has_goal = !is.null(bg_get_goal(project, scope)),
    goal = bg_get_goal(project, scope)
  ))

  descriptor
}

#' @keywords internal
bg_protocol_operator_context <- function(project, item) {
  payload <- item$payload %||% list()
  basis <- item$basis %||% list()
  scope <- item$scope %||% "project"

  focus_node_ids <- sort(unique(c(
    basis$node_ids %||% character(),
    payload$node_ids %||% character(),
    payload$fit_node_ids %||% character(),
    payload$source_node_id %||% character(),
    payload$inputs %||% character()
  )))
  focus_summary_ids <- sort(unique(c(
    basis$summary_ids %||% character(),
    payload$summary_ids %||% character()
  )))
  focus_branch_ids <- sort(unique(c(
    basis$branch_ids %||% character(),
    payload$branch_ids %||% character(),
    if (startsWith(scope, "branch:")) scope else character()
  )))
  primary_node_id <- if (length(focus_node_ids) > 0) {
    focus_node_ids[[1]]
  } else {
    NULL
  }
  primary_branch_id <- if (length(focus_branch_ids) > 0) {
    focus_branch_ids[[1]]
  } else {
    NULL
  }

  bg_drop_null_fields(list(
    scope_kind = bg_scope_kind(scope),
    primary_node_id = primary_node_id,
    primary_branch_id = primary_branch_id,
    focus_node_ids = focus_node_ids,
    focus_summary_ids = focus_summary_ids,
    focus_branch_ids = focus_branch_ids
  ))
}

#' @keywords internal
bg_enrich_protocol_obligation <- function(project, obligation) {
  obligation$scope_label <- bg_scope_label(project, obligation$scope)
  obligation$scope_kind <- bg_scope_kind(obligation$scope)
  obligation$operator_context <- bg_protocol_operator_context(
    project,
    obligation
  )
  obligation
}

#' @keywords internal
bg_enrich_protocol_action <- function(project, action) {
  action$scope_label <- bg_scope_label(project, action$scope)
  action$scope_kind <- bg_scope_kind(action$scope)
  action$operator_context <- bg_protocol_operator_context(project, action)

  template_ref <- bg_action_template_ref(action)
  if (!is.null(template_ref)) {
    action$template <- bg_list_templates(template_ref)
  }

  action$invocation <- list(
    command = "bg_execute_action",
    args = list(action_id = action$action_id),
    prompt = bg_action_invocation_prompt(action),
    input = bg_action_input_descriptor(action)
  )

  action
}

#' @keywords internal
bg_action_invocation_prompt <- function(action) {
  payload <- action$payload %||% list()
  decision_type <- payload$decision_type %||% NULL

  if (identical(decision_type, "goal_update")) {
    return("Describe the inferential goal for this branch:")
  }

  template_ref <- bg_action_template_ref(action)
  if (identical(template_ref, "review_decision")) {
    return(bg_template_review_prompt(action, decision_type))
  }

  action$title %||% NULL
}

#' @keywords internal
bg_action_input_descriptor <- function(action) {
  payload <- action$payload %||% list()
  decision_type <- payload$decision_type %||% NULL
  template_ref <- bg_action_template_ref(action)

  fields <- if (identical(decision_type, "goal_update")) {
    list(
      choice = list(
        type = "string",
        required = TRUE,
        enum = payload$allowed_goal_kinds %||%
          c("observable_prediction", "latent_inference"),
        description = "Goal kind to record for the branch."
      ),
      choice_label = list(
        type = "string",
        required = FALSE,
        description = "Optional user-facing label for the goal."
      ),
      rationale = list(
        type = "string",
        required = TRUE,
        description = "Rationale for the inferential goal."
      )
    )
  } else if (identical(template_ref, "review_decision")) {
    field_choice <- list(
      type = "string",
      required = TRUE,
      description = bg_template_review_prompt(action, decision_type)
    )
    choice_spec <- bg_action_choice_spec(action)
    if (!is.null(choice_spec)) {
      field_choice$enum <- names(choice_spec)
    }

    list(
      choice = field_choice,
      choice_label = list(
        type = "string",
        required = FALSE,
        description = "Optional display label for the chosen decision."
      ),
      rationale = list(
        type = "string",
        required = TRUE,
        description = "Rationale for the recorded review decision."
      )
    )
  } else if (template_ref %in% c("diagnostic_check", "branch_comparison")) {
    list(
      label = list(
        type = "string",
        required = FALSE,
        default = payload$default_label %||% NULL,
        description = "Optional label override for the created node."
      )
    )
  } else if (identical(template_ref, "branch_and_modify_fit")) {
    list(
      label = list(
        type = "string",
        required = FALSE,
        default = payload$default_label %||% NULL,
        description = "Optional label override for the remediation branch."
      ),
      apply_parameter_suggestions = list(
        type = "boolean",
        required = FALSE,
        default = TRUE,
        description = "Whether to apply Bayesgrove's suggested parameter edits to the new branch root."
      )
    )
  } else {
    list()
  }

  list(
    mode = "non_interactive",
    fields = bg_protocol_named_list(fields)
  )
}
