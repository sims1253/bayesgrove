# --- Workflow Pack Registry ---

#' @keywords internal
bg_builtin_workflow_registry <- local({
  registry_env <- new.env(parent = emptyenv())

  function() {
    if (is.null(registry_env$registry)) {
      pack_version <- "0.1.0"

      registry_env$registry <- list(
        "bayesgrove.default_bayesian" = list(
          pack_id = "bayesgrove.default_bayesian",
          version = "0.2.0",
          title = "Default Bayesian workflow",
          description = paste(
            "Core guided Bayesian workflow semantics covering inferential goals,",
            "computation review, fit criticism, candidate comparison, and branch disposition."
          ),
          stability = "stable",
          obligation_providers = list(
            bg_default_bayesian_goal_obligations,
            bg_default_bayesian_summary_obligations,
            bg_default_bayesian_fit_criticism_obligations,
            bg_default_bayesian_comparison_obligations,
            bg_default_bayesian_disposition_obligations
          ),
          action_providers = list(
            bg_default_bayesian_goal_actions,
            bg_default_bayesian_summary_actions,
            bg_default_bayesian_fit_criticism_actions,
            bg_default_bayesian_comparison_decision_actions,
            bg_default_bayesian_disposition_actions
          )
        ),
        "bayesgrove.process_guidance" = list(
          pack_id = "bayesgrove.process_guidance",
          version = pack_version,
          title = "Process guidance pack",
          description = paste(
            "Adds Bayesian-workflow prompts for preflight planning,",
            "iteration, and out-of-sample stability review."
          ),
          stability = "experimental",
          obligation_providers = list(
            bg_pack_process_obligations
          ),
          action_providers = list(
            bg_pack_process_actions
          )
        ),
        "bayesgrove.model_taxonomy" = list(
          pack_id = "bayesgrove.model_taxonomy",
          version = pack_version,
          title = "Model taxonomy pack",
          description = paste(
            "Adds PAD classification and utility-tradeoff prompts based on the",
            "unified Bayesian model taxonomy."
          ),
          stability = "experimental",
          obligation_providers = list(
            bg_pack_taxonomy_obligations
          ),
          action_providers = list(
            bg_pack_taxonomy_actions
          )
        ),
        "bayesgrove.stan_workflow" = list(
          pack_id = "bayesgrove.stan_workflow",
          version = pack_version,
          title = "Stan workflow pack",
          description = paste(
            "Provides a stanflow-inspired workflow bundle with core Bayesgrove,",
            "process guidance, model taxonomy, predictive checks, selection,",
            "and Stan-specific diagnostic review."
          ),
          stability = "experimental",
          includes = c(
            "bayesgrove.default_bayesian",
            "bayesgrove.process_guidance",
            "bayesgrove.model_taxonomy",
            "bayesgrove.prior_workflow",
            "bayesgrove.model_checks",
            "bayesgrove.model_selection"
          ),
          obligation_providers = list(
            bg_pack_causal_stan_obligations
          ),
          action_providers = list(
            bg_pack_causal_stan_actions
          )
        ),
        "bayesgrove.prior_workflow" = list(
          pack_id = "bayesgrove.prior_workflow",
          version = pack_version,
          title = "Prior workflow pack",
          description = paste(
            "Extends the guided loop with prior rationale and prior predictive review."
          ),
          stability = "experimental",
          obligation_providers = list(
            bg_pack_prior_obligations
          ),
          action_providers = list(
            bg_pack_prior_actions
          )
        ),
        "bayesgrove.model_checks" = list(
          pack_id = "bayesgrove.model_checks",
          version = pack_version,
          title = "Model checks pack",
          description = paste(
            "Adds posterior predictive and simulation-based calibration review semantics."
          ),
          stability = "experimental",
          obligation_providers = list(
            bg_pack_checks_obligations
          ),
          action_providers = list(
            bg_pack_checks_actions
          )
        ),
        "bayesgrove.model_selection" = list(
          pack_id = "bayesgrove.model_selection",
          version = pack_version,
          title = "Model selection pack",
          description = paste(
            "Adds model-comparison and stacking-weight review semantics."
          ),
          stability = "experimental",
          obligation_providers = list(
            bg_pack_selection_obligations
          ),
          action_providers = list(
            bg_pack_selection_actions
          )
        ),
        "bayesgrove.causal_minimal" = list(
          pack_id = "bayesgrove.causal_minimal",
          version = pack_version,
          title = "Minimal causal framing pack",
          description = paste(
            "Adds lightweight causal-question prompts for branch-scoped workflows."
          ),
          stability = "experimental",
          obligation_providers = list(
            bg_pack_causal_minimal_obligations
          ),
          action_providers = list(
            bg_pack_causal_minimal_actions
          )
        ),
        "bayesgrove.causal_dagitty" = list(
          pack_id = "bayesgrove.causal_dagitty",
          version = pack_version,
          title = "Dagitty causal workflow pack",
          description = paste(
            "Adds dagitty-backed adjustment-set and implication review prompts",
            "for causal branches."
          ),
          stability = "experimental",
          obligation_providers = list(
            bg_pack_causal_dagitty_obligations
          ),
          action_providers = list(
            bg_pack_causal_dagitty_actions
          )
        ),
        "bayesgrove.pad_scaffold" = list(
          pack_id = "bayesgrove.pad_scaffold",
          version = pack_version,
          title = "PAD scaffold pack",
          description = paste(
            "Adds PAD taxonomy and utility-annotation prompts once a branch has a goal."
          ),
          stability = "experimental",
          obligation_providers = list(
            bg_pack_taxonomy_pad_obligations
          ),
          action_providers = list(
            bg_pack_taxonomy_pad_actions
          )
        )
      )
    }

    registry_env$registry
  }
})

# --- Pack Resolution & Lookup ---

#' @keywords internal
bg_lookup_workflow_pack <- function(pack_id) {
  bg_builtin_workflow_registry()[[pack_id]] %||% NULL
}

#' @keywords internal
bg_normalize_workflow_pack_ref <- function(spec) {
  if (is.character(spec) && length(spec) == 1) {
    spec <- list(pack_id = spec)
  }

  if (!is.list(spec) || is.null(spec$pack_id) || !is.character(spec$pack_id)) {
    cli::cli_abort("Workflow packs must be strings or lists with a `pack_id`.")
  }

  # One-release deprecation alias: bayesguide.* -> bayesgrove.*
  # TODO(0.9.0): remove this alias; only bayesgrove.* pack ids remain valid.
  if (startsWith(spec$pack_id, "bayesguide.")) {
    new_id <- sub("^bayesguide\\.", "bayesgrove.", spec$pack_id)
    if (!is.null(bg_lookup_workflow_pack(new_id))) {
      cli::cli_warn(
        "Pack id {.val {spec$pack_id}} is deprecated; use {.val {new_id}}."
      )
      spec$pack_id <- new_id
    }
  }

  pack <- bg_lookup_workflow_pack(spec$pack_id)
  if (is.null(pack)) {
    cli::cli_abort("Unknown workflow pack {.val {spec$pack_id}}.")
  }

  list(
    pack_id = spec$pack_id,
    version = spec$version %||% pack$version,
    config = spec$config %||% list()
  )
}

#' @keywords internal
bg_normalize_workflow_pack_refs <- function(specs) {
  specs <- specs %||% list()
  if (length(specs) == 0) {
    return(list())
  }

  unname(lapply(specs, bg_normalize_workflow_pack_ref))
}

#' List active workflow packs
#'
#' @param project A `bg_handle`.
#'
#' @return A list of active workflow-pack descriptors. The built-in default
#'   pack id is `bayesgrove.default_bayesian`. Additional built-in pack ids are
#'   `bayesgrove.prior_workflow`, `bayesgrove.model_checks`,
#'   `bayesgrove.model_selection`, `bayesgrove.causal_minimal`, and
#'   `bayesgrove.pad_scaffold`.
#'
#' @details The built-in default pack is an opinionated Bayesian workflow
#'   layer. It derives computation-review obligations from fresh warning/error
#'   summaries, adds branch-scoped fit criticism for model-diagnostic evidence,
#'   requires project-scoped comparison decisions when multiple fit candidates
#'   are clean, and asks each candidate branch to be explicitly accepted or
#'   rejected after a current comparison exists. The optional phase-10 packs
#'   extend that vocabulary with prior rationale and prior predictive review,
#'   posterior predictive and SBC review, model-selection evidence including
#'   stacking weights, minimal causal framing, and PAD annotations with utility
#'   dimensions.
#' @export
bg_workflow_packs <- function(project) {
  S7::check_is_S7(project, bg_handle)
  bg_normalize_workflow_pack_refs(
    bg_read_project_config(project)$workflow_packs %||% list()
  )
}

# --- Workflow Scope Resolution & Context ---

#' @keywords internal
bg_resolve_workflow_scope <- function(project, scope, branch_id = NULL) {
  scope <- match.arg(scope, c("project", "branch"))

  if (identical(scope, "project")) {
    return("project")
  }

  if (
    is.null(branch_id) || !is.character(branch_id) || length(branch_id) != 1
  ) {
    cli::cli_abort("`branch_id` is required when `scope = \"branch\"`.")
  }

  branches <- bg_read_branch_registry(project)$branches
  if (is.null(branches[[branch_id]])) {
    cli::cli_abort("Branch {.val {branch_id}} not found in branch registry.")
  }

  branch_id
}

#' Build a workflow context for protocol evaluation
#'
#' @param project A `bg_handle`.
#' @param scope One of `project` or `branch`.
#' @param branch_id Optional branch id, required for branch-scoped queries.
#'
#' @return A `bg_workflow_context` plain-data list.
#' @noRd
bg_workflow_context <- function(
  project,
  scope = c("project", "branch"),
  branch_id = NULL
) {
  resolved_scope <- bg_resolve_workflow_scope(project, scope, branch_id)
  bg_build_workflow_context(project, scope = resolved_scope)
}

# Protocol value normalization, canonicalization, and context collection
# functions are in protocol-helpers.R.

#' Expand pack refs by resolving `includes` (pack composition by data).
#'
#' Each pack ref may declare `includes = c(...)` naming other packs whose
#' providers should also run. This resolves the transitive closure with cycle
#' detection and dedup by pack id, so a pack's providers run at most once even
#' when reachable through multiple include chains. Included packs inherit the
#' config of the including pack ref unless the included pack is also directly
#' active (in which case its own config wins).
#'
#' @param pack_refs A list of normalized pack refs (from
#'   bg_normalize_workflow_pack_refs).
#' @return A list of expanded pack refs, deduplicated by pack_id, with included
#'   packs appearing after their includer.
#' @keywords internal
#' @noRd
bg_resolve_pack_includes <- function(pack_refs) {
  if (length(pack_refs) == 0) {
    return(list())
  }

  resolved <- list()
  # Tracks packs already expanded via includes (transitive closure). Top-level
  # active packs are NOT deduped here — literal duplicates are handled by
  # bg_merge_protocol_items so the merged item records pack_ids correctly.
  included_seen <- character(0)

  resolve_includes <- function(pack_ref, path) {
    pack_id <- pack_ref$pack_id

    if (pack_id %in% path) {
      cli::cli_warn(
        "Cycle detected in pack includes: {.val {c(path, pack_id)}}. Skipping."
      )
      return()
    }

    if (pack_id %in% included_seen) {
      return()
    }

    pack <- bg_lookup_workflow_pack(pack_id)
    if (is.null(pack)) {
      return()
    }

    included_seen <<- c(included_seen, pack_id)

    # Resolve included packs first (depth-first) so the includer's own
    # providers run last, matching the old explicit-bundle ordering.
    includes <- pack$includes %||% character()
    for (included_id in includes) {
      included_ref <- list(
        pack_id = included_id,
        version = pack_ref$version,
        config = pack_ref$config
      )
      resolve_includes(included_ref, c(path, pack_id))
    }
  }

  for (pack_ref in pack_refs) {
    # Expand this pack's includes (depth-first, deduped across the closure).
    resolve_includes(pack_ref, character(0))
  }

  # Build the final list: for each top-level pack ref, emit its included
  # packs (in resolution order) then the pack itself. Each included pack
  # appears at most once across the whole closure.
  emitted <- character(0)
  for (pack_ref in pack_refs) {
    pack <- bg_lookup_workflow_pack(pack_ref$pack_id)
    includes <- pack$includes %||% character()
    for (included_id in includes) {
      if (!included_id %in% emitted) {
        emitted <- c(emitted, included_id)
        resolved <- c(
          resolved,
          list(list(
            pack_id = included_id,
            version = pack_ref$version,
            config = pack_ref$config
          ))
        )
      }
    }
    resolved <- c(resolved, list(pack_ref))
  }

  resolved
}

#' @keywords internal
bg_dispatch_obligation_providers <- function(context, pack_ref) {
  pack <- bg_lookup_workflow_pack(pack_ref$pack_id)
  providers <- pack$obligation_providers %||% list()
  obligations <- unlist(
    lapply(providers, function(provider) {
      provider(context, pack_config = pack_ref$config %||% list())
    }),
    recursive = FALSE,
    use.names = FALSE
  )

  lapply(obligations, bg_canonicalize_obligation, pack_ref = pack_ref)
}

#' @keywords internal
bg_dispatch_action_providers <- function(context, obligations, pack_ref) {
  pack <- bg_lookup_workflow_pack(pack_ref$pack_id)
  providers <- pack$action_providers %||% list()
  actions <- unlist(
    lapply(providers, function(provider) {
      provider(
        context,
        obligations = obligations,
        pack_config = pack_ref$config %||% list()
      )
    }),
    recursive = FALSE,
    use.names = FALSE
  )

  lapply(actions, bg_canonicalize_action, pack_ref = pack_ref)
}

# --- Blocking Obligations & External Holds ---

#' @keywords internal
bg_blocking_obligation_holds <- function(project, obligations, graph = NULL) {
  # Thread the already-loaded graph through to avoid re-reading it on every
  # protocol evaluation (the run loop calls this once per executed node). Callers
  # that already hold the recomputed-state graph pass it; otherwise fall back to
  # a fresh read so behavior is unchanged for one-shot callers.
  if (is.null(graph)) {
    graph <- bg_read_graph(project)
  }
  holds <- list()

  for (obligation in obligations) {
    if (!identical(obligation$severity, "blocking")) {
      next
    }

    node_ids <- obligation$metadata$hold_node_ids %||%
      obligation$basis$node_ids %||%
      character()
    for (node_id in node_ids) {
      descendants <- bg_dagri_descendants(graph, node_id)
      if (length(descendants) == 0) {
        next
      }

      reason <- obligation$title %||% obligation$kind
      for (descendant in descendants) {
        holds[[descendant]] <- reason
      }
    }
  }

  bg_protocol_named_list(holds)
}

#' @keywords internal
bg_workflow_external_holds <- function(
  project,
  plan = NULL,
  state = NULL,
  graph = NULL
) {
  if (length(bg_workflow_packs(project)) == 0) {
    return(bg_protocol_named_list())
  }

  bg_next_actions_impl(
    project,
    resolved_scope = "project",
    plan = plan,
    state = state,
    graph = graph
  )$metadata$external_holds %||%
    bg_protocol_named_list()
}

# --- Next Actions Engine ---

#' Compute deterministic next workflow actions
#'
#' Evaluates the active workflow packs against a derived workflow context and
#' returns obligations, suggested actions, and planner-ready external holds.
#' Callers can pass `result$metadata$external_holds` into [bg_plan()] to keep
#' workflow holds distinct from structural blockers.
#'
#' For the built-in `bayesgrove.default_bayesian` pack, the returned
#' obligations and actions can include:
#' - `review_computation_validity` and matching `computation_review` actions
#'   for fresh warning/error summaries,
#' - `review_fit_criticism` plus `fit_criticism` and `branch_and_modify`
#'   actions for branch-scoped fit or diagnostic problems,
#' - `compare_candidate_branches` plus comparison-node creation or
#'   `model_comparison` decision actions when multiple clean fit candidates
#'   exist, and
#' - `accept_or_reject_branch` plus `branch_disposition` actions after a
#'   current comparison exists for an active candidate set.
#'
#' When the optional phase-10 packs are active, the result can also include
#' prior-rationale recording, prior and posterior predictive review,
#' simulation-based calibration review, model-selection review keyed to
#' `model_comparison` and `stacking_weights` summaries, causal-question
#' prompts, and PAD annotation review.
#'
#' @param project A `bg_handle`.
#' @param scope One of `project` or `branch`.
#' @param branch_id Optional branch id, required for branch-scoped queries.
#'
#' @return A `bg_next_actions_result` plain-data list with `context`,
#'   `obligations`, `actions`, and `metadata$external_holds`.
#' @export
bg_next_actions <- function(
  project,
  scope = c("project", "branch"),
  branch_id = NULL
) {
  S7::check_is_S7(project, bg_handle)

  resolved_scope <- bg_resolve_workflow_scope(project, scope, branch_id)
  bg_next_actions_impl(project, resolved_scope, plan = NULL)
}

#' @keywords internal
bg_next_actions_impl <- function(
  project,
  resolved_scope,
  plan = NULL,
  state = NULL,
  graph = NULL
) {
  active_packs <- bg_resolve_pack_includes(bg_workflow_packs(project))
  contexts <- bg_collect_workflow_contexts(
    project,
    resolved_scope,
    plan = plan,
    state = state,
    graph = graph
  )

  obligation_items <- list()
  for (context in contexts) {
    for (pack_ref in active_packs) {
      obligation_items <- c(
        obligation_items,
        bg_dispatch_obligation_providers(context, pack_ref)
      )
    }
  }
  obligations <- bg_merge_protocol_items(obligation_items, "obligation_id")
  obligations <- bg_protocol_named_list(lapply(
    obligations,
    bg_enrich_protocol_obligation,
    project = project
  ))

  action_items <- list()
  for (context in contexts) {
    scoped_obligations <- Filter(
      function(obligation) identical(obligation$scope, context$scope),
      obligations
    )
    for (pack_ref in active_packs) {
      action_items <- c(
        action_items,
        bg_dispatch_action_providers(context, scoped_obligations, pack_ref)
      )
    }
  }
  actions <- bg_merge_protocol_items(action_items, "action_id")
  actions <- bg_protocol_named_list(lapply(
    actions,
    bg_enrich_protocol_action,
    project = project
  ))

  result <- list(
    context = contexts[[1]],
    obligations = bg_protocol_named_list(obligations),
    actions = bg_protocol_named_list(actions),
    metadata = list(
      evaluated_scopes = vapply(contexts, `[[`, character(1), "scope"),
      external_holds = bg_protocol_named_list(
        bg_blocking_obligation_holds(project, obligations, graph = graph)
      )
    )
  )
  # Add a class so a print method can render the obligations as a numbered
  # checklist; the underlying structure stays a plain-data list.
  structure(result, class = c("bg_next_actions_result", "list"))
}

# --- Protocol Result Partitioning ---

#' Partition protocol results by scope
#'
#' Helper for UI layers to group obligations and actions by scope. Returns
#' a nested structure where each scope has its own obligations and actions,
#' making branch-aware rendering straightforward.
#'
#' @param result A `bg_next_actions` result object.
#' @param project Optional `bg_handle` to include display labels.
#'
#' @return A named list where each element corresponds to a scope, containing:
#'   - `scope`: The scope string
#'   - `scope_label`: Human-readable label (if project provided)
#'   - `obligations`: List of obligations for this scope
#'   - `actions`: List of actions for this scope
#' @export
bg_partition_protocol_by_scope <- function(result, project = NULL) {
  obligations <- result$obligations %||% list()
  actions <- result$actions %||% list()
  evaluated_scopes <- result$metadata$evaluated_scopes %||% character()

  obligation_scopes <- vapply(
    obligations,
    function(x) x$scope %||% NA_character_,
    character(1)
  )
  action_scopes <- vapply(
    actions,
    function(x) x$scope %||% NA_character_,
    character(1)
  )

  # Prefer the evaluation order when available, but fall back to the
  # scopes carried by the protocol items so ad hoc/mock results still work.
  scopes <- unique(c(evaluated_scopes, obligation_scopes, action_scopes))
  scopes <- scopes[!is.na(scopes) & nzchar(scopes)]

  partitioned <- list()

  for (scope in scopes) {
    scope_obligations <- Filter(
      function(x) identical(x$scope, scope),
      obligations
    )
    scope_actions <- Filter(
      function(x) identical(x$scope, scope),
      actions
    )

    scope_label <- if (!is.null(project)) {
      bg_scope_label(project, scope)
    } else {
      scope
    }

    scope_descriptor <- if (!is.null(project)) {
      bg_protocol_scope_descriptor(project, scope)
    } else {
      list(
        scope = scope,
        scope_kind = bg_scope_kind(scope),
        scope_label = scope_label
      )
    }

    partitioned[[scope]] <- bg_drop_null_fields(list(
      scope = scope_descriptor$scope,
      scope_kind = scope_descriptor$scope_kind,
      scope_label = scope_descriptor$scope_label,
      branch_context = scope_descriptor$branch_context %||% NULL,
      obligations = bg_protocol_named_list(scope_obligations),
      actions = bg_protocol_named_list(scope_actions)
    ))
  }

  # Add summary counts at the top level for convenience
  partitioned$summary <- list(
    n_scopes = length(scopes),
    n_obligations = length(obligations),
    n_actions = length(actions),
    n_blocking = sum(vapply(
      obligations,
      function(x) identical(x$severity, "blocking"),
      integer(1)
    )),
    scopes = scopes
  )

  partitioned
}

# --- Default Bayesian: Goal Obligations ---

#' @keywords internal
bg_default_bayesian_goal_obligations <- function(
  context,
  pack_config = list()
) {
  if (
    !startsWith(context$scope, "branch:") ||
      !bg_is_active_lifecycle(
        bg_lifecycle_state(context$scope_context$branch_metadata %||% list())
      ) ||
      !is.null(context$inferential_goal) ||
      isTRUE(context$scope_context$branch_metadata$goal_optional)
  ) {
    return(list())
  }

  list(list(
    kind = "set_inferential_goal",
    scope = context$scope,
    severity = "blocking",
    title = "Set an inferential goal",
    basis = list(
      node_ids = context$scope_context$branch_root %||% character(),
      decision_ids = character()
    ),
    explanation = list(
      why = "This branch has no active inferential goal decision.",
      references = bg_workflow_references(c("workflow_core", "taxonomy"))
    ),
    metadata = list(source_keys = c("workflow_core", "taxonomy"))
  ))
}

# --- Default Bayesian: Summary Obligations ---

#' @keywords internal
bg_default_bayesian_summary_obligations <- function(
  context,
  pack_config = list()
) {
  summaries <- Filter(
    function(summary) {
      !isTRUE(summary$is_stale) &&
        summary$severity %in% c("warning", "error")
    },
    context$evidence$summaries %||% list()
  )
  if (length(summaries) == 0) {
    return(list())
  }

  summary_ids <- sort(unique(vapply(
    summaries,
    `[[`,
    character(1),
    "summary_id"
  )))
  reviewed_ids <- bg_default_bayesian_current_decision_summary_ids(
    decisions = context$evidence$decisions %||% list(),
    scope = context$scope,
    kind = "computation_review",
    fresh_summary_ids = summary_ids
  )
  pending <- Filter(
    function(summary) {
      !summary$summary_id %in% reviewed_ids
    },
    summaries
  )
  if (length(pending) == 0) {
    return(list())
  }

  list(list(
    kind = "review_computation_validity",
    scope = context$scope,
    severity = "blocking",
    title = "Review computation validity",
    basis = list(
      summary_ids = vapply(pending, `[[`, character(1), "summary_id"),
      decision_ids = character(),
      node_ids = unique(vapply(pending, `[[`, character(1), "node_id"))
    ),
    explanation = list(
      why = "Fresh warning or error summaries are active in this scope.",
      references = bg_workflow_references(c(
        "workflow_core",
        "stan_diagnostics"
      ))
    ),
    metadata = list(
      source_keys = c("workflow_core", "stan_diagnostics"),
      summary_kinds = unique(vapply(
        pending,
        `[[`,
        character(1),
        "summary_kind"
      ))
    )
  ))
}

# --- Default Bayesian: Goal Actions ---

#' @keywords internal
bg_default_bayesian_goal_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  goal_obligation <- bg_find_obligation(obligations, "set_inferential_goal")
  if (is.null(goal_obligation)) {
    return(list())
  }

  list(list(
    kind = "record_decision",
    scope = context$scope,
    title = "Record an inferential goal",
    basis = list(
      obligation_refs = list(list(
        kind = "set_inferential_goal",
        scope = context$scope
      )),
      node_ids = context$scope_context$branch_root %||% character()
    ),
    payload = list(
      decision_type = "goal_update",
      allowed_goal_kinds = pack_config$goal_kinds %||%
        c(
          "observable_prediction",
          "latent_inference"
        )
    ),
    explanation = list(
      why_now = "Branch-scoped workflow guidance stays blocked until a goal is set.",
      references = bg_workflow_references(c("workflow_core", "taxonomy"))
    ),
    metadata = list(source_keys = c("workflow_core", "taxonomy"))
  ))
}

# --- Default Bayesian: Summary Actions ---

#' @keywords internal
bg_default_bayesian_summary_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  obligation <- bg_find_obligation(obligations, "review_computation_validity")
  if (is.null(obligation)) {
    return(list())
  }
  node_ids <- obligation$basis$node_ids %||% character()
  summary_ids <- obligation$basis$summary_ids %||% character()

  actions <- list(list(
    kind = "record_decision",
    scope = context$scope,
    title = "Record a computation review",
    basis = list(
      obligation_refs = list(list(
        kind = "review_computation_validity",
        scope = context$scope
      )),
      node_ids = node_ids
    ),
    payload = list(
      template_ref = "review_decision",
      decision_type = "computation_review",
      summary_ids = summary_ids,
      node_ids = node_ids
    ),
    explanation = list(
      why_now = "A blocking computation-validity obligation is active.",
      references = bg_workflow_references(c(
        "workflow_core",
        "stan_diagnostics"
      ))
    ),
    metadata = list(source_keys = c("workflow_core", "stan_diagnostics"))
  ))

  if (length(node_ids) > 0) {
    source_node_id <- node_ids[[1]]
    source_node <- context$structural$nodes[[source_node_id]] %||% NULL

    modification_hint <- NULL
    parameter_suggestions <- list()
    continuation_kinds <- c("check", "ppc")

    if (!is.null(source_node)) {
      summary_kinds <- obligation$metadata$summary_kinds %||% character()
      if ("hmc_diagnostics" %in% summary_kinds) {
        modification_hint <- "reparametrize"
      } else if ("optimizer_diagnostics" %in% summary_kinds) {
        modification_hint <- "adjust_tolerances"
      }

      # Compute parameter suggestions using the helper
      parameter_suggestions <- bg_parameter_suggestions_from_hint(
        hint = modification_hint,
        current_params = source_node$params %||% list()
      )
    }

    actions <- c(
      actions,
      list(list(
        kind = "branch_and_modify",
        scope = context$scope,
        title = "Branch and modify to resolve diagnostics",
        basis = list(
          obligation_refs = list(list(
            kind = "review_computation_validity",
            scope = context$scope
          )),
          node_ids = node_ids
        ),
        payload = list(
          template_ref = "branch_and_modify_fit",
          source_node_id = source_node_id,
          modification_hint = modification_hint,
          default_label = if (!is.null(source_node)) {
            bg_revised_label(source_node)
          } else {
            NULL
          },
          parameter_suggestions = parameter_suggestions,
          continuation_kinds = continuation_kinds,
          auto_run = TRUE
        ),
        explanation = list(
          why_now = "A new branch preserves provenance while you iterate on diagnostics.",
          references = bg_workflow_references(c(
            "workflow_core",
            "stan_diagnostics"
          ))
        ),
        metadata = list(source_keys = c("workflow_core", "stan_diagnostics"))
      ))
    )
  }

  actions
}
