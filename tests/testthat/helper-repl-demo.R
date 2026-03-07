test_bg_update_meta <- getFromNamespace(
  "bg_update_branch_metadata",
  "bayesgrove"
)

test_demo_repl_register_kinds <- function(handle) {
  bg_register_node_kind(handle, "data_prep", executor = function(node, inputs) {
    set.seed(123)
    data.frame(group = rep(1:5, each = 10), y = rnorm(50))
  })

  bg_register_node_kind(handle, "compile", executor = function(node, inputs) {
    list(model = "compiled_binary")
  })

  bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
    param_val <- node$params$parametrization %||% "centered"
    variant <- node$params$variant %||% "baseline"

    if (identical(param_val, "non-centered")) {
      return(list(
        status = "fit_completed",
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            severity = "ok",
            passed = TRUE,
            metrics = list(
              divergences = 0,
              rhat_max = 1.01,
              variant = variant
            )
          )
        )
      ))
    }

    list(
      status = "fit_completed",
      summaries = list(
        list(
          summary_kind = "hmc_diagnostics",
          severity = "warning",
          passed = FALSE,
          metrics = list(
            divergences = 15,
            rhat_max = 1.02,
            variant = variant
          )
        )
      )
    )
  })

  bg_register_node_kind(handle, "ppc", executor = function(node, inputs) {
    list(plot = "ppc_density_plot")
  })

  bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
    list(
      comparison = "loo_compare",
      models = names(inputs),
      summaries = list(list(
        summary_kind = "comparison_results",
        severity = "ok",
        passed = TRUE
      ))
    )
  })
}

test_demo_repl_fixture <- function(
  checkpoint = c(
    "warning_branch",
    "comparison_ready",
    "disposition_ready",
    "healthy"
  )
) {
  checkpoint <- match.arg(checkpoint)

  tmp <- tempfile("repl-demo-fixture-")
  dir.create(tmp, recursive = TRUE)
  handle <- bg_init(
    path = tmp,
    project_name = "Hierarchical Analysis",
    workflow_packs = list("bayesguide.default_bayesian")
  )

  test_demo_repl_register_kinds(handle)

  n_data <- bg_add_node(handle, "data_prep", label = "Load Survey Data")
  n_compile <- bg_add_node(handle, "compile", label = "Compile Model")
  n_fit <- bg_add_node(
    handle,
    "fit",
    label = "Baseline Fit",
    inputs = c(n_data, n_compile),
    params = list(
      parametrization = "non-centered",
      variant = "baseline"
    )
  )
  n_ppc <- bg_add_node(
    handle,
    "ppc",
    label = "Posterior Predictive Check",
    inputs = n_fit
  )

  bg_run(handle, targets = n_fit, mode = "sync")

  warning_branch <- bg_branch_with_continuation(
    project = handle,
    node_id = n_fit,
    label = "Fit Centered Parametrization"
  )
  bg_update_node(
    handle,
    warning_branch$branch$root_node_id,
    label = "Fit Centered Parametrization",
    params = list(
      parametrization = "centered",
      variant = "centered_branch"
    )
  )
  if (length(warning_branch$continuation_nodes) > 0) {
    for (entry in warning_branch$continuation_nodes) {
      bg_update_node(
        handle,
        entry$clone_id,
        label = "Posterior Predictive Check (branch)"
      )
    }
  }
  bg_set_goal(
    project = handle,
    branch_id = warning_branch$branch$branch_id,
    kind = "observable_prediction",
    label = "Compare parametrizations",
    rationale = "Compare the branch against the clean baseline fit."
  )
  bg_run(handle, targets = warning_branch$branch$root_node_id, mode = "sync")

  revised_branch <- NULL
  compare_node_id <- NULL
  initial_scope <- "project"

  if (checkpoint %in% c("comparison_ready", "disposition_ready", "healthy")) {
    revised_branch <- bg_branch_with_continuation(
      project = handle,
      node_id = warning_branch$branch$root_node_id,
      label = "Fit Non-Centered Revision",
      continuation_kinds = c("ppc")
    )
    bg_update_node(
      handle,
      revised_branch$branch$root_node_id,
      label = "Fit Non-Centered Revision",
      params = list(
        parametrization = "non-centered",
        variant = "revision_branch"
      )
    )
    if (length(revised_branch$continuation_nodes) > 0) {
      for (entry in revised_branch$continuation_nodes) {
        bg_update_node(
          handle,
          entry$clone_id,
          label = "Posterior Predictive Check (revision)"
        )
      }
    }
    test_bg_update_meta(
      handle,
      revised_branch$branch$branch_id,
      list(
        goal_optional = TRUE,
        branch_purpose = "technical_revision",
        template_ref = "branch_and_modify_fit"
      )
    )
    bg_run(handle, targets = revised_branch$branch$root_node_id, mode = "sync")
    bg_retire_node(
      handle,
      warning_branch$branch$root_node_id,
      recursive = TRUE
    )
  }

  if (checkpoint %in% c("disposition_ready", "healthy")) {
    comparison_action <- Filter(
      function(action) {
        identical(action$kind, "create_node_from_template") &&
          identical(action$payload$template_ref %||% NULL, "branch_comparison")
      },
      bg_next_actions(handle, scope = "project")$actions
    )[[1]]

    compare_node_id <- bg_add_node(
      handle,
      kind = "compare",
      label = "Compare Baseline vs Revision",
      inputs = comparison_action$payload$inputs
    )
    bg_run(handle, targets = compare_node_id, mode = "sync")

    comparison_decision <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type %||% NULL, "model_comparison")
      },
      bg_next_actions(handle, scope = "project")$actions
    )[[1]]
    bg_record_decision(
      handle,
      scope = "project",
      prompt = "Compare candidate branches",
      choice = "prefer_revised_branch",
      rationale = "The revised branch resolves the diagnostic warning cleanly.",
      kind = "model_comparison",
      metadata = comparison_decision$payload[c(
        "fit_node_ids",
        "summary_ids",
        "candidate_signature",
        "comparison_signature",
        "comparison_context"
      )]
    )

    initial_scope <- revised_branch$branch$branch_id
  }

  if (identical(checkpoint, "healthy")) {
    disposition_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(
            action$payload$decision_type %||% NULL,
            "branch_disposition"
          )
      },
      bg_next_actions(
        handle,
        scope = "branch",
        branch_id = revised_branch$branch$branch_id
      )$actions
    )[[1]]
    bg_record_decision(
      handle,
      scope = revised_branch$branch$branch_id,
      prompt = "Accept or reject branch",
      choice = "accept",
      rationale = "Promote the revised branch as the accepted analysis path.",
      kind = "branch_disposition",
      metadata = list(
        disposition = "accept",
        summary_ids = disposition_action$payload$summary_ids,
        comparison_signature = disposition_action$payload$comparison_signature,
        comparison_context = disposition_action$payload$comparison_context
      )
    )
  }

  list(
    handle = handle,
    checkpoint = checkpoint,
    initial_scope = initial_scope,
    n_data = n_data,
    n_compile = n_compile,
    n_fit = n_fit,
    n_ppc = n_ppc,
    warning_branch = warning_branch$branch,
    revised_branch = revised_branch$branch %||% NULL,
    comparison_node_id = compare_node_id
  )
}
