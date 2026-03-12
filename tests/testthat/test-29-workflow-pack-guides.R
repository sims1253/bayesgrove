describe("Workflow guidance packs", {
  record_action_decision <- function(
    handle,
    action,
    choice,
    rationale,
    scope = action$scope
  ) {
    payload <- action$payload %||% list()

    bg_record_decision(
      project = handle,
      scope = scope,
      prompt = action$title %||% (payload$decision_type %||% "decision"),
      choice = choice,
      rationale = rationale,
      kind = payload$decision_type %||% "note",
      metadata = payload[setdiff(names(payload), "decision_type")]
    )
  }

  make_fit_project <- function(
    workflow_packs,
    severity = "ok",
    cleanup_env = parent.frame()
  ) {
    tmp <- tempfile("workflow-pack-guides-")
    dir.create(tmp, recursive = TRUE)
    withr::defer(unlink(tmp, recursive = TRUE), envir = cleanup_env)

    handle <- bg_init(path = tmp, workflow_packs = workflow_packs)

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          passed = identical(severity, "ok"),
          severity = severity,
          metrics = list(divergent_transitions = if (identical(severity, "ok")) 0 else 4)
        ))
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline fit",
      inputs = source_id
    )

    bg_run(handle, targets = fit_id, mode = "sync")

    list(handle = handle, source_id = source_id, fit_id = fit_id)
  }

  make_branch_fit_project <- function(
    workflow_packs,
    goal_kind = "latent_inference",
    cleanup_env = parent.frame()
  ) {
    fixture <- make_fit_project(
      workflow_packs,
      cleanup_env = cleanup_env
    )
    handle <- fixture$handle

    branch <- bg_branch(handle, fixture$fit_id, label = "Alternative fit")
    bg_set_goal(
      project = handle,
      branch_id = branch$branch_id,
      kind = goal_kind,
      label = goal_kind,
      rationale = paste("Goal:", goal_kind)
    )

    bg_run(handle, targets = branch$root_node_id, mode = "sync")

    list(
      handle = handle,
      source_id = fixture$source_id,
      fit_id = fixture$fit_id,
      branch = branch
    )
  }

  it("registers the process guidance pack", {
    pack_ids <- c("bayesgrove.process_guidance")

    registry <- bg_builtin_workflow_registry()
    expect_true(all(pack_ids %in% names(registry)))
    expect_true(all(vapply(
      pack_ids,
      function(pack_id) !is.null(bg_lookup_workflow_pack(pack_id)),
      logical(1)
    )))
  })

  it("tracks preflight and generalization guidance decisions", {
    fixture <- make_fit_project("bayesgrove.process_guidance")
    handle <- fixture$handle

    initial <- bg_next_actions(handle, scope = "project")
    obligation_kinds <- vapply(initial$obligations, `[[`, character(1), "kind")
    expect_true("review_workflow_preflight" %in% obligation_kinds)

    preflight_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "workflow_preflight")
      },
      initial$actions
    )[[1]]

    record_action_decision(
      handle,
      preflight_action,
      choice = "preflight_recorded",
      rationale = paste(
        "The project starts with a regularized model, simulated-data rehearsal,",
        "and explicit acceptance criteria."
      )
    )

    bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(list(
          summary_kind = "pareto_k_diagnostics",
          passed = FALSE,
          severity = "warning",
          metrics = list(k_over_threshold = 2)
        ))
      )
    })

    compare_id <- bg_add_node(
      handle,
      kind = "compare",
      label = "LOO review",
      inputs = fixture$fit_id
    )
    bg_run(handle, targets = compare_id, mode = "sync")

    review <- bg_next_actions(handle, scope = "project")
    expect_true(any(vapply(
      review$obligations,
      function(obligation) {
        identical(obligation$kind, "review_out_of_sample_stability")
      },
      logical(1)
    )))

    review_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "workflow_generalization_review")
      },
      review$actions
    )[[1]]

    record_action_decision(
      handle,
      review_action,
      choice = "pareto_follow_up_recorded",
      rationale = paste(
        "The workflow will inspect the influential observations and rerun",
        "comparison after targeted refits."
      )
    )

    cleared <- bg_next_actions(handle, scope = "project")
    expect_false(any(vapply(
      cleared$obligations,
      function(obligation) {
        obligation$kind %in% c(
          "review_workflow_preflight",
          "review_out_of_sample_stability"
        )
      },
      logical(1)
    )))
  })

  it("tracks model taxonomy and utility trade-off decisions", {
    fixture <- make_branch_fit_project("bayesgrove.model_taxonomy")
    handle <- fixture$handle

    initial <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    obligation_kinds <- vapply(initial$obligations, `[[`, character(1), "kind")
    expect_true("classify_model_taxonomy" %in% obligation_kinds)
    expect_true("review_utility_tradeoffs" %in% obligation_kinds)

    taxonomy_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "model_taxonomy")
      },
      initial$actions
    )[[1]]
    utility_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "utility_tradeoff_review")
      },
      initial$actions
    )[[1]]

    expect_true(all(c(
      "causal_consistency",
      "parameter_recoverability"
    ) %in% utility_action$payload$suggested_primary_utilities))

    record_action_decision(
      handle,
      taxonomy_action,
      choice = "PAD model with posterior approximation",
      rationale = paste(
        "The branch is best described as a PAD workflow with training data and",
        "posterior approximation kept explicit."
      )
    )
    record_action_decision(
      handle,
      utility_action,
      choice = "latent inference utilities recorded",
      rationale = paste(
        "The branch prioritizes causal consistency, recoverability, and robust",
        "posterior computation over raw speed."
      )
    )

    cleared <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    expect_false(any(vapply(
      cleared$obligations,
      function(obligation) {
        obligation$kind %in% c(
          "classify_model_taxonomy",
          "review_utility_tradeoffs"
        )
      },
      logical(1)
    )))
  })

  it("tracks Stan-specific diagnostic review prompts", {
    fixture <- make_fit_project(
      workflow_packs = "bayesgrove.stan_workflow",
      severity = "warning"
    )
    handle <- fixture$handle

    initial <- bg_next_actions(handle, scope = "project")
    expect_true(any(vapply(
      initial$obligations,
      function(obligation) {
        identical(obligation$kind, "review_stan_diagnostics")
      },
      logical(1)
    )))

    stan_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "stan_diagnostic_review")
      },
      initial$actions
    )[[1]]

    expect_true(all(c(
      "raise_adapt_delta",
      "reparameterize"
    ) %in% stan_action$payload$suggested_repairs))

    record_action_decision(
      handle,
      stan_action,
      choice = "sampler repair plan recorded",
      rationale = paste(
        "The workflow will reparameterize the model and rerun with a higher",
        "adapt_delta before trusting downstream comparisons."
      )
    )

    after_review <- bg_next_actions(handle, scope = "project")
    expect_false(any(vapply(
      after_review$obligations,
      function(obligation) {
        identical(obligation$kind, "review_stan_diagnostics")
      },
      logical(1)
    )))
  })

  it("tracks projection-predictive review prompts", {
    fixture <- make_fit_project("bayesgrove.stan_workflow")
    handle <- fixture$handle

    bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(list(
          summary_kind = "projpred_selection",
          passed = TRUE,
          severity = "ok",
          metrics = list(selected_terms = c("x1", "x2"))
        ))
      )
    })

    compare_id <- bg_add_node(
      handle,
      kind = "compare",
      label = "Projection predictive",
      inputs = fixture$fit_id
    )
    bg_run(handle, targets = compare_id, mode = "sync")

    review <- bg_next_actions(handle, scope = "project")
    expect_true(any(vapply(
      review$obligations,
      function(obligation) {
        identical(obligation$kind, "review_projection_predictive_selection")
      },
      logical(1)
    )))

    projection_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "projection_selection_review")
      },
      review$actions
    )[[1]]

    record_action_decision(
      handle,
      projection_action,
      choice = "projection submodel accepted",
      rationale = paste(
        "The selected submodel will be refit and compared against the reference",
        "model before branch disposition."
      )
    )

    cleared <- bg_next_actions(handle, scope = "project")
    expect_false(any(vapply(
      cleared$obligations,
      function(obligation) {
        identical(obligation$kind, "review_projection_predictive_selection")
      },
      logical(1)
    )))
  })
})
