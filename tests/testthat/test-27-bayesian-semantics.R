describe("Phase 10 Bayesian semantics packs", {
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

  make_source_fit_project <- function(
    workflow_packs,
    cleanup_env = parent.frame()
  ) {
    tmp <- tempfile("phase10-bayesian-semantics-")
    dir.create(tmp, recursive = TRUE)
    withr::defer(unlink(tmp, recursive = TRUE), envir = cleanup_env)

    handle <- bg_init(path = tmp, workflow_packs = workflow_packs)

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      variant <- node$params$variant %||% "baseline"

      list(
        result = list(ok = TRUE, variant = variant),
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          passed = TRUE,
          severity = "ok",
          metrics = list(variant = variant)
        ))
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline fit",
      inputs = source_id,
      params = list(variant = "baseline")
    )

    bg_run(handle, targets = fit_id, mode = "sync")

    list(handle = handle, source_id = source_id, fit_id = fit_id)
  }

  make_branch_fit_project <- function(
    workflow_packs,
    goal_kind = NULL,
    cleanup_env = parent.frame()
  ) {
    fixture <- make_source_fit_project(
      workflow_packs,
      cleanup_env = cleanup_env
    )
    handle <- fixture$handle

    branch <- bg_branch(handle, fixture$fit_id, label = "Alternative fit")
    bg_update_node(
      handle,
      branch$root_node_id,
      params = list(variant = "alternative")
    )

    if (!is.null(goal_kind)) {
      bg_set_goal(
        project = handle,
        branch_id = branch$branch_id,
        kind = goal_kind,
        label = goal_kind,
        rationale = paste("Goal:", goal_kind)
      )
    }

    bg_run(handle, targets = branch$root_node_id, mode = "sync")

    list(
      handle = handle,
      source_id = fixture$source_id,
      fit_id = fixture$fit_id,
      branch = branch
    )
  }

  it("registers the phase 10 built-in packs", {
    pack_ids <- c(
      "bayesgrove.prior_workflow",
      "bayesgrove.model_checks",
      "bayesgrove.model_selection",
      "bayesgrove.causal_minimal",
      "bayesgrove.pad_scaffold"
    )

    registry <- bg_builtin_workflow_registry()
    expect_true(all(pack_ids %in% names(registry)))
    expect_true(all(vapply(
      pack_ids,
      function(pack_id) !is.null(bg_lookup_workflow_pack(pack_id)),
      logical(1)
    )))
  })

  it("tracks prior workflow obligations and clears them after rationale and review", {
    fixture <- make_source_fit_project("bayesgrove.prior_workflow")
    handle <- fixture$handle

    bg_register_node_kind(
      handle,
      "prior_check",
      executor = function(node, inputs) {
        list(
          result = list(ok = TRUE),
          summaries = list(list(
            summary_kind = "prior_predictive_check",
            passed = TRUE,
            severity = "ok",
            metrics = list(max_abs = 1)
          ))
        )
      }
    )

    initial <- bg_next_actions(handle, scope = "project")
    expect_true(all(
      c(
        "record_prior_rationale",
        "run_prior_predictive_check"
      ) %in%
        vapply(initial$obligations, `[[`, character(1), "kind")
    ))

    rationale_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "prior_rationale")
      },
      initial$actions
    )[[1]]
    record_action_decision(
      handle,
      rationale_action,
      choice = "priors_documented",
      rationale = "The prior reflects domain constraints and weak regularization."
    )

    prior_check_id <- bg_add_node(
      handle,
      kind = "prior_check",
      label = "Prior predictive",
      inputs = fixture$fit_id
    )
    bg_run(handle, targets = prior_check_id, mode = "sync")

    review <- bg_next_actions(handle, scope = "project")
    expect_true(any(vapply(
      review$obligations,
      function(obligation) {
        identical(obligation$kind, "review_prior_predictive_check")
      },
      logical(1)
    )))

    review_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "prior_check_review")
      },
      review$actions
    )[[1]]
    record_action_decision(
      handle,
      review_action,
      choice = "prior_checks_pass",
      rationale = "The prior predictive summaries look plausible for the intended observables."
    )

    cleared <- bg_next_actions(handle, scope = "project")
    expect_length(cleared$obligations, 0)
    expect_length(cleared$actions, 0)
  })

  it("tracks posterior predictive checks and review decisions", {
    fixture <- make_source_fit_project("bayesgrove.model_checks")
    handle <- fixture$handle

    bg_register_node_kind(handle, "ppc", executor = function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(list(
          summary_kind = "posterior_predictive_check",
          passed = TRUE,
          severity = "ok",
          metrics = list(elpd_gap = 0.05)
        ))
      )
    })

    initial <- bg_next_actions(handle, scope = "project")
    expect_true(any(vapply(
      initial$obligations,
      function(obligation) {
        identical(obligation$kind, "run_posterior_predictive_check")
      },
      logical(1)
    )))

    ppc_id <- bg_add_node(
      handle,
      kind = "ppc",
      label = "Posterior predictive",
      inputs = fixture$fit_id
    )
    bg_run(handle, targets = ppc_id, mode = "sync")

    review <- bg_next_actions(handle, scope = "project")
    review_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "posterior_check_review")
      },
      review$actions
    )[[1]]

    record_action_decision(
      handle,
      review_action,
      choice = "posterior_checks_pass",
      rationale = "The posterior predictive behavior is adequate for the current goal."
    )

    cleared <- bg_next_actions(handle, scope = "project")
    expect_false(any(vapply(
      cleared$obligations,
      function(obligation) {
        identical(obligation$kind, "run_posterior_predictive_check") ||
          identical(obligation$kind, "review_posterior_predictive_check")
      },
      logical(1)
    )))
  })

  it("tracks SBC obligations and review decisions for latent-inference branches", {
    fixture <- make_branch_fit_project(
      workflow_packs = "bayesgrove.model_checks",
      goal_kind = "latent_inference"
    )
    handle <- fixture$handle

    bg_register_node_kind(handle, "sbc", executor = function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(list(
          summary_kind = "sbc_result",
          passed = TRUE,
          severity = "ok",
          metrics = list(uniformity_p = 0.6)
        ))
      )
    })

    initial <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    expect_true(any(vapply(
      initial$obligations,
      function(obligation) identical(obligation$kind, "run_sbc"),
      logical(1)
    )))

    sbc_id <- bg_add_node(
      handle,
      kind = "sbc",
      label = "SBC",
      inputs = fixture$branch$root_node_id
    )
    bg_run(handle, targets = sbc_id, mode = "sync")

    review <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    review_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "sbc_review")
      },
      review$actions
    )[[1]]

    record_action_decision(
      handle,
      review_action,
      choice = "sbc_acceptable",
      rationale = "The SBC summaries do not indicate calibration problems."
    )

    cleared <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    expect_false(any(vapply(
      cleared$obligations,
      function(obligation) {
        identical(obligation$kind, "run_sbc") ||
          identical(obligation$kind, "review_sbc")
      },
      logical(1)
    )))
  })

  it("uses model comparison and stacking summaries for explicit review decisions", {
    fixture <- make_branch_fit_project("bayesgrove.model_selection")
    handle <- fixture$handle

    bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
      list(
        result = list(recommended = names(inputs)[[1]]),
        summaries = list(
          list(
            summary_kind = "model_comparison",
            passed = TRUE,
            severity = "ok",
            metrics = list(metric = "elpd", winner = names(inputs)[[1]])
          ),
          list(
            summary_kind = "stacking_weights",
            passed = TRUE,
            severity = "ok",
            metrics = list(weights = c(0.7, 0.3))
          )
        )
      )
    })

    initial <- bg_next_actions(handle, scope = "project")
    expect_true(any(vapply(
      initial$obligations,
      function(obligation) identical(obligation$kind, "review_model_selection"),
      logical(1)
    )))
    expect_true(any(vapply(
      initial$actions,
      function(action) identical(action$kind, "create_node_from_template"),
      logical(1)
    )))

    compare_id <- bg_add_node(
      handle,
      kind = "compare",
      label = "Compare fits",
      inputs = c(fixture$fit_id, fixture$branch$root_node_id)
    )
    bg_run(handle, targets = compare_id, mode = "sync")

    review <- bg_next_actions(handle, scope = "project")
    comparison_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "model_comparison")
      },
      review$actions
    )[[1]]

    expect_true(isTRUE(comparison_action$payload$stacking_available))

    record_action_decision(
      handle,
      comparison_action,
      choice = "prefer_baseline",
      rationale = "The comparison summaries and stacking weights favor the baseline branch."
    )

    cleared <- bg_next_actions(handle, scope = "project")
    expect_false(any(vapply(
      cleared$obligations,
      function(obligation) identical(obligation$kind, "review_model_selection"),
      logical(1)
    )))
  })

  it("surfaces honest scaffold actions for causal and PAD packs", {
    fixture <- make_branch_fit_project(
      workflow_packs = c(
        "bayesgrove.causal_minimal",
        "bayesgrove.pad_scaffold"
      ),
      goal_kind = "latent_inference"
    )
    handle <- fixture$handle

    initial <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    obligation_kinds <- vapply(initial$obligations, `[[`, character(1), "kind")
    expect_true("frame_causal_question" %in% obligation_kinds)
    expect_true("review_pad_annotation" %in% obligation_kinds)

    causal_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "causal_question")
      },
      initial$actions
    )[[1]]
    pad_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "pad_annotation_review")
      },
      initial$actions
    )[[1]]

    record_action_decision(
      handle,
      causal_action,
      choice = "estimand: average treatment effect",
      rationale = "This branch should only support the stated estimand after DAG review."
    )
    record_action_decision(
      handle,
      pad_action,
      choice = "PAD: PAD model with latent-inference utilities",
      rationale = "The branch is a PAD workflow centered on latent inference and robustness."
    )

    cleared <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    expect_false(any(vapply(
      cleared$obligations,
      function(obligation) {
        obligation$kind %in%
          c(
            "frame_causal_question",
            "review_pad_annotation"
          )
      },
      logical(1)
    )))
  })
})
