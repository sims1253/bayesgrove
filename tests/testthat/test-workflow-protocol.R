describe("Workflow protocol APIs", {
  it("lists built-in templates and supports lookup by template_ref", {
    templates <- bg_list_templates()

    expect_equal(
      sort(names(templates)),
      sort(c(
        "diagnostic_check",
        "branch_comparison",
        "branch_and_modify_fit",
        "review_decision"
      ))
    )
    expect_equal(templates$branch_comparison$template_ref, "branch_comparison")
    expect_equal(templates$branch_comparison$operation_type, "create_node")
    expect_equal(
      bg_list_templates("branch_and_modify_fit")$template_ref,
      "branch_and_modify_fit"
    )
    expect_null(bg_list_templates("missing_template"))
  })

  it("returns empty protocol results when no workflow packs are active", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp, workflow_packs = list())

    expect_equal(bg_workflow_packs(handle), list())

    result <- bg_next_actions(handle)
    expect_equal(names(result$obligations), character())
    expect_equal(names(result$actions), character())
    expect_equal(names(result$metadata$external_holds), character())
    expect_equal(result$context$active_packs, list())
  })

  it("emits a blocking obligation from fresh warning summaries", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(
      handle,
      "fit",
      executor = function(node, inputs) {
        list(
          result = list(ok = TRUE),
          summaries = list(
            list(
              summary_kind = "optimizer_diagnostics",
              passed = FALSE,
              severity = "warning",
              metrics = list(max_gradient = 0.1)
            )
          )
        )
      }
    )

    node_id <- bg_add_node(handle, kind = "fit", label = "Fit")
    bg_run(handle)

    result <- bg_next_actions(handle)

    expect_length(result$obligations, 1)
    obligation <- result$obligations[[1]]
    expect_equal(obligation$kind, "review_computation_validity")
    expect_equal(obligation$severity, "blocking")
    expect_equal(obligation$scope, "project")
    expect_equal(obligation$scope_label, "Project")
    expect_equal(obligation$basis$node_ids, node_id)
    expect_length(obligation$basis$summary_ids, 1)
  })

  it("emits a goal-setting action for a branch without an inferential goal", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "fit")
    seed_id <- bg_add_node(handle, kind = "fit", label = "Seed")
    branch <- bg_branch(handle, seed_id, label = "Alternative")
    branch_id <- branch$branch_id

    result <- bg_next_actions(handle, scope = "branch", branch_id = branch_id)

    expect_length(result$obligations, 1)
    expect_equal(result$obligations[[1]]$kind, "set_inferential_goal")
    expect_length(result$actions, 1)
    expect_equal(result$actions[[1]]$kind, "record_decision")
    expect_equal(result$actions[[1]]$payload$decision_type, "goal_update")
    expect_equal(result$actions[[1]]$scope_label, "Alternative")
    expect_equal(result$actions[[1]]$scope_kind, "branch")
    expect_equal(result$actions[[1]]$invocation$command, "bg_execute_action")
    expect_equal(
      result$actions[[1]]$invocation$prompt,
      "Describe the inferential goal for this branch:"
    )
    expect_equal(
      result$actions[[1]]$operator_context$primary_branch_id,
      branch_id
    )
  })

  it("executes a goal-setting action through bg_execute_action()", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "fit")
    seed_id <- bg_add_node(handle, kind = "fit", label = "Seed")
    branch <- bg_branch(handle, seed_id, label = "Alternative")

    result <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = branch$branch_id
    )
    action <- result$actions[[1]]

    exec <- bg_execute_action(
      handle,
      action$action_id,
      overrides = list(
        choice = "observable_prediction",
        choice_label = "Predict y",
        rationale = "This branch should target predictive performance."
      )
    )

    expect_equal(exec$action_id, action$action_id)
    expect_equal(
      bg_get_goal(handle, branch$branch_id)$kind,
      "observable_prediction"
    )
  })

  it("deduplicates identical obligations and actions at runtime", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list(
        "bayesgrove.default_bayesian",
        "bayesgrove.default_bayesian"
      )
    )

    bg_register_node_kind(handle, "fit")
    seed_id <- bg_add_node(handle, kind = "fit", label = "Seed")
    branch <- bg_branch(handle, seed_id, label = "Alternative")
    branch_id <- branch$branch_id

    result <- bg_next_actions(handle, scope = "branch", branch_id = branch_id)

    expect_length(result$obligations, 1)
    expect_length(result$actions, 1)
    expect_equal(
      result$obligations[[1]]$metadata$pack_id,
      "bayesgrove.default_bayesian"
    )
    expect_equal(
      result$obligations[[1]]$metadata$pack_ids,
      "bayesgrove.default_bayesian"
    )
  })

  it("aggregates branch protocol results into project-scope queries", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(
      handle,
      "fit",
      executor = function(node, inputs) {
        list(
          result = list(ok = TRUE, node_id = node$id),
          summaries = list(
            list(
              summary_kind = "optimizer_diagnostics",
              passed = FALSE,
              severity = "warning"
            )
          )
        )
      }
    )

    baseline_id <- bg_add_node(handle, kind = "fit", label = "Baseline")
    branch <- bg_branch(handle, baseline_id, label = "Alternative")
    branch_id <- branch$branch_id

    bg_run(handle, targets = branch$root_node_id)

    branch_result <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = branch_id
    )
    project_result <- bg_next_actions(handle, scope = "project")

    expect_equal(branch_result$context$scope, branch_id)
    expect_true(all(vapply(
      branch_result$obligations,
      function(x) identical(x$scope, branch_id),
      logical(1)
    )))

    expect_equal(project_result$context$scope, "project")
    expect_true(branch_id %in% project_result$metadata$evaluated_scopes)
    expect_true(any(vapply(
      project_result$obligations,
      function(x) identical(x$scope, branch_id),
      logical(1)
    )))
  })

  it("emits branch_and_modify action alongside record_decision for computation review", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(
      handle,
      "fit",
      executor = function(node, inputs) {
        list(
          result = list(ok = TRUE),
          summaries = list(
            list(
              summary_kind = "hmc_diagnostics",
              passed = FALSE,
              severity = "warning",
              metrics = list(divergences = 15)
            )
          )
        )
      }
    )

    node_id <- bg_add_node(handle, kind = "fit", label = "Fit Centered")
    bg_run(handle)

    result <- bg_next_actions(handle)

    expect_gte(length(result$obligations), 1)
    expect_true(all(
      vapply(
        result$obligations,
        `[[`,
        character(1),
        "kind"
      ) ==
        "review_computation_validity"
    ))

    expect_length(result$actions, 2)

    record_action <- Filter(
      function(a) identical(a$kind, "record_decision"),
      result$actions
    )[[1]]
    expect_equal(record_action$payload$template_ref, "review_decision")
    expect_equal(record_action$payload$decision_type, "computation_review")
    expect_equal(record_action$template$template_ref, "review_decision")
    expect_equal(record_action$invocation$command, "bg_execute_action")
    expect_equal(
      record_action$invocation$prompt,
      "Is this computation acceptable for downstream use?"
    )
    expect_equal(record_action$operator_context$primary_node_id, node_id)

    branch_action <- Filter(
      function(a) identical(a$kind, "branch_and_modify"),
      result$actions
    )[[1]]
    expect_equal(branch_action$payload$template_ref, "branch_and_modify_fit")
    expect_equal(branch_action$payload$source_node_id, node_id)
    expect_equal(branch_action$payload$modification_hint, "reparametrize")
    expect_match(branch_action$payload$default_label, "revised")
    expect_match(branch_action$title, "Branch and modify")
    expect_equal(branch_action$operator_context$primary_node_id, node_id)
  })

  it("adds branch context to partitioned protocol output", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "fit")
    seed_id <- bg_add_node(handle, kind = "fit", label = "Seed")
    branch <- bg_branch(handle, seed_id, label = "Alternative")

    partitioned <- bg_partition_protocol_by_scope(
      bg_next_actions(handle),
      handle
    )

    expect_equal(partitioned[[branch$branch_id]]$scope_kind, "branch")
    expect_equal(
      partitioned[[branch$branch_id]]$branch_context$branch_id,
      branch$branch_id
    )
    expect_equal(
      partitioned[[branch$branch_id]]$branch_context$label,
      "Alternative"
    )
  })

  it("includes parameter_suggestions and continuation_kinds in branch_and_modify payload", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(
      handle,
      "fit",
      executor = function(node, inputs) {
        list(
          result = list(ok = TRUE),
          summaries = list(
            list(
              summary_kind = "hmc_diagnostics",
              passed = FALSE,
              severity = "warning",
              metrics = list(divergences = 15)
            )
          )
        )
      }
    )

    node_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit Centered",
      params = list(parametrization = "centered")
    )
    bg_run(handle)

    result <- bg_next_actions(handle)

    branch_action <- Filter(
      function(a) identical(a$kind, "branch_and_modify"),
      result$actions
    )[[1]]

    # Check enhanced payload fields
    expect_true(is.list(branch_action$payload$parameter_suggestions))
    expect_equal(
      branch_action$payload$parameter_suggestions$parametrization,
      "non-centered"
    )
    expect_true(is.character(branch_action$payload$continuation_kinds))
    expect_true("check" %in% branch_action$payload$continuation_kinds)
    expect_true("ppc" %in% branch_action$payload$continuation_kinds)
    expect_true(isTRUE(branch_action$payload$auto_run))
  })

  it("emits comparison action when multiple clean fits exist", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      param_val <- node$params$parametrization %||% "centered"
      repaired <- identical(node$params$revision %||% 1L, 2L)
      severity <- if (identical(param_val, "non-centered") || repaired) {
        "ok"
      } else {
        "warning"
      }
      list(
        result = list(ok = TRUE, parametrization = param_val),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = identical(severity, "ok"),
            severity = severity,
            metrics = list(
              divergences = if (identical(severity, "ok")) 0 else 15
            )
          )
        )
      )
    })
    bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
      list(compared = names(inputs))
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit1_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit Centered",
      inputs = source_id
    )
    bg_run(handle)

    branch <- bg_branch(handle, fit1_id, label = "Fit Non-Centered")
    bg_set_goal(
      project = handle,
      branch_id = branch$branch_id,
      kind = "observable_prediction",
      label = "Predictive comparison",
      rationale = "This branch is intended for comparison against the baseline fit."
    )
    bg_update_node(
      handle,
      branch$root_node_id,
      params = list(parametrization = "non-centered")
    )
    bg_run(handle, targets = branch$root_node_id)

    result <- bg_next_actions(handle, scope = "project")

    # First fit has warnings, so only review_computation_validity obligation
    expect_gte(length(result$obligations), 1)
    expect_true(any(vapply(
      result$obligations,
      function(o) identical(o$kind, "review_computation_validity"),
      logical(1)
    )))

    # No comparison obligation yet because one fit has warnings
    compare_obl <- Filter(
      function(o) identical(o$kind, "compare_candidate_branches"),
      result$obligations
    )
    expect_length(compare_obl, 0)

    # Fix the first fit. Changing the executor body would invalidate the
    # branch fit's cache too (executor body is part of the fingerprint, see
    # Phase 2.1), so instead drive the existing executor's severity via a
    # param it already reads: a "non-centered"-style clean parametrization.
    bg_invalidate(handle, fit1_id, recursive = TRUE)
    bg_update_node(
      handle,
      fit1_id,
      params = list(parametrization = "clean", revision = 2L)
    )

    bg_run(handle, targets = fit1_id)

    result <- bg_next_actions(handle, scope = "project")

    # Now we have a comparison obligation because both fits are clean
    compare_obl <- Filter(
      function(o) identical(o$kind, "compare_candidate_branches"),
      result$obligations
    )
    expect_length(compare_obl, 1)
    expect_equal(compare_obl[[1]]$severity, "blocking")

    # And we should have a create_node_from_template action
    compare_action <- Filter(
      function(a) identical(a$kind, "create_node_from_template"),
      result$actions
    )
    expect_length(compare_action, 1)
    expect_equal(compare_action[[1]]$payload$template_ref, "branch_comparison")
    expect_length(compare_action[[1]]$payload$inputs, 2)
  })

  it("does not offer branch comparison for non-fit diagnostics", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )
    bg_register_summary_kind(handle, "posterior_diagnostics")

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          passed = TRUE,
          severity = "ok"
        ))
      )
    })
    bg_register_node_kind(handle, "check", executor = function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(list(
          summary_kind = "posterior_diagnostics",
          passed = TRUE,
          severity = "ok"
        ))
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline",
      inputs = source_id
    )
    check_id <- bg_add_node(
      handle,
      kind = "check",
      label = "Check",
      inputs = fit_id
    )

    bg_run(handle, targets = check_id)

    result <- bg_next_actions(handle, scope = "project")
    compare_action <- Filter(
      function(a) identical(a$kind, "create_node_from_template"),
      result$actions
    )

    expect_length(compare_action, 0)
  })

  it("partitions protocol results by scope for UI consumption", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(
      handle,
      "fit",
      executor = function(node, inputs) {
        list(
          result = list(ok = TRUE, node_id = node$id),
          summaries = list(list(
            summary_kind = "optimizer_diagnostics",
            passed = FALSE,
            severity = "warning"
          ))
        )
      }
    )

    baseline_id <- bg_add_node(handle, kind = "fit", label = "Baseline")
    branch <- bg_branch(handle, baseline_id, label = "Alternative")
    branch_id <- branch$branch_id

    bg_run(handle, targets = branch$root_node_id)

    result <- bg_next_actions(handle, scope = "project")
    partitioned <- bg_partition_protocol_by_scope(result, project = handle)

    # Should have summary and scope entries
    expect_true("summary" %in% names(partitioned))
    expect_true("project" %in% names(partitioned))
    expect_true(branch_id %in% names(partitioned))

    # Summary should have counts
    expect_equal(partitioned$summary$n_scopes, 2)
    expect_true(partitioned$summary$n_obligations >= 1)
    expect_true(partitioned$summary$n_blocking >= 1)

    # Each scope entry should have the expected structure
    project_entry <- partitioned[["project"]]
    expect_equal(project_entry$scope, "project")
    expect_equal(project_entry$scope_label, "Project")
    expect_true(is.list(project_entry$obligations))
    expect_true(is.list(project_entry$actions))

    # Branch entry should have branch label
    branch_entry <- partitioned[[branch_id]]
    expect_equal(branch_entry$scope, branch_id)
    expect_equal(branch_entry$scope_label, "Alternative")
  })

  it("partitions ad hoc protocol results without evaluated_scopes metadata", {
    result <- list(
      obligations = list(
        list(
          scope = "project",
          severity = "blocking",
          title = "Review diagnostics"
        )
      ),
      actions = list(
        list(
          scope = "branch:test",
          kind = "record_decision",
          title = "Set a goal"
        )
      ),
      metadata = list()
    )

    partitioned <- bg_partition_protocol_by_scope(result)

    expect_true("project" %in% names(partitioned))
    expect_true("branch:test" %in% names(partitioned))
    expect_equal(partitioned$summary$n_scopes, 2)
    expect_length(partitioned$project$obligations, 1)
    expect_length(partitioned[["branch:test"]]$actions, 1)
  })

  it("tracks obligations per branch when branches have different states", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(
      handle,
      "fit",
      executor = function(node, inputs) {
        list(
          result = list(ok = TRUE, node_id = node$id),
          summaries = list(list(
            summary_kind = "optimizer_diagnostics",
            passed = FALSE,
            severity = "warning"
          ))
        )
      }
    )

    n_fit <- bg_add_node(handle, kind = "fit", label = "Base Fit")

    # Create two branches: one with warning, one clean (no run yet)
    branch_with_warning <- bg_branch(handle, n_fit, label = "Branch A")
    branch_clean <- bg_branch(handle, n_fit, label = "Branch B")

    # Set goal on branch A to avoid goal obligation
    bg_set_goal(
      project = handle,
      branch_id = branch_with_warning$branch_id,
      kind = "observable_prediction",
      label = "Predictive goal",
      rationale = "Test goal"
    )
    bg_set_goal(
      project = handle,
      branch_id = branch_clean$branch_id,
      kind = "observable_prediction",
      label = "Predictive goal",
      rationale = "Test goal"
    )

    # Run only branch A to produce a warning
    bg_run(handle, targets = branch_with_warning$root_node_id)

    # Query branch A - should have computation review and fit criticism obligations
    result_a <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = branch_with_warning$branch_id
    )
    expect_true(any(vapply(
      result_a$obligations,
      function(x) identical(x$kind, "review_computation_validity"),
      logical(1)
    )))
    expect_true(any(vapply(
      result_a$obligations,
      function(x) identical(x$kind, "review_fit_criticism"),
      logical(1)
    )))

    # Query branch B - should have no obligations (not run yet, goal set)
    result_b <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = branch_clean$branch_id
    )
    expect_length(result_b$obligations, 0)

    # Project query should aggregate both
    project_result <- bg_next_actions(handle, scope = "project")
    partitioned <- bg_partition_protocol_by_scope(project_result)

    # Branch A should have obligations (2: computation_review + fit_criticism), Branch B should not
    a_entry <- partitioned[[branch_with_warning$branch_id]]
    b_entry <- partitioned[[branch_clean$branch_id]]

    expect_gte(length(a_entry$obligations), 2)
    expect_length(b_entry$obligations, 0)
  })

  it("emits branch-specific goal actions for branches without goals", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "test")

    n1 <- bg_add_node(handle, kind = "test", label = "Node 1")
    n2 <- bg_add_node(handle, kind = "test", label = "Node 2")

    branch_with_goal <- bg_branch(handle, n1, label = "Has Goal")
    branch_without_goal <- bg_branch(handle, n2, label = "No Goal")

    bg_set_goal(
      project = handle,
      branch_id = branch_with_goal$branch_id,
      kind = "observable_prediction",
      label = "Predictive goal",
      rationale = "Test goal"
    )

    # Query project scope - should include goal obligation only for branch without goal
    result <- bg_next_actions(handle, scope = "project")
    partitioned <- bg_partition_protocol_by_scope(result)

    # Branch with goal should have no goal obligation
    with_goal_entry <- partitioned[[branch_with_goal$branch_id]]
    expect_length(with_goal_entry$obligations, 0)

    # Branch without goal should have goal obligation and action
    without_goal_entry <- partitioned[[branch_without_goal$branch_id]]
    expect_length(without_goal_entry$obligations, 1)
    expect_equal(
      without_goal_entry$obligations[[1]]$kind,
      "set_inferential_goal"
    )
    expect_length(without_goal_entry$actions, 1)
    expect_equal(
      without_goal_entry$actions[[1]]$payload$decision_type,
      "goal_update"
    )
  })

  it("creates comparison node from template action payload", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      # Use params to differentiate fits - both produce clean diagnostics
      variant <- node$params$variant %||% "A"
      list(
        result = list(ok = TRUE, variant = variant),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = TRUE,
            severity = "ok",
            metrics = list(divergences = 0, variant = variant)
          )
        )
      )
    })
    bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
      list(compared = names(inputs))
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit1_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit Variant A",
      inputs = source_id,
      params = list(variant = "A")
    )

    # Run the first fit to produce clean summary
    bg_run(handle)

    # Branch to create a second fit in a different scope
    branch <- bg_branch(handle, fit1_id, label = "Fit Variant B")
    bg_set_goal(
      project = handle,
      branch_id = branch$branch_id,
      kind = "observable_prediction",
      label = "Comparison goal",
      rationale = "This branch is for comparison."
    )

    # Update the branch to use different variant - this makes it a distinct fit
    bg_update_node(
      handle,
      branch$root_node_id,
      params = list(variant = "B")
    )

    # Run the branch fit to produce another clean summary
    bg_run(handle, targets = branch$root_node_id)

    result <- bg_next_actions(handle, scope = "project")

    # Should have comparison obligation (both fits have clean diagnostics)
    comparison_obl <- Filter(
      function(o) identical(o$kind, "compare_candidate_branches"),
      result$obligations
    )
    expect_length(comparison_obl, 1)

    # Should have comparison action
    compare_action <- Filter(
      function(a) identical(a$kind, "create_node_from_template"),
      result$actions
    )
    expect_length(compare_action, 1)
    expect_equal(compare_action[[1]]$payload$template_ref, "branch_comparison")

    # Simulate REPL execution of the comparison action
    action <- compare_action[[1]]
    payload <- action$payload
    input_ids <- payload$inputs

    expect_length(input_ids, 2)
    expect_true(fit1_id %in% input_ids)
    expect_true(branch$root_node_id %in% input_ids)

    # Create the comparison node programmatically (mimicking REPL execution)
    compare_node_id <- bg_add_node(
      project = handle,
      kind = "compare",
      label = action$title,
      inputs = input_ids
    )

    # Verify the node was created correctly
    graph <- bg_read_graph(handle)
    compare_node <- graph$nodes[[compare_node_id]]
    expect_equal(compare_node$kind, "compare")
    expect_equal(compare_node$label, action$title)

    # Verify the edges were created
    incoming_edges <- Filter(function(e) e$to == compare_node_id, graph$edges)
    expect_length(incoming_edges, 2)

    edge_sources <- vapply(incoming_edges, `[[`, character(1), "from")
    expect_true(all(c(fit1_id, branch$root_node_id) %in% edge_sources))

    # Run the comparison node
    compare_run <- bg_run(handle, targets = compare_node_id)
    expect_equal(compare_run$summary$total_executed, 1)

    # Verify comparison result
    compare_result <- bg_result(handle, compare_node_id)
    expect_length(compare_result$compared, 2)
  })

  # ==========================================================================
  # Phase 4: Stronger Default Workflow Pack Tests
  # ==========================================================================

  it("emits review_fit_criticism obligation for branch-scoped non-ok summaries", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = FALSE,
            severity = "warning",
            metrics = list(divergences = 15)
          )
        )
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit",
      inputs = source_id
    )
    branch <- bg_branch(handle, fit_id, label = "Test Branch")
    branch_id <- branch$branch_id

    bg_set_goal(
      project = handle,
      branch_id = branch_id,
      kind = "observable_prediction",
      label = "Test goal",
      rationale = "Test goal"
    )

    bg_run(handle, targets = branch$root_node_id)

    result <- bg_next_actions(handle, scope = "branch", branch_id = branch_id)

    # Should have both computation_review and fit_criticism obligations
    obligation_kinds <- vapply(result$obligations, `[[`, character(1), "kind")
    expect_true("review_computation_validity" %in% obligation_kinds)
    expect_true("review_fit_criticism" %in% obligation_kinds)

    # Both should be blocking
    criticism_obl <- Filter(
      function(o) identical(o$kind, "review_fit_criticism"),
      result$obligations
    )[[1]]
    expect_equal(criticism_obl$severity, "blocking")
    expect_equal(criticism_obl$scope, branch_id)
    expect_true(length(criticism_obl$basis$summary_ids) >= 1)
  })

  it("clears fit_criticism obligation when decision addresses the summaries", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = FALSE,
            severity = "warning",
            metrics = list(divergences = 15)
          )
        )
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit",
      inputs = source_id
    )
    branch <- bg_branch(handle, fit_id, label = "Test Branch")
    branch_id <- branch$branch_id

    bg_set_goal(
      project = handle,
      branch_id = branch_id,
      kind = "observable_prediction",
      label = "Test goal",
      rationale = "Test goal"
    )

    bg_run(handle, targets = branch$root_node_id)

    # Get the summary IDs
    result <- bg_next_actions(handle, scope = "branch", branch_id = branch_id)
    computation_obl <- Filter(
      function(o) identical(o$kind, "review_computation_validity"),
      result$obligations
    )[[1]]
    summary_ids <- computation_obl$basis$summary_ids

    # Record a fit_criticism decision addressing these summaries
    bg_record_decision(
      project = handle,
      scope = branch_id,
      prompt = "Review fit criticism",
      choice = "acceptable",
      rationale = "Diagnostics are acceptable for this analysis",
      kind = "fit_criticism",
      metadata = list(summary_ids = summary_ids)
    )

    # Check that fit_criticism obligation is cleared
    result_after <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = branch_id
    )
    obligation_kinds <- vapply(
      result_after$obligations,
      `[[`,
      character(1),
      "kind"
    )
    expect_false("review_fit_criticism" %in% obligation_kinds)
  })

  it("does not clear fit_criticism obligation when summaries are stale", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })

    # First executor produces warning
    warning_executor <- function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = FALSE,
            severity = "warning",
            metrics = list(divergences = 15)
          )
        )
      )
    }

    bg_register_node_kind(handle, "fit", executor = warning_executor)

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit",
      inputs = source_id
    )
    branch <- bg_branch(handle, fit_id, label = "Test Branch")
    branch_id <- branch$branch_id

    bg_set_goal(
      project = handle,
      branch_id = branch_id,
      kind = "observable_prediction",
      label = "Test goal",
      rationale = "Test goal"
    )

    bg_run(handle, targets = branch$root_node_id)

    # Get the summary IDs and record decision
    result <- bg_next_actions(handle, scope = "branch", branch_id = branch_id)
    computation_obl <- Filter(
      function(o) identical(o$kind, "review_computation_validity"),
      result$obligations
    )[[1]]
    old_summary_ids <- computation_obl$basis$summary_ids

    bg_record_decision(
      project = handle,
      scope = branch_id,
      prompt = "Review fit criticism",
      choice = "acceptable",
      rationale = "Diagnostics are acceptable",
      kind = "fit_criticism",
      metadata = list(summary_ids = old_summary_ids)
    )

    # Now rerun with new warning summaries (stale the old ones)
    bg_invalidate(handle, branch$root_node_id, recursive = TRUE)
    bg_run(handle, targets = branch$root_node_id)

    # The fit_criticism decision addressed old (now stale) summaries
    # New fresh summaries should trigger a new obligation
    result_after <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = branch_id
    )
    obligation_kinds <- vapply(
      result_after$obligations,
      `[[`,
      character(1),
      "kind"
    )
    expect_true("review_fit_criticism" %in% obligation_kinds)
    expect_true("review_computation_validity" %in% obligation_kinds)
  })

  it("emits compare_candidate_branches obligation with two clean fits", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      variant <- node$params$variant %||% "baseline"
      list(
        result = list(ok = TRUE, variant = variant),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = TRUE,
            severity = "ok",
            metrics = list(divergences = 0, variant = variant)
          )
        )
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit1_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit 1",
      inputs = source_id,
      params = list(variant = "baseline")
    )
    bg_run(handle)

    branch <- bg_branch(handle, fit1_id, label = "Fit 2")
    bg_set_goal(
      project = handle,
      branch_id = branch$branch_id,
      kind = "observable_prediction",
      label = "Comparison goal",
      rationale = "For comparison"
    )
    bg_update_node(
      handle,
      branch$root_node_id,
      params = list(variant = "alternative")
    )
    bg_run(handle, targets = branch$root_node_id)

    result <- bg_next_actions(handle, scope = "project")

    comparison_obl <- Filter(
      function(o) identical(o$kind, "compare_candidate_branches"),
      result$obligations
    )
    expect_length(comparison_obl, 1)
    expect_equal(comparison_obl[[1]]$severity, "blocking")
    expect_length(comparison_obl[[1]]$basis$node_ids, 2)
  })

  it("does not emit comparison obligation when a candidate has blocking review", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })

    clean_executor <- function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = TRUE,
            severity = "ok"
          )
        )
      )
    }

    warning_executor <- function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = FALSE,
            severity = "warning"
          )
        )
      )
    }

    bg_register_node_kind(handle, "fit", executor = clean_executor)

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit1_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Clean Fit",
      inputs = source_id
    )
    bg_run(handle)

    # Branch with warning
    branch <- bg_branch(handle, fit1_id, label = "Problematic Fit")
    bg_set_goal(
      project = handle,
      branch_id = branch$branch_id,
      kind = "observable_prediction",
      label = "Comparison goal",
      rationale = "For comparison"
    )

    # Switch executor to produce warning for branch
    bg_register_node_kind(handle, "fit", executor = warning_executor)
    bg_run(handle, targets = branch$root_node_id)

    result <- bg_next_actions(handle, scope = "project")

    # Should not have comparison obligation because one candidate has warnings
    comparison_obl <- Filter(
      function(o) identical(o$kind, "compare_candidate_branches"),
      result$obligations
    )
    expect_length(comparison_obl, 0)
  })

  it("clears comparison obligation after model_comparison decision", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      variant <- node$params$variant %||% "baseline"
      list(
        result = list(ok = TRUE, variant = variant),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = TRUE,
            severity = "ok",
            metrics = list(variant = variant)
          )
        )
      )
    })
    bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
      list(
        result = list(compared = names(inputs)),
        summaries = list(list(
          summary_kind = "comparison_results",
          passed = TRUE,
          severity = "ok"
        ))
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit1_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit 1",
      inputs = source_id,
      params = list(variant = "baseline")
    )
    bg_run(handle)

    branch <- bg_branch(handle, fit1_id, label = "Fit 2")
    bg_set_goal(
      project = handle,
      branch_id = branch$branch_id,
      kind = "observable_prediction",
      label = "Comparison goal",
      rationale = "For comparison"
    )
    bg_update_node(
      handle,
      branch$root_node_id,
      params = list(variant = "alternative")
    )
    bg_run(handle, targets = branch$root_node_id)

    compare_id <- bg_add_node(
      handle,
      kind = "compare",
      label = "Compare Fits",
      inputs = c(fit1_id, branch$root_node_id)
    )
    bg_run(handle, targets = compare_id)

    # Verify comparison obligation exists
    result_before <- bg_next_actions(handle, scope = "project")
    expect_true(any(vapply(
      result_before$obligations,
      function(o) identical(o$kind, "compare_candidate_branches"),
      logical(1)
    )))

    comparison_action <- Filter(
      function(a) {
        identical(a$kind, "record_decision") &&
          identical(a$payload$decision_type, "model_comparison")
      },
      result_before$actions
    )[[1]]

    bg_record_decision(
      project = handle,
      scope = "project",
      prompt = "Compare models",
      choice = "prefer_branch",
      rationale = "Branch fit has better diagnostics",
      kind = "model_comparison",
      metadata = list(
        fit_node_ids = comparison_action$payload$fit_node_ids,
        summary_ids = comparison_action$payload$summary_ids,
        candidate_signature = comparison_action$payload$candidate_signature,
        comparison_signature = comparison_action$payload$comparison_signature,
        comparison_context = comparison_action$payload$comparison_context
      )
    )

    # Verify comparison obligation is cleared
    result_after <- bg_next_actions(handle, scope = "project")
    comparison_obl <- Filter(
      function(o) identical(o$kind, "compare_candidate_branches"),
      result_after$obligations
    )
    expect_length(comparison_obl, 0)
  })

  it("emits accept_or_reject_branch obligation after comparison", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      variant <- node$params$variant %||% "baseline"
      list(
        result = list(ok = TRUE, variant = variant),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = TRUE,
            severity = "ok",
            metrics = list(variant = variant)
          )
        )
      )
    })
    bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
      list(
        result = list(compared = names(inputs)),
        summaries = list(
          list(
            summary_kind = "comparison_results",
            passed = TRUE,
            severity = "ok"
          )
        )
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit1_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit 1",
      inputs = source_id,
      params = list(variant = "baseline")
    )
    bg_run(handle)

    branch <- bg_branch(handle, fit1_id, label = "Fit 2")
    bg_set_goal(
      project = handle,
      branch_id = branch$branch_id,
      kind = "observable_prediction",
      label = "Comparison goal",
      rationale = "For comparison"
    )
    bg_update_node(
      handle,
      branch$root_node_id,
      params = list(variant = "alternative")
    )
    bg_run(handle, targets = branch$root_node_id)

    # Create comparison node and run it
    compare_id <- bg_add_node(
      handle,
      kind = "compare",
      label = "Compare Fits",
      inputs = c(fit1_id, branch$root_node_id)
    )
    bg_run(handle, targets = compare_id)

    result <- bg_next_actions(handle, scope = "project")
    comparison_action <- Filter(
      function(a) {
        identical(a$kind, "record_decision") &&
          identical(a$payload$decision_type, "model_comparison")
      },
      result$actions
    )[[1]]

    # Only the branch-scoped candidate should emit a disposition obligation.
    disposition_obls <- Filter(
      function(o) identical(o$kind, "accept_or_reject_branch"),
      result$obligations
    )
    expect_length(disposition_obls, 1)
    expect_identical(disposition_obls[[1]]$scope, branch$branch_id)
    expect_true(all(vapply(
      disposition_obls,
      function(o) identical(o$severity, "blocking"),
      logical(1)
    )))
    expect_false(is.null(disposition_obls[[1]]$basis$comparison_signature))
    expect_identical(
      disposition_obls[[1]]$basis$comparison_signature,
      comparison_action$payload$comparison_signature
    )
  })

  it("clears accept_or_reject obligation after branch_disposition decision", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      variant <- node$params$variant %||% "baseline"
      list(
        result = list(ok = TRUE, variant = variant),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = TRUE,
            severity = "ok",
            metrics = list(variant = variant)
          )
        )
      )
    })
    bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
      list(
        result = list(compared = names(inputs)),
        summaries = list(
          list(
            summary_kind = "comparison_results",
            passed = TRUE,
            severity = "ok"
          )
        )
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit1_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit 1",
      inputs = source_id,
      params = list(variant = "baseline")
    )
    bg_run(handle)

    branch <- bg_branch(handle, fit1_id, label = "Fit 2")
    bg_set_goal(
      project = handle,
      branch_id = branch$branch_id,
      kind = "observable_prediction",
      label = "Comparison goal",
      rationale = "For comparison"
    )
    bg_update_node(
      handle,
      branch$root_node_id,
      params = list(variant = "alternative")
    )
    bg_run(handle, targets = branch$root_node_id)

    compare_id <- bg_add_node(
      handle,
      kind = "compare",
      label = "Compare Fits",
      inputs = c(fit1_id, branch$root_node_id)
    )
    bg_run(handle, targets = compare_id)

    # Verify disposition obligation exists for the branch
    result_before <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = branch$branch_id
    )
    disposition_obls <- Filter(
      function(o) identical(o$kind, "accept_or_reject_branch"),
      result_before$obligations
    )
    expect_length(disposition_obls, 1)
    expect_identical(disposition_obls[[1]]$scope, branch$branch_id)

    disposition_action <- Filter(
      function(a) {
        identical(a$kind, "record_decision") &&
          identical(a$payload$decision_type, "branch_disposition")
      },
      result_before$actions
    )[[1]]

    # Record branch_disposition decision
    bg_record_decision(
      project = handle,
      scope = branch$branch_id,
      prompt = "Accept or reject branch",
      choice = "accept",
      rationale = "This branch performs better",
      kind = "branch_disposition",
      metadata = list(
        disposition = "accept",
        summary_ids = disposition_action$payload$summary_ids,
        comparison_signature = disposition_action$payload$comparison_signature,
        comparison_context = disposition_action$payload$comparison_context
      )
    )

    # Verify obligation is cleared for this branch
    result_after <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = branch$branch_id
    )
    disposition_obls_after <- Filter(
      function(o) identical(o$kind, "accept_or_reject_branch"),
      result_after$obligations
    )
    expect_length(disposition_obls_after, 0)
  })

  it("re-emits accept_or_reject obligation after comparison evidence changes", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      variant <- node$params$variant %||% "baseline"
      list(
        result = list(ok = TRUE, variant = variant),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = TRUE,
            severity = "ok",
            metrics = list(variant = variant)
          )
        )
      )
    })
    bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
      variants <- vapply(
        inputs,
        function(input) input$result$variant %||% "baseline",
        character(1)
      )

      list(
        result = list(compared = names(inputs), variants = variants),
        summaries = list(
          list(
            summary_kind = "comparison_results",
            passed = TRUE,
            severity = "ok",
            metrics = list(variants = variants)
          )
        )
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit1_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit 1",
      inputs = source_id,
      params = list(variant = "baseline")
    )
    bg_run(handle)

    branch <- bg_branch(handle, fit1_id, label = "Fit 2")
    bg_set_goal(
      project = handle,
      branch_id = branch$branch_id,
      kind = "observable_prediction",
      label = "Comparison goal",
      rationale = "For comparison"
    )
    bg_update_node(
      handle,
      branch$root_node_id,
      params = list(variant = "alternative")
    )
    bg_run(handle, targets = branch$root_node_id)

    compare_id <- bg_add_node(
      handle,
      kind = "compare",
      label = "Compare Fits",
      inputs = c(fit1_id, branch$root_node_id)
    )
    bg_run(handle, targets = compare_id)

    first_branch_actions <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = branch$branch_id
    )
    first_disposition_action <- Filter(
      function(a) {
        identical(a$kind, "record_decision") &&
          identical(a$payload$decision_type, "branch_disposition")
      },
      first_branch_actions$actions
    )[[1]]
    first_signature <- first_disposition_action$payload$comparison_signature

    bg_record_decision(
      project = handle,
      scope = branch$branch_id,
      prompt = "Accept or reject branch",
      choice = "accept",
      rationale = "This branch performs better",
      kind = "branch_disposition",
      metadata = list(
        disposition = "accept",
        summary_ids = first_disposition_action$payload$summary_ids,
        comparison_signature = first_signature,
        comparison_context = first_disposition_action$payload$comparison_context
      )
    )

    expect_length(
      Filter(
        function(o) identical(o$kind, "accept_or_reject_branch"),
        bg_next_actions(
          handle,
          scope = "branch",
          branch_id = branch$branch_id
        )$obligations
      ),
      0
    )

    bg_update_node(
      handle,
      branch$root_node_id,
      params = list(variant = "alternative_v2")
    )
    bg_run(handle, targets = branch$root_node_id)
    bg_run(handle, targets = compare_id)

    refreshed_branch_actions <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = branch$branch_id
    )
    refreshed_obligations <- Filter(
      function(o) identical(o$kind, "accept_or_reject_branch"),
      refreshed_branch_actions$obligations
    )
    expect_length(refreshed_obligations, 1)

    refreshed_action <- Filter(
      function(a) {
        identical(a$kind, "record_decision") &&
          identical(a$payload$decision_type, "branch_disposition")
      },
      refreshed_branch_actions$actions
    )[[1]]
    refreshed_signature <- refreshed_action$payload$comparison_signature

    expect_false(is.null(refreshed_signature))
    expect_false(identical(refreshed_signature, first_signature))
    expect_identical(
      refreshed_obligations[[1]]$basis$comparison_signature,
      refreshed_signature
    )
  })

  it("produces deterministic obligation ids across repeated evaluation", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = FALSE,
            severity = "warning"
          )
        )
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit",
      inputs = source_id
    )
    bg_run(handle)

    result1 <- bg_next_actions(handle, scope = "project")
    result2 <- bg_next_actions(handle, scope = "project")

    ids1 <- sort(vapply(
      result1$obligations,
      `[[`,
      character(1),
      "obligation_id"
    ))
    ids2 <- sort(vapply(
      result2$obligations,
      `[[`,
      character(1),
      "obligation_id"
    ))

    expect_equal(ids1, ids2)

    action_ids1 <- sort(vapply(
      result1$actions,
      `[[`,
      character(1),
      "action_id"
    ))
    action_ids2 <- sort(vapply(
      result2$actions,
      `[[`,
      character(1),
      "action_id"
    ))

    expect_equal(action_ids1, action_ids2)
  })

  it("emits fit_criticism actions alongside computation_review actions", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      list(
        result = list(ok = TRUE),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = FALSE,
            severity = "warning",
            metrics = list(divergences = 15)
          )
        )
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit",
      inputs = source_id
    )
    branch <- bg_branch(handle, fit_id, label = "Test Branch")
    branch_id <- branch$branch_id

    bg_set_goal(
      project = handle,
      branch_id = branch_id,
      kind = "observable_prediction",
      label = "Test goal",
      rationale = "Test goal"
    )

    bg_run(handle, targets = branch$root_node_id)

    result <- bg_next_actions(handle, scope = "branch", branch_id = branch_id)

    # Should have fit_criticism record_decision action
    criticism_actions <- Filter(
      function(a) {
        identical(a$kind, "record_decision") &&
          identical(a$payload$decision_type, "fit_criticism")
      },
      result$actions
    )
    expect_gte(length(criticism_actions), 1)
    expect_equal(criticism_actions[[1]]$payload$template_ref, "review_decision")

    # Should also have branch_and_modify action for criticism
    branch_actions <- Filter(
      function(a) identical(a$kind, "branch_and_modify"),
      result$actions
    )
    expect_gte(length(branch_actions), 1)
    expect_equal(
      branch_actions[[1]]$payload$template_ref,
      "branch_and_modify_fit"
    )
  })

  it("emits model_comparison decision action when comparison node exists", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      variant <- node$params$variant %||% "baseline"
      list(
        result = list(ok = TRUE, variant = variant),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = TRUE,
            severity = "ok",
            metrics = list(variant = variant)
          )
        )
      )
    })
    bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
      list(result = list(compared = names(inputs)))
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit1_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit 1",
      inputs = source_id,
      params = list(variant = "baseline")
    )
    bg_run(handle)

    branch <- bg_branch(handle, fit1_id, label = "Fit 2")
    bg_set_goal(
      project = handle,
      branch_id = branch$branch_id,
      kind = "observable_prediction",
      label = "Comparison goal",
      rationale = "For comparison"
    )
    bg_update_node(
      handle,
      branch$root_node_id,
      params = list(variant = "alternative")
    )
    bg_run(handle, targets = branch$root_node_id)

    # Create comparison node
    compare_id <- bg_add_node(
      handle,
      kind = "compare",
      label = "Compare Fits",
      inputs = c(fit1_id, branch$root_node_id)
    )
    bg_run(handle, targets = compare_id)

    result <- bg_next_actions(handle, scope = "project")

    # Should have model_comparison decision action
    comparison_actions <- Filter(
      function(a) {
        identical(a$kind, "record_decision") &&
          identical(a$payload$decision_type, "model_comparison")
      },
      result$actions
    )
    expect_gte(length(comparison_actions), 1)
    expect_equal(
      comparison_actions[[1]]$payload$template_ref,
      "review_decision"
    )
  })

  it("bg_blocking_obligation_holds traverses retired (inactive) descendants", {
    # Regression for the Phase 6 graph-threading change: the holds check must
    # use the RAW graph (all nodes/edges), not the active graph, so an
    # obligation on a node upstream of a RETIRED node still holds the
    # downstream active node. A->B(retired)->C: descendants of A are {B, C},
    # so a blocking obligation on A must hold C even though B is inactive.
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) 1)
    a <- bg_add_node(handle, kind = "data", label = "A")
    b <- bg_add_node(handle, kind = "data", label = "B", inputs = a)
    c <- bg_add_node(handle, kind = "data", label = "C", inputs = b)

    # Retire B only (recursive = FALSE): C stays active but, in the active
    # graph, B and its edges A->B, B->C are dropped, isolating C from A.
    bg_retire_node(handle, b, recursive = FALSE)

    obligations <- list(list(
      severity = "blocking",
      title = "test hold on A",
      metadata = list(hold_node_ids = a)
    ))

    # Default (graph = NULL -> raw read): descendants of A traverse retired B
    # to reach C, so C is held.
    holds_default <- bayesgrove:::bg_blocking_obligation_holds(
      handle,
      obligations
    )
    expect_true(c %in% names(holds_default))
    expect_true(b %in% names(holds_default))

    # Passing the ACTIVE graph would DROP B and the edges A->B, B->C, so C would
    # NOT be reachable. Asserting the default does NOT behave like the active
    # graph locks in the raw-graph semantics the run loop relies on.
    active_graph <- bayesgrove:::bg_dagri_recompute_state(
      bayesgrove:::bg_active_graph(
        handle,
        graph = bayesgrove:::bg_read_graph(handle)
      )
    )
    holds_active <- bayesgrove:::bg_blocking_obligation_holds(
      handle,
      obligations,
      graph = active_graph
    )
    expect_false(c %in% names(holds_active))
  })
})
