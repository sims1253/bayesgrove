describe("Workflow protocol APIs", {
  it("returns empty protocol results when no workflow packs are active", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp, workflow_packs = list())

    expect_equal(bg_workflow_packs(handle), list())

    result <- bg_next_actions(handle)
    expect_equal(result$obligations, list())
    expect_equal(result$actions, list())
    expect_equal(result$context$active_packs, list())
  })

  it("emits a blocking obligation from fresh warning summaries", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
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
    bg_run(handle, mode = "sync")

    result <- bg_next_actions(handle)

    expect_length(result$obligations, 1)
    obligation <- result$obligations[[1]]
    expect_equal(obligation$kind, "review_computation_validity")
    expect_equal(obligation$severity, "blocking")
    expect_equal(obligation$scope, "project")
    expect_equal(obligation$basis$node_ids, node_id)
    expect_length(obligation$basis$summary_ids, 1)
  })

  it("emits a goal-setting action for a branch without an inferential goal", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
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
  })

  it("deduplicates identical obligations and actions at runtime", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list(
        "bayesguide.default_bayesian",
        "bayesguide.default_bayesian"
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
      "bayesguide.default_bayesian"
    )
    expect_equal(
      result$obligations[[1]]$metadata$pack_ids,
      "bayesguide.default_bayesian"
    )
  })

  it("aggregates branch protocol results into project-scope queries", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
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

    bg_run(handle, targets = branch$root_node_id, mode = "sync")

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
})
