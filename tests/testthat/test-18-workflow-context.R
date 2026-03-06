describe("Workflow persistence and context", {
  it("persists branch records when creating a branch", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "test_kind")
    n1 <- bg_add_node(handle, kind = "test_kind", label = "Source")
    n2 <- bg_add_node(handle, kind = "test_kind", label = "Fit", inputs = n1)
    n3 <- bg_branch(handle, n2, label = "Fit Branch")

    registry <- bg_read_branch_registry(handle)
    expect_length(registry$branches, 1)

    branch <- registry$branches[[1]]
    expect_equal(branch$root_node_id, n3)
    expect_equal(branch$source_node_id, n2)
    expect_equal(branch$label, "Fit Branch")
    expect_true(startsWith(branch$branch_id, "branch:"))
  })

  it("updates the goal registry from goal-setting decisions", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "test_kind")
    n1 <- bg_add_node(handle, kind = "test_kind", label = "Seed")
    n2 <- bg_branch(handle, n1, label = "Main")

    branch <- Filter(
      function(x) identical(x$root_node_id, n2),
      bg_read_branch_registry(handle)$branches
    )[[1]]

    decision <- bg_record_decision(
      project = handle,
      scope = branch$branch_id,
      kind = "goal_update",
      prompt = "Set inferential goal",
      choice = "Held-out predictive performance",
      rationale = "This branch is for predictive comparison.",
      metadata = list(
        goal = list(
          kind = "observable_prediction",
          label = "Held-out predictive performance",
          metadata = list(priority = "high")
        )
      )
    )

    goals <- bg_read_goal_registry(handle)$branch_goals
    expect_true(branch$branch_id %in% names(goals))
    expect_equal(goals[[branch$branch_id]]$decision_id, decision$decision_id)
    expect_equal(goals[[branch$branch_id]]$kind, "observable_prediction")
    expect_equal(goals[[branch$branch_id]]$metadata$priority, "high")
  })

  it("persists execution summaries with runtime metadata", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(
      handle,
      "fit",
      executor = function(node, inputs) {
        list(
          result = list(ok = TRUE, node = node$id),
          summaries = list(
            list(
              summary_kind = "optimizer_diagnostics",
              passed = FALSE,
              severity = "warning",
              metrics = list(max_gradient = 0.02)
            )
          )
        )
      }
    )

    node_id <- bg_add_node(handle, kind = "fit", label = "Fit")
    bg_run(handle, mode = "sync")

    summaries <- bg_read_summaries(handle)
    expect_length(summaries, 1)

    summary <- summaries[[1]]
    expect_equal(summary$node_id, node_id)
    expect_equal(summary$summary_kind, "optimizer_diagnostics")
    expect_equal(summary$severity, "warning")
    expect_true(summary$is_fresh)
    expect_false(summary$is_stale)
    expect_true(file.exists(file.path(
      tmp,
      ".bayesgrove",
      "workflow",
      "summaries.jsonl"
    )))
  })

  it("marks summaries stale after invalidation supersedes the artifact binding", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(
      handle,
      "fit",
      executor = function(node, inputs) {
        list(
          result = list(ok = TRUE),
          summaries = list(
            list(
              summary_kind = "optimizer_diagnostics",
              passed = TRUE,
              severity = "ok",
              metrics = list(rhat = 1)
            )
          )
        )
      }
    )

    node_id <- bg_add_node(handle, kind = "fit", label = "Fit")
    bg_run(handle, mode = "sync")
    bg_invalidate(handle, node_id, recursive = TRUE)

    summaries <- bg_read_summaries(handle)
    expect_length(summaries, 1)
    expect_true(summaries[[1]]$is_stale)

    plan <- bg_plan(handle)
    expect_true(node_id %in% plan$to_execute)
  })

  it("builds scope-aware workflow contexts for project and branch queries", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "source")
    bg_register_node_kind(handle, "fit")

    n_source <- bg_add_node(handle, kind = "source", label = "Data")
    n_fit <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline",
      inputs = n_source
    )
    n_branch <- bg_branch(handle, n_fit, label = "Alternative")

    branch <- Filter(
      function(x) identical(x$root_node_id, n_branch),
      bg_read_branch_registry(handle)$branches
    )[[1]]

    bg_set_goal(
      project = handle,
      branch_id = branch$branch_id,
      kind = "observable_prediction",
      label = "Held-out predictive performance",
      rationale = "Track predictive quality on the branched fit."
    )

    bg_record_decision(
      handle,
      scope = "project",
      prompt = "Why branch?",
      choice = "Compare alternatives",
      rationale = "We need a project-level comparison note."
    )
    bg_record_decision(
      handle,
      scope = branch$branch_id,
      prompt = "Why this branch goal?",
      choice = "Predictive focus",
      rationale = "This branch is scoped to predictive comparison."
    )

    project_context <- bg_build_workflow_context(handle, scope = "project")
    branch_context <- bg_build_workflow_context(
      handle,
      scope = branch$branch_id
    )

    expect_true(all(
      c(n_source, n_fit) %in% names(project_context$structural$nodes)
    ))
    expect_false(n_branch %in% names(project_context$structural$nodes))

    expect_equal(names(branch_context$structural$nodes), n_branch)
    expect_equal(branch_context$scope_context$branch_root, n_branch)
    expect_equal(branch_context$inferential_goal$kind, "observable_prediction")

    decision_scopes <- vapply(
      branch_context$evidence$decisions,
      `[[`,
      character(1),
      "scope"
    )
    expect_true("project" %in% decision_scopes)
    expect_true(branch$branch_id %in% decision_scopes)
  })
})
