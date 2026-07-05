describe("Workflow persistence and context", {
  it("persists branch records when creating a branch", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "test_kind")
    n1 <- bg_add_node(handle, kind = "test_kind", label = "Source")
    n2 <- bg_add_node(handle, kind = "test_kind", label = "Fit", inputs = n1)
    branch_record <- bg_branch(handle, n2, label = "Fit Branch")
    n3 <- branch_record$root_node_id

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
    branch <- bg_branch(handle, n1, label = "Main")

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
    bg_run(handle)

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

    index_path <- file.path(tmp, ".bayesgrove", "cache", "index.json")
    expect_true(file.exists(index_path))
    index_doc <- jsonlite::read_json(index_path, simplifyVector = FALSE)
    expect_equal(index_doc$schema_name, "bg_artifact_index")
    expect_equal(index_doc$schema_version, 1L)
    expect_equal(index_doc$project_id, handle@project_id)
    expect_true(node_id %in% names(index_doc$entries))
    expect_length(index_doc$entries[[node_id]], 1)
  })

  it("validates the project handle in bg_scope_label", {
    expect_error(bg_scope_label(1, "project"), "bg_handle")
    expect_error(bg_scope_label(1, "branch:test"), "bg_handle")
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
    bg_run(handle)
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
    branch <- bg_branch(handle, n_fit, label = "Alternative")
    n_branch <- branch$root_node_id

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

  it("keeps project-scoped contexts free of branch-only summaries", {
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
        result = list(node_id = node$id),
        summaries = list(list(
          summary_kind = "optimizer_diagnostics",
          passed = FALSE,
          severity = "warning"
        ))
      )
    })

    n_source <- bg_add_node(handle, kind = "source", label = "Data")
    n_fit <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline",
      inputs = n_source
    )
    branch <- bg_branch(handle, n_fit, label = "Alternative")
    n_branch <- branch$root_node_id

    bg_run(handle, targets = n_branch)

    project_context <- bg_build_workflow_context(handle, scope = "project")
    branch_context <- bg_build_workflow_context(
      handle,
      scope = branch$branch_id
    )

    expect_equal(
      project_context$evidence$summaries,
      list(),
      info = "Project-scoped context should not pull branch-only summaries into the project evidence set."
    )
    expect_equal(
      unique(vapply(
        branch_context$evidence$summaries,
        `[[`,
        character(1),
        "node_id"
      )),
      n_branch
    )
  })

  it("keeps project summaries visible inside branch contexts", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_summary_kind(handle, "data_diagnostics")

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(
        result = list(rows = 10L),
        summaries = list(list(
          summary_kind = "data_diagnostics",
          passed = FALSE,
          severity = "warning"
        ))
      )
    })
    bg_register_node_kind(handle, "fit")

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline",
      inputs = source_id
    )
    branch <- bg_branch(handle, fit_id, label = "Alternative")

    bg_run(handle, targets = source_id)

    project_context <- bg_build_workflow_context(handle, scope = "project")
    branch_context <- bg_build_workflow_context(
      handle,
      scope = branch$branch_id
    )

    expect_true(
      source_id %in%
        vapply(
          project_context$evidence$summaries,
          `[[`,
          character(1),
          "node_id"
        )
    )
    expect_true(
      source_id %in%
        vapply(
          branch_context$evidence$summaries,
          `[[`,
          character(1),
          "node_id"
        )
    )
  })

  it("lists branches with metadata via bg_list_branches", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "test_kind")
    n1 <- bg_add_node(handle, kind = "test_kind", label = "Source")
    n2 <- bg_add_node(handle, kind = "test_kind", label = "Fit", inputs = n1)

    # No branches initially
    expect_length(bg_list_branches(handle), 0)

    # Create first branch with goal
    branch1 <- bg_branch(handle, n2, label = "First Branch")
    bg_set_goal(
      project = handle,
      branch_id = branch1$branch_id,
      kind = "observable_prediction",
      label = "Predictive goal",
      rationale = "Test goal"
    )

    # Create second branch without goal
    branch2 <- bg_branch(handle, n1, label = "Second Branch")

    branches <- bg_list_branches(handle)
    expect_length(branches, 2)

    expect_true(branch1$branch_id %in% names(branches))
    expect_true(branch2$branch_id %in% names(branches))

    b1 <- branches[[branch1$branch_id]]
    expect_equal(b1$label, "First Branch")
    expect_equal(b1$root_node_id, branch1$root_node_id)
    expect_true(b1$has_goal)
    expect_equal(b1$lifecycle, "active")

    b2 <- branches[[branch2$branch_id]]
    expect_equal(b2$label, "Second Branch")
    expect_false(b2$has_goal)
    expect_equal(b2$lifecycle, "active")
  })

  it("excludes retired branches from active workflow contexts", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      list(result = list(ok = TRUE))
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline",
      inputs = source_id
    )
    branch <- bg_branch(handle, fit_id, label = "Alternative")

    branch_context <- bg_build_workflow_context(
      handle,
      scope = branch$branch_id
    )
    expect_equal(names(branch_context$structural$nodes), branch$root_node_id)

    bg_retire_branch(handle, branch$branch_id)

    retired_context <- bg_build_workflow_context(
      handle,
      scope = branch$branch_id
    )
    expect_length(retired_context$structural$nodes, 0L)

    actions <- bg_next_actions(handle, scope = "project")
    evaluated_scopes <- actions$metadata$evaluated_scopes %||% character()
    expect_false(branch$branch_id %in% evaluated_scopes)
  })

  it("provides human-readable scope labels via bg_scope_label", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "test_kind")
    n1 <- bg_add_node(handle, kind = "test_kind", label = "Source")
    branch <- bg_branch(handle, n1, label = "My Branch")

    expect_equal(bg_scope_label(handle, "project"), "Project")
    expect_equal(bg_scope_label(handle, branch$branch_id), "My Branch")

    # Unknown branch falls back to truncated id
    unknown_label <- bg_scope_label(handle, "branch:unknown123")
    expect_true(startsWith(unknown_label, "Branch "))
  })
})
