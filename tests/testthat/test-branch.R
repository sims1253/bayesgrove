describe("Graph Branching", {
  it("creates a clone of a node with upstream edges", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind("test_kind")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    n1 <- bg_add_node(handle, kind = "test_kind", label = "Source")
    n2 <- bg_add_node(
      handle,
      kind = "test_kind",
      label = "Transform",
      inputs = n1
    )

    # Branch n2
    branch <- bg_branch(handle, n2, label = "Transform Branch")
    n3 <- branch$root_node_id
    expect_true(startsWith(n3, "node_"))
    expect_true(startsWith(branch$branch_id, "branch:"))

    g2 <- bg_read_graph(handle)

    # The new node should exist and have the new label
    expect_equal(g2$nodes[[n3]]$label, "Transform Branch")

    # The new node should have an edge from n1
    edges_to_n3 <- Filter(function(e) e$to == n3, g2$edges)
    expect_equal(length(edges_to_n3), 1)
    expect_equal(edges_to_n3[[1]]$from, n1)
  })
})

describe("Branch with continuation", {
  it("creates a branch without continuation when no downstream nodes exist", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["fit"]] <- dagriculture::dagri_kind("fit")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    n1 <- bg_add_node(handle, kind = "fit", label = "Fit")
    record <- bg_branch(
      project = handle,
      node_id = n1,
      label = "Branched Fit",
      continue = TRUE
    )

    expect_true(startsWith(record$branch_id, "branch:"))
    expect_equal(length(record$continuation_nodes), 0)
  })

  it("always carries a continuation_nodes field (empty by default)", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["fit"]] <- dagriculture::dagri_kind("fit")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    n1 <- bg_add_node(handle, kind = "fit", label = "Fit")
    record <- bg_branch(handle, n1, label = "Plain branch")

    expect_true("continuation_nodes" %in% names(record))
    expect_equal(length(record$continuation_nodes), 0)
  })

  it("rejects invalid continue arguments", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["fit"]] <- dagriculture::dagri_kind("fit")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    n1 <- bg_add_node(handle, kind = "fit", label = "Fit")
    expect_error(
      bg_branch(handle, n1, continue = 42),
      class = "rlang_error"
    )
  })

  it("creates a branch with cloned downstream nodes", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["source"]] <- dagriculture::dagri_kind("source")
    graph$registry$kinds[["fit"]] <- dagriculture::dagri_kind("fit")
    graph$registry$kinds[["check"]] <- dagriculture::dagri_kind("check")
    graph$registry$kinds[["ppc"]] <- dagriculture::dagri_kind("ppc")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    n_source <- bg_add_node(handle, kind = "source", label = "Data")
    n_fit <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit",
      inputs = n_source
    )
    n_check <- bg_add_node(
      handle,
      kind = "check",
      label = "Diagnostics",
      inputs = n_fit
    )
    n_ppc <- bg_add_node(
      handle,
      kind = "ppc",
      label = "PPC",
      inputs = n_fit
    )

    # Branch with continuation (all immediate children)
    record <- bg_branch(
      project = handle,
      node_id = n_fit,
      label = "Branched Fit",
      continue = TRUE
    )

    expect_equal(length(record$continuation_nodes), 2)

    g2 <- bg_read_graph(handle)

    # Check that continuation nodes exist and have correct structure
    check_cont <- record$continuation_nodes[[n_check]]
    ppc_cont <- record$continuation_nodes[[n_ppc]]

    expect_true(!is.null(check_cont))
    expect_true(!is.null(ppc_cont))
    expect_equal(check_cont$kind, "check")
    expect_equal(ppc_cont$kind, "ppc")

    # Verify cloned nodes exist in graph
    expect_true(check_cont$clone_id %in% names(g2$nodes))
    expect_true(ppc_cont$clone_id %in% names(g2$nodes))

    # Verify cloned nodes have edges from the branch root
    edges_to_check <- Filter(
      function(e) e$to == check_cont$clone_id,
      g2$edges
    )
    edges_to_ppc <- Filter(
      function(e) e$to == ppc_cont$clone_id,
      g2$edges
    )

    expect_equal(length(edges_to_check), 1)
    expect_equal(edges_to_check[[1]]$from, record$root_node_id)
    expect_equal(length(edges_to_ppc), 1)
    expect_equal(edges_to_ppc[[1]]$from, record$root_node_id)
  })

  it("filters continuation by node kind", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["source"]] <- dagriculture::dagri_kind("source")
    graph$registry$kinds[["fit"]] <- dagriculture::dagri_kind("fit")
    graph$registry$kinds[["check"]] <- dagriculture::dagri_kind("check")
    graph$registry$kinds[["ppc"]] <- dagriculture::dagri_kind("ppc")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    n_source <- bg_add_node(handle, kind = "source", label = "Data")
    n_fit <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit",
      inputs = n_source
    )
    n_check <- bg_add_node(
      handle,
      kind = "check",
      label = "Diagnostics",
      inputs = n_fit
    )
    n_ppc <- bg_add_node(
      handle,
      kind = "ppc",
      label = "PPC",
      inputs = n_fit
    )

    # Branch with continuation filtered to only 'check'
    record <- bg_branch(
      project = handle,
      node_id = n_fit,
      label = "Branched Fit",
      continue = c("check")
    )

    expect_equal(length(record$continuation_nodes), 1)
    expect_true(n_check %in% names(record$continuation_nodes))
    expect_false(n_ppc %in% names(record$continuation_nodes))
  })

  it("preserves branch provenance on continuation nodes", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["fit"]] <- dagriculture::dagri_kind("fit")
    graph$registry$kinds[["check"]] <- dagriculture::dagri_kind("check")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    n_fit <- bg_add_node(handle, kind = "fit", label = "Fit")
    n_check <- bg_add_node(
      handle,
      kind = "check",
      label = "Diagnostics",
      inputs = n_fit
    )

    record <- bg_branch(
      project = handle,
      node_id = n_fit,
      label = "Branched Fit",
      continue = TRUE
    )

    g2 <- bg_read_graph(handle)
    check_cont <- record$continuation_nodes[[n_check]]
    cloned_node <- g2$nodes[[check_cont$clone_id]]

    # Verify provenance metadata
    expect_equal(cloned_node$metadata$branched_from, n_check)
    expect_equal(cloned_node$metadata$branch_id, record$branch_id)
  })

  it("allows running downstream nodes on the branched path", {
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
      severity <- if (identical(param_val, "non-centered")) "ok" else "warning"
      list(
        result = list(fitted = TRUE, parametrization = param_val),
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          passed = identical(severity, "ok"),
          severity = severity,
          metrics = list(divergences = if (identical(severity, "ok")) 0 else 15)
        ))
      )
    })
    bg_register_node_kind(handle, "check", executor = function(node, inputs) {
      list(checked = TRUE, fit_parametrization = inputs[[1]]$parametrization)
    })

    n_source <- bg_add_node(handle, kind = "source", label = "Data")
    n_fit <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit Centered",
      inputs = n_source
    )
    n_check <- bg_add_node(
      handle,
      kind = "check",
      label = "Diagnostics",
      inputs = n_fit
    )

    # Run the original fit (produces warning)
    bg_run(handle, targets = n_fit)

    # Branch with continuation and set non-centered parametrization
    record <- bg_branch(
      project = handle,
      node_id = n_fit,
      label = "Fit Non-Centered",
      continue = c("check")
    )

    bg_update_node(
      handle,
      record$root_node_id,
      params = list(parametrization = "non-centered")
    )

    # Set goal on the branch to avoid goal-blocking obligation
    bg_set_goal(
      project = handle,
      branch_id = record$branch_id,
      kind = "observable_prediction",
      label = "Branch goal",
      rationale = "Testing branch continuation"
    )

    # Run the branch fit first
    check_cont <- record$continuation_nodes[[n_check]]
    bg_run(handle, targets = record$root_node_id)

    # Now run the check continuation (downstream)
    bg_run(handle, targets = check_cont$clone_id)

    # Verify the continuation node ran and has correct result
    check_result <- bg_result(handle, check_cont$clone_id)
    expect_true(check_result$checked)
    expect_equal(check_result$fit_parametrization, "non-centered")

    # Verify the branch fit has clean diagnostics
    summaries <- bg_read_summaries(handle)
    branch_summaries <- Filter(
      function(s) s$node_id == record$root_node_id && isTRUE(s$is_fresh),
      summaries
    )
    expect_length(branch_summaries, 1)
    expect_equal(branch_summaries[[1]]$severity, "ok")
  })

  it("enables comparison workflow after branch with continuation", {
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
      severity <- if (identical(param_val, "non-centered")) "ok" else "ok"
      list(
        result = list(fitted = TRUE, parametrization = param_val),
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          passed = TRUE,
          severity = severity,
          metrics = list(divergences = 0)
        ))
      )
    })
    bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
      list(compared = names(inputs))
    })

    n_source <- bg_add_node(handle, kind = "source", label = "Data")
    n_fit1 <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit Centered",
      inputs = n_source,
      params = list(parametrization = "centered")
    )

    # Run the first fit
    bg_run(handle, targets = n_fit1)

    # Branch to create second fit
    record <- bg_branch(
      project = handle,
      node_id = n_fit1,
      label = "Fit Non-Centered",
      continue = TRUE
    )

    bg_update_node(
      handle,
      record$root_node_id,
      params = list(parametrization = "non-centered")
    )

    # Set goal on branch
    bg_set_goal(
      project = handle,
      branch_id = record$branch_id,
      kind = "observable_prediction",
      label = "Comparison goal",
      rationale = "Compare parametrizations"
    )

    # Run the branch fit
    bg_run(handle, targets = record$root_node_id)

    # Check that both fits exist and can be compared
    next_actions <- bg_next_actions(handle, scope = "project")

    # The stronger default pack should require an explicit comparison decision
    blocking <- Filter(
      function(o) identical(o$severity, "blocking"),
      next_actions$obligations
    )
    expect_length(blocking, 1)
    expect_equal(blocking[[1]]$kind, "compare_candidate_branches")

    # Should have comparison action
    compare_actions <- Filter(
      function(a) {
        identical(a$kind, "create_node_from_template") &&
          identical(a$payload$template_ref, "branch_comparison")
      },
      next_actions$actions
    )
    expect_length(compare_actions, 1)

    # The comparison should include both fits
    compare_action <- compare_actions[[1]]
    expect_true(n_fit1 %in% compare_action$payload$inputs)
    expect_true(record$root_node_id %in% compare_action$payload$inputs)
  })

  it("retires a warning branch so it no longer participates in planning", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      severity <- if (
        identical(node$params$parametrization %||% "centered", "centered")
      ) {
        "warning"
      } else {
        "ok"
      }
      list(
        result = list(
          parametrization = node$params$parametrization %||% "centered"
        ),
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          passed = identical(severity, "ok"),
          severity = severity
        ))
      )
    })
    bg_register_node_kind(handle, "ppc", executor = function(node, inputs) {
      list(ppc = TRUE)
    })

    n_source <- bg_add_node(handle, kind = "source", label = "Data")
    n_fit <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline",
      inputs = n_source
    )
    bg_run(handle, targets = n_fit)

    branch <- bg_branch(
      project = handle,
      node_id = n_fit,
      label = "Problematic branch",
      continue = TRUE
    )
    bg_update_node(
      handle,
      branch$root_node_id,
      params = list(parametrization = "centered")
    )
    bg_run(handle, targets = branch$root_node_id)

    before_retire <- bg_next_actions(handle, scope = "project")
    expect_true(any(vapply(
      before_retire$obligations,
      function(o) identical(o$scope, branch$branch_id),
      logical(1)
    )))

    expect_no_error(bg_result(handle, branch$root_node_id))

    bg_retire_branch(handle, branch$branch_id)

    after_retire <- bg_next_actions(handle, scope = "project")
    expect_false(any(vapply(
      after_retire$obligations,
      function(o) identical(o$scope, branch$branch_id),
      logical(1)
    )))

    plan <- bg_plan(handle)
    expect_false(branch$root_node_id %in% plan$graph_plan$topo_order)
    expect_no_error(bg_result(handle, branch$root_node_id))
  })

  it("retire command semantics prevent rerunning retired lineages", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    state <- new.env(parent = emptyenv())
    state$fit_runs <- 0L

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(rows = 10L)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      state$fit_runs <- state$fit_runs + 1L
      list(fit_runs = state$fit_runs)
    })

    n_source <- bg_add_node(handle, kind = "source", label = "Data")
    n_fit <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline",
      inputs = n_source
    )
    bg_run(handle)

    branch <- bg_branch(handle, n_fit, label = "Alternative")
    bg_retire_node(handle, branch$root_node_id, recursive = TRUE)

    bg_invalidate(handle, branch$root_node_id, recursive = TRUE)
    run_res <- bg_run(handle)

    expect_equal(run_res$summary$total_executed, 0L)
    expect_equal(state$fit_runs, 1L)
  })
})

describe("Parameter suggestions from hints", {
  bg_param_hint_fn <- getFromNamespace(
    "bg_parameter_suggestions_from_hint",
    "bayesgrove"
  )

  it("returns empty suggestions for NULL hint", {
    suggestions <- bg_param_hint_fn(NULL)
    expect_length(suggestions, 0)
  })

  it("returns empty suggestions for empty hint", {
    suggestions <- bg_param_hint_fn("")
    expect_length(suggestions, 0)
  })

  it("suggests non-centered parametrization for reparametrize hint", {
    suggestions <- bg_param_hint_fn(
      hint = "reparametrize",
      current_params = list(parametrization = "centered")
    )
    expect_equal(suggestions$parametrization, "non-centered")
  })

  it("suggests centered parametrization when already non-centered", {
    suggestions <- bg_param_hint_fn(
      hint = "reparametrize",
      current_params = list(parametrization = "non-centered")
    )
    expect_equal(suggestions$parametrization, "centered")
  })

  it("returns empty suggestions when parametrization param not present", {
    suggestions <- bg_param_hint_fn(
      hint = "reparametrize",
      current_params = list(iter = 1000)
    )
    expect_length(suggestions, 0)
  })

  it("suggests looser tolerance for adjust_tolerances hint", {
    suggestions <- bg_param_hint_fn(
      hint = "adjust_tolerances",
      current_params = list(tolerance = 1e-6)
    )
    expect_equal(suggestions$tolerance, 1e-4)
  })

  it("suggests even looser tolerance when already at 1e-4", {
    suggestions <- bg_param_hint_fn(
      hint = "adjust_tolerances",
      current_params = list(tolerance = 1e-4)
    )
    expect_equal(suggestions$tolerance, 1e-3)
  })

  it("suggests adapt_delta when tolerance not present", {
    suggestions <- bg_param_hint_fn(
      hint = "adjust_tolerances",
      current_params = list(adapt_delta = 0.8)
    )
    expect_equal(suggestions$adapt_delta, 0.95)
  })

  it("suggests higher adapt_delta when already at 0.95", {
    suggestions <- bg_param_hint_fn(
      hint = "adjust_tolerances",
      current_params = list(adapt_delta = 0.95)
    )
    expect_equal(suggestions$adapt_delta, 0.99)
  })

  it("suggests doubled iterations for increase_iterations hint", {
    suggestions <- bg_param_hint_fn(
      hint = "increase_iterations",
      current_params = list(iter = 1000)
    )
    expect_equal(suggestions$iter, 2000L)
  })

  it("returns empty suggestions for unknown hint", {
    suggestions <- bg_param_hint_fn(
      hint = "unknown_hint",
      current_params = list(parametrization = "centered")
    )
    expect_length(suggestions, 0)
  })
})
