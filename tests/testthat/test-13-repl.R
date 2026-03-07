repl_ns <- function(name) {
  getFromNamespace(name, "bayesgrove")
}

decision_ns <- function(name) {
  getFromNamespace(name, "bayesgrove")
}

describe("Interactive REPL", {
  it("aborts when run non-interactively", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    # testthat runs non-interactively by default, so it should error
    expect_error(bg_repl(handle), "must be run in an interactive R session")
  })

  it("formats status rows for display", {
    rows <- repl_ns("bg_repl_status_rows")(list(
      workflow_state = "blocked",
      runnable_nodes = 0L,
      cached_nodes = 6L,
      blocked_nodes = 1L,
      pending_gates = 1L,
      active_jobs = 0L,
      total_nodes = 7L
    ))

    expect_equal(rows[["Workflow state"]], "Blocked")
    expect_equal(rows[["Runnable now"]], "0")
    expect_equal(rows[["Cached nodes"]], "6")
    expect_equal(rows[["Total nodes"]], "7")
  })

  it("builds node rows with cache and policy-hold details", {
    fixture <- make_workflow_hold_fixture()
    handle <- fixture$handle

    bg_run(handle, targets = fixture$fit_id, mode = "sync")
    rows <- repl_ns("bg_repl_node_rows")(handle)
    rows_by_id <- stats::setNames(rows, vapply(rows, `[[`, character(1), "id"))

    expect_equal(rows_by_id[[fixture$source_id]]$state, "cached")
    expect_equal(rows_by_id[[fixture$fit_id]]$state, "cached")
    expect_equal(rows_by_id[[fixture$compare_id]]$state, "held")
    expect_match(rows_by_id[[fixture$compare_id]]$detail, "^Policy hold:")
  })

  it("builds gate rows with route context", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      NULL
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data source")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline fit",
      inputs = source_id
    )
    gate <- bg_add_gate(
      handle,
      from = source_id,
      to = fit_id,
      prompt = "Proceed to model fitting?",
      options = c("yes", "no")
    )

    rows <- repl_ns("bg_repl_gate_rows")(bg_pending_gates(handle))

    expect_length(rows, 1L)
    expect_equal(rows[[1]]$id, gate$id)
    expect_equal(rows[[1]]$route, "Data source -> Baseline fit")
    expect_equal(rows[[1]]$options, "yes, no")
  })

  it("lists available branches", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      NULL
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data source")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit",
      inputs = source_id
    )

    # Initially no branches
    branches <- repl_ns("bg_repl_list_branches")(handle)
    expect_length(branches, 0L)

    # Create a branch
    branch <- bg_branch(handle, fit_id, label = "Test branch")

    branches <- repl_ns("bg_repl_list_branches")(handle)
    expect_length(branches, 1L)
    expect_equal(branches[[1]], branch$branch_id)
  })

  it("formats scope label correctly", {
    expect_equal(repl_ns("bg_repl_scope_label")(NULL), "project")
    expect_equal(repl_ns("bg_repl_scope_label")("project"), "project")
    expect_equal(
      repl_ns("bg_repl_scope_label")("branch:abc123"),
      "branch:abc123"
    )
  })

  it("coerces scientific notation for parameter updates", {
    expect_equal(repl_ns("bg_repl_coerce_param_value")("1e-4"), 1e-4)
    expect_equal(repl_ns("bg_repl_coerce_param_value")("-2.5"), -2.5)
    expect_equal(
      repl_ns("bg_repl_coerce_param_value")("non-centered"),
      "non-centered"
    )
  })

  it("prints guide with scope context", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data source")

    # Should not error for project scope
    expect_no_error(repl_ns("bg_repl_print_guide")(handle, "project"))
  })

  it("prints actions with scope context", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data source")

    # Should return list (possibly empty)
    actions <- repl_ns("bg_repl_print_actions")(handle, "project")
    expect_true(is.list(actions))
  })

  it("prints actions when payloads contain vectors and nested lists", {
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
              summary_kind = "hmc_diagnostics",
              passed = FALSE,
              severity = "warning",
              metrics = list(divergences = 15)
            )
          )
        )
      }
    )
    bg_register_node_kind(handle, "ppc", executor = function(node, inputs) {
      list(ppc = TRUE)
    })

    n_fit <- bg_add_node(handle, kind = "fit", label = "Fit Centered")
    bg_add_node(handle, kind = "ppc", label = "PPC", inputs = n_fit)
    bg_run(handle, mode = "sync")

    expect_no_error(repl_ns("bg_repl_print_actions")(handle, "project"))
  })

  it("prints scope with available branches", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data source")

    # Should not error
    expect_no_error(repl_ns("bg_repl_print_scope")(handle, "project"))
  })

  it("prints goal for branch scope", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data source")
    branch <- bg_branch(handle, source_id, label = "Test")

    # Should not error for project scope (no goal)
    expect_no_error(repl_ns("bg_repl_print_goal")(handle, "project"))
  })

  it("prints decisions for scope", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data source")

    # Record a decision
    bg_record_decision(
      project = handle,
      scope = "project",
      prompt = "Test decision",
      choice = "yes",
      rationale = "Testing"
    )

    # Should not error
    expect_no_error(repl_ns("bg_repl_print_decisions")(handle, "project"))
  })

  it("provides updated help lines with new commands", {
    help_lines <- repl_ns("bg_repl_help_lines")()

    expect_true(any(grepl("^scope", help_lines)))
    expect_true(any(grepl("^actions", help_lines)))
    expect_true(any(grepl("^do", help_lines)))
    expect_true(any(grepl("^goal", help_lines)))
    expect_true(any(grepl("^decisions", help_lines)))
    expect_true(any(grepl("^use", help_lines)))
  })

  it("filters nodes by branch scope", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      NULL
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data source")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit",
      inputs = source_id
    )

    # Create a branch
    branch <- bg_branch(handle, fit_id, label = "Test branch")
    branch_root <- branch$root_node_id

    # Print nodes for project scope - should include all
    all_graph <- repl_ns("bg_repl_print_nodes")(handle, "project")
    all_nodes <- names(all_graph$nodes)
    expect_true(source_id %in% all_nodes)
    expect_true(fit_id %in% all_nodes)
    expect_true(branch_root %in% all_nodes)

    # Print nodes for branch scope - should only include branch nodes
    branch_graph <- repl_ns("bg_repl_print_nodes")(handle, branch$branch_id)
    branch_nodes <- names(branch_graph$nodes)
    expect_true(branch_root %in% branch_nodes)
    expect_false(source_id %in% branch_nodes)
    expect_false(fit_id %in% branch_nodes)
    expect_length(branch_graph$edges, 0)
  })

  it("provides post-action hints", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data source")

    # Should return hints without error
    hints <- repl_ns("bg_repl_post_action_hint")(
      handle,
      "project",
      "record_decision"
    )
    expect_true(is.character(hints))
    expect_true(length(hints) > 0)
  })

  it("returns post-action hints without raw cli markup", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })

    bg_add_node(handle, kind = "source", label = "Data source")

    hints <- repl_ns("bg_repl_post_action_hint")(
      handle,
      "project",
      "branch_and_modify"
    )

    expect_false(any(grepl("\\{\\.code", hints)))
    expect_false(any(grepl("\\{cli::", hints)))
  })

  it("shows goal status in branches list", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data source")
    branch <- bg_branch(handle, source_id, label = "Test branch")

    # Get branch list - should show no goal
    branches <- bg_list_branches(handle)
    expect_false(isTRUE(branches[[branch$branch_id]]$has_goal))

    # Set a goal
    bg_set_goal(
      project = handle,
      branch_id = branch$branch_id,
      kind = "observable_prediction",
      label = "Predict outcomes",
      rationale = "Testing"
    )

    # Now should show goal
    branches <- bg_list_branches(handle)
    expect_true(isTRUE(branches[[branch$branch_id]]$has_goal))
  })

  it("executes branch_and_modify with enhanced payload", {
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
              summary_kind = "hmc_diagnostics",
              passed = FALSE,
              severity = "warning",
              metrics = list(divergences = 15)
            )
          )
        )
      }
    )
    bg_register_node_kind(handle, "check", executor = function(node, inputs) {
      list(checked = TRUE)
    })
    bg_register_node_kind(handle, "ppc", executor = function(node, inputs) {
      list(ppc = TRUE)
    })

    n_fit <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit Centered",
      params = list(parametrization = "centered")
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

    bg_run(handle, mode = "sync")

    # Get the branch_and_modify action
    result <- bg_next_actions(handle)
    branch_action <- Filter(
      function(a) identical(a$kind, "branch_and_modify"),
      result$actions
    )[[1]]

    # Verify the action has the enhanced payload
    expect_equal(branch_action$payload$source_node_id, n_fit)
    expect_equal(branch_action$payload$modification_hint, "reparametrize")
    expect_equal(
      branch_action$payload$parameter_suggestions$parametrization,
      "non-centered"
    )
    expect_true("check" %in% branch_action$payload$continuation_kinds)
    expect_true("ppc" %in% branch_action$payload$continuation_kinds)
  })

  it("creates continuation nodes when executing branch_and_modify action", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
    )

    bg_register_node_kind(handle, "fit")
    bg_register_node_kind(handle, "check")
    bg_register_node_kind(handle, "ppc")

    n_fit <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit",
      params = list(parametrization = "centered")
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

    # Create a synthetic action for testing
    action <- list(
      action_id = "test_branch_action",
      kind = "branch_and_modify",
      scope = "project",
      title = "Test Branch",
      basis = list(node_ids = n_fit),
      payload = list(
        source_node_id = n_fit,
        modification_hint = "reparametrize",
        default_label = "Fit (revised)",
        parameter_suggestions = list(parametrization = "non-centered"),
        continuation_kinds = c("check", "ppc"),
        auto_run = FALSE
      )
    )

    # Execute via bg_branch_with_continuation (simulating REPL execution)
    result <- bg_branch_with_continuation(
      project = handle,
      node_id = n_fit,
      label = action$payload$default_label,
      continuation_kinds = action$payload$continuation_kinds
    )

    # Verify branch was created
    expect_true(!is.null(result$branch))
    expect_true(startsWith(result$branch$branch_id, "branch:"))

    # Verify continuation nodes were created
    expect_equal(length(result$continuation_nodes), 2)
    expect_true(n_check %in% names(result$continuation_nodes))
    expect_true(n_ppc %in% names(result$continuation_nodes))

    # Verify graph has the cloned nodes
    graph <- bg_read_graph(handle)
    check_clone <- result$continuation_nodes[[n_check]]
    ppc_clone <- result$continuation_nodes[[n_ppc]]

    expect_true(check_clone$clone_id %in% names(graph$nodes))
    expect_true(ppc_clone$clone_id %in% names(graph$nodes))
    expect_equal(graph$nodes[[check_clone$clone_id]]$kind, "check")
    expect_equal(graph$nodes[[ppc_clone$clone_id]]$kind, "ppc")

    # Verify parameter suggestions can be applied
    branch_root <- result$branch$root_node_id
    bg_update_node(
      handle,
      branch_root,
      params = action$payload$parameter_suggestions
    )

    updated_graph <- bg_read_graph(handle)
    expect_equal(
      updated_graph$nodes[[branch_root]]$params$parametrization,
      "non-centered"
    )
  })

  it("treats guided remediation branches as technical revisions in the demo flow", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
    )

    bg_register_node_kind(handle, "data_prep", executor = function(node, inputs) {
      data.frame(group = rep(1:5, each = 10), y = seq_len(50))
    })
    bg_register_node_kind(handle, "compile", executor = function(node, inputs) {
      list(model = "compiled_binary")
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      param_val <- node$params$parametrization %||% "centered"
      if (identical(param_val, "non-centered")) {
        return(list(
          status = "fit_completed",
          summaries = list(
            list(
              summary_kind = "hmc_diagnostics",
              severity = "ok",
              passed = TRUE,
              metrics = list(divergences = 0)
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
            metrics = list(divergences = 15)
          )
        )
      )
    })
    bg_register_node_kind(handle, "ppc", executor = function(node, inputs) {
      list(plot = "ppc_density_plot")
    })

    n_data <- bg_add_node(handle, "data_prep", label = "Load Survey Data")
    n_compile <- bg_add_node(handle, "compile", label = "Compile Model")
    n_fit <- bg_add_node(
      handle,
      "fit",
      label = "Fit Centered Parametrization",
      inputs = c(n_data, n_compile),
      params = list(parametrization = "centered")
    )
    bg_add_node(
      handle,
      "ppc",
      label = "Posterior Predictive Check",
      inputs = n_fit
    )

    bg_run(handle, mode = "sync")

    action <- Filter(
      function(a) identical(a$kind, "branch_and_modify"),
      bg_next_actions(handle)$actions
    )[[1]]

    answers <- c("Fit Non-Centered", "Y")
    answer_idx <- 0L
    branch_result <- testthat::with_mocked_bindings(
      repl_ns("bg_repl_execute_branch_and_modify")(handle, action, "project"),
      bg_repl_readline = function(prompt = "") {
        answer_idx <<- answer_idx + 1L
        answers[[answer_idx]]
      },
      .package = "bayesgrove"
    )

    branch_root <- branch_result$branch$root_node_id
    expect_equal(
      bg_read_graph(handle)$nodes[[branch_root]]$params$parametrization,
      "non-centered"
    )
    run_res <- bg_run(handle, targets = branch_root, mode = "sync")
    expect_equal(run_res$status, "succeeded")
    expect_no_error(bg_result(handle, branch_root))

    decisions <- decision_ns("bg_read_decisions")(handle)
    expect_true(length(decisions) >= 1)

    bg_invalidate(handle, n_fit)
    obligations <- bg_next_actions(handle, scope = "project")$obligations
    expect_false(any(vapply(
      obligations,
      function(o) identical(o$kind, "set_inferential_goal"),
      logical(1)
    )))
  })
})
