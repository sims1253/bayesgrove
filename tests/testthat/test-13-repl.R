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
      alternatives = c("yes", "no")
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

  it("prints guide and actions for branch scopes", {
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
    branch <- bg_branch(handle, fit_id, label = "Alternative")

    expect_no_error(repl_ns("bg_repl_print_guide")(handle, branch$branch_id))
    expect_no_error(repl_ns("bg_repl_print_actions")(handle, branch$branch_id))
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

  it("builds decision rows in reverse chronological order", {
    handle <- structure(list(), class = "mock_handle")

    rows <- testthat::with_mocked_bindings(
      repl_ns("bg_repl_decision_rows")(handle, "project"),
      bg_read_decisions = function(project) {
        list(
          dec_first = list(
            decision_id = "dec_first",
            kind = "note",
            scope = "project",
            choice = "one",
            created_at = "2026-03-08T00:00:00Z",
            rationale = "Initial rationale."
          ),
          dec_second = list(
            decision_id = "dec_second",
            kind = "note",
            scope = "project",
            choice = "two",
            created_at = "2026-03-08T00:00:01Z",
            rationale = paste(rep("long rationale", 12), collapse = " ")
          )
        )
      },
      .package = "bayesgrove"
    )

    expect_equal(rows[[1]]$choice, "two")
    expect_equal(rows[[2]]$choice, "one")
    expect_gt(nchar(rows[[1]]$rationale), 100)
  })

  it("provides updated help lines with new commands", {
    help_lines <- repl_ns("bg_repl_help_lines")()

    expect_true(any(grepl("^scope", help_lines)))
    expect_true(any(grepl("^actions", help_lines)))
    expect_true(any(grepl("^do", help_lines)))
    expect_true(any(grepl("^goal", help_lines)))
    expect_true(any(grepl("^decisions", help_lines)))
    expect_true(any(grepl("^use", help_lines)))
    expect_true(any(grepl("^retire\\s", help_lines)))
    expect_true(any(grepl("^retire-branch", help_lines)))
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

  it("builds dashboard state with holds and recent decisions", {
    fixture <- make_workflow_hold_fixture()
    handle <- fixture$handle
    bg_run(handle, targets = fixture$fit_id, mode = "sync")
    bg_record_decision(
      project = handle,
      scope = "project",
      prompt = "Checkpoint",
      choice = "keep going",
      rationale = "Need a visible dashboard decision row."
    )

    dashboard <- repl_ns("bg_repl_dashboard_state")(handle, "project")

    expect_equal(dashboard$status$workflow_state, "blocked")
    expect_true(any(vapply(
      dashboard$held_nodes,
      function(row) identical(row$id, fixture$compare_id),
      logical(1)
    )))
    expect_equal(dashboard$decisions[[1]]$choice, "keep going")
  })

  it("filters dashboard gates to the active branch scope", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) NULL)
    bg_register_node_kind(handle, "ppc", executor = function(node, inputs) NULL)

    source_id <- bg_add_node(handle, kind = "source", label = "Data source")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline fit",
      inputs = source_id
    )
    ppc_id <- bg_add_node(
      handle,
      kind = "ppc",
      label = "Posterior check",
      inputs = fit_id
    )
    branch <- bg_branch(handle, fit_id, label = "Alternative fit")

    bg_add_gate(
      handle,
      from = source_id,
      to = fit_id,
      prompt = "Project gate",
      alternatives = c("yes", "no")
    )
    bg_add_gate(
      handle,
      from = fit_id,
      to = ppc_id,
      prompt = "Project downstream gate",
      alternatives = c("yes", "no")
    )
    bg_add_gate(
      handle,
      from = source_id,
      to = branch$root_node_id,
      prompt = "Branch gate",
      alternatives = c("yes", "no")
    )

    dashboard <- repl_ns("bg_repl_dashboard_state")(handle, branch$branch_id)
    prompts <- unname(vapply(
      dashboard$pending_gates,
      `[[`,
      character(1),
      "prompt"
    ))

    expect_equal(prompts, "Branch gate")
  })

  it("builds lineage rows including the current branch", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })
    source_id <- bg_add_node(handle, kind = "source", label = "Data source")
    parent <- bg_branch(handle, source_id, label = "Parent branch")
    child <- bg_branch(handle, parent$root_node_id, label = "Child branch")

    rows <- repl_ns("bg_repl_lineage_rows")(handle, child$branch_id)

    expect_equal(rows[[1]]$branch_id, parent$branch_id)
    expect_false(rows[[1]]$current)
    expect_equal(rows[[1]]$depth, 0L)
    expect_equal(rows[[2]]$branch_id, child$branch_id)
    expect_true(rows[[2]]$current)
    expect_equal(rows[[2]]$depth, 1L)
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

  it("executes the selected suggested action through the confirmation seam", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    state <- new.env(parent = emptyenv())

    action <- list(
      action_id = "act_test",
      kind = "record_decision",
      scope = "branch:test",
      title = "Record an inferential goal",
      explanation = list(why_now = "Branch guidance is blocked."),
      basis = list(node_ids = "node_123"),
      payload = list(decision_type = "goal_update")
    )

    result <- testthat::with_mocked_bindings(
      repl_ns("bg_repl_execute_suggested_action")(
        handle,
        scope = "project",
        idx = 1L,
        heading = "Next Action"
      ),
      bg_next_actions = function(...) {
        list(actions = list(action), obligations = list())
      },
      bg_partition_protocol_by_scope = function(...) {
        list(summary = list(scopes = "project"))
      },
      bg_scope_label = function(project, scope) scope,
      bg_repl_readline = function(prompt = "") "y",
      bg_repl_execute_action = function(project, action, scope = "project") {
        state$executed <- list(action_id = action$action_id, scope = scope)
        "executed"
      },
      .package = "bayesgrove"
    )

    expect_true(result$executed)
    expect_equal(result$action$action_id, "act_test")
    expect_equal(state$executed$scope, "project")
  })

  it("returns without executing when action preview is declined", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    state <- new.env(parent = emptyenv())
    state$executed <- FALSE

    action <- list(
      action_id = "act_test",
      kind = "record_decision",
      scope = "project",
      title = "Record something"
    )

    result <- testthat::with_mocked_bindings(
      repl_ns("bg_repl_execute_suggested_action")(handle, scope = "project"),
      bg_next_actions = function(...) {
        list(actions = list(action), obligations = list())
      },
      bg_partition_protocol_by_scope = function(...) {
        list(summary = list(scopes = "project"))
      },
      bg_scope_label = function(project, scope) scope,
      bg_repl_readline = function(prompt = "") "n",
      bg_repl_execute_action = function(project, action, scope = "project") {
        state$executed <- TRUE
      },
      .package = "bayesgrove"
    )

    expect_false(result$executed)
    expect_false(state$executed)
  })

  it("parses export arguments for format and path", {
    default_export <- repl_ns("bg_repl_parse_export_args")("")
    markdown_export <- repl_ns("bg_repl_parse_export_args")(
      "md reports/workflow.md"
    )
    inferred_export <- repl_ns("bg_repl_parse_export_args")(
      "reports/workflow.html"
    )

    expect_equal(default_export$format, "html")
    expect_null(default_export$path)
    expect_equal(markdown_export$format, "md")
    expect_equal(markdown_export$path, "reports/workflow.md")
    expect_equal(inferred_export$format, "html")
    expect_equal(inferred_export$path, "reports/workflow.html")
  })

  it("passes parsed export arguments through to bg_export_report", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    state <- new.env(parent = emptyenv())

    path <- testthat::with_mocked_bindings(
      repl_ns("bg_repl_export_report_command")(
        handle,
        "md reports/workflow.md"
      ),
      bg_export_report = function(
        project,
        path = NULL,
        format = c("html", "md"),
        out_file = NULL
      ) {
        state$captured <- list(path = path, format = format[[1]])
        file.path(project@path, path)
      },
      .package = "bayesgrove"
    )

    expect_equal(state$captured$format, "md")
    expect_equal(state$captured$path, "reports/workflow.md")
    expect_match(path, "reports/workflow\\.md$")
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

    bg_register_node_kind(
      handle,
      "data_prep",
      executor = function(node, inputs) {
        data.frame(group = rep(1:5, each = 10), y = seq_len(50))
      }
    )
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
    state <- new.env(parent = emptyenv())
    state$answer_idx <- 0L
    branch_result <- testthat::with_mocked_bindings(
      bg_apply_template_action(handle, action, "project", interactive = TRUE),
      bg_repl_readline = function(prompt = "") {
        state$answer_idx <- state$answer_idx + 1L
        answers[[state$answer_idx]]
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

  it("executes branch-scoped decision actions against the action scope", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
    )

    bg_register_node_kind(handle, "fit")
    fit_id <- bg_add_node(handle, kind = "fit", label = "Fit")
    branch <- bg_branch(handle, fit_id, label = "Alternative")

    action <- list(
      action_id = "test_goal_action",
      kind = "record_decision",
      scope = branch$branch_id,
      title = "Record an inferential goal",
      payload = list(
        decision_type = "goal_update",
        allowed_goal_kinds = c("observable_prediction", "latent_inference")
      )
    )

    answers <- c(
      "1",
      "Revised comparison goal",
      "Keep this branch in comparison."
    )
    state <- new.env(parent = emptyenv())
    state$answer_idx <- 0L

    testthat::with_mocked_bindings(
      repl_ns("bg_repl_execute_action")(handle, action, scope = "project"),
      bg_repl_readline = function(prompt = "") {
        state$answer_idx <- state$answer_idx + 1L
        answers[[state$answer_idx]]
      },
      .package = "bayesgrove"
    )

    goal <- bg_get_goal(handle, branch$branch_id)
    expect_equal(goal$kind, "observable_prediction")
    expect_equal(goal$label, "Revised comparison goal")
    expect_null(bg_get_goal(handle, "project"))
  })

  it("creates diagnostic checks from template-backed actions", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "fit")
    bg_register_node_kind(handle, "check")

    fit_id <- bg_add_node(handle, kind = "fit", label = "Baseline Fit")
    action <- list(
      action_id = "test_diagnostic_check",
      kind = "create_node_from_template",
      scope = "project",
      title = "Create diagnostic check",
      basis = list(node_ids = fit_id),
      payload = list(
        template_ref = "diagnostic_check",
        source_node_id = fit_id
      )
    )

    result <- testthat::with_mocked_bindings(
      repl_ns("bg_repl_execute_action")(handle, action, scope = "project"),
      bg_repl_readline = function(prompt = "") "",
      .package = "bayesgrove"
    )

    graph <- bg_read_graph(handle)
    expect_true(result$node_id %in% names(graph$nodes))
    expect_equal(graph$nodes[[result$node_id]]$kind, "check")
    expect_equal(
      graph$nodes[[result$node_id]]$label,
      "Diagnostics: Baseline Fit"
    )

    edges <- Filter(function(e) e$to == result$node_id, graph$edges)
    expect_length(edges, 1L)
    expect_equal(edges[[1]]$from, fit_id)
  })

  it("preserves backward compatibility for branch_comparison template actions", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "fit")
    bg_register_node_kind(handle, "compare")

    fit_a <- bg_add_node(handle, kind = "fit", label = "Fit A")
    fit_b <- bg_add_node(handle, kind = "fit", label = "Fit B")
    action <- list(
      action_id = "test_branch_comparison",
      kind = "create_node_from_template",
      scope = "project",
      title = "Create comparison node",
      basis = list(node_ids = c(fit_a, fit_b)),
      payload = list(
        template_ref = "branch_comparison",
        inputs = c(fit_a, fit_b)
      )
    )

    result <- testthat::with_mocked_bindings(
      bg_apply_template_action(handle, action, "project", interactive = TRUE),
      bg_repl_readline = function(prompt = "") "",
      .package = "bayesgrove"
    )

    graph <- bg_read_graph(handle)
    expect_equal(graph$nodes[[result$node_id]]$kind, "compare")

    edges <- Filter(function(e) e$to == result$node_id, graph$edges)
    expect_equal(
      unname(sort(vapply(edges, `[[`, character(1), "from"))),
      sort(c(fit_a, fit_b))
    )
  })

  it("executes branch_and_modify_fit through bg_branch_with_continuation", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "fit")
    bg_register_node_kind(handle, "check")
    bg_register_node_kind(handle, "ppc")

    fit_id <- bg_add_node(handle, kind = "fit", label = "Fit")
    bg_add_node(handle, kind = "check", label = "Diagnostics", inputs = fit_id)
    bg_add_node(handle, kind = "ppc", label = "PPC", inputs = fit_id)

    action <- list(
      action_id = "test_template_branch",
      kind = "branch_and_modify",
      scope = "project",
      title = "Branch and modify fit",
      basis = list(node_ids = fit_id),
      payload = list(
        template_ref = "branch_and_modify_fit",
        source_node_id = fit_id,
        continuation_kinds = c("check", "ppc")
      )
    )

    branch_with_continuation <- repl_ns("bg_branch_with_continuation")
    state <- new.env(parent = emptyenv())

    result <- testthat::with_mocked_bindings(
      repl_ns("bg_repl_execute_action")(handle, action, scope = "project"),
      bg_repl_readline = local({
        answers <- c("", "n")
        state <- new.env(parent = emptyenv())
        state$idx <- 0L
        function(prompt = "") {
          state$idx <- state$idx + 1L
          answers[[state$idx]]
        }
      }),
      bg_branch_with_continuation = function(
        project,
        node_id,
        label = NULL,
        copy_params = TRUE,
        continuation_kinds = NULL,
        continuation_depth = 1L
      ) {
        state$called <- list(
          node_id = node_id,
          label = label,
          continuation_kinds = continuation_kinds
        )
        branch_with_continuation(
          project = project,
          node_id = node_id,
          label = label,
          copy_params = copy_params,
          continuation_kinds = continuation_kinds,
          continuation_depth = continuation_depth
        )
      },
      .package = "bayesgrove"
    )

    expect_equal(state$called$node_id, fit_id)
    expect_equal(state$called$continuation_kinds, c("check", "ppc"))
    expect_true(startsWith(result$branch$branch_id, "branch:"))
  })

  it("records review_decision template decisions with intended basis", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    action <- list(
      action_id = "test_review_decision",
      kind = "record_decision",
      scope = "project",
      title = "Record model comparison decision",
      payload = list(
        template_ref = "review_decision",
        decision_type = "model_comparison",
        fit_node_ids = c("node_a", "node_b"),
        branch_ids = c("project", "branch:alternative"),
        summary_ids = c("sum_1", "sum_2"),
        candidate_signature = "cand_sig",
        comparison_signature = "cmp_sig",
        comparison_context = list(
          candidate_signature = "cand_sig",
          comparison_signature = "cmp_sig"
        )
      )
    )

    decision <- testthat::with_mocked_bindings(
      repl_ns("bg_repl_execute_action")(handle, action, scope = "project"),
      bg_repl_readline = local({
        answers <- c("prefer_branch_b", "Cleaner diagnostics and better fit.")
        state <- new.env(parent = emptyenv())
        state$idx <- 0L
        function(prompt = "") {
          state$idx <- state$idx + 1L
          answers[[state$idx]]
        }
      }),
      .package = "bayesgrove"
    )

    expect_equal(decision$kind, "model_comparison")
    expect_equal(decision$choice, "prefer_branch_b")
    expect_equal(decision$metadata$template_ref, "review_decision")
    expect_equal(decision$metadata$fit_node_ids, c("node_a", "node_b"))
    expect_equal(decision$metadata$summary_ids, c("sum_1", "sum_2"))
    expect_equal(decision$metadata$comparison_signature, "cmp_sig")
  })

  it("supports focused demo checkpoints for comparison and disposition", {
    comparison_demo <- test_demo_repl_fixture("comparison_ready")
    comparison_actions <- bg_next_actions(
      comparison_demo$handle,
      scope = "project"
    )$actions
    comparison_template <- Filter(
      function(action) {
        identical(action$kind, "create_node_from_template") &&
          identical(action$payload$template_ref %||% NULL, "branch_comparison")
      },
      comparison_actions
    )
    expect_length(comparison_template, 1L)
    expect_equal(comparison_demo$initial_scope, "project")

    disposition_demo <- test_demo_repl_fixture("disposition_ready")
    disposition_actions <- bg_next_actions(
      disposition_demo$handle,
      scope = "branch",
      branch_id = disposition_demo$revised_branch$branch_id
    )$actions
    disposition_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(
            action$payload$decision_type %||% NULL,
            "branch_disposition"
          )
      },
      disposition_actions
    )
    expect_length(disposition_action, 1L)
    expect_equal(
      disposition_demo$initial_scope,
      disposition_demo$revised_branch$branch_id
    )
  })

  it("supports the scripted demo flow through comparison and branch disposition", {
    demo <- test_demo_repl_fixture("warning_branch")
    handle <- demo$handle

    initial_actions <- bg_next_actions(handle, scope = "project")
    expect_equal(
      unname(vapply(initial_actions$actions, `[[`, character(1), "title")),
      c(
        "Record a computation review",
        "Branch and modify to resolve diagnostics",
        "Record a fit criticism review"
      )
    )

    revised <- testthat::with_mocked_bindings(
      repl_ns("bg_repl_execute_action")(
        handle,
        initial_actions$actions[[2]],
        scope = "project"
      ),
      bg_repl_readline = local({
        answers <- c("Fit Non-Centered Revision", "Y")
        state <- new.env(parent = emptyenv())
        state$idx <- 0L
        function(prompt = "") {
          state$idx <- state$idx + 1L
          answers[[state$idx]]
        }
      }),
      .package = "bayesgrove"
    )

    revised_root <- revised$branch$root_node_id
    revised_branch <- revised$branch$branch_id

    run_res <- bg_run(handle, targets = revised_root, mode = "sync")
    expect_equal(run_res$status, "succeeded")

    stale_node <- repl_ns("bg_repl_find_node")(
      handle,
      "Fit Centered Parametrization"
    )
    bg_retire_node(handle, stale_node, recursive = TRUE)

    after_invalidate <- bg_next_actions(handle, scope = "project")
    expect_equal(
      unname(vapply(after_invalidate$actions, `[[`, character(1), "title")),
      "Create comparison node"
    )

    comparison <- testthat::with_mocked_bindings(
      repl_ns("bg_repl_execute_action")(
        handle,
        after_invalidate$actions[[1]],
        scope = "project"
      ),
      bg_repl_readline = function(prompt = "") {
        "Compare Baseline vs Revision"
      },
      .package = "bayesgrove"
    )

    compare_run <- bg_run(handle, targets = comparison$node_id, mode = "sync")
    expect_equal(compare_run$status, "succeeded")

    after_compare <- bg_next_actions(handle, scope = "project")
    expect_equal(
      unname(vapply(after_compare$actions, `[[`, character(1), "title")),
      c("Record model comparison decision", "Record branch disposition")
    )

    expect_no_error(repl_ns("bg_repl_print_guide")(handle, revised_branch))
    expect_no_error(repl_ns("bg_repl_print_actions")(handle, revised_branch))

    model_comparison <- testthat::with_mocked_bindings(
      repl_ns("bg_repl_execute_action")(
        handle,
        after_compare$actions[[1]],
        scope = "project"
      ),
      bg_repl_readline = local({
        answers <- c(
          "prefer_revised_branch",
          "The revised branch resolves the diagnostic warning cleanly."
        )
        state <- new.env(parent = emptyenv())
        state$idx <- 0L
        function(prompt = "") {
          state$idx <- state$idx + 1L
          answers[[state$idx]]
        }
      }),
      .package = "bayesgrove"
    )
    expect_s3_class(model_comparison, "bg_decision_record")

    after_model_comparison <- bg_next_actions(handle, scope = "project")
    expect_equal(
      unname(vapply(
        after_model_comparison$actions,
        `[[`,
        character(1),
        "title"
      )),
      "Record branch disposition"
    )

    branch_disposition <- testthat::with_mocked_bindings(
      repl_ns("bg_repl_execute_action")(
        handle,
        after_model_comparison$actions[[1]],
        scope = "project"
      ),
      bg_repl_readline = local({
        answers <- c(
          "1",
          "Promote the revised branch as the accepted analysis path."
        )
        state <- new.env(parent = emptyenv())
        state$idx <- 0L
        function(prompt = "") {
          state$idx <- state$idx + 1L
          answers[[state$idx]]
        }
      }),
      .package = "bayesgrove"
    )
    expect_s3_class(branch_disposition, "bg_decision_record")

    final_actions <- bg_next_actions(handle, scope = "project")
    expect_length(final_actions$actions, 0L)
    expect_length(final_actions$obligations, 0L)
  })

  it("prints dashboard without error", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    expect_no_error(repl_ns("bg_repl_print_dashboard")(handle, "project"))
  })

  it("renders a dashboard from precomputed state without refetching rows", {
    handle <- structure(list(), class = "mock_handle")
    dashboard <- list(
      scope = "branch:test",
      status = list(
        workflow_state = "blocked",
        runnable_nodes = 0L,
        cached_nodes = 1L,
        blocked_nodes = 1L,
        pending_gates = 1L,
        active_jobs = 0L,
        total_nodes = 2L,
        messages = character()
      ),
      guide = list(
        scope = "branch:test",
        scope_label = "Test branch",
        actions = list(obligations = list(), actions = list()),
        partitioned = list(summary = list(scopes = "branch:test"))
      ),
      held_nodes = list(),
      decisions = list(),
      pending_gates = list(),
      lineage = list(
        list(
          branch_id = "branch:test",
          label = "Test branch",
          depth = 0L,
          current = TRUE
        )
      )
    )

    expect_no_error(testthat::with_mocked_bindings(
      repl_ns("bg_repl_print_dashboard_state")(dashboard, handle),
      bg_repl_guide_state = function(...) cli::cli_abort("guide recomputed"),
      bg_repl_held_node_rows = function(...) cli::cli_abort("holds recomputed"),
      bg_repl_decision_rows = function(...) {
        cli::cli_abort("decisions recomputed")
      },
      bg_repl_pending_gates = function(...) cli::cli_abort("gates recomputed"),
      bg_repl_lineage_rows = function(...) cli::cli_abort("lineage recomputed"),
      .package = "bayesgrove"
    ))
  })

  it("prints lineage without error", {
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

    # In project scope it returns NULL
    expect_null(repl_ns("bg_repl_print_lineage")(handle, "project"))

    # In branch scope it prints lineage
    expect_no_error(repl_ns("bg_repl_print_lineage")(handle, branch$branch_id))
  })

  it("includes new commands in help lines", {
    help_lines <- repl_ns("bg_repl_help_lines")()
    expect_true(any(grepl("^dashboard\\s+", help_lines)))
    expect_true(any(grepl("^next\\s+", help_lines)))
    expect_true(any(grepl("^lineage\\s+", help_lines)))
    expect_true(any(grepl("^export\\s+", help_lines)))
  })
})
