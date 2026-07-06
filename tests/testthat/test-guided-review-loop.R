describe("Guided review loop case study", {
  make_case_study_fixture <- function() {
    tmp <- tempfile("guided-review-loop-")
    dir.create(tmp, recursive = TRUE)
    handle <- bg_init(
      path = tmp,
      project_name = "Guided Review Loop",
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      data.frame(
        group = rep(c("a", "b", "c"), each = 4),
        x = c(-1.2, -0.8, -0.3, 0.0, 0.1, 0.4, 0.8, 1.0, -0.5, -0.1, 0.6, 1.2),
        y = c(0.4, 0.8, 0.7, 0.9, 1.1, 1.3, 1.5, 1.7, 0.6, 0.9, 1.4, 1.8)
      )
    })

    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      parametrization <- node$params$parametrization %||% "non-centered"
      variant <- node$params$variant %||% "baseline"

      severity <- if (identical(parametrization, "centered")) {
        "warning"
      } else {
        "ok"
      }
      divergences <- if (identical(severity, "warning")) 11L else 0L
      score <- switch(
        variant,
        baseline = 0.90,
        centered_review = 0.82,
        repaired_branch = 0.95,
        robust_branch = 0.91,
        0.88
      )

      list(
        result = list(
          variant = variant,
          parametrization = parametrization,
          expected_elpd = score,
          rows = nrow(inputs[[1]])
        ),
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          passed = identical(severity, "ok"),
          severity = severity,
          metrics = list(
            divergences = divergences,
            expected_elpd = score,
            variant = variant,
            parametrization = parametrization
          )
        ))
      )
    })

    bg_register_node_kind(handle, "ppc", executor = function(node, inputs) {
      list(
        result = list(
          branch_variant = inputs[[1]]$variant,
          conclusion = "Posterior predictive check placeholder"
        )
      )
    })

    bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
      scores <- vapply(inputs, function(x) x$expected_elpd, numeric(1))
      labels <- names(inputs)
      ranking <- data.frame(
        fit = labels,
        expected_elpd = unname(scores),
        row.names = NULL
      )
      ranking <- ranking[order(ranking$expected_elpd, decreasing = TRUE), ]

      list(
        ranking = ranking,
        recommended = ranking$fit[[1]],
        summaries = list(list(
          summary_kind = "comparison_results",
          passed = TRUE,
          severity = "ok",
          metrics = list(recommended = ranking$fit[[1]])
        ))
      )
    })

    source_id <- bg_add_node(
      handle,
      kind = "source",
      label = "Synthetic cohort"
    )
    baseline_fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline fit",
      inputs = source_id,
      params = list(
        variant = "baseline",
        parametrization = "non-centered"
      )
    )
    baseline_ppc_id <- bg_add_node(
      handle,
      kind = "ppc",
      label = "Baseline PPC",
      inputs = baseline_fit_id
    )

    warning_record <- bg_branch(
      project = handle,
      node_id = baseline_fit_id,
      label = "Centered branch",
      continue = "ppc"
    )
    warning_branch <- list(
      branch = warning_record,
      continuation_nodes = warning_record$continuation_nodes
    )
    warning_fit_id <- warning_branch$branch$root_node_id
    warning_ppc_id <- warning_branch$continuation_nodes[[
      baseline_ppc_id
    ]]$clone_id

    bg_update_node(
      handle,
      warning_fit_id,
      label = "Centered branch",
      params = list(
        variant = "centered_review",
        parametrization = "centered"
      )
    )
    bg_update_node(handle, warning_ppc_id, label = "Centered branch PPC")

    bg_set_goal(
      project = handle,
      branch_id = warning_branch$branch$branch_id,
      kind = "observable_prediction",
      label = "Repair the centered branch without losing forecast skill",
      rationale = "This branch explores a questionable fit before accepting it."
    )

    robust_record <- bg_branch(
      project = handle,
      node_id = baseline_fit_id,
      label = "Robust branch",
      continue = "ppc"
    )
    robust_branch <- list(
      branch = robust_record,
      continuation_nodes = robust_record$continuation_nodes
    )
    robust_fit_id <- robust_branch$branch$root_node_id
    robust_ppc_id <- robust_branch$continuation_nodes[[
      baseline_ppc_id
    ]]$clone_id

    bg_update_node(
      handle,
      robust_fit_id,
      label = "Robust branch",
      params = list(
        variant = "robust_branch",
        parametrization = "non-centered"
      )
    )
    bg_update_node(handle, robust_ppc_id, label = "Robust branch PPC")

    bg_set_goal(
      project = handle,
      branch_id = robust_branch$branch$branch_id,
      kind = "observable_prediction",
      label = "Keep a clean alternative for later comparison",
      rationale = "A second branch lets the project compare explicit alternatives."
    )

    list(
      handle = handle,
      warning_branch = warning_branch$branch,
      warning_fit_id = warning_fit_id,
      warning_ppc_id = warning_ppc_id,
      robust_branch = robust_branch$branch,
      robust_fit_id = robust_fit_id
    )
  }

  find_action <- function(
    actions,
    kind,
    decision_type = NULL,
    template_ref = NULL
  ) {
    Filter(
      function(action) {
        identical(action$kind, kind) &&
          (is.null(decision_type) ||
            identical(action$payload$decision_type %||% NULL, decision_type)) &&
          (is.null(template_ref) ||
            identical(action$payload$template_ref %||% NULL, template_ref))
      },
      actions
    )[[1]]
  }

  it("covers holds, lineage, stale evidence, comparison, decisions, and report export", {
    fixture <- make_case_study_fixture()
    handle <- fixture$handle

    bg_run(handle, targets = fixture$warning_fit_id)

    warning_project_protocol <- bg_next_actions(handle, scope = "project")
    partitioned <- bg_partition_protocol_by_scope(
      warning_project_protocol,
      project = handle
    )
    warning_scope <- partitioned[[fixture$warning_branch$branch_id]]

    expect_true(any(vapply(
      warning_scope$obligations,
      function(x) identical(x$kind, "review_fit_criticism"),
      logical(1)
    )))

    held_plan <- bg_plan(
      handle,
      external_holds = warning_project_protocol$metadata$external_holds
    )
    expect_equal(
      held_plan$held_by_policy[[fixture$warning_ppc_id]],
      "Review fit criticism"
    )

    criticism_action <- find_action(
      bg_next_actions(
        handle,
        scope = "branch",
        branch_id = fixture$warning_branch$branch_id
      )$actions,
      kind = "record_decision",
      decision_type = "fit_criticism"
    )
    bg_record_decision(
      handle,
      scope = fixture$warning_branch$branch_id,
      prompt = "How should the centered branch be handled?",
      choice = "needs_reparametrization",
      rationale = "Repair the centered fit before it participates downstream.",
      kind = "fit_criticism",
      metadata = criticism_action$payload[c("node_ids", "summary_ids")]
    )

    repaired_record <- bg_branch(
      project = handle,
      node_id = fixture$warning_fit_id,
      label = "Repaired branch",
      continue = "ppc"
    )
    repaired_branch <- list(
      branch = repaired_record,
      continuation_nodes = repaired_record$continuation_nodes
    )
    repaired_fit_id <- repaired_branch$branch$root_node_id

    bg_update_node(
      handle,
      repaired_fit_id,
      label = "Repaired branch",
      params = list(
        variant = "centered_review",
        parametrization = "centered",
        revision = 1L
      )
    )

    bg_set_goal(
      project = handle,
      branch_id = repaired_branch$branch$branch_id,
      kind = "observable_prediction",
      label = "Resolve the warning and keep the branch comparable",
      rationale = "This branch continues the criticism loop with a repair."
    )

    expect_equal(
      bg_branch_lineage(handle, repaired_branch$branch$branch_id),
      fixture$warning_branch$branch_id
    )

    bg_run(handle, targets = repaired_fit_id)
    bg_update_node(
      handle,
      repaired_fit_id,
      label = "Repaired branch",
      params = list(
        variant = "repaired_branch",
        parametrization = "non-centered",
        revision = 2L
      )
    )
    bg_invalidate(handle, repaired_fit_id, recursive = TRUE)
    bg_run(handle, targets = repaired_fit_id)

    repaired_summaries <- bg_read_summaries(
      handle,
      scope = repaired_branch$branch$branch_id
    )
    repaired_summaries <- Filter(
      function(x) identical(x$node_id, repaired_fit_id),
      repaired_summaries
    )
    expect_equal(length(repaired_summaries), 2)
    expect_equal(
      sum(vapply(
        repaired_summaries,
        function(x) identical(x$severity, "warning"),
        logical(1)
      )),
      1
    )
    expect_equal(
      sum(vapply(
        repaired_summaries,
        function(x) identical(x$severity, "ok"),
        logical(1)
      )),
      1
    )
    expect_equal(
      sum(vapply(
        repaired_summaries,
        function(x) isTRUE(x$is_stale),
        logical(1)
      )),
      1
    )

    bg_run(handle, targets = fixture$robust_fit_id)

    comparison_action <- find_action(
      bg_next_actions(handle, scope = "project")$actions,
      kind = "create_node_from_template",
      template_ref = "branch_comparison"
    )
    comparison_node_id <- bg_add_node(
      handle,
      kind = "compare",
      label = "Compare repaired vs robust branch",
      inputs = comparison_action$payload$inputs
    )
    bg_run(handle, targets = comparison_node_id)

    comparison_decision_action <- find_action(
      bg_next_actions(handle, scope = "project")$actions,
      kind = "record_decision",
      decision_type = "model_comparison"
    )
    bg_record_decision(
      handle,
      scope = "project",
      prompt = "Which branch should anchor the final report?",
      choice = "prefer_repaired_branch",
      rationale = "The repaired branch is both clean and strongest in the comparison.",
      kind = "model_comparison",
      metadata = comparison_decision_action$payload[c(
        "fit_node_ids",
        "branch_ids",
        "summary_ids",
        "candidate_signature",
        "comparison_signature",
        "comparison_context"
      )]
    )

    repaired_disposition_action <- find_action(
      bg_next_actions(
        handle,
        scope = "branch",
        branch_id = repaired_branch$branch$branch_id
      )$actions,
      kind = "record_decision",
      decision_type = "branch_disposition"
    )
    bg_record_decision(
      handle,
      scope = repaired_branch$branch$branch_id,
      prompt = "Should the repaired branch be accepted?",
      choice = "accept",
      rationale = "Accept the branch preferred by the model comparison.",
      kind = "branch_disposition",
      metadata = list(
        disposition = "accept",
        summary_ids = repaired_disposition_action$payload$summary_ids,
        comparison_signature = repaired_disposition_action$payload$comparison_signature,
        comparison_context = repaired_disposition_action$payload$comparison_context
      )
    )

    robust_disposition_action <- find_action(
      bg_next_actions(
        handle,
        scope = "branch",
        branch_id = fixture$robust_branch$branch_id
      )$actions,
      kind = "record_decision",
      decision_type = "branch_disposition"
    )
    bg_record_decision(
      handle,
      scope = fixture$robust_branch$branch_id,
      prompt = "Should the robust branch be accepted?",
      choice = "reject",
      rationale = "Keep it as a documented alternative instead of the final path.",
      kind = "branch_disposition",
      metadata = list(
        disposition = "reject",
        summary_ids = robust_disposition_action$payload$summary_ids,
        comparison_signature = robust_disposition_action$payload$comparison_signature,
        comparison_context = robust_disposition_action$payload$comparison_context
      )
    )

    snapshot <- bg_snapshot(handle)
    decision_kinds <- vapply(snapshot$decisions, `[[`, character(1), "kind")

    expect_true("fit_criticism" %in% decision_kinds)
    expect_true("model_comparison" %in% decision_kinds)
    expect_equal(
      sum(decision_kinds == "branch_disposition"),
      2
    )

    report_path <- bg_export_report(
      handle,
      path = "case-study-report.md",
      format = "md"
    )
    expect_true(file.exists(report_path))
    expect_match(
      paste(readLines(report_path, warn = FALSE), collapse = "\n"),
      "Decision Provenance"
    )
  })
})
