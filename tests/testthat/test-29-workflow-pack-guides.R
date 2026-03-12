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
})
