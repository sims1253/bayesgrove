describe("Workflow source references", {
  make_fit_project <- function(
    workflow_packs = "bayesguide.default_bayesian",
    summary_kind = "hmc_diagnostics",
    severity = "warning",
    cleanup_env = parent.frame()
  ) {
    tmp <- tempfile("workflow-sources-")
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
          summary_kind = summary_kind,
          passed = identical(severity, "ok"),
          severity = severity,
          metrics = list(
            divergent_transitions = 4,
            variant = variant
          )
        ))
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Fit",
      inputs = source_id,
      params = list(variant = "baseline")
    )
    bg_run(handle, targets = fit_id, mode = "sync")

    list(handle = handle, source_id = source_id, fit_id = fit_id)
  }

  make_comparison_project <- function(cleanup_env = parent.frame()) {
    fixture <- make_fit_project(
      workflow_packs = "bayesgrove.model_selection",
      severity = "ok",
      cleanup_env = cleanup_env
    )
    handle <- fixture$handle

    branch <- bg_branch(handle, fixture$fit_id, label = "Alternative fit")
    bg_update_node(
      handle,
      branch$root_node_id,
      params = list(variant = "alternative")
    )
    bg_run(handle, targets = branch$root_node_id, mode = "sync")

    list(handle = handle, fit_id = fixture$fit_id, branch = branch)
  }

  it("expands source keys into canonical references", {
    refs <- bg_workflow_references(c("workflow_core", "sbc"))

    expect_true(any(grepl("Bayesian Workflow", refs, fixed = TRUE)))
    expect_true(any(grepl("Simulation-Based Calibration", refs, fixed = TRUE)))
  })

  it("attaches primary sources to default computation review guidance", {
    fixture <- make_fit_project()

    next_actions <- bg_next_actions(fixture$handle, scope = "project")
    obligation <- Filter(
      function(x) identical(x$kind, "review_computation_validity"),
      next_actions$obligations
    )[[1]]
    action <- Filter(
      function(x) identical(x$payload$decision_type %||% NULL, "computation_review"),
      next_actions$actions
    )[[1]]

    expect_true(any(grepl("Bayesian Workflow", obligation$explanation$references, fixed = TRUE)))
    expect_true(any(grepl("Hamiltonian Monte Carlo", obligation$explanation$references, fixed = TRUE)))
    expect_equal(action$explanation$references, obligation$explanation$references)
  })

  it("attaches model-comparison sources to phase10 comparison actions", {
    fixture <- make_comparison_project()

    next_actions <- bg_next_actions(fixture$handle, scope = "project")
    action <- Filter(
      function(x) {
        identical(x$kind, "create_node_from_template") ||
          identical(x$payload$decision_type %||% NULL, "model_comparison")
      },
      next_actions$actions
    )[[1]]

    expect_true(any(grepl("leave-one-out cross-validation", action$explanation$references, fixed = TRUE)))
    expect_true(any(grepl("Stacking", action$explanation$references, fixed = TRUE)))
  })
})
