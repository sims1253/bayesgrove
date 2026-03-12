describe("Dagitty causal workflow pack", {
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

  make_branch_fit_project <- function(
    workflow_packs,
    formula = "y ~ treatment + z",
    cleanup_env = parent.frame()
  ) {
    tmp <- tempfile("causal-dagitty-pack-")
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
          passed = TRUE,
          severity = "ok"
        ))
      )
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline fit",
      inputs = source_id,
      params = list(formula = formula)
    )
    bg_run(handle, targets = fit_id, mode = "sync")

    branch <- bg_branch(handle, fit_id, label = "Causal branch")
    bg_set_goal(
      project = handle,
      branch_id = branch$branch_id,
      kind = "latent_inference",
      label = "latent_inference",
      rationale = "Causal effect estimation"
    )
    bg_run(handle, targets = branch$root_node_id, mode = "sync")

    list(handle = handle, fit_id = fit_id, branch = branch)
  }

  it("registers the dagitty causal pack", {
    registry <- bg_builtin_workflow_registry()

    expect_true("bayesgrove.causal_dagitty" %in% names(registry))
    expect_false(is.null(bg_lookup_workflow_pack("bayesgrove.causal_dagitty")))
  })

  it("tracks dagitty adjustment and implication reviews", {
    skip_if_not_installed("dagitty")

    fixture <- make_branch_fit_project("bayesgrove.causal_dagitty")
    handle <- fixture$handle

    bg_register_node_kind(
      handle,
      "dagitty_adjustment",
      executor = function(node, inputs) {
        dag <- dagitty::dagitty(
          node$params$dag_code %||%
            "dag { z -> treatment; z -> outcome; treatment -> outcome }"
        )
        adjustment_sets <- dagitty::adjustmentSets(
          dag,
          exposure = node$params$exposure %||% "treatment",
          outcome = node$params$outcome %||% "outcome"
        )
        first_set <- adjustment_sets[[1]] %||% character()

        list(
          adjustment_sets = adjustment_sets,
          summaries = list(list(
            summary_kind = "dagitty_adjustment",
            passed = TRUE,
            severity = "ok",
            metrics = list(
              n_sets = length(adjustment_sets),
              first_set = paste(first_set, collapse = ","),
              exposure = node$params$exposure %||% "treatment",
              outcome = node$params$outcome %||% "outcome"
            )
          ))
        )
      }
    )

    bg_register_node_kind(
      handle,
      "dagitty_implications",
      executor = function(node, inputs) {
        dag <- dagitty::dagitty(
          node$params$dag_code %||%
            "dag { z -> treatment; z -> outcome; treatment -> outcome }"
        )
        implications <- dagitty::impliedConditionalIndependencies(dag)

        list(
          implications = implications,
          summaries = list(list(
            summary_kind = "dagitty_implications",
            passed = TRUE,
            severity = "ok",
            metrics = list(n_implications = length(implications))
          ))
        )
      }
    )

    initial <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    expect_true(any(vapply(
      initial$obligations,
      function(obligation) {
        identical(obligation$kind, "derive_causal_adjustment")
      },
      logical(1)
    )))

    adjustment_id <- bg_add_node(
      handle,
      kind = "dagitty_adjustment",
      label = "Adjustment set",
      inputs = fixture$branch$root_node_id,
      params = list(
        dag_code = "dag { z -> treatment; z -> outcome; treatment -> outcome }",
        exposure = "treatment",
        outcome = "outcome"
      )
    )
    bg_run(handle, targets = adjustment_id, mode = "sync")

    after_adjustment <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    adjustment_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "causal_adjustment_review")
      },
      after_adjustment$actions
    )[[1]]

    record_action_decision(
      handle,
      adjustment_action,
      choice = "adjust_for_z",
      rationale = "The DAG implies that adjusting for z identifies the target effect."
    )

    implication_id <- bg_add_node(
      handle,
      kind = "dagitty_implications",
      label = "DAG implications",
      inputs = fixture$branch$root_node_id,
      params = list(
        dag_code = paste(
          "dag { w -> z; z -> treatment; z -> outcome; treatment -> outcome }"
        )
      )
    )
    bg_run(handle, targets = implication_id, mode = "sync")

    after_implications <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    implication_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "causal_implication_review")
      },
      after_implications$actions
    )[[1]]

    record_action_decision(
      handle,
      implication_action,
      choice = "implications_logged",
      rationale = "The DAG's implied independencies define the falsification checks for this branch."
    )

    cleared <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    expect_false(any(vapply(
      cleared$obligations,
      function(obligation) {
        obligation$kind %in%
          c(
            "derive_causal_adjustment",
            "review_causal_adjustment",
            "check_causal_implications",
            "review_causal_implications"
          )
      },
      logical(1)
    )))
  })

  it("derives causal selection contracts and enforces formula consistency", {
    fixture <- make_branch_fit_project(
      "bayesgrove.causal_dagitty",
      formula = "y ~ treatment + post_treatment"
    )
    handle <- fixture$handle

    bg_register_node_kind(
      handle,
      "dagitty_adjustment",
      executor = function(node, inputs) {
        list(
          summaries = list(list(
            summary_kind = "dagitty_adjustment",
            passed = TRUE,
            severity = "ok",
            metrics = list(
              exposure = "treatment",
              outcome = "outcome",
              adjustment_set = "z"
            )
          ))
        )
      }
    )

    bg_register_node_kind(
      handle,
      "causal_selection_contract",
      executor = function(node, inputs) {
        list(
          summaries = list(list(
            summary_kind = "causal_selection_contract",
            passed = TRUE,
            severity = "ok",
            metrics = list(
              required_terms = c("treatment", "z"),
              forbidden_terms = "post_treatment",
              ranked_candidate_terms = c("w", "x")
            )
          ))
        )
      }
    )

    bg_register_node_kind(handle, "check")
    check_id <- bg_add_node(
      handle,
      kind = "check",
      label = "Downstream check",
      inputs = fixture$branch$root_node_id
    )

    adjustment_id <- bg_add_node(
      handle,
      kind = "dagitty_adjustment",
      label = "Adjustment set",
      inputs = fixture$branch$root_node_id
    )
    bg_run(handle, targets = adjustment_id, mode = "sync")

    after_adjustment <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    adjustment_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(action$payload$decision_type, "causal_adjustment_review")
      },
      after_adjustment$actions
    )[[1]]

    record_action_decision(
      handle,
      adjustment_action,
      choice = "adjust_for_z",
      rationale = "The branch will identify the effect by adjusting for z."
    )

    need_contract <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    expect_true(any(vapply(
      need_contract$obligations,
      function(obligation) {
        identical(obligation$kind, "derive_causal_selection_contract")
      },
      logical(1)
    )))

    contract_id <- bg_add_node(
      handle,
      kind = "causal_selection_contract",
      label = "Selection contract",
      inputs = fixture$branch$root_node_id
    )
    bg_run(handle, targets = contract_id, mode = "sync")

    after_contract <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    contract_action <- Filter(
      function(action) {
        identical(action$kind, "record_decision") &&
          identical(
            action$payload$decision_type,
            "causal_selection_contract_review"
          )
      },
      after_contract$actions
    )[[1]]

    expect_setequal(
      contract_action$payload$required_terms,
      c("treatment", "z")
    )
    expect_equal(contract_action$payload$forbidden_terms, "post_treatment")

    record_action_decision(
      handle,
      contract_action,
      choice = "contract_locked",
      rationale = paste(
        "Treatment and z are locked for identification; post_treatment is",
        "forbidden; w and x are admissible precision candidates."
      )
    )

    blocked <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    expect_true(any(vapply(
      blocked$obligations,
      function(obligation) {
        identical(obligation$kind, "enforce_causal_formula_contract")
      },
      logical(1)
    )))

    revise_action <- Filter(
      function(action) identical(action$kind, "branch_and_modify"),
      blocked$actions
    )[[1]]
    suggested_formula <- revise_action$payload$parameter_suggestions$formula
    rhs_terms <- bg_phase10_formula_terms(suggested_formula)
    expect_true(all(c("treatment", "z") %in% rhs_terms))
    expect_false("post_treatment" %in% rhs_terms)

    holds <- bg_workflow_external_holds(handle)
    expect_true(check_id %in% names(holds))

    bg_update_node(
      handle,
      fixture$branch$root_node_id,
      params = list(formula = "y ~ treatment + z + w")
    )

    cleared <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = fixture$branch$branch_id
    )
    expect_false(any(vapply(
      cleared$obligations,
      function(obligation) {
        identical(obligation$kind, "enforce_causal_formula_contract")
      },
      logical(1)
    )))
    expect_false(check_id %in% names(bg_workflow_external_holds(handle)))
  })
})
