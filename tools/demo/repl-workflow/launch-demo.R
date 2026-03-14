.bg_demo_upd_meta <- getFromNamespace(
  "bg_update_branch_metadata",
  "bayesgrove"
)

bg_demo_repl_load_package <- function() {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The repl workflow demo needs the pkgload package.", call. = FALSE)
  }

  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
}

bg_demo_repl_register_kinds <- function(handle) {
  bg_register_node_kind(handle, "data_prep", executor = function(node, inputs) {
    set.seed(123)
    data.frame(group = rep(1:5, each = 10), y = rnorm(50))
  })

  bg_register_node_kind(handle, "compile", executor = function(node, inputs) {
    list(model = "compiled_binary")
  })

  bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
    param_val <- node$params$parametrization %||% "centered"
    variant <- node$params$variant %||% "baseline"

    if (identical(param_val, "non-centered")) {
      Sys.sleep(1.5)
      return(list(
        status = "fit_completed",
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            severity = "ok",
            passed = TRUE,
            metrics = list(
              divergences = 0,
              rhat_max = 1.01,
              variant = variant
            )
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
          metrics = list(
            divergences = 15,
            rhat_max = 1.02,
            variant = variant
          )
        )
      )
    )
  })

  bg_register_node_kind(handle, "ppc", executor = function(node, inputs) {
    list(plot = "ppc_density_plot")
  })

  bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
    list(
      loo_comparison = data.frame(
        model = c("Alternative: Robust Prior (fixed)", "Baseline Fit"),
        elpd_diff = c(0.0, -14.2),
        se_diff = c(0.0, 4.5),
        p_loo = c(5.2, 12.1)
      ),
      summaries = list(list(
        summary_kind = "comparison_results",
        severity = "ok",
        passed = TRUE
      ))
    )
  })
}

bg_demo_build_base <- function(project_root) {
  handle <- bg_init(
    path = project_root,
    project_name = "Hierarchical Analysis",
    workflow_packs = list("bayesguide.default_bayesian")
  )

  bg_demo_repl_register_kinds(handle)

  n_data <- bg_add_node(handle, "data_prep", label = "Load Survey Data")
  n_compile <- bg_add_node(handle, "compile", label = "Compile Model")
  n_fit <- bg_add_node(
    handle,
    "fit",
    label = "Baseline Fit",
    inputs = c(n_data, n_compile),
    params = list(
      parametrization = "non-centered",
      variant = "baseline"
    )
  )
  n_ppc <- bg_add_node(
    handle,
    "ppc",
    label = "Posterior Predictive Check",
    inputs = n_fit
  )

  suppressMessages(bg_run(handle, targets = n_fit, mode = "sync"))

  list(
    handle = handle,
    n_data = n_data,
    n_compile = n_compile,
    n_fit = n_fit,
    n_ppc = n_ppc
  )
}

bg_demo_warn_branch <- function(handle, n_fit) {
  centered_branch <- bg_branch_with_continuation(
    project = handle,
    node_id = n_fit,
    label = "Alternative: Robust Prior"
  )

  bg_update_node(
    handle,
    centered_branch$branch$root_node_id,
    label = "Alternative: Robust Prior",
    params = list(
      parametrization = "centered",
      variant = "robust_prior_centered"
    )
  )

  if (length(centered_branch$continuation_nodes) > 0) {
    for (entry in centered_branch$continuation_nodes) {
      bg_update_node(
        handle,
        entry$clone_id,
        label = "Posterior Predictive Check (branch)"
      )
    }
  }

  bg_set_goal(
    project = handle,
    branch_id = centered_branch$branch$branch_id,
    kind = "observable_prediction",
    label = "Compare priors",
    rationale = "Compare a robust prior branch against the clean baseline fit."
  )

  suppressMessages(
    bg_run(handle, targets = centered_branch$branch$root_node_id, mode = "sync")
  )

  centered_branch
}

bg_demo_revision_branch <- function(handle, warning_branch) {
  revised <- bg_branch_with_continuation(
    project = handle,
    node_id = warning_branch$branch$root_node_id,
    label = "Alternative: Robust Prior (fixed)",
    continuation_kinds = c("ppc")
  )

  bg_update_node(
    handle,
    revised$branch$root_node_id,
    label = "Alternative: Robust Prior (fixed)",
    params = list(
      parametrization = "non-centered",
      variant = "robust_prior_non_centered"
    )
  )

  if (length(revised$continuation_nodes) > 0) {
    for (entry in revised$continuation_nodes) {
      bg_update_node(
        handle,
        entry$clone_id,
        label = "Posterior Predictive Check (revision)"
      )
    }
  }

  .bg_demo_upd_meta(
    handle,
    revised$branch$branch_id,
    list(
      goal_optional = TRUE,
      branch_purpose = "technical_revision",
      template_ref = "branch_and_modify_fit"
    )
  )

  suppressMessages(bg_run(
    handle,
    targets = revised$branch$root_node_id,
    mode = "sync"
  ))

  revised
}

bg_demo_compare_node <- function(
  handle,
  label = "Compare Baseline vs Revision"
) {
  project_actions <- bg_next_actions(handle, scope = "project")$actions
  comparison_action <- Filter(
    function(action) {
      identical(action$kind, "create_node_from_template") &&
        identical(action$payload$template_ref %||% NULL, "branch_comparison")
    },
    project_actions
  )[[1]]

  compare_node_id <- bg_add_node(
    handle,
    kind = "compare",
    label = label,
    inputs = comparison_action$payload$inputs
  )

  suppressMessages(bg_run(handle, targets = compare_node_id, mode = "sync"))
  compare_node_id
}

bg_demo_record_comparison <- function(handle) {
  project_actions <- bg_next_actions(handle, scope = "project")$actions
  comparison_action <- Filter(
    function(action) {
      identical(action$kind, "record_decision") &&
        identical(action$payload$decision_type %||% NULL, "model_comparison")
    },
    project_actions
  )[[1]]

  bg_record_decision(
    handle,
    scope = "project",
    prompt = "Compare candidate branches",
    choice = "prefer_revised_branch",
    rationale = "The robust prior model has significantly better predictive performance (ELPD diff 14.2, SE 4.5).",
    kind = "model_comparison",
    metadata = comparison_action$payload[c(
      "fit_node_ids",
      "summary_ids",
      "candidate_signature",
      "comparison_signature",
      "comparison_context"
    )]
  )
}

bg_demo_record_disposition <- function(handle, branch_id) {
  branch_actions <- bg_next_actions(
    handle,
    scope = "branch",
    branch_id = branch_id
  )$actions

  disposition_action <- Filter(
    function(action) {
      identical(action$kind, "record_decision") &&
        identical(action$payload$decision_type %||% NULL, "branch_disposition")
    },
    branch_actions
  )[[1]]

  bg_record_decision(
    handle,
    scope = branch_id,
    prompt = "Accept or reject branch",
    choice = "accept",
    rationale = "Promote the revised branch as the accepted analysis path.",
    kind = "branch_disposition",
    metadata = list(
      disposition = "accept",
      summary_ids = disposition_action$payload$summary_ids,
      comparison_signature = disposition_action$payload$comparison_signature,
      comparison_context = disposition_action$payload$comparison_context
    )
  )
}

bg_demo_register_showcase_kinds <- function(handle) {
  bg_register_node_kind(handle, "load_data", executor = function(node, inputs) {
    list(
      rows = 200L,
      missing_outcome_rows = 10L,
      note = "Loaded the analysis table."
    )
  })

  bg_register_node_kind(
    handle,
    "clean_data",
    executor = function(node, inputs) {
      raw <- inputs[[1]]
      rows_before <- raw$rows %||% 200L
      dropped_rows <- raw$missing_outcome_rows %||% 10L
      rows_after <- rows_before - dropped_rows

      list(
        rows_before = rows_before,
        rows_after = rows_after,
        dropped_rows = dropped_rows,
        dropped_pct = sprintf("%.0f%%", 100 * dropped_rows / rows_before),
        rule = "Removed rows with missing outcome values"
      )
    }
  )

  bg_register_node_kind(
    handle,
    "fit_simple",
    executor = function(node, inputs) {
      cleaned <- inputs[[1]]

      list(
        model = "linear_regression",
        formula = "outcome ~ treatment + age",
        rows_used = cleaned$rows_after %||% NA_integer_,
        status = "Fit completed after explicit data review."
      )
    }
  )
}

bg_demo_build_showcase <- function(project_root) {
  handle <- bg_init(
    path = project_root,
    project_name = "Missing Data Review",
    workflow_packs = list("bayesguide.default_bayesian")
  )

  bg_demo_register_showcase_kinds(handle)

  n_load <- bg_add_node(handle, "load_data", label = "Load Data")
  n_clean <- bg_add_node(
    handle,
    "clean_data",
    label = "Clean Data",
    inputs = n_load
  )
  n_fit <- bg_add_node(
    handle,
    "fit_simple",
    label = "Baseline Fit",
    inputs = n_clean
  )

  bg_add_gate(
    project = handle,
    from = n_clean,
    to = n_fit,
    prompt = "Proceed after dropping 5% of rows with missing outcome values?",
    alternatives = c("yes", "no")
  )

  suppressMessages(bg_run(handle, targets = n_clean, mode = "sync"))

  list(
    handle = handle,
    n_load = n_load,
    n_clean = n_clean,
    n_fit = n_fit
  )
}

demo_repl_showcase <- function(start_repl = interactive()) {
  bg_demo_repl_load_package()

  project_root <- file.path(tempdir(), "bg-vhs-showcase")
  unlink(project_root, recursive = TRUE, force = TRUE)

  demo <- bg_demo_build_showcase(project_root)
  demo$project_root <- project_root

  assign("bg_repl_demo", demo, envir = .GlobalEnv)

  if (isTRUE(start_repl)) {
    bg_repl(demo$handle, initial_scope = "project")
  }

  invisible(demo)
}

bg_demo_repl_intro <- function(checkpoint) {
  switch(
    checkpoint,
    "warning_branch" = list(
      heading = "bayesgrove: Remediation Workflow Demo",
      lines = c(
        "You are jumping into an analysis with a clean baseline fit and one problematic branch.",
        "The centered branch has divergent transitions and branch-scoped review obligations.",
        "Use the template-backed remediation action to branch, revise, rerun, and retire the stale warning path."
      ),
      bullets = c(
        "status - See workflow state (blocked by protocol)",
        "guide - See active obligations and suggested actions",
        "do <n> - Execute the remediation template action",
        "run - Run the revised branch",
        "retire <label> - Retire the stale warning branch"
      )
    ),
    "comparison_ready" = list(
      heading = "bayesgrove: Comparison Template Demo",
      lines = c(
        "You are resuming after remediation.",
        "The stale warning branch is retired and two clean fits remain.",
        "The next guided step is the built-in branch_comparison template."
      ),
      bullets = c(
        "status - Confirm the project is blocked only on comparison",
        "guide - Inspect the comparison obligation",
        "do <n> - Execute the comparison template action",
        "run - Run the new comparison node"
      )
    ),
    "disposition_ready" = list(
      heading = "bayesgrove: Branch Disposition Demo",
      lines = c(
        "A comparison node already exists and the project-level comparison decision has been recorded.",
        "The current branch scope now needs an explicit accept or reject disposition.",
        "Use the review-decision action to resolve the remaining branch obligation."
      ),
      bullets = c(
        "status - Confirm the branch is blocked on disposition",
        "guide - Inspect the branch disposition obligation",
        "do <n> - Execute the review-decision action",
        "nodes - Inspect the final accepted path"
      )
    ),
    "healthy" = list(
      heading = "bayesgrove: Healthy Workflow Demo",
      lines = c(
        "All remediation, comparison, and disposition decisions are already recorded.",
        "Use this checkpoint for inspection rather than action execution."
      ),
      bullets = c(
        "status - Confirm the workflow is healthy",
        "decisions - Review recorded decisions",
        "nodes - Inspect the final graph"
      )
    )
  )
}

bg_demo_repl_print_intro <- function(checkpoint) {
  intro <- bg_demo_repl_intro(checkpoint)

  cli::cli_h1(intro$heading)
  for (line in intro$lines) {
    cli::cli_text(line)
  }
  cli::cli_text("")
  cli::cli_text("Suggested loop:")
  cli::cli_bullets(stats::setNames(
    as.list(intro$bullets),
    rep("*", length(intro$bullets))
  ))
  cli::cli_text("")
}

demo_repl_workflow <- function(
  start_repl = interactive(),
  checkpoint = c(
    "warning_branch",
    "comparison_ready",
    "disposition_ready",
    "healthy"
  )
) {
  checkpoint <- match.arg(checkpoint)
  bg_demo_repl_load_package()

  project_root <- file.path(
    tempdir(),
    sprintf("bg-vhs-repl-%s", checkpoint)
  )
  unlink(project_root, recursive = TRUE, force = TRUE)

  base <- bg_demo_build_base(project_root)
  handle <- base$handle
  warning_branch <- bg_demo_warn_branch(handle, base$n_fit)

  revised_branch <- NULL
  compare_node_id <- NULL
  initial_scope <- "project"

  if (checkpoint %in% c("comparison_ready", "disposition_ready", "healthy")) {
    revised_branch <- bg_demo_revision_branch(
      handle,
      warning_branch
    )
    bg_retire_node(
      handle,
      warning_branch$branch$root_node_id,
      recursive = TRUE
    )
  }

  if (checkpoint %in% c("disposition_ready", "healthy")) {
    compare_node_id <- bg_demo_compare_node(handle)
    bg_demo_record_comparison(handle)
    initial_scope <- revised_branch$branch$branch_id
  }

  if (identical(checkpoint, "healthy")) {
    bg_demo_record_disposition(
      handle,
      revised_branch$branch$branch_id
    )
  }

  demo <- list(
    handle = handle,
    project_root = project_root,
    checkpoint = checkpoint,
    initial_scope = initial_scope,
    n_data = base$n_data,
    n_compile = base$n_compile,
    n_fit = base$n_fit,
    n_ppc = base$n_ppc,
    warning_branch = warning_branch$branch,
    revised_branch = revised_branch$branch %||% NULL,
    comparison_node_id = compare_node_id
  )

  assign("bg_repl_demo", demo, envir = .GlobalEnv)
  bg_demo_repl_print_intro(checkpoint)

  if (isTRUE(start_repl)) {
    bg_repl(handle, initial_scope = initial_scope)
  }

  invisible(demo)
}

if (interactive() && !identical(getOption("bg_demo_autostart"), FALSE)) {
  demo_repl_workflow(start_repl = TRUE)
}
