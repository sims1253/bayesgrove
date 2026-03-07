demo_repl_workflow <- function(start_repl = interactive()) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The repl workflow demo needs the pkgload package.", call. = FALSE)
  }

  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)

  project_root <- file.path(tempdir(), "bg-vhs-repl-demo4")
  unlink(project_root, recursive = TRUE, force = TRUE)

  handle <- bg_init(
    path = project_root,
    project_name = "Hierarchical Analysis",
    workflow_packs = list("bayesguide.default_bayesian")
  )

  # Executor that can produce warning or clean diagnostics based on params
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
    if (param_val == "non-centered") {
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

    # Centered parametrization produces divergences on the branch fit.
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
      comparison = "loo_compare",
      models = names(inputs),
      summaries = list(list(
        summary_kind = "comparison_results",
        severity = "ok",
        passed = TRUE
      ))
    )
  })

  # Build the graph with a clean baseline fit.
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

  # Create a problematic centered branch with downstream continuation.
  centered_branch <- bg_branch_with_continuation(
    project = handle,
    node_id = n_fit,
    label = "Fit Centered Parametrization"
  )
  bg_update_node(
    handle,
    centered_branch$branch$root_node_id,
    label = "Fit Centered Parametrization",
    params = list(
      parametrization = "centered",
      variant = "centered_branch"
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
    label = "Compare parametrizations",
    rationale = "Compare the branch against the clean baseline fit."
  )
  suppressMessages(
    bg_run(handle, targets = centered_branch$branch$root_node_id, mode = "sync")
  )

  demo <- list(
    handle = handle,
    project_root = project_root,
    n_data = n_data,
    n_compile = n_compile,
    n_fit = n_fit,
    n_ppc = n_ppc,
    warning_branch = centered_branch$branch
  )

  assign("bg_repl_demo", demo, envir = .GlobalEnv)

  cli::cli_h1("BayesGrove: Guided Workflow Demo")
  cli::cli_text(
    "You are jumping into an analysis with a clean baseline fit and one problematic branch."
  )
  cli::cli_text("")
  cli::cli_text(
    "The {.emph centered} branch has divergent transitions and branch-scoped review obligations."
  )
  cli::cli_text(
    "After remediation, the guided loop continues into comparison and explicit branch acceptance."
  )
  cli::cli_text("")
  cli::cli_text(
    "The guided loop:"
  )
  cli::cli_bullets(c(
    "*" = "{.code status} - See workflow state (blocked by protocol)",
    "*" = "{.code guide} - See active obligations and suggested actions",
    "*" = "{.code do <n>} - Execute the suggested remediation, goal, comparison, or decision action",
    "*" = "{.code run} - Run the revised branch and then the comparison",
    "*" = "{.code retire <label>} - Retire the stale warning branch",
    "*" = "{.code guide} - Follow the comparison and branch-disposition steps",
    "*" = "{.code nodes} - See the final graph"
  ))
  cli::cli_text("")

  if (isTRUE(start_repl)) {
    bg_repl(handle)
  }

  invisible(demo)
}

if (interactive()) {
  demo_repl_workflow(start_repl = TRUE)
}
