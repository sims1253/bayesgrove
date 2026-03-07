demo_repl_workflow <- function(start_repl = interactive()) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The repl workflow demo needs the pkgload package.", call. = FALSE)
  }

  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)

  project_root <- file.path(tempdir(), "bg-vhs-repl-demo3")
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
    if (param_val == "non-centered") {
      Sys.sleep(1.5)
      return(list(
        status = "fit_completed",
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            severity = "ok",
            passed = TRUE,
            metrics = list(divergences = 0, rhat_max = 1.01)
          )
        )
      ))
    }

    # Centered parametrization produces divergences
    list(
      status = "fit_completed",
      summaries = list(
        list(
          summary_kind = "hmc_diagnostics",
          severity = "warning",
          passed = FALSE,
          metrics = list(divergences = 15, rhat_max = 1.02)
        )
      )
    )
  })

  bg_register_node_kind(handle, "ppc", executor = function(node, inputs) {
    list(plot = "ppc_density_plot")
  })

  bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
    input_labels <- vapply(
      inputs,
      function(inp) {
        inp$label %||% "unknown"
      },
      character(1)
    )
    list(
      comparison = "loo_compare",
      models = names(inputs),
      labels = input_labels,
      winner = names(inputs)[[1]]
    )
  })

  # Build the graph - a simple fit with downstream PPC
  n_data <- bg_add_node(handle, "data_prep", label = "Load Survey Data")
  n_compile <- bg_add_node(handle, "compile", label = "Compile Model")
  n_fit <- bg_add_node(
    handle,
    "fit",
    label = "Fit Centered Parametrization",
    inputs = c(n_data, n_compile),
    params = list(parametrization = "centered")
  )
  n_ppc <- bg_add_node(
    handle,
    "ppc",
    label = "Posterior Predictive Check",
    inputs = n_fit
  )

  # Pre-run just the fit - it will produce a warning summary
  # The protocol engine will detect this and hold downstream PPC
  initial_run <- suppressMessages(bg_run(handle, mode = "sync"))

  demo <- list(
    handle = handle,
    project_root = project_root,
    n_data = n_data,
    n_compile = n_compile,
    n_fit = n_fit,
    n_ppc = n_ppc
  )

  assign("bg_repl_demo", demo, envir = .GlobalEnv)

  cli::cli_h1("BayesGrove: Guided Workflow Demo")
  cli::cli_text(
    "You are jumping into an analysis with a diagnostic warning."
  )
  cli::cli_text("")
  cli::cli_text(
    "The {.emph centered} parametrization has divergent transitions."
  )
  cli::cli_text(
    "Downstream work (PPC) is held until you resolve the issue."
  )
  cli::cli_text("")
  cli::cli_text(
    "The guided loop:"
  )
  cli::cli_bullets(c(
    "*" = "{.code status} - See workflow state (blocked by protocol)",
    "*" = "{.code guide} - See active obligations and suggested actions",
    "*" = "{.code do <n>} - Execute action #{.emph n} (branch and modify)",
    "*" = "{.code run} - Run the modified branch",
    "*" = "{.code guide} - Verify obligation resolved",
    "*" = "{.code nodes} - See the updated graph"
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
