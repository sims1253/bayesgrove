make_workflow_hold_fixture <- function() {
  tmp <- tempfile("workflow-hold-fixture-")
  dir.create(tmp, recursive = TRUE)
  config <- new.env(parent = emptyenv())
  config$fit_summary_mode <- "warning"

  handle <- bg_init(
    path = tmp,
    workflow_packs = list("bayesguide.default_bayesian")
  )

  bg_register_node_kind(handle, "source", executor = function(node, inputs) {
    list(rows = 10L)
  })
  bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
    severity <- if (identical(config$fit_summary_mode, "warning")) "warning" else "ok"
    list(
      result = list(
        fitted = TRUE,
        source_rows = inputs[[1]]$rows,
        revision = node$params$revision %||% 1L,
        summary_mode = config$fit_summary_mode
      ),
      summaries = list(list(
        summary_kind = "optimizer_diagnostics",
        passed = identical(severity, "ok"),
        severity = severity,
        metrics = list(
          max_gradient = if (identical(severity, "ok")) 0.001 else 0.1
        )
      ))
    )
  })
  bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
    list(compared_node_ids = names(inputs))
  })

  source_id <- bg_add_node(handle, kind = "source", label = "Data")
  fit_id <- bg_add_node(
    handle,
    kind = "fit",
    label = "Baseline fit",
    inputs = source_id,
    params = list(revision = 1L)
  )
  compare_id <- bg_add_node(
    handle,
    kind = "compare",
    label = "Posterior comparison",
    inputs = fit_id
  )

  list(
    handle = handle,
    source_id = source_id,
    fit_id = fit_id,
    compare_id = compare_id,
    set_fit_summary_mode = function(mode) {
      config$fit_summary_mode <- mode
      invisible(mode)
    }
  )
}
