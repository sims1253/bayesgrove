# Shared fixtures for the execution provenance tests.

# A source kind whose output ignores its params, and a fit kind that reports
# its consumed data and an effective setting. `pad` classifies every fit
# param, so fit executions record a P/A/D snapshot.
make_provenance_project <- function(
  path = withr::local_tempdir(.local_envir = parent.frame())
) {
  handle <- bg_init(path = path)
  bg_register_node_kind(handle, "src", executor = function(node, inputs) {
    list(value = 2)
  })
  bg_register_node_kind(
    handle,
    "fit",
    executor = function(node, inputs) {
      data_id <- node$params$data_input %||% names(inputs)[[1]]
      list(
        result = list(mean = inputs[[data_id]]$value * node$params$scale),
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          severity = "ok",
          passed = TRUE,
          metrics = list(max_rhat = 1)
        )),
        metadata = list(
          provenance = list(
            consumed = list(D = data_id),
            effective = list(A = list(chains = 4L))
          )
        )
      )
    },
    pad = list(
      P = "model",
      A = "scale",
      locator = "data_input",
      other = "thresholds"
    )
  )
  handle
}

final_job_for <- function(handle, node_id) {
  jobs <- Filter(function(job) identical(job$node_id, node_id), bg_jobs(handle))
  jobs[[length(jobs)]]
}
