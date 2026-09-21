# Direct coverage for the bg_write_summaries() export. Everything else
# exercises it only through bg_run(); these tests pin the entry-point's own
# contract: the empty-input short-circuit and the shape of the persisted
# entries keyed by summary_id.

describe("bg_write_summaries", {
  it("short-circuits on empty summary input without touching the log", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    expect_equal(
      bg_write_summaries(handle, "node_x", "cas:abc", "fp1", NULL),
      list()
    )
    expect_equal(
      bg_write_summaries(handle, "node_x", "cas:abc", "fp1", list()),
      list()
    )
    expect_false(file.exists(bayesgrove:::bg_summary_log_path(handle)))
  })

  it("persists summaries as entries keyed by summary_id", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(result = 1)
    })
    node_id <- bg_add_node(handle, kind = "data", label = "A")

    persisted <- bg_write_summaries(
      handle,
      node_id = node_id,
      artifact_ref = "cas:deadbeef",
      execution_fingerprint = "fp_predict",
      summaries = list(
        list(
          summary_kind = "hmc_diagnostics",
          passed = TRUE,
          severity = "ok",
          metrics = list(divergences = 0)
        ),
        list(
          summary_kind = "hmc_diagnostics",
          severity = "warning",
          metrics = list()
        )
      )
    )

    # Two summaries of the same kind still get distinct summary_ids, and the
    # returned list is keyed by them.
    expect_length(persisted, 2L)
    expect_equal(
      names(persisted),
      unname(vapply(persisted, `[[`, character(1), "summary_id"))
    )
    expect_false(identical(names(persisted)[[1]], names(persisted)[[2]]))

    for (entry in persisted) {
      expect_equal(entry$schema_name, "bg_summary_entry")
      expect_equal(entry$schema_version, 2L)
      expect_match(entry$summary_id, "^sum_")
      expect_equal(entry$project_id, handle@project_id)
      expect_equal(entry$node_id, node_id)
      expect_equal(entry$artifact_ref, "cas:deadbeef")
      expect_equal(entry$execution_fingerprint, "fp_predict")
      expect_true(nzchar(entry$created_at))
    }

    # Payload fields round-trip into the persisted entries; a missing passed
    # flag simply stays absent.
    first <- persisted[[1]]
    expect_equal(first$summary_kind, "hmc_diagnostics")
    expect_equal(first$passed, TRUE)
    expect_equal(first$severity, "ok")
    expect_equal(first$metrics, list(divergences = 0))
    expect_null(persisted[[2]]$passed)

    # The same keys are what bg_read_summaries() finds in the log.
    read_back <- bg_read_summaries(handle)
    expect_true(all(names(persisted) %in% names(read_back)))
    expect_equal(read_back[[first$summary_id]]$summary_kind, "hmc_diagnostics")
  })
})
