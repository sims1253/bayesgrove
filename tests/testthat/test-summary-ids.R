describe("summary id generation", {
  it("derives 64-bit ids so concurrent summaries cannot alias", {
    handle <- bg_init(path = withr::local_tempdir())
    bg_register_node_kind(handle, "data")
    node <- bg_add_node(handle, "data")

    written <- bg_write_summaries(
      handle,
      node_id = node,
      artifact_ref = "cas:sha256:abc",
      execution_fingerprint = "sha256:deadbeef",
      summaries = list(
        list(
          summary_kind = "hmc_diagnostics",
          severity = "ok",
          metrics = list()
        )
      )
    )

    summary_id <- names(written)[[1L]]
    # 16 hex chars of digest after the prefix (the old generator emitted 8).
    expect_match(summary_id, "^sum_[0-9a-f]{16}$")

    # Identical content must still yield a distinct id: the random draw,
    # not the content alone, separates re-emitted summaries.
    again <- bg_write_summaries(
      handle,
      node_id = node,
      artifact_ref = "cas:sha256:abc",
      execution_fingerprint = "sha256:deadbeef",
      summaries = list(
        list(
          summary_kind = "hmc_diagnostics",
          severity = "ok",
          metrics = list()
        )
      )
    )

    expect_false(identical(names(again)[[1L]], summary_id))
  })
})
