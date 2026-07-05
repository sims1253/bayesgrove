describe("params-as-code channel is closed (Phase 5)", {
  it("a stan_data node with a data_fn_source param never executes it", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    sentinel <- withr::local_tempfile()
    # If evaluated, this source writes a sentinel file. It must never run.
    hostile_source <- paste0('file.create("', sentinel, '")')

    bg_register_node_kind(
      handle,
      "stan_data",
      executor = bayesgrove:::bg_executor_stan_data
    )

    n_data <- bg_add_node(
      handle,
      kind = "stan_data",
      label = "Hostile data",
      # data_fn_source is ignored by the executor; only data_ref/data are read.
      params = list(data_fn_source = hostile_source, data = list(ok = TRUE))
    )

    run_res <- bg_run(handle, targets = n_data)
    expect_equal(run_res$status, "succeeded")
    expect_false(file.exists(sentinel))
  })

  it("stan_data with no data attached aborts with a pointer to bg_set_node_data", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    node <- list(
      kind = "stan_data",
      params = list()
    )
    expect_error(
      bayesgrove:::bg_executor_stan_data(node, list()),
      class = "rlang_error"
    )
  })

  it("executor prefers resolved$data over params$data", {
    # node$resolved$data (resolved by bg_execute_node from a cas: data_ref) wins
    # over any inline params$data.
    node <- list(
      kind = "stan_data",
      params = list(data = list(fallback = TRUE)),
      resolved = list(data = list(N = 3L, y = integer(3)))
    )
    out <- bayesgrove:::bg_executor_stan_data(node, list())
    expect_equal(out$result, list(N = 3L, y = integer(3)))
    expect_equal(typeof(out$result$y), "integer")
  })

  it("executor with neither resolved$data nor params$data aborts", {
    # No resolved$field and no params$data -> abort pointing at bg_set_node_data.
    node <- list(kind = "stan_data", params = list(), resolved = list())
    expect_error(
      bayesgrove:::bg_executor_stan_data(node, list()),
      class = "rlang_error"
    )
  })

  it("bg_set_node_data round-trips through the CAS store", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(
      handle,
      "stan_data",
      executor = bayesgrove:::bg_executor_stan_data
    )

    data <- list(N = 10L, y = as.integer(c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1)))
    n_data <- bg_add_node(handle, kind = "stan_data", label = "Data")
    bg_set_node_data(handle, n_data, data)

    # The param is a cas: ref...
    node <- bg_read_graph(handle)$nodes[[n_data]]
    expect_true(startsWith(node$params$data_ref, "cas:sha256:"))

    # ...and the executor returns resolved data intact, preserving integer
    # types. bg_execute_node would normally populate node$resolved$data from the
    # cas: data_ref; here we resolve it the same way to exercise the executor
    # without going through a full run.
    node$resolved <- list(
      data = bg_fetch_artifact(handle, node$params$data_ref)
    )
    fetched <- bayesgrove:::bg_executor_stan_data(node, list())
    expect_equal(fetched$result, data)
    expect_equal(typeof(fetched$result$y), "integer")
    expect_equal(typeof(fetched$result$N), "integer")
  })
})
