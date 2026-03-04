describe("Fingerprinting", {
  it("computes deterministic fingerprints for a single node", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data_source")
    n1 <- bg_add_node(
      handle,
      kind = "data_source",
      params = list(path = "data.csv")
    )

    f1 <- bg_compute_fingerprint(handle, n1)
    f2 <- bg_compute_fingerprint(handle, n1)

    expect_true(startsWith(f1, "sha256:"))
    expect_equal(f1, f2)
  })

  it("changes fingerprint when params change", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data_source")
    n1 <- bg_add_node(
      handle,
      kind = "data_source",
      params = list(path = "data.csv")
    )
    f1 <- bg_compute_fingerprint(handle, n1)

    bg_update_node(handle, n1, params = list(path = "data2.csv"))
    f2 <- bg_compute_fingerprint(handle, n1)

    expect_false(f1 == f2)
  })

  it("changes fingerprint when upstream fingerprints change", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data")
    bg_register_node_kind(handle, "model")
    n1 <- bg_add_node(handle, kind = "data", label = "A")
    n2 <- bg_add_node(handle, kind = "model", label = "B", inputs = n1)

    f2_v1 <- bg_compute_fingerprint(
      handle,
      n2,
      upstream_fingerprints = stats::setNames(list("hashA"), n1)
    )

    f2_v2 <- bg_compute_fingerprint(
      handle,
      n2,
      upstream_fingerprints = stats::setNames(list("hashB"), n1)
    )

    expect_false(f2_v1 == f2_v2)
  })

  it("incorporates backend runtime signatures for compile nodes", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "compile")

    mock_backend <- list(
      backend_compile = function() {},
      backend_fit = function() {},
      backend_source_hash = function(params) {
        "mock_stan_code_hash"
      },
      backend_runtime_signature = function(params) {
        list(
          fingerprint_fields = list(version = "1.0"),
          compatibility_fields = list(os = "linux")
        )
      }
    )
    bg_register_backend(handle, "mock_cmdstanr", mock_backend)

    n1 <- bg_add_node(
      handle,
      kind = "compile",
      params = list(backend = "mock_cmdstanr")
    )

    f1 <- bg_compute_fingerprint(handle, n1)
    expect_true(startsWith(f1, "sha256:"))
  })
})
