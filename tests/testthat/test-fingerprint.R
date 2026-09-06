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

  it("distinguishes params that differ only beyond 4 decimal places", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data_source")
    n1 <- bg_add_node(
      handle,
      kind = "data_source",
      params = list(mu = 1.23456789)
    )
    f1 <- bg_compute_fingerprint(handle, n1)

    bg_update_node(handle, n1, params = list(mu = 1.23456999))
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

  it("fingerprints a node with no executor (planning without executors)", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "compile")

    n1 <- bg_add_node(handle, kind = "compile", params = list(model = "test"))

    f1 <- bg_compute_fingerprint(handle, n1)
    expect_true(startsWith(f1, "sha256:"))
  })
})

describe("Executor fingerprinting (Phase 2.1)", {
  it("changes the fingerprint when the executor body changes", {
    tmp_a <- withr::local_tempdir()
    handle_a <- bg_init(path = tmp_a)

    bg_register_node_kind(
      handle_a,
      "data",
      executor = function(node, inputs) {
        list(result = 1)
      }
    )
    n_a <- bg_add_node(handle_a, kind = "data", label = "A")
    f_a <- bg_compute_fingerprint(handle_a, n_a)

    # Same params/kind, different executor body -> different fingerprint.
    tmp_b <- withr::local_tempdir()
    handle_b <- bg_init(path = tmp_b)

    bg_register_node_kind(
      handle_b,
      "data",
      executor = function(node, inputs) {
        list(result = 2)
      }
    )
    n_b <- bg_add_node(handle_b, kind = "data", label = "A")
    f_b <- bg_compute_fingerprint(handle_b, n_b)

    expect_false(f_a == f_b)
  })

  it("produces identical fingerprints for identical registrations", {
    manifest <- list(r = "4.5.0", bayesgrove = "0.6.0")

    exec <- function(node, inputs) {
      list(result = 42)
    }

    tmp_a <- withr::local_tempdir()
    handle_a <- bg_init(path = tmp_a)
    bg_register_node_kind(handle_a, "data", executor = exec)
    n_a <- bg_add_node(handle_a, kind = "data", label = "A")
    f_a <- bg_compute_fingerprint(
      handle_a,
      n_a,
      environment_manifest = manifest
    )

    tmp_b <- withr::local_tempdir()
    handle_b <- bg_init(path = tmp_b)
    bg_register_node_kind(handle_b, "data", executor = exec)
    n_b <- bg_add_node(handle_b, kind = "data", label = "A")
    f_b <- bg_compute_fingerprint(
      handle_b,
      n_b,
      environment_manifest = manifest
    )

    expect_equal(f_a, f_b)
  })

  it("bumps the format version so an old-index artifact is not a cache hit", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data")
    n1 <- bg_add_node(handle, kind = "data", label = "A")

    current_fp <- bg_compute_fingerprint(handle, n1)

    # Build the format-1 fingerprint for the SAME node/params/manifest by
    # replaying the fingerprint payload with format_version = "1". This is
    # what a 0.x cache index would have stored before the bump, so a genuine
    # format-bump invalidation must make the current fingerprint differ.
    graph <- bg_read_graph(handle)
    node <- graph$nodes[[n1]]
    params <- node$params %||% list()
    sorted_params <- if (length(params) > 0 && !is.null(names(params))) {
      params[order(names(params))]
    } else {
      params
    }
    params_json <- jsonlite::toJSON(
      sorted_params,
      auto_unbox = TRUE,
      null = "null"
    )
    manifest <- bg_resolve_environment_manifest(list())
    env_json <- jsonlite::toJSON(manifest, auto_unbox = TRUE, null = "null")
    legacy_components <- list(
      format = "1",
      kind = node$kind,
      params = as.character(params_json),
      upstreams = character(0),
      env = as.character(env_json),
      executor = "",
      backend = ""
    )
    legacy_payload <- jsonlite::toJSON(
      legacy_components,
      auto_unbox = TRUE,
      null = "null"
    )
    stale_fp <- sprintf(
      "sha256:%s",
      digest::digest(legacy_payload, algo = "sha256")
    )

    # The format bump must make the current (format 3) fingerprint differ.
    expect_false(current_fp == stale_fp)

    # And an index keyed by the legacy fingerprint must not yield a hit when
    # the store is queried with the current fingerprint.
    idx <- list()
    idx[[stale_fp]] <- list(
      artifact_ref = "cas:sha256:deadbeef",
      status = "active",
      created_at = "2020-01-01T00:00:00Z",
      bindings = list(
        n1 = list(
          node_id = n1,
          status = "active",
          updated_at = "2020-01-01T00:00:00Z"
        )
      )
    )
    bg_write_artifact_index(handle, idx)

    expect_null(bg_check_artifact(handle, current_fp, node_id = n1))
  })
})
