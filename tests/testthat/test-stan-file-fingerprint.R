describe("Stan-file source hashing (Phase 4)", {
  it("bg_source_hash_component hashes file contents, not the path", {
    stan_a <- withr::local_tempfile(
      lines = "parameters { real x; } model { x ~ normal(0, 1); }"
    )
    stan_b <- withr::local_tempfile(
      lines = "parameters { real x; } model { x ~ normal(0, 5); }"
    )

    # Same contents at two different paths -> same source component.
    same_at_a <- withr::local_tempfile(lines = "model { x ~ normal(0, 1); }")
    same_at_b <- withr::local_tempfile(lines = "model { x ~ normal(0, 1); }")

    node_a <- list(kind = "cmdstanr_fit", params = list(stan_file = stan_a))
    node_b <- list(kind = "cmdstanr_fit", params = list(stan_file = stan_b))
    node_same_a <- list(
      kind = "cmdstanr_fit",
      params = list(stan_file = same_at_a)
    )
    node_same_b <- list(
      kind = "cmdstanr_fit",
      params = list(stan_file = same_at_b)
    )

    expect_equal(
      bayesgrove:::bg_source_hash_component(node_a),
      bayesgrove:::bg_source_hash_component(node_a)
    )
    expect_false(
      bayesgrove:::bg_source_hash_component(node_a) ==
        bayesgrove:::bg_source_hash_component(node_b)
    )
    expect_equal(
      bayesgrove:::bg_source_hash_component(node_same_a),
      bayesgrove:::bg_source_hash_component(node_same_b)
    )
  })

  it("fingerprint changes when the Stan file's contents change (path fixed)", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "stan_fit")

    stan_file <- withr::local_tempfile(lines = "model { x ~ normal(0, 1); }")
    n1 <- bg_add_node(
      handle,
      kind = "stan_fit",
      params = list(stan_file = stan_file)
    )

    manifest <- list(r = "4.5.0", bayesgrove = "0.6.0")
    f1 <- bg_compute_fingerprint(
      handle,
      n1,
      environment_manifest = manifest
    )

    # Overwrite the file at the SAME path with different contents.
    writeLines("model { x ~ normal(0, 5); }", stan_file)
    f2 <- bg_compute_fingerprint(
      handle,
      n1,
      environment_manifest = manifest
    )

    expect_false(f1 == f2)
  })

  it("returns a sentinel for a missing stan_file and '' for nodes without one", {
    expect_equal(
      bayesgrove:::bg_source_hash_component(
        list(kind = "stan_fit", params = list(stan_file = "/no/such/file.stan"))
      ),
      "missing_stan_file"
    )
    expect_equal(
      bayesgrove:::bg_source_hash_component(
        list(kind = "stan_fit", params = list())
      ),
      ""
    )
    expect_equal(
      bayesgrove:::bg_source_hash_component(
        list(kind = "brms_fit", params = list(formula = "y ~ x"))
      ),
      ""
    )
  })

  it("re-executes a stan-file node after the file is edited (cache invalidation)", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    # Fake executor carrying a stan_file param; covered by param presence, not
    # kind name. No real Stan is invoked.
    bg_register_node_kind(
      handle,
      "fake_stan_fit",
      executor = function(node, inputs) {
        list(result = list(ran = TRUE, stan_file = node$params$stan_file))
      }
    )

    stan_file <- withr::local_tempfile(lines = "model { x ~ normal(0, 1); }")
    n1 <- bg_add_node(
      handle,
      kind = "fake_stan_fit",
      params = list(stan_file = stan_file)
    )

    run1 <- bg_run(handle, targets = n1)
    expect_equal(run1$status, "succeeded")
    expect_equal(run1$summary$total_executed, 1L)

    # Second run with an unchanged file: cache hit, no execution.
    run2 <- bg_run(handle, targets = n1)
    expect_equal(run2$status, "succeeded")
    expect_equal(run2$summary$total_executed, 0L)

    # Edit the file contents at the same path: must re-execute.
    writeLines("model { x ~ normal(0, 5); }", stan_file)
    run3 <- bg_run(handle, targets = n1)
    expect_equal(run3$status, "succeeded")
    expect_equal(run3$summary$total_executed, 1L)
  })

  it("bg_source_hash_component hashes the #include closure", {
    write_model <- function(root, prior) {
      dir.create(file.path(root, "parts"), recursive = TRUE)
      writeLines(
        c(
          "parameters { real theta; }",
          "model {",
          "  #include \"parts/prior.stan\"",
          "}"
        ),
        file.path(root, "main.stan")
      )
      writeLines(prior, file.path(root, "parts", "prior.stan"))
      normalizePath(file.path(root, "main.stan"), mustWork = FALSE)
    }

    root_a <- withr::local_tempdir()
    root_b <- withr::local_tempdir()
    main_a <- write_model(root_a, "theta ~ normal(0, 1);")
    main_b <- write_model(root_b, "theta ~ normal(0, 1);")

    node_a <- list(kind = "cmdstanr_fit", params = list(stan_file = main_a))
    node_b <- list(kind = "cmdstanr_fit", params = list(stan_file = main_b))

    # Same closure layout and contents at different absolute roots: the
    # component is machine-stable.
    expect_equal(
      bayesgrove:::bg_source_hash_component(node_a),
      bayesgrove:::bg_source_hash_component(node_b)
    )

    # Editing only the included file changes the component at a fixed path.
    writeLines(
      "theta ~ normal(0, 5);",
      file.path(root_a, "parts", "prior.stan")
    )
    expect_false(
      bayesgrove:::bg_source_hash_component(node_a) ==
        bayesgrove:::bg_source_hash_component(node_b)
    )

    # A missing include is distinct from a present one.
    root_c <- withr::local_tempdir()
    main_c <- write_model(root_c, "theta ~ normal(0, 1);")
    file.remove(file.path(root_c, "parts", "prior.stan"))
    node_c <- list(kind = "cmdstanr_fit", params = list(stan_file = main_c))
    writeLines(
      "theta ~ normal(0, 1);",
      file.path(root_b, "parts", "prior.stan")
    )
    expect_false(
      bayesgrove:::bg_source_hash_component(node_c) ==
        bayesgrove:::bg_source_hash_component(node_b)
    )

    # Missing includes key on the directive (referencing file plus raw
    # target), so identical layouts with the same missing include are
    # machine-stable too.
    root_d <- withr::local_tempdir()
    main_d <- write_model(root_d, "theta ~ normal(0, 1);")
    file.remove(file.path(root_d, "parts", "prior.stan"))
    node_d <- list(kind = "cmdstanr_fit", params = list(stan_file = main_d))
    expect_equal(
      bayesgrove:::bg_source_hash_component(node_c),
      bayesgrove:::bg_source_hash_component(node_d)
    )
  })

  it("re-executes a stan-file node after an included file is edited", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(
      handle,
      "fake_stan_fit",
      executor = function(node, inputs) {
        list(result = list(ran = TRUE, stan_file = node$params$stan_file))
      }
    )

    dir.create(file.path(tmp, "parts"))
    stan_file <- file.path(tmp, "main.stan")
    writeLines(
      c(
        "parameters { real theta; }",
        "model {",
        "  #include \"parts/prior.stan\"",
        "}"
      ),
      stan_file
    )
    writeLines("theta ~ normal(0, 1);", file.path(tmp, "parts", "prior.stan"))

    n1 <- bg_add_node(
      handle,
      kind = "fake_stan_fit",
      params = list(stan_file = stan_file)
    )

    run1 <- bg_run(handle, targets = n1)
    expect_equal(run1$status, "succeeded")
    expect_equal(run1$summary$total_executed, 1L)

    run2 <- bg_run(handle, targets = n1)
    expect_equal(run2$summary$total_executed, 0L)

    # Edit ONLY the included file; the main program's path and contents are
    # unchanged, but the closure hash must invalidate the cache.
    writeLines("theta ~ normal(0, 5);", file.path(tmp, "parts", "prior.stan"))
    run3 <- bg_run(handle, targets = n1)
    expect_equal(run3$status, "succeeded")
    expect_equal(run3$summary$total_executed, 1L)
  })
})
