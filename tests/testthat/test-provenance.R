# Execution provenance in job records, P/A/D declarations and snapshots,
# resolution, and execution-stamped summaries
# (architecture/rules-and-evidence.md, section 2). Fixtures live in
# helper-provenance.R.

describe("bg_canonical_digest()", {
  it("is invariant to the order of named list entries at every depth", {
    a <- list(b = 1, a = list(y = "x", x = c(1.5, 2)))
    b <- list(a = list(x = c(1.5, 2), y = "x"), b = 1)
    expect_identical(bg_canonical_digest(a), bg_canonical_digest(b))
    expect_false(identical(
      bg_canonical_digest(a),
      bg_canonical_digest(list(a = list(x = 1, y = "x"), b = 1))
    ))
  })

  it("is stable across a JSON round trip", {
    x <- list(z = list(), y = bg_empty_named_list(), x = 0.1, w = list("a"))
    expect_identical(
      bg_canonical_digest(x),
      bg_canonical_digest(bg_json_roundtrip(x))
    )
  })
})

describe("execution provenance in job records", {
  it("records provenance on the final record of a succeeded job", {
    handle <- make_provenance_project()
    src <- bg_add_node(handle, kind = "src")
    fit <- bg_add_node(
      handle,
      kind = "fit",
      params = list(model = "m1", scale = 3, data_input = src),
      inputs = src
    )
    expect_equal(bg_run(handle)$status, "succeeded")

    job <- final_job_for(handle, fit)
    prov <- job$provenance
    expect_identical(prov$artifact_ref, job$result_ref)
    expect_identical(
      prov$fingerprint,
      bg_plan(handle)$metadata$fingerprints[[fit]]
    )
    expect_length(prov$inputs, 1L)
    expect_identical(prov$inputs[[1]]$node_id, src)
    expect_identical(
      prov$inputs[[1]]$artifact_ref,
      final_job_for(handle, src)$result_ref
    )
    expect_identical(prov$consumed$D, list(src))
    expect_null(prov$consumed$subject)
    expect_identical(prov$effective$A$chains, 4L)
    expect_identical(prov$environment$r, as.character(getRversion()))
    expect_true(is.list(prov$pad))
  })

  it("applies the defaults for executors that report nothing", {
    handle <- make_provenance_project()
    src <- bg_add_node(handle, kind = "src")
    bg_run(handle)

    prov <- final_job_for(handle, src)$provenance
    expect_identical(prov$inputs, list())
    expect_identical(prov$consumed$D, list())
    expect_null(prov$consumed$subject)
    expect_identical(prov$consumed$data_files, list())
    expect_length(prov$effective$P, 0L)
    expect_length(prov$effective$A, 0L)
    # The src kind declares no pad.
    expect_null(prov$pad)
    expect_true("pad" %in% names(prov))
  })

  it("survives reopening and job compaction", {
    tmp <- withr::local_tempdir()
    handle <- make_provenance_project(tmp)
    src <- bg_add_node(handle, kind = "src")
    fit <- bg_add_node(
      handle,
      kind = "fit",
      params = list(model = "m1", scale = 3),
      inputs = src
    )
    bg_run(handle)
    before <- final_job_for(handle, fit)$provenance
    bg_compact_jobs(handle)
    bg_close(handle)

    reopened <- bg_open(tmp, readonly = TRUE)
    after <- final_job_for(reopened, fit)$provenance
    # Key order may differ between the in-memory and persisted records; the
    # canonical form, and therefore every digest, may not.
    expect_identical(bg_canonical_json(after), bg_canonical_json(before))
  })

  it("records no provenance for failed jobs", {
    handle <- bg_init(path = withr::local_tempdir())
    bg_register_node_kind(handle, "boom", executor = function(node, inputs) {
      stop("no")
    })
    node <- bg_add_node(handle, kind = "boom")
    expect_equal(bg_run(handle)$status, "failed")
    job <- final_job_for(handle, node)
    expect_identical(job$status, "failed")
    expect_null(job$provenance)
  })

  it("keeps only reported ids that are inputs of the execution", {
    report <- bg_normalize_executor_report(
      list(
        provenance = list(
          consumed = list(D = c("b", "ghost"), subject = "ghost"),
          effective = list(P = "not a named list")
        )
      ),
      input_ids = c("a", "b")
    )
    expect_identical(report$consumed$D, list("b"))
    expect_null(report$consumed$subject)
    expect_length(report$effective$P, 0L)

    defaults <- bg_normalize_executor_report(list(), input_ids = c("a", "b"))
    expect_identical(defaults$consumed$D, list("a", "b"))
  })

  it("never fails the run when provenance capture errors", {
    handle <- make_provenance_project()
    src <- bg_add_node(handle, kind = "src")
    testthat::with_mocked_bindings(
      bg_build_execution_provenance = function(...) stop("capture broke"),
      expect_warning(
        res <- bg_run(handle),
        "Could not record execution provenance"
      ),
      .package = "bayesgrove"
    )
    expect_equal(res$status, "succeeded")
    expect_null(final_job_for(handle, src)$provenance)
  })
})

describe("P/A/D declarations and snapshots", {
  it("validates declarations", {
    handle <- bg_init(path = withr::local_tempdir())
    expect_error(
      bg_register_node_kind(handle, "k", pad = list(Q = "x")),
      "Unknown categor"
    )
    expect_error(
      bg_register_node_kind(handle, "k", pad = list(P = "x", A = "x")),
      "more than one"
    )
    expect_error(
      bg_register_node_kind(handle, "k", pad = list(P = 1)),
      "character vector"
    )
    expect_error(bg_register_node_kind(handle, "k", pad = "P"), "named list")
  })

  it("persists the declaration and restores it on open", {
    tmp <- withr::local_tempdir()
    handle <- make_provenance_project(tmp)
    expect_identical(handle@registries$node_kinds$fit$pad$P, "model")
    bg_close(handle)

    reopened <- bg_open(tmp, readonly = TRUE)
    pad <- reopened@registries$node_kinds$fit$pad
    expect_identical(pad$P, "model")
    expect_identical(pad$A, "scale")
    expect_identical(pad$D, character())
    expect_identical(pad$locator, "data_input")
    expect_identical(pad$other, "thresholds")
    expect_null(reopened@registries$node_kinds$src$pad)
  })

  it("records digests recomputable from the persisted record", {
    handle <- make_provenance_project()
    src <- bg_add_node(handle, kind = "src")
    fit <- bg_add_node(
      handle,
      kind = "fit",
      params = list(model = "m1", scale = 3, data_input = src),
      inputs = src
    )
    bg_run(handle)

    pad <- final_job_for(handle, fit)$provenance$pad
    expect_identical(pad$P$kind, "fit")
    expect_identical(pad$P$params, list(model = "m1"))
    expect_identical(pad$A$params, list(scale = 3L))
    expect_identical(pad$A$effective, list(chains = 4L))
    expect_match(pad$A$executor, "^sha256:")
    expect_identical(pad$D$data, list(final_job_for(handle, src)$result_ref))
    expect_identical(pad$D$producers, list(src))
    expect_identical(pad$D$locator, list(data_input = src))
    for (category in c("P", "A", "D")) {
      component <- pad[[category]]
      component$digest <- NULL
      expect_identical(
        pad[[category]]$digest,
        bg_pad_component_digest(component, category),
        info = category
      )
    }
  })

  it("records no pad when a parameter is unclassified", {
    handle <- make_provenance_project()
    src <- bg_add_node(handle, kind = "src")
    fit <- bg_add_node(
      handle,
      kind = "fit",
      params = list(model = "m1", scale = 3, extra = TRUE),
      inputs = src
    )
    bg_run(handle)
    prov <- final_job_for(handle, fit)$provenance
    expect_false(is.null(prov))
    expect_null(prov$pad)
  })

  it("identifies data by content, not by producer node", {
    handle <- make_provenance_project()
    # Different params, so different fingerprints, but identical output bytes.
    s1 <- bg_add_node(handle, kind = "src", params = list(tag = "a"))
    s2 <- bg_add_node(handle, kind = "src", params = list(tag = "b"))
    f1 <- bg_add_node(
      handle,
      kind = "fit",
      params = list(model = "m1", scale = 3),
      inputs = s1
    )
    f2 <- bg_add_node(
      handle,
      kind = "fit",
      params = list(model = "m1", scale = 3),
      inputs = s2
    )
    bg_run(handle)

    d1 <- final_job_for(handle, f1)$provenance$pad$D
    d2 <- final_job_for(handle, f2)$provenance$pad$D
    expect_identical(d1$digest, d2$digest)
    expect_false(identical(d1$producers, d2$producers))
  })

  it("identifies a Stan program by its source contents, not its path", {
    handle <- bg_init(path = withr::local_tempdir())
    bg_register_node_kind(
      handle,
      "stan_like",
      executor = function(node, inputs) list(ok = TRUE),
      pad = list(P = "stan_file", A = "chains")
    )
    dir_a <- withr::local_tempdir()
    dir_b <- withr::local_tempdir()
    program <- "parameters { real mu; } model { mu ~ normal(0, 1); }"
    writeLines(program, file.path(dir_a, "model.stan"))
    writeLines(program, file.path(dir_b, "model.stan"))
    n1 <- bg_add_node(
      handle,
      kind = "stan_like",
      params = list(stan_file = file.path(dir_a, "model.stan"), chains = 2)
    )
    n2 <- bg_add_node(
      handle,
      kind = "stan_like",
      params = list(stan_file = file.path(dir_b, "model.stan"), chains = 2)
    )
    bg_run(handle)

    p1 <- final_job_for(handle, n1)$provenance$pad$P
    p2 <- final_job_for(handle, n2)$provenance$pad$P
    expect_identical(p1$digest, p2$digest)
    expect_identical(
      p1$stan_source,
      digest::digest(file = file.path(dir_a, "model.stan"), algo = "sha256")
    )
    expect_false("stan_file" %in% names(p1$params))
    expect_false(identical(p1$locator$stan_file, p2$locator$stan_file))
  })
})

describe("bg_node_pad()", {
  it("returns the provenance of the execution behind the node's artifact", {
    handle <- make_provenance_project()
    src <- bg_add_node(handle, kind = "src")
    fit <- bg_add_node(
      handle,
      kind = "fit",
      params = list(model = "m1", scale = 3),
      inputs = src
    )
    expect_null(bg_node_pad(handle, fit))
    bg_run(handle)

    job <- final_job_for(handle, fit)
    pad <- bg_node_pad(handle, fit)
    expect_identical(pad$job_id, job$job_id)
    expect_identical(pad$artifact_ref, job$result_ref)
    expect_identical(pad$pad, job$provenance$pad)
    expect_error(bg_node_pad(handle, "node_missing"), "not found")
  })

  it("returns NULL for executions without recorded provenance", {
    handle <- make_provenance_project()
    src <- bg_add_node(handle, kind = "src")
    bg_run(handle)
    # Simulate a pre-provenance execution: its final record lacks the field.
    bayesgrove:::bg_update_job(
      handle,
      final_job_for(handle, src)$job_id,
      provenance = NULL
    )
    expect_null(bg_node_pad(handle, src))
  })

  it("returns NULL when the artifact is missing or corrupt in the CAS", {
    handle <- make_provenance_project()
    src <- bg_add_node(handle, kind = "src")
    bg_run(handle)
    ref <- final_job_for(handle, src)$result_ref
    writeLines("tampered", bg_cas_blob_path(handle, ref))
    expect_null(bg_node_pad(handle, src))
  })

  it("resolves cache reuse to the representative execution", {
    handle <- make_provenance_project()
    a <- bg_add_node(handle, kind = "src", label = "a")
    bg_run(handle, targets = a)
    # Same kind and params, so the same fingerprint: b reuses a's artifact.
    b <- bg_add_node(handle, kind = "src", label = "b")
    run <- bg_run(handle)
    expect_equal(run$summary$total_executed, 0L)

    job_a <- final_job_for(handle, a)
    expect_identical(bg_node_pad(handle, b)$job_id, job_a$job_id)
  })
})

describe("execution-stamped summaries", {
  it("stamps summaries written by an execution with its job id", {
    handle <- make_provenance_project()
    src <- bg_add_node(handle, kind = "src")
    fit <- bg_add_node(
      handle,
      kind = "fit",
      params = list(model = "m1", scale = 3),
      inputs = src
    )
    bg_run(handle)
    summaries <- Filter(
      function(s) identical(s$node_id, fit),
      bg_read_summaries(handle)
    )
    expect_length(summaries, 1L)
    expect_identical(summaries[[1]]$job_id, final_job_for(handle, fit)$job_id)
  })

  it("rejects caller-supplied job ids in bg_write_summaries()", {
    handle <- make_provenance_project()
    src <- bg_add_node(handle, kind = "src")
    bg_run(handle)
    ref <- final_job_for(handle, src)$result_ref
    fp <- final_job_for(handle, src)$provenance$fingerprint
    expect_error(
      bg_write_summaries(
        handle,
        src,
        ref,
        fp,
        list(list(summary_kind = "hmc_diagnostics", job_id = "job_forged"))
      ),
      "must not supply"
    )
    written <- bg_write_summaries(
      handle,
      src,
      ref,
      fp,
      list(list(summary_kind = "hmc_diagnostics"))
    )
    expect_null(written[[1]]$job_id)
  })
})
