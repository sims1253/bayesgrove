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
    job <- final_job_for(handle, src)
    expect_null(job[["provenance"]])
    # The failure is durable, distinguishing it from a legacy execution.
    expect_identical(job$capture_error, "capture broke")
    expect_null(bg_node_pad(handle, src))
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

describe("file identities captured around execution", {
  # Kinds whose executor edits the file it is handed while it runs, as a long
  # fit would observe if its Stan source or data file were edited meanwhile.
  register_mutating_kinds <- function(handle) {
    bg_register_node_kind(
      handle,
      "path_src",
      executor = function(node, inputs) {
        node$params$path
      }
    )
    bg_register_node_kind(
      handle,
      "stan_mutator",
      executor = function(node, inputs) {
        before <- readLines(node$params$stan_file)
        if (isTRUE(node$params$mutate)) {
          cat(
            "// edited during the fit\n",
            file = node$params$stan_file,
            append = TRUE
          )
        }
        list(
          result = list(lines = length(before)),
          metadata = list(provenance = list(consumed = list(D = character())))
        )
      },
      pad = list(P = "stan_file", other = "mutate")
    )
    bg_register_node_kind(
      handle,
      "data_mutator",
      executor = function(node, inputs) {
        path <- inputs[[1]]
        read <- readLines(path)
        if (isTRUE(node$params$mutate)) {
          cat("tampered\n", file = path, append = TRUE)
        }
        list(
          result = list(read = read),
          metadata = list(
            provenance = list(
              consumed = list(
                D = names(inputs)[[1]],
                # A self-reported hash is never certified; the engine's wins.
                data_files = list(list(
                  node_id = names(inputs)[[1]],
                  sha256 = "forged"
                ))
              )
            )
          )
        )
      },
      pad = list(other = "mutate")
    )
  }

  it("records the Stan source hash taken before the executor ran", {
    handle <- bg_init(path = withr::local_tempdir())
    register_mutating_kinds(handle)
    stan_file <- withr::local_tempfile(fileext = ".stan")
    writeLines("parameters { real mu; }", stan_file)
    before <- digest::digest(file = stan_file, algo = "sha256")
    node <- bg_add_node(
      handle,
      kind = "stan_mutator",
      params = list(stan_file = stan_file, mutate = FALSE)
    )
    bg_run(handle)
    prov <- final_job_for(handle, node)$provenance
    expect_identical(prov$pad$P$stan_source, before)
    expect_null(prov$capture_error)
  })

  it("records no pad when the Stan source changed during execution", {
    handle <- bg_init(path = withr::local_tempdir())
    register_mutating_kinds(handle)
    stan_file <- withr::local_tempfile(fileext = ".stan")
    writeLines("parameters { real mu; }", stan_file)
    node <- bg_add_node(
      handle,
      kind = "stan_mutator",
      params = list(stan_file = stan_file, mutate = TRUE)
    )
    expect_equal(bg_run(handle)$status, "succeeded")

    job <- final_job_for(handle, node)
    prov <- job$provenance
    expect_null(prov$pad)
    expect_true("pad" %in% names(prov))
    expect_match(prov$capture_error, "changed during execution")
    expect_match(prov$capture_error, "stan_file", fixed = TRUE)
    # The edited source also changes the node's predicted fingerprint, so the
    # execution no longer backs the node's current artifact.
    expect_null(bg_node_pad(handle, node))
  })

  it("supplies pre-invocation hashes for reported data files", {
    handle <- bg_init(path = withr::local_tempdir())
    register_mutating_kinds(handle)
    data_file <- withr::local_tempfile(fileext = ".csv")
    writeLines("y", data_file)
    src <- bg_add_node(
      handle,
      kind = "path_src",
      params = list(path = data_file)
    )
    fit <- bg_add_node(
      handle,
      kind = "data_mutator",
      params = list(mutate = FALSE),
      inputs = src
    )
    bg_run(handle)
    prov <- final_job_for(handle, fit)$provenance
    sha <- digest::digest(file = data_file, algo = "sha256")
    expect_identical(prov$consumed$data_files[[1]]$sha256, sha)
    expect_identical(prov$pad$D$data, list(paste0("sha256:", sha)))
  })

  it("records no pad when a data file changed during execution", {
    handle <- bg_init(path = withr::local_tempdir())
    register_mutating_kinds(handle)
    data_file <- withr::local_tempfile(fileext = ".csv")
    writeLines("y", data_file)
    before <- digest::digest(file = data_file, algo = "sha256")
    src <- bg_add_node(
      handle,
      kind = "path_src",
      params = list(path = data_file)
    )
    fit <- bg_add_node(
      handle,
      kind = "data_mutator",
      params = list(mutate = TRUE),
      inputs = src
    )
    bg_run(handle)
    prov <- final_job_for(handle, fit)$provenance
    expect_null(prov$pad)
    expect_match(prov$capture_error, "data file", fixed = TRUE)
    expect_match(prov$capture_error, src, fixed = TRUE)
    # Only the pre-invocation hash is ever recorded.
    expect_identical(prov$consumed$data_files[[1]]$sha256, before)
  })

  it("detects changed and vanished files in the snapshot helper", {
    path <- withr::local_tempfile()
    writeLines("a", path)
    node <- list(params = list())
    unchanged <- bg_with_file_identities(
      node,
      list(n = path),
      NULL,
      function() 1
    )
    expect_identical(unchanged$value, 1)
    expect_false(unchanged$files$data_files$n$changed)
    expect_null(unchanged$files$error)
    expect_identical(unchanged$files$data_files$n$node_id, "n")

    removed <- bg_with_file_identities(
      node,
      list(n = path),
      NULL,
      function() unlink(path)
    )
    expect_true(removed$files$data_files$n$changed)
  })
})

describe("executor envelopes and inputs", {
  it("unwraps an envelope with metadata and no summaries", {
    expect_identical(
      bg_normalize_execution_result(list(result = 1, metadata = list(x = 2))),
      list(artifact = 1, summaries = list(), metadata = list(x = 2))
    )
    expect_identical(
      bg_normalize_execution_result(
        list(artifact = 1, metadata = list())
      )$artifact,
      1
    )
    # Ordinary list artifacts stay whole.
    ordinary <- list(
      list(result = 1, other = 2, metadata = 3),
      list(result = 1),
      list(metadata = 1),
      list(result = 1, artifact = 2, metadata = 3)
    )
    for (value in ordinary) {
      expect_identical(bg_normalize_execution_result(value)$artifact, value)
    }
  })

  it("records a report returned without summaries", {
    handle <- make_provenance_project()
    src <- bg_add_node(handle, kind = "src")
    bg_register_node_kind(handle, "quiet", executor = function(node, inputs) {
      list(
        result = "fit",
        metadata = list(
          provenance = list(
            consumed = list(subject = names(inputs)[[1]]),
            effective = list(A = list(chains = 2L))
          )
        )
      )
    })
    node <- bg_add_node(handle, kind = "quiet", inputs = src)
    bg_run(handle)

    expect_identical(bg_result(handle, node), "fit")
    prov <- final_job_for(handle, node)$provenance
    expect_identical(prov$consumed$subject, src)
    expect_identical(prov$effective$A$chains, 2L)
  })

  it("passes NULL upstream artifacts to the executor", {
    handle <- bg_init(path = withr::local_tempdir())
    bg_register_node_kind(handle, "nothing", executor = function(node, inputs) {
      NULL
    })
    bg_register_node_kind(handle, "count", executor = function(node, inputs) {
      list(n = length(inputs), ids = names(inputs))
    })
    up <- bg_add_node(handle, kind = "nothing")
    down <- bg_add_node(handle, kind = "count", inputs = up)
    bg_run(handle)

    expect_identical(bg_result(handle, down), list(n = 1L, ids = up))
    expect_length(final_job_for(handle, down)$provenance$inputs, 1L)
  })
})

describe("file report reconciliation", {
  # A path source feeding a reader whose behaviour is chosen by `mode`. Every
  # reader param is classified, so a pad is recorded unless reconciliation
  # finds a problem.
  make_reader_project <- function(data_file) {
    handle <- bg_init(
      path = withr::local_tempdir(.local_envir = parent.frame())
    )
    bg_register_node_kind(
      handle,
      "path_src",
      executor = function(node, inputs) {
        node$params$path
      }
    )
    bg_register_node_kind(
      handle,
      "reader",
      executor = function(node, inputs) {
        id <- names(inputs)[[1]]
        path <- inputs[[1]]
        mode <- node$params$mode
        consumed <- switch(
          mode,
          # Reads a sibling derived from the input path, reported under the
          # input's id.
          derived = {
            other <- paste0(path, ".derived")
            readLines(other)
            list(D = id, data_files = list(list(node_id = id, path = other)))
          },
          # Reads a file that only appears during execution.
          created = {
            created <- paste0(path, ".new")
            writeLines(as.character(stats::runif(1)), created)
            readLines(created)
            list(D = id, data_files = list(list(path = created)))
          },
          # Declares the file-like input unconsumed, then edits it.
          unconsumed = {
            cat("edited\n", file = path, append = TRUE)
            list(D = character())
          },
          # Consumes the input as D without listing it as a data file.
          d_only = {
            readLines(path)
            if (isTRUE(node$params$mutate)) {
              cat("edited\n", file = path, append = TRUE)
            }
            list(D = id)
          },
          # Reports the input path in a different but equivalent spelling.
          respelled = {
            readLines(path)
            list(
              D = id,
              data_files = list(list(
                node_id = id,
                path = file.path(dirname(path), ".", basename(path))
              ))
            )
          },
          # Lists the input as a data file but excludes it from D, then
          # edits it.
          undeclared = {
            readLines(path)
            cat("edited\n", file = path, append = TRUE)
            list(
              D = character(),
              data_files = list(list(node_id = id, path = path))
            )
          },
          # The input names a directory; a child file changes during the run.
          directory = {
            child <- file.path(path, "child.csv")
            readLines(child)
            writeLines(as.character(stats::runif(1)), child)
            list(D = id)
          },
          # The input names a file that only appears during execution.
          late = {
            writeLines(as.character(stats::runif(1)), path)
            readLines(path)
            list(D = id)
          }
        )
        list(
          result = list(mode = mode),
          metadata = list(provenance = list(consumed = consumed))
        )
      },
      pad = list(other = c("mode", "mutate"))
    )
    src <- bg_add_node(
      handle,
      kind = "path_src",
      params = list(path = data_file)
    )
    list(handle = handle, src = src)
  }

  run_reader <- function(
    mode,
    mutate = FALSE,
    input = c("file", "dir", "none")
  ) {
    input <- match.arg(input)
    data_file <- withr::local_tempfile(
      fileext = ".csv",
      .local_envir = parent.frame()
    )
    withr::defer(
      unlink(paste0(data_file, c(".derived", ".new"))),
      envir = parent.frame()
    )
    before <- NULL
    if (identical(input, "file")) {
      writeLines("y", data_file)
      writeLines("derived", paste0(data_file, ".derived"))
      before <- digest::digest(file = data_file, algo = "sha256")
    } else if (identical(input, "dir")) {
      dir.create(data_file)
      writeLines("child", file.path(data_file, "child.csv"))
    }
    project <- make_reader_project(data_file)
    reader <- bg_add_node(
      project$handle,
      kind = "reader",
      params = list(mode = mode, mutate = mutate),
      inputs = project$src
    )
    bg_run(project$handle)
    list(
      provenance = final_job_for(project$handle, reader)$provenance,
      src = project$src,
      sha = before
    )
  }

  it("rejects a report whose path disagrees with the input's snapshot", {
    out <- run_reader("derived")
    expect_null(out$provenance$pad)
    expect_match(out$provenance$capture_error, "does not match input")
    # The derived file never receives the input file's hash.
    recorded <- vapply(
      out$provenance$consumed$data_files,
      function(file) file$path,
      character(1)
    )
    expect_false(any(endsWith(recorded, ".derived")))
  })

  it("rejects a consumed file the engine did not hash before the run", {
    out <- run_reader("created")
    expect_null(out$provenance$pad)
    expect_match(out$provenance$capture_error, "not a consumed D input")
  })

  it("rejects a data file reported for an input outside consumed D", {
    out <- run_reader("undeclared")
    expect_null(out$provenance$pad)
    expect_match(out$provenance$capture_error, "not a consumed D input")
  })

  it("rejects a directory-valued D input", {
    out <- run_reader("directory", input = "dir")
    expect_null(out$provenance$pad)
    expect_match(out$provenance$capture_error, "directory")
  })

  it("rejects a D input file that was missing before the run", {
    out <- run_reader("late", input = "none")
    expect_null(out$provenance$pad)
    expect_match(out$provenance$capture_error, "missing")
    expect_identical(out$provenance$consumed$data_files, list())
  })

  it("resolves relative paths in the executing process", {
    # The executor runs in a working directory the main process does not
    # share (a daemon started elsewhere). Its snapshot and reported paths are
    # normalized there, so reconciliation elsewhere compares them as is.
    worker_dir <- withr::local_tempdir()
    writeLines("y", file.path(worker_dir, "input.csv"))
    executed <- withr::with_dir(
      worker_dir,
      bg_with_file_identities(
        list(params = list()),
        list(n = "input.csv"),
        NULL,
        function() {
          list(
            artifact = readLines("input.csv"),
            metadata = list(
              provenance = list(
                consumed = list(
                  D = "n",
                  data_files = list(list(node_id = "n", path = "input.csv"))
                )
              )
            )
          )
        }
      )
    )
    expected <- normalizePath(file.path(worker_dir, "input.csv"))
    expect_identical(executed$files$data_files$n$path, expected)
    reported <- executed$value$metadata$provenance$consumed$data_files
    expect_identical(reported[[1]]$path, expected)

    caller_dir <- withr::local_tempdir()
    reconciled <- withr::with_dir(
      caller_dir,
      bg_reconcile_data_files(reported, executed$files$data_files, "n")
    )
    expect_identical(reconciled$errors, character())
    expect_identical(
      reconciled$data_files[[1]]$sha256,
      digest::digest(file = expected, algo = "sha256")
    )
  })

  it("ignores changes to file-like inputs that were not consumed", {
    out <- run_reader("unconsumed")
    expect_null(out$provenance$capture_error)
    expect_true(is.list(out$provenance$pad))
    expect_identical(out$provenance$consumed$data_files, list())
  })

  it("identifies a consumed D input file by its pre-run hash", {
    out <- run_reader("d_only")
    expect_null(out$provenance$capture_error)
    expect_identical(out$provenance$consumed$data_files[[1]]$sha256, out$sha)
    expect_identical(
      out$provenance$pad$D$data,
      list(paste0("sha256:", out$sha))
    )

    changed <- run_reader("d_only", mutate = TRUE)
    expect_null(changed$provenance$pad)
    expect_match(changed$provenance$capture_error, "changed during execution")
  })

  it("compares reported paths after normalization", {
    out <- run_reader("respelled")
    expect_null(out$provenance$capture_error)
    expect_identical(out$provenance$consumed$data_files[[1]]$sha256, out$sha)
  })

  it("reconciles reports against the snapshot directly", {
    snapshot <- list(
      a = list(
        node_id = "a",
        path = "/data/a.csv",
        kind = "file",
        sha256 = "h1",
        changed = FALSE
      ),
      b = list(
        node_id = "b",
        path = "/data/b.csv",
        kind = "file",
        sha256 = "h2",
        changed = TRUE
      ),
      d = list(
        node_id = "d",
        path = "/data/dir",
        kind = "directory",
        sha256 = NA_character_,
        changed = TRUE
      )
    )
    # b changed but is not consumed; a is consumed as D only.
    ok <- bg_reconcile_data_files(list(), snapshot, d_ids = "a")
    expect_identical(ok$errors, character())
    expect_identical(
      ok$data_files,
      list(list(node_id = "a", path = "/data/a.csv", sha256 = "h1"))
    )

    by_path <- bg_reconcile_data_files(
      list(list(node_id = NULL, path = "/data/a.csv")),
      snapshot,
      d_ids = "a"
    )
    expect_identical(by_path$errors, character())
    expect_identical(by_path$data_files[[1]]$node_id, "a")

    # Paths are compared as normalized strings; the main process never
    # resolves them itself.
    relative <- bg_reconcile_data_files(
      list(list(node_id = "a", path = "a.csv")),
      snapshot,
      d_ids = "a"
    )
    expect_match(relative$errors, "does not match input")

    outside_d <- bg_reconcile_data_files(
      list(list(node_id = "c", path = NULL)),
      snapshot,
      d_ids = "a"
    )
    expect_match(outside_d$errors, "not a consumed D input")

    not_a_path <- bg_reconcile_data_files(
      list(list(node_id = "e", path = NULL)),
      snapshot,
      d_ids = "e"
    )
    expect_match(not_a_path$errors, "not a path")

    consumed_change <- bg_reconcile_data_files(list(), snapshot, d_ids = "b")
    expect_match(consumed_change$errors, "changed during execution")

    directory <- bg_reconcile_data_files(list(), snapshot, d_ids = "d")
    expect_match(directory$errors, "directory")
    expect_identical(directory$data_files, list())
  })
})
