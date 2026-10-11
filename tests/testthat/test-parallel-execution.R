# Parallel execution (Milestone 3) -------------------------------------------
#
# Integration + unit coverage for the wave scheduler's parallel path. The
# daemon-backed tests skip on CRAN and when mirai/carrier are unavailable.
# They intentionally also run under pkgload::load_all(): the parallel
# dispatcher detects dev sessions and has each daemon load the executing
# source checkout (see bg_daemons_resolve_by_name()), so daemons run the
# code under test rather than a stale install.

describe("Parallel execution (Milestone 3)", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  skip_if_not_installed("carrier")
  skip_if_not_installed("purrr")

  it("runs independent siblings in parallel across daemons", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    # Executor reports the pid it ran on, so we can confirm >1 daemon was used.
    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(suffix = node$params$suffix %||% "?", pid = Sys.getpid())
    })
    root <- bg_add_node(handle, kind = "data", label = "root")
    l1 <- bg_add_node(
      handle,
      kind = "data",
      label = "l1",
      params = list(suffix = "1"),
      inputs = root
    )
    l2 <- bg_add_node(
      handle,
      kind = "data",
      label = "l2",
      params = list(suffix = "2"),
      inputs = root
    )
    l3 <- bg_add_node(
      handle,
      kind = "data",
      label = "l3",
      params = list(suffix = "3"),
      inputs = root
    )

    mirai::daemons(2, dispatcher = TRUE)
    on.exit(mirai::daemons(NULL), add = TRUE)

    run_res <- bg_run(handle, parallel = "always")
    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$summary$total_executed, 4)

    # Artifacts are correct and distinct per node.
    expect_equal(bg_result(handle, l1)$suffix, "1")
    expect_equal(bg_result(handle, l2)$suffix, "2")
    expect_equal(bg_result(handle, l3)$suffix, "3")

    # The leaves ran on daemon pids (different from the main process); at
    # least two distinct daemon pids were used for the 3-leaf wave.
    main_pid <- Sys.getpid()
    leaf_pids <- c(
      bg_result(handle, l1)$pid,
      bg_result(handle, l2)$pid,
      bg_result(handle, l3)$pid
    )
    expect_true(all(leaf_pids != main_pid))
    expect_gte(length(unique(leaf_pids)), 1L)
  })

  it("produces byte-identical artifacts to a sequential run", {
    # The determinism guarantee: parallel and sequential runs of the same graph
    # produce identical artifact refs (content-addressed) and identical result
    # objects. Built with deterministic executors (no pid/timestamp in output).
    make_graph <- function(handle) {
      bg_register_node_kind(handle, "data", executor = function(node, inputs) {
        list(
          label = node$label,
          suffix = node$params$suffix %||% "?",
          up_sum = sum(vapply(inputs, function(x) x$value %||% 0, numeric(1)))
        )
      })
      root <- bg_add_node(
        handle,
        kind = "data",
        label = "root",
        params = list(suffix = "r")
      )
      l1 <- bg_add_node(
        handle,
        kind = "data",
        label = "l1",
        params = list(suffix = "1"),
        inputs = root
      )
      l2 <- bg_add_node(
        handle,
        kind = "data",
        label = "l2",
        params = list(suffix = "2"),
        inputs = root
      )
      list(root = root, l1 = l1, l2 = l2)
    }

    # Sequential run.
    tmp_seq <- withr::local_tempdir()
    h_seq <- bg_init(path = tmp_seq)
    ids_seq <- make_graph(h_seq)
    run_seq <- bg_run(h_seq, parallel = "never")
    expect_equal(run_seq$status, "succeeded")

    # Parallel run of the same graph in a fresh project.
    tmp_par <- withr::local_tempdir()
    h_par <- bg_init(path = tmp_par)
    ids_par <- make_graph(h_par)
    mirai::daemons(2, dispatcher = TRUE)
    on.exit(mirai::daemons(NULL), add = TRUE)
    run_par <- bg_run(h_par, parallel = "always")
    expect_equal(run_par$status, "succeeded")

    # Artifact objects are identical (content-addressed determinism: same
    # executor + same inputs => same ref => same deserialized object).
    for (nm in names(ids_seq)) {
      obj_seq <- bg_result(h_seq, ids_seq[[nm]])
      obj_par <- bg_result(h_par, ids_par[[nm]])
      expect_equal(obj_seq, obj_par, info = paste("node", nm))
    }
  })

  it("completes a dependent chain under parallel dispatch", {
    # A two-wave graph: root (wave 1) then a single dependent leaf (wave 2).
    # The leaf wave has only one node, so it runs sequentially even under
    # parallel='always' (length(wave) > 1 is required for parallel dispatch).
    # This confirms the wave loop correctly progresses between waves when
    # parallel dispatch is enabled.
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(
        value = node$params$x %||% 1,
        saw_input = length(inputs) > 0L
      )
    })
    root <- bg_add_node(handle, kind = "data", label = "root")
    leaf <- bg_add_node(handle, kind = "data", label = "leaf", inputs = root)

    mirai::daemons(2, dispatcher = TRUE)
    on.exit(mirai::daemons(NULL), add = TRUE)

    run_res <- bg_run(handle, parallel = "always")
    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$summary$total_executed, 2)
    expect_false(bg_result(handle, root)$saw_input)
    expect_true(bg_result(handle, leaf)$saw_input)
  })

  it("reloads the checkout on daemons when its source fingerprint changes", {
    # Regression: the daemon-side load used to be keyed on the checkout path
    # alone, so daemons that had already loaded the checkout kept executing
    # its previous state after a mid-session re-run of load_all() with edited
    # code. The dev-session dispatch crate now ships a fingerprint of the
    # package sources (bg_checkout_fingerprint()), and each daemon compares
    # it with the fingerprint it last loaded, recorded in the daemon's loaded
    # namespace (bg_daemon_state), and reloads the checkout when they differ.
    #
    # The fingerprint is mocked to change between the two dispatches below,
    # so the test is deterministic and fast (no real source files are
    # touched). The executor reads the recorded fingerprint on whatever
    # daemon runs it, so the results prove whether the reload actually
    # happened: without the fingerprint comparison the second dispatch would
    # keep reporting the first fingerprint.
    skip_if(
      bayesgrove:::bg_daemons_resolve_by_name(),
      "fingerprint-based reload only applies to load_all() dev sessions"
    )
    counter <- local({
      n <- 0L
      function(path) {
        n <<- n + 1L
        paste0("mocked-fingerprint-", n)
      }
    })
    local_mocked_bindings(
      bg_checkout_fingerprint = counter,
      .package = "bayesgrove"
    )

    # Two sibling base nodes form one wave with length > 1, so each run takes
    # exactly one parallel dispatch and therefore consumes one fingerprint.
    # Reading bg_daemon_state via ::: works on the daemon even though the
    # checkout is loaded with attach = FALSE, because ::: resolves through
    # the namespace registry rather than the search path.
    executor_src <- paste0(
      "function(node, inputs) list(",
      "loaded = bayesgrove:::bg_daemon_state$checkout_fingerprint, ",
      "probe = 'fingerprint')"
    )
    probe_run <- function() {
      tmp <- withr::local_tempdir()
      handle <- bg_init(path = tmp)
      bg_register_node_kind(
        handle,
        "data",
        executor = eval(parse(text = executor_src))
      )
      a <- bg_add_node(handle, kind = "data", label = "a")
      b <- bg_add_node(handle, kind = "data", label = "b")
      run_res <- bg_run(handle, parallel = "always")
      expect_equal(run_res$status, "succeeded")
      expect_equal(run_res$summary$total_executed, 2)
      c(bg_result(handle, a)$loaded, bg_result(handle, b)$loaded)
    }

    mirai::daemons(2, dispatcher = TRUE)
    on.exit(mirai::daemons(NULL), add = TRUE)

    first <- probe_run()
    expect_equal(first, rep("mocked-fingerprint-1", 2))

    # Same daemons, "edited" sources: the changed fingerprint must make each
    # daemon reload the checkout before resolving the worker.
    second <- probe_run()
    expect_equal(second, rep("mocked-fingerprint-2", 2))
  })
})

describe("bg_prepare_executor_for_ship (crating)", {
  it("routes builtin executors by ref without shipping a function", {
    kind_reg <- list(executor_ref = "builtin:data")
    out <- bayesgrove:::bg_prepare_executor_for_ship(kind_reg, "data")
    expect_equal(out$mode, "builtin")
    expect_equal(out$ref, "builtin:data")
    expect_null(out$fn)
  })

  it("aborts when a user executor is not a function", {
    kind_reg <- list(executor_ref = NULL, executor = "not a function")
    expect_error(
      bayesgrove:::bg_prepare_executor_for_ship(kind_reg, "broken"),
      class = "rlang_error"
    )
  })

  it("ships a user executor as-is (with warning) when no source is available", {
    fn <- function(node, inputs) list(x = 1)
    kind_reg <- list(executor_ref = NULL, executor = fn, executor_source = NULL)
    expect_warning(
      out <- bayesgrove:::bg_prepare_executor_for_ship(kind_reg, "user"),
      "self-contained"
    )
    expect_equal(out$mode, "function")
    expect_false(isTRUE(out$rebuilt_from_source))
    expect_identical(out$fn, fn)
  })

  it("registry entries from bg_register_node_kind carry executor_source", {
    # Regression: the in-memory registry entry used to hold only (name,
    # executor), so parallel dispatch could never rebuild from source in the
    # session the executor was registered and warned on every wave.
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(x = 1)
    })
    entry <- handle@registries$node_kinds[["data"]]
    expect_true(is.character(entry$executor_source))
    expect_true(nzchar(entry$executor_source))
    out <- expect_no_warning(
      bayesgrove:::bg_prepare_executor_for_ship(entry, "data")
    )
    expect_true(isTRUE(out$rebuilt_from_source))
  })

  it("rebuilds a user executor from its persisted source when available", {
    # A self-contained executor defined as source text (as bg_register_node_kind
    # would persist it). Rebuilding from source yields a daemon-safe closure
    # whose parent is a globalenv child (no capture of caller runtime state).
    src <- "function(node, inputs) list(value = node$params$x %||% 0)"
    kind_reg <- list(
      executor_ref = NULL,
      executor = eval(parse(text = src)),
      executor_source = src
    )
    out <- bayesgrove:::bg_prepare_executor_for_ship(kind_reg, "user")
    expect_equal(out$mode, "function")
    expect_true(isTRUE(out$rebuilt_from_source))
    expect_true(is.function(out$fn))
    # The rebuilt function behaves like the original on a sample node.
    expect_equal(
      out$fn(list(params = list(x = 7)), list()),
      list(value = 7)
    )
  })
})

describe("bg_compute_wave / bg_parallel_active (Milestone 3)", {
  it("bg_parallel_active returns FALSE for 'never'", {
    expect_false(bayesgrove:::bg_parallel_active("never"))
  })

  it("bg_parallel_active errors for 'always' when mirai is absent or no daemons", {
    skip_if_not_installed("mirai")
    # No daemons set -> 'always' should error.
    expect_error(
      bayesgrove:::bg_parallel_active("always"),
      class = "rlang_error"
    )
    # 'auto' with no daemons -> FALSE (graceful fallback).
    expect_false(bayesgrove:::bg_parallel_active("auto"))
  })

  it("declares the complete dependency contract for parallel dispatch", {
    requirements <- bayesgrove:::bg_parallel_requirements()

    expect_equal(names(requirements), c("mirai", "purrr", "carrier"))
    expect_true(utils::compareVersion(requirements[["purrr"]], "1.1.0") >= 0)
  })

  it("flags whether daemons can resolve the executing bayesgrove copy", {
    # Daemons are fresh R processes that can only load bayesgrove from an
    # installed library. By-name worker resolution is therefore only faithful
    # when this process itself executes that installed copy. Under
    # pkgload::load_all() the executing namespace lives in the source
    # checkout: daemons must NOT resolve by name there, because that would
    # fail without an install or silently run a stale one.
    #
    # Skipped under covr: instrumentation loads a traced copy of the package
    # from a temporary path that is neither the source checkout nor the
    # installed location a fresh daemon would load, so both session arms of
    # this probe describe a session shape covr does not really represent and
    # the expectations would not hold there.
    skip_on_covr()
    ns <- asNamespace("bayesgrove")
    executing <- getNamespaceInfo(ns, "path")
    installed <- find.package("bayesgrove", lib.loc = .libPaths(), quiet = TRUE)
    matches_install <- !is.na(installed) &&
      identical(
        normalizePath(executing, mustWork = FALSE),
        normalizePath(installed, mustWork = FALSE)
      )

    from_checkout <- identical(
      normalizePath(executing, mustWork = FALSE),
      normalizePath(test_path("../.."), mustWork = FALSE)
    )
    if (from_checkout) {
      # Dev session (test_local / test_file under load_all).
      expect_false(bayesgrove:::bg_daemons_resolve_by_name())
    } else {
      # Installed session (R CMD check / library()): the executing copy is
      # the one a daemon finds on the library path.
      expect_true(matches_install)
      expect_true(bayesgrove:::bg_daemons_resolve_by_name())
    }
  })
})

describe("Parallel execution provenance", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  skip_if_not_installed("carrier")
  skip_if_not_installed("purrr")

  # Each node reports its consumed inputs and an effective setting read on
  # the process that ran it (its pid), so the record can be traced to the
  # worker.
  make_reporting_graph <- function(handle) {
    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(
        result = list(suffix = node$params$suffix %||% "root"),
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          severity = "ok",
          passed = TRUE,
          metrics = list(max_rhat = 1)
        )),
        metadata = list(
          provenance = list(
            consumed = list(D = names(inputs)),
            effective = list(A = list(pid = Sys.getpid()))
          )
        )
      )
    })
    root <- bg_add_node(handle, kind = "data", label = "root")
    leaves <- vapply(
      c("1", "2"),
      function(suffix) {
        bg_add_node(
          handle,
          kind = "data",
          label = paste0("leaf", suffix),
          params = list(suffix = suffix),
          inputs = root
        )
      },
      character(1)
    )
    list(root = root, leaves = unname(leaves))
  }

  final_job <- function(handle, node_id) {
    jobs <- Filter(
      function(job) identical(job$node_id, node_id),
      bg_jobs(handle)
    )
    jobs[[length(jobs)]]
  }

  it("records provenance from worker metadata on the main process", {
    handle <- bg_init(path = withr::local_tempdir())
    ids <- make_reporting_graph(handle)

    mirai::daemons(2, dispatcher = TRUE)
    on.exit(mirai::daemons(NULL), add = TRUE)
    run_res <- bg_run(handle, parallel = "always")
    expect_equal(run_res$status, "succeeded")

    root_job <- final_job(handle, ids$root)
    for (leaf in ids$leaves) {
      job <- final_job(handle, leaf)
      prov <- job$provenance
      expect_identical(prov$artifact_ref, job$result_ref, info = leaf)
      expect_identical(prov$consumed$D, list(ids$root), info = leaf)
      expect_identical(
        prov$inputs[[1]]$artifact_ref,
        root_job$result_ref,
        info = leaf
      )
      # The effective setting describes the worker that ran the executor.
      expect_false(identical(prov$effective$A$pid, Sys.getpid()), info = leaf)
      expect_true("bayesgrove" %in% names(prov$environment), info = leaf)

      summaries <- Filter(
        function(s) identical(s$node_id, leaf),
        bg_read_summaries(handle)
      )
      expect_identical(summaries[[1]]$job_id, job$job_id, info = leaf)
    }
  })

  it("reruns cached nodes under parallel dispatch", {
    handle <- bg_init(path = withr::local_tempdir())
    ids <- make_reporting_graph(handle)
    bg_run(handle, parallel = "never")
    before <- vapply(
      ids$leaves,
      function(leaf) final_job(handle, leaf)$job_id,
      character(1)
    )

    mirai::daemons(2, dispatcher = TRUE)
    on.exit(mirai::daemons(NULL), add = TRUE)
    run_res <- bg_run(handle, parallel = "always", rerun = ids$leaves)
    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$summary$total_executed, 2L)
    expect_identical(run_res$metadata$rerun_pending, character())
    for (i in seq_along(ids$leaves)) {
      job <- final_job(handle, ids$leaves[[i]])
      expect_false(identical(job$job_id, before[[i]]))
      expect_identical(
        bg_node_pad(handle, ids$leaves[[i]])$job_id,
        job$job_id
      )
    }
  })

  it("takes file identities on the worker, before the executor runs", {
    handle <- bg_init(path = withr::local_tempdir())
    data_dir <- withr::local_tempdir()
    bg_register_node_kind(handle, "path", executor = function(node, inputs) {
      node$params$path
    })
    # Each reader appends to its file when asked, as an edit during a long
    # fit would.
    bg_register_node_kind(handle, "reader", executor = function(node, inputs) {
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
              D = names(inputs),
              data_files = list(list(node_id = names(inputs)[[1]]))
            )
          )
        )
      )
    })
    files <- file.path(data_dir, c("a.csv", "b.csv"))
    for (f in files) {
      writeLines("y", f)
    }
    hashes <- vapply(
      files,
      function(f) digest::digest(file = f, algo = "sha256"),
      character(1)
    )
    sources <- vapply(
      files,
      function(f) bg_add_node(handle, kind = "path", params = list(path = f)),
      character(1)
    )
    readers <- c(
      bg_add_node(
        handle,
        kind = "reader",
        params = list(mutate = FALSE),
        inputs = sources[[1]]
      ),
      bg_add_node(
        handle,
        kind = "reader",
        params = list(mutate = TRUE),
        inputs = sources[[2]]
      )
    )

    mirai::daemons(2, dispatcher = TRUE)
    on.exit(mirai::daemons(NULL), add = TRUE)
    bg_run(handle, parallel = "always")

    clean <- final_job(handle, readers[[1]])$provenance
    expect_identical(clean$consumed$data_files[[1]]$sha256, unname(hashes[[1]]))
    expect_null(clean$capture_error)

    changed <- final_job(handle, readers[[2]])$provenance
    expect_match(changed$capture_error, "changed during execution")
    expect_identical(
      changed$consumed$data_files[[1]]$sha256,
      unname(hashes[[2]])
    )
  })

  it("fails only the node whose finalization errors", {
    handle <- bg_init(path = withr::local_tempdir())
    bg_register_node_kind(handle, "src", executor = function(node, inputs) 1)
    # A worker can succeed while returning a summary the main process
    # rejects (an invalid severity).
    bg_register_node_kind(handle, "leaf", executor = function(node, inputs) {
      list(
        result = node$params$label,
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          severity = node$params$severity,
          passed = TRUE
        ))
      )
    })
    root <- bg_add_node(handle, kind = "src")
    bad <- bg_add_node(
      handle,
      kind = "leaf",
      params = list(label = "bad", severity = "catastrophic"),
      inputs = root
    )
    good <- bg_add_node(
      handle,
      kind = "leaf",
      params = list(label = "good", severity = "ok"),
      inputs = root
    )

    mirai::daemons(2, dispatcher = TRUE)
    on.exit(mirai::daemons(NULL), add = TRUE)
    run <- bg_run(handle, parallel = "always")

    expect_equal(run$status, "failed")
    expect_match(run$error$message, "invalid")
    statuses <- vapply(bg_jobs(handle), `[[`, character(1), "status")
    expect_false(any(statuses == "running"))
    expect_identical(final_job(handle, bad)$status, "failed")
    expect_identical(final_job(handle, good)$status, "succeeded")
    expect_false(is.null(final_job(handle, good)$provenance))
  })

  it("removes finalized reruns from the pending set when a sibling fails", {
    handle <- bg_init(path = withr::local_tempdir())
    flag <- file.path(withr::local_tempdir(), "fail")
    bg_register_node_kind(handle, "flaky", executor = function(node, inputs) {
      if (isTRUE(node$params$may_fail) && file.exists(node$params$flag)) {
        stop("flaky failure")
      }
      list(draw = stats::runif(1))
    })
    ok <- bg_add_node(
      handle,
      kind = "flaky",
      label = "ok",
      params = list(flag = flag, may_fail = FALSE)
    )
    bad <- bg_add_node(
      handle,
      kind = "flaky",
      label = "bad",
      params = list(flag = flag, may_fail = TRUE)
    )
    bg_run(handle, parallel = "never")
    writeLines("x", flag)

    mirai::daemons(2, dispatcher = TRUE)
    on.exit(mirai::daemons(NULL), add = TRUE)
    run <- bg_run(handle, parallel = "always", rerun = c(ok, bad))
    expect_equal(run$status, "failed")
    expect_identical(run$metadata$rerun_pending, bad)
    expect_identical(final_job(handle, ok)$status, "succeeded")
    expect_identical(final_job(handle, bad)$status, "failed")
  })
})
