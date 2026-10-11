describe("cmdstanr integration (Phase 4.5)", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  skip_if_not_installed("posterior")

  stan_file <- withr::local_tempfile(fileext = ".stan")
  writeLines(
    c(
      "data { int<lower=1> N; array[N] int<lower=0,upper=1> y; }",
      "parameters { real<lower=0,upper=1> theta; }",
      "model { theta ~ beta(1,1); y ~ bernoulli(theta); }"
    ),
    stan_file
  )

  # Stan needs typed integers; JSON-serialized node params would corrupt them,
  # so data is attached via the CAS-backed bg_set_node_data which preserves R
  # types via saveRDS.
  bernoulli_data <- list(
    N = 10L,
    y = as.integer(c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1))
  )

  it("fits a tiny model and emits an hmc_diagnostics summary", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_use_cmdstanr(handle)

    n_data <- bg_add_node(
      handle,
      kind = "stan_data",
      label = "Bernoulli data"
    )
    bg_set_node_data(handle, n_data, bernoulli_data)
    n_fit <- bg_add_node(
      handle,
      kind = "cmdstanr_fit",
      label = "Bernoulli fit",
      inputs = n_data,
      params = list(
        stan_file = stan_file,
        data_input = "stan_data",
        chains = 1,
        iter_warmup = 200,
        iter_sampling = 200,
        seed = 123
      )
    )

    run_res <- bg_run(handle, targets = n_fit)
    expect_equal(run_res$status, "succeeded")

    summaries <- bg_read_summaries(handle)
    hmc <- Filter(
      function(s) identical(s$summary_kind, "hmc_diagnostics"),
      summaries
    )
    expect_gte(length(hmc), 1)
    expect_equal(hmc[[1]]$summary_kind, "hmc_diagnostics")
    expect_true(hmc[[1]]$severity %in% c("ok", "warning", "error"))

    # The job records the execution's provenance with the settings read back
    # from the fitted object, and the summary is stamped with the job.
    fit_jobs <- Filter(
      function(job) identical(job$node_id, n_fit),
      bg_jobs(handle, status = "succeeded")
    )
    job <- fit_jobs[[length(fit_jobs)]]
    prov <- job$provenance
    expect_identical(prov$artifact_ref, job$result_ref)
    expect_identical(prov$consumed$D, list(n_data))
    expect_identical(hmc[[1]]$job_id, job$job_id)
    effective <- prov$effective$A
    expect_equal(effective$chains, 1)
    expect_equal(effective$iter_warmup, 200)
    expect_equal(effective$iter_sampling, 200)
    # Not set on the node, so CmdStan's defaults are what the fit used.
    expect_equal(effective$adapt_delta, 0.8)
    expect_equal(effective$max_treedepth, 10)
    expect_equal(effective$seed, 123)
    expect_null(prov$capture_error)
    expect_true(is.character(effective$algorithm))
    expect_match(effective$stan_version, "^[0-9]+\\.[0-9]+\\.[0-9]+$")

    # Every cmdstanr_fit param is classified, so a P/A/D snapshot exists and
    # the Stan program is identified by its contents.
    pad <- bg_node_pad(handle, n_fit)
    expect_identical(pad$job_id, job$job_id)
    expect_identical(
      pad$pad$P$stan_source,
      digest::digest(file = stan_file, algo = "sha256")
    )
    expect_identical(pad$pad$A$params$seed, 123L)
    expect_identical(pad$pad$D$data, list(prov$inputs[[1]]$artifact_ref))
    expect_match(pad$pad$P$digest, "^[0-9a-f]{64}$")
  })

  it("cached fit survives saveRDS and can call $draws()", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_use_cmdstanr(handle)

    n_data <- bg_add_node(
      handle,
      kind = "stan_data",
      label = "Data"
    )
    bg_set_node_data(handle, n_data, bernoulli_data)
    n_fit <- bg_add_node(
      handle,
      kind = "cmdstanr_fit",
      label = "Fit",
      inputs = n_data,
      params = list(
        stan_file = stan_file,
        data_input = "stan_data",
        chains = 1,
        iter_warmup = 200,
        iter_sampling = 200,
        seed = 456
      )
    )

    bg_run(handle, targets = n_fit)

    fit <- bg_result(handle, n_fit)
    tmp_rds <- withr::local_tempfile(fileext = ".rds")
    saveRDS(fit, tmp_rds)
    reloaded <- readRDS(tmp_rds)
    expect_true(!is.null(reloaded$draws()))
  })

  it("artifact survives a fresh bg_open + fetch", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_use_cmdstanr(handle)

    n_data <- bg_add_node(
      handle,
      kind = "stan_data",
      label = "Data"
    )
    bg_set_node_data(handle, n_data, bernoulli_data)
    n_fit <- bg_add_node(
      handle,
      kind = "cmdstanr_fit",
      label = "Fit",
      inputs = n_data,
      params = list(
        stan_file = stan_file,
        data_input = "stan_data",
        chains = 1,
        iter_warmup = 200,
        iter_sampling = 200,
        seed = 789
      )
    )

    bg_run(handle, targets = n_fit)
    bg_close(handle)

    reopened <- bg_open(path = tmp)
    fit <- bg_result(reopened, n_fit)
    expect_true(!is.null(fit$draws()))
  })
})
