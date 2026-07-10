describe("HMC severity rules (Phase 4.5)", {
  it("returns ok for clean diagnostics", {
    metrics <- list(
      divergences = 0L,
      max_treedepth_hits = 0L,
      max_rhat = 1.0,
      min_bulk_ess = 1000,
      min_tail_ess = 1000,
      e_bfmi = 0.5,
      num_transitions = 4000L
    )
    expect_equal(bg_hmc_severity(metrics), "ok")
  })

  it("returns error when max_rhat exceeds 1.05", {
    metrics <- list(
      divergences = 0L,
      max_treedepth_hits = 0L,
      max_rhat = 1.06,
      min_bulk_ess = 1000,
      min_tail_ess = 1000,
      e_bfmi = 0.5,
      num_transitions = 4000L
    )
    expect_equal(bg_hmc_severity(metrics), "error")
  })

  it("returns error when divergence rate exceeds 1 percent", {
    metrics <- list(
      divergences = 50L,
      max_treedepth_hits = 0L,
      max_rhat = 1.0,
      min_bulk_ess = 1000,
      min_tail_ess = 1000,
      e_bfmi = 0.5,
      num_transitions = 4000L
    )
    expect_equal(bg_hmc_severity(metrics), "error")
  })

  it("divergence rate uses total transitions across chains (not per-chain)", {
    # Ground truth from the plan: 4 chains x 1000 draws with 20 divergences.
    # divergences are summed across chains, so num_transitions must be the
    # total across chains (4000), giving a rate of 0.005 (below the 1%
    # threshold) -> "warning", NOT "error". The old bug divided by the
    # per-chain count (1000), inflating the rate to 0.02 and tripping "error".
    metrics <- list(
      divergences = 20L,
      max_treedepth_hits = 0L,
      max_rhat = 1.0,
      min_bulk_ess = 1000,
      min_tail_ess = 1000,
      e_bfmi = 0.5,
      num_transitions = 4000L
    )
    expect_equal(bg_hmc_severity(metrics), "warning")
  })

  it("returns warning when rhat is between 1.01 and 1.05", {
    metrics <- list(
      divergences = 0L,
      max_treedepth_hits = 0L,
      max_rhat = 1.03,
      min_bulk_ess = 1000,
      min_tail_ess = 1000,
      e_bfmi = 0.5,
      num_transitions = 4000L
    )
    expect_equal(bg_hmc_severity(metrics), "warning")
  })

  it("returns warning when ESS is below 400", {
    metrics <- list(
      divergences = 0L,
      max_treedepth_hits = 0L,
      max_rhat = 1.0,
      min_bulk_ess = 200,
      min_tail_ess = 1000,
      e_bfmi = 0.5,
      num_transitions = 4000L
    )
    expect_equal(bg_hmc_severity(metrics), "warning")
  })

  it("returns warning when E-BFMI is below 0.3", {
    metrics <- list(
      divergences = 0L,
      max_treedepth_hits = 0L,
      max_rhat = 1.0,
      min_bulk_ess = 1000,
      min_tail_ess = 1000,
      e_bfmi = 0.25,
      num_transitions = 4000L
    )
    expect_equal(bg_hmc_severity(metrics), "warning")
  })

  it("returns warning when there are any divergences (below rate threshold)", {
    metrics <- list(
      divergences = 1L,
      max_treedepth_hits = 0L,
      max_rhat = 1.0,
      min_bulk_ess = 1000,
      min_tail_ess = 1000,
      e_bfmi = 0.5,
      num_transitions = 4000L
    )
    expect_equal(bg_hmc_severity(metrics), "warning")
  })

  it("does not treat a missing transition count as a zero divergence rate", {
    metrics <- list(
      divergences = 2L,
      max_treedepth_hits = 0L,
      max_rhat = 1.0,
      min_bulk_ess = 1000,
      min_tail_ess = 1000,
      e_bfmi = 0.5,
      num_transitions = NA_integer_
    )

    rate <- bayesgrove:::bg_hmc_divergence_rate(metrics)
    expect_false(rate$available)
    expect_true(is.na(rate$value))
    expect_equal(bg_hmc_severity(metrics), "warning")
  })

  it("returns warning when treedepth saturation occurs", {
    metrics <- list(
      divergences = 0L,
      max_treedepth_hits = 5L,
      max_rhat = 1.0,
      min_bulk_ess = 1000,
      min_tail_ess = 1000,
      e_bfmi = 0.5,
      num_transitions = 4000L
    )
    expect_equal(bg_hmc_severity(metrics), "warning")
  })

  it("allows threshold overrides via node params", {
    metrics <- list(
      divergences = 0L,
      max_treedepth_hits = 0L,
      max_rhat = 1.02,
      min_bulk_ess = 1000,
      min_tail_ess = 1000,
      e_bfmi = 0.5,
      num_transitions = 4000L
    )
    expect_equal(
      bg_hmc_severity(metrics, thresholds = list(rhat_warn = 1.01)),
      "warning"
    )
    expect_equal(
      bg_hmc_severity(metrics, thresholds = list(rhat_warn = 1.05)),
      "ok"
    )
  })

  it("scales the ESS warning threshold by chain count when n_chains is given", {
    # Vehtari et al. (2021): ~100 effective samples per chain. With 4 chains
    # the default ess_warn becomes 400; an ESS of 350 trips warning.
    metrics <- list(
      divergences = 0L,
      max_treedepth_hits = 0L,
      max_rhat = 1.0,
      min_bulk_ess = 350,
      min_tail_ess = 1000,
      e_bfmi = 0.5,
      num_transitions = 4000L,
      n_chains = 4L
    )
    expect_equal(bg_hmc_severity(metrics), "warning")

    # With a single chain, the default ess_warn is 100; an ESS of 350 is fine.
    metrics$min_bulk_ess <- 350
    metrics$n_chains <- 1L
    expect_equal(bg_hmc_severity(metrics), "ok")

    # With 8 chains, ess_warn defaults to 800; an ESS of 350 trips warning.
    metrics$n_chains <- 8L
    expect_equal(bg_hmc_severity(metrics), "warning")
  })

  it("keeps the historical 400 default when n_chains is absent", {
    # No n_chains entry: ess_warn stays 400. ESS of 350 -> warning (same as
    # the pre-existing behavior).
    metrics <- list(
      divergences = 0L,
      max_treedepth_hits = 0L,
      max_rhat = 1.0,
      min_bulk_ess = 350,
      min_tail_ess = 1000,
      e_bfmi = 0.5,
      num_transitions = 4000L
    )
    expect_equal(bg_hmc_severity(metrics), "warning")
  })

  it("an explicit thresholds$ess_warn wins over the n_chains-derived default", {
    metrics <- list(
      divergences = 0L,
      max_treedepth_hits = 0L,
      max_rhat = 1.0,
      min_bulk_ess = 350,
      min_tail_ess = 1000,
      e_bfmi = 0.5,
      num_transitions = 4000L,
      n_chains = 8L
    )
    # n_chains=8 would default ess_warn to 800 (350 < 800 -> warning), but an
    # explicit override lowers it to 300, so 350 >= 300 -> ok.
    expect_equal(
      bg_hmc_severity(metrics, thresholds = list(ess_warn = 300)),
      "ok"
    )
    # And an explicit override can also raise it.
    expect_equal(
      bg_hmc_severity(metrics, thresholds = list(ess_warn = 500)),
      "warning"
    )
  })
})

describe("Executor registration (Phase 4.2)", {
  it("bg_use_cmdstanr registers built-in node kinds", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_use_cmdstanr(handle)

    kinds <- names(handle@registries$node_kinds)
    expect_true("cmdstanr_fit" %in% kinds)
    expect_true("stan_data" %in% kinds)
    expect_true("prior_fit" %in% kinds)
    expect_true("loo" %in% kinds)
    expect_true("compare" %in% kinds)
    expect_true("ppc" %in% kinds)

    # Each kind has a built-in executor resolved.
    for (kind in c("cmdstanr_fit", "stan_data", "loo")) {
      entry <- handle@registries$node_kinds[[kind]]
      expect_true(startsWith(entry$executor_ref, "builtin:"))
      expect_true(is.function(entry$executor))
    }
  })

  it("built-in executors persist as executor_ref and restore on reopen", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_use_cmdstanr(handle)
    bg_close(handle)

    reopened <- bg_open(path = tmp)
    entry <- reopened@registries$node_kinds[["cmdstanr_fit"]]
    expect_true(!is.null(entry))
    expect_true(startsWith(entry$executor_ref, "builtin:"))
    # Built-ins restore with a live executor (no trust gate).
    expect_true(is.function(entry$executor))
  })

  it("bg_use_brms registers brms node kinds", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_use_brms(handle)

    kinds <- names(handle@registries$node_kinds)
    expect_true("brms_fit" %in% kinds)
    expect_true("brms_prior_fit" %in% kinds)
    expect_true("loo" %in% kinds)
  })
})
