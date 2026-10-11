# Built-in executor provenance reports, effective-settings read-back, and
# the built-in P/A/D declarations (architecture/rules-and-evidence.md,
# section 2). The read-back helpers are exercised against stub objects so
# they run without cmdstanr or brms.

describe("built-in P/A/D declarations", {
  it("registers the built-in declarations and falls back to them on open", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_use_cmdstanr(handle)
    bg_use_brms(handle)
    kinds <- handle@registries$node_kinds
    expect_identical(kinds$cmdstanr_fit$pad$P, "stan_file")
    expect_identical(
      kinds$prior_fit$pad$A,
      c("iter_warmup", "iter_sampling", "seed")
    )
    expect_true(all(c("backend", "algorithm") %in% kinds$brms_fit$pad$A))
    expect_identical(kinds$brms_prior_fit$pad$P, "formula")
    expect_identical(kinds$brms_fit$pad$other, "thresholds")
    expect_null(kinds$loo$pad)
    bg_close(handle)

    # A project registered before declarations existed has none persisted.
    config <- bayesgrove:::bg_read_project_config_path(tmp)
    config$runtime_manifest$node_kinds$cmdstanr_fit$pad <- NULL
    bayesgrove:::bg_write_project_config_path(tmp, config)
    reopened <- bg_open(tmp, readonly = TRUE)
    expect_identical(
      reopened@registries$node_kinds$cmdstanr_fit$pad$P,
      "stan_file"
    )
  })
})

describe("fit data reports", {
  it("identifies file-path data by its content hash", {
    handle <- bg_init(path = withr::local_tempdir())
    data_file <- withr::local_tempfile(fileext = ".json")
    writeLines('{"N": 3}', data_file)
    bg_register_node_kind(
      handle,
      "path_src",
      executor = function(node, inputs) {
        node$params$path
      }
    )
    bg_register_node_kind(
      handle,
      "file_fit",
      executor = function(node, inputs) {
        selection <- bg_resolve_fit_data(node, inputs)
        list(
          result = list(read = readLines(selection$data)),
          summaries = list(),
          metadata = bg_fit_executor_report(selection, effective = NULL)
        )
      },
      pad = list(locator = "data_input")
    )
    src <- bg_add_node(
      handle,
      kind = "path_src",
      params = list(path = data_file)
    )
    fit <- bg_add_node(handle, kind = "file_fit", inputs = src)
    bg_run(handle)

    prov <- final_job_for(handle, fit)$provenance
    sha <- digest::digest(file = data_file, algo = "sha256")
    expect_identical(prov$consumed$D, list(src))
    expect_identical(prov$consumed$data_files[[1]]$sha256, sha)
    expect_identical(prov$consumed$data_files[[1]]$node_id, src)
    expect_identical(prov$pad$D$data, list(paste0("sha256:", sha)))
  })
})

describe("built-in executor reports", {
  it("selects fit data and reports the selected input id", {
    inputs <- list(a = list(x = 1), b = list(x = 2))
    chosen <- bg_resolve_fit_data(list(params = list(data_input = "b")), inputs)
    expect_identical(chosen$input_id, "b")
    expect_identical(chosen$data, list(x = 2))
    fallback <- bg_resolve_fit_data(
      list(params = list(data_input = "z")),
      inputs
    )
    expect_identical(fallback$input_id, "a")
    empty <- bg_resolve_fit_data(
      list(params = list()),
      list(),
      default = list()
    )
    expect_null(empty$input_id)
    expect_identical(empty$data, list())

    report <- bg_fit_executor_report(chosen, list(P = list(), A = list()))
    expect_identical(report$provenance$consumed$D, "b")
    expect_identical(report$provenance$consumed$data_files, list())
    expect_identical(
      bg_fit_executor_report(empty, NULL)$provenance$consumed$D,
      character()
    )
  })

  it("reports the subject a check evaluated", {
    skip_if_not_installed("posterior")
    yrep <- matrix(c(0, 0, 10, 10, 20, 20), nrow = 3, byrow = TRUE)
    colnames(yrep) <- c("yrep[1]", "yrep[2]")
    inputs <- list(data_node = list(y = c(9, 11)), fit_node = yrep)
    node <- list(
      params = list(
        stats = "mean",
        fit_input = "fit_node",
        data_input = "data_node"
      )
    )
    res <- bg_executor_ppc(node, inputs)
    expect_identical(res$metadata$provenance$consumed$subject, "fit_node")
  })

  it("reports the subject of loo", {
    skip_if_not_installed("posterior")
    skip_if_not_installed("loo")
    set.seed(4)
    arr <- array(
      rnorm(4 * 50 * 5, mean = -1),
      dim = c(iteration = 50, chain = 4, variable = 5),
      dimnames = list(
        iteration = NULL,
        chain = NULL,
        variable = paste0("log_lik[", 1:5, "]")
      )
    )
    fit <- list(draws = function() posterior::as_draws(arr))
    class(fit) <- c("CmdStanMCMC", class(fit))
    res <- bg_executor_loo(list(params = list()), list(fit_node = fit))
    expect_identical(res$metadata$provenance$consumed$subject, "fit_node")
  })

  it("resolves the brms backend and algorithm from params, then options", {
    withr::local_options(brms.backend = NULL, brms.algorithm = NULL)
    expect_identical(
      bg_brms_engine(list(params = list())),
      list(backend = "rstan", algorithm = "sampling")
    )
    withr::local_options(
      brms.backend = "cmdstanr",
      brms.algorithm = "meanfield"
    )
    expect_identical(
      bg_brms_engine(list(params = list())),
      list(backend = "cmdstanr", algorithm = "meanfield")
    )
    expect_identical(
      bg_brms_engine(list(
        params = list(backend = "rstan", algorithm = "sampling")
      )),
      list(backend = "rstan", algorithm = "sampling")
    )
  })
})

describe("effective-settings read-back", {
  cmdstan_meta <- list(
    stan_version_major = 2,
    stan_version_minor = 36,
    stan_version_patch = 0,
    algorithm = "hmc",
    iter_warmup = 200,
    iter_sampling = 300,
    adapt_delta = 0.9,
    max_treedepth = 12,
    seed = 123
  )

  it("reads cmdstanr settings from metadata() and num_chains()", {
    fit <- list(
      metadata = function() cmdstan_meta,
      num_chains = function() 4L
    )
    effective <- bg_cmdstanr_effective(fit)
    expect_length(effective$P, 0L)
    expect_identical(effective$A$stan_version, "2.36.0")
    expect_identical(effective$A$algorithm, "hmc")
    expect_identical(effective$A$chains, 4L)
    expect_identical(effective$A$iter_warmup, 200)
    expect_identical(effective$A$iter_sampling, 300)
    expect_identical(effective$A$adapt_delta, 0.9)
    expect_identical(effective$A$max_treedepth, 12)
    expect_identical(effective$A$seed, 123)
  })

  it("marks HMC settings inapplicable in fixed_param mode", {
    meta <- utils::modifyList(cmdstan_meta, list(algorithm = "fixed_param"))
    settings <- bg_cmdstan_sampler_settings(meta, chains = 1L)
    expect_identical(settings$adapt_delta, "inapplicable")
    expect_identical(settings$max_treedepth, "inapplicable")
    expect_identical(settings$chains, 1L)
  })

  it("records unavailable settings instead of failing", {
    fit <- list(
      metadata = function() stop("no csv"),
      num_chains = function() stop("no chains")
    )
    effective <- bg_cmdstanr_effective(fit)
    expect_identical(effective$A$chains, "unavailable")
    expect_identical(effective$A$seed, "unavailable")
    expect_identical(effective$A$stan_version, "unavailable")
    expect_identical(
      bg_stan_version_readback(list(
        stan_version_major = 2,
        stan_version_minor = NA,
        stan_version_patch = 0
      )),
      "unavailable"
    )
  })

  it("reads brms family and applied priors", {
    family <- list(family = "poisson", link = "log")
    expect_identical(
      bg_brms_family_readback(family),
      list(family = "poisson", link = "log")
    )
    expect_identical(bg_brms_family_readback(NULL), "unavailable")

    priors <- data.frame(
      prior = c("normal(0, 1)", ""),
      class = c("b", "Intercept"),
      coef = c("x", ""),
      source = c("user", "default"),
      stringsAsFactors = FALSE
    )
    rows <- bg_brms_prior_readback(priors)
    expect_length(rows, 2L)
    expect_identical(
      rows[[1]],
      list(prior = "normal(0, 1)", class = "b", coef = "x", source = "user")
    )
    expect_identical(bg_brms_prior_readback(NULL), "unavailable")
  })

  it("reads rstan sampler settings from @sim and @stan_args", {
    settings <- bg_rstan_sampler_settings(
      sim = list(chains = 2L, iter = 2000, warmup = 1000),
      stan_args = list(list(
        algorithm = "NUTS",
        seed = 42L,
        control = list(adapt_delta = 0.95, max_treedepth = 11)
      ))
    )
    expect_identical(settings$chains, 2L)
    expect_identical(settings$iter_warmup, 1000)
    expect_identical(settings$iter_sampling, 1000)
    expect_identical(settings$adapt_delta, 0.95)
    expect_identical(settings$max_treedepth, 11)
    expect_identical(settings$seed, 42L)
    expect_identical(settings$sampler, "NUTS")

    bare <- bg_rstan_sampler_settings(
      sim = list(chains = 1L),
      stan_args = list()
    )
    expect_identical(bare$adapt_delta, "unavailable")
    expect_identical(bare$iter_sampling, "unavailable")
  })

  it("reads brms sampler settings by backend and algorithm", {
    stanfit <- structure(list(), metadata = list(metadata = cmdstan_meta))
    cmdstan <- bg_brms_sampler_settings("cmdstanr", "sampling", stanfit)
    expect_identical(cmdstan$adapt_delta, 0.9)
    # This stub has no @sim slot, so the chain count is unavailable.
    expect_identical(cmdstan$chains, "unavailable")

    variational <- bg_brms_sampler_settings("cmdstanr", "meanfield", stanfit)
    expect_identical(variational$adapt_delta, "inapplicable")
    expect_identical(
      bg_brms_sampler_settings("other", "sampling", stanfit),
      "unavailable"
    )

    # Without the stanfit slots, rstan settings are unavailable, not errors.
    rstan <- bg_brms_sampler_settings("rstan", "sampling", list())
    expect_identical(rstan$chains, "unavailable")
    expect_identical(rstan$adapt_delta, "unavailable")
  })

  it("reads brms fit fields defensively from a stub", {
    fit <- structure(
      list(
        backend = "cmdstanr",
        algorithm = "sampling",
        version = list(brms = package_version("2.21.0")),
        fit = structure(list(), metadata = list(metadata = cmdstan_meta))
      ),
      class = "brmsfit"
    )
    effective <- bg_brms_effective(fit)
    expect_identical(effective$A$backend, "cmdstanr")
    expect_identical(effective$A$algorithm, "sampling")
    expect_identical(effective$A$version, list(brms = "2.21.0"))
    expect_identical(effective$A$sampler$iter_sampling, 300)
    # family()/prior_summary() need a real brmsfit (and brms).
    expect_true(is.list(effective$P))
    expect_named(effective$P, c("family", "priors"))
  })
})
