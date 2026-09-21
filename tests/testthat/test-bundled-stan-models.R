describe("bundled Stan models", {
  stan_dir <- system.file("stan", package = "bayesgrove")

  it("declare yrep and log_lik in their generated quantities source", {
    for (model in c("normal_iid.stan", "lognormal_iid.stan")) {
      code <- readLines(file.path(stan_dir, model), warn = FALSE)

      expect_true(any(grepl("array\\[N\\] real yrep;", code)))
      expect_true(any(grepl("vector\\[N\\] log_lik;", code)))
      expect_true(any(grepl("log_lik\\[n\\] = .*_lpdf", code)))
    }
  })

  it("compile and sample, exposing yrep and log_lik with data-shaped dimensions", {
    skip_on_cran()
    skip_if_not_installed("cmdstanr")
    skip_if_not_installed("posterior")

    # cmdstanr can be installed without a CmdStan toolchain; compiling the
    # bundled programs needs the latter.
    tryCatch(
      cmdstanr::cmdstan_path(),
      error = function(e) testthat::skip("CmdStan is not installed")
    )

    datasets <- list(
      normal_iid.stan = list(
        N = 10L,
        y = c(0.11, -0.24, 0.53, 0.31, -0.08, 0.19, 0.42, -0.33, 0.05, -0.02)
      ),
      lognormal_iid.stan = list(
        N = 10L,
        y = c(1.12, 0.79, 1.70, 1.36, 0.92, 1.21, 1.52, 0.72, 1.05, 0.98)
      )
    )

    for (model_name in names(datasets)) {
      stan_file <- file.path(stan_dir, model_name)
      data <- datasets[[model_name]]
      n <- data$N

      # Compile into a temp dir so nothing is written next to the bundled
      # programs inside the installed package.
      exe_dir <- withr::local_tempdir()
      mod <- cmdstanr::cmdstan_model(
        stan_file = stan_file,
        dir = exe_dir,
        compile = TRUE
      )
      expect_true(file.exists(mod$exe_file()))

      fit <- mod$sample(
        data = data,
        chains = 2,
        iter_warmup = 50,
        iter_sampling = 50,
        seed = 1234
      )

      variables <- fit$draws_variables()
      expect_true("yrep" %in% variables)
      expect_true("log_lik" %in% variables)

      yrep <- posterior::as_draws_matrix(fit$draws(variables = "yrep"))
      log_lik <- posterior::as_draws_matrix(fit$draws(variables = "log_lik"))
      expect_equal(dim(yrep), c(100L, n))
      expect_equal(dim(log_lik), c(100L, n))
      expect_true(all(is.finite(yrep)))
      expect_true(all(is.finite(log_lik)))

      # Posterior predictions live on each model's observation scale.
      if (model_name == "lognormal_iid.stan") {
        expect_true(all(yrep > 0))
      }
    }
  })
})
