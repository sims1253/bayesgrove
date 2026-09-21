inherits_warning <- function(cnd) {
  inherits(cnd, "warning")
}

inherits_error <- function(cnd) {
  inherits(cnd, "error")
}

reset_handle_deprecation_warnings <- function() {
  rm(
    list = ls(envir = .bg_deprecated_handle_env),
    envir = .bg_deprecated_handle_env
  )
}

collect_conditions <- function(expr) {
  conds <- list()
  withCallingHandlers(
    tryCatch(
      force(expr),
      error = function(e) {
        conds[[length(conds) + 1L]] <<- e
        NULL
      }
    ),
    warning = function(w) {
      conds[[length(conds) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  conds
}

describe("Practitioner fit helpers", {
  it("bg_fit_stan names its backend setup function before changing the graph", {
    handle <- bg_init(withr::local_tempdir())

    expect_error(
      bg_fit_stan(handle, "missing.stan", list()),
      "bg_use_cmdstanr"
    )
    expect_length(bg_read_graph(handle)$nodes, 0L)
  })

  it("bg_fit_brms names its backend setup function before changing the graph", {
    handle <- bg_init(withr::local_tempdir())

    expect_error(
      bg_fit_brms(handle, y ~ x, data.frame(y = 1, x = 1)),
      "bg_use_brms"
    )
    expect_length(bg_read_graph(handle)$nodes, 0L)
  })

  it("bg_fit_brms creates a neutral data node", {
    handle <- bg_init(withr::local_tempdir())
    bg_use_brms(handle)

    testthat::with_mocked_bindings(
      bg_fit_brms(handle, y ~ x, data.frame(y = 1, x = 1)),
      bg_run = function(...) NULL,
      .package = "bayesgrove"
    )

    nodes <- bg_read_graph(handle)$nodes
    kinds <- unname(vapply(nodes, `[[`, character(1), "kind"))
    expect_equal(kinds, c("data", "brms_fit"))
  })
})

describe("fit helper argument naming", {
  it("maps the deprecated handle argument onto project for bg_fit_stan", {
    handle <- bg_init(withr::local_tempdir())
    reset_handle_deprecation_warnings()

    expect_warning(
      expect_error(
        bg_fit_stan(
          handle = handle,
          stan_file = "missing.stan",
          data = list()
        ),
        "bg_use_cmdstanr"
      ),
      "deprecated"
    )
    expect_length(bg_read_graph(handle)$nodes, 0L)
  })

  it("maps the deprecated handle argument onto project for bg_fit_brms", {
    handle <- bg_init(withr::local_tempdir())
    reset_handle_deprecation_warnings()

    expect_warning(
      expect_error(
        bg_fit_brms(
          handle = handle,
          formula = y ~ x,
          data = data.frame(y = 1, x = 1)
        ),
        "bg_use_brms"
      ),
      "deprecated"
    )
    expect_length(bg_read_graph(handle)$nodes, 0L)
  })

  it("runs the full node-creation path through the deprecated alias", {
    handle <- bg_init(withr::local_tempdir())
    bg_use_brms(handle)
    reset_handle_deprecation_warnings()

    testthat::with_mocked_bindings(
      suppressWarnings(
        bg_fit_brms(
          handle = handle,
          formula = y ~ x,
          data = data.frame(y = 1, x = 1)
        )
      ),
      bg_run = function(...) NULL,
      .package = "bayesgrove"
    )

    nodes <- bg_read_graph(handle)$nodes
    kinds <- unname(vapply(nodes, `[[`, character(1), "kind"))
    expect_equal(kinds, c("data", "brms_fit"))
  })

  it("accepts project by name without a deprecation warning", {
    handle <- bg_init(withr::local_tempdir())

    expect_error(
      bg_fit_stan(project = handle, "missing.stan", list()),
      "bg_use_cmdstanr"
    )
    expect_error(
      bg_fit_brms(project = handle, y ~ x, data.frame(y = 1, x = 1)),
      "bg_use_brms"
    )
  })

  it("rejects project and handle supplied together", {
    handle <- bg_init(withr::local_tempdir())

    expect_error(
      bg_fit_stan(
        project = handle,
        handle = handle,
        stan_file = "missing.stan",
        data = list()
      ),
      "not both"
    )
    expect_error(
      bg_fit_brms(
        project = handle,
        handle = handle,
        formula = y ~ x,
        data = data.frame(y = 1, x = 1)
      ),
      "not both"
    )
    # Naming the deprecated alias while passing later arguments positionally
    # shifts them into `project`, which the same guard catches.
    expect_error(
      bg_fit_stan(handle = handle, "missing.stan", list()),
      "not both"
    )
  })

  it("warns about the deprecated handle only once per session", {
    handle <- bg_init(withr::local_tempdir())
    reset_handle_deprecation_warnings()

    first <- collect_conditions(
      bg_fit_stan(handle = handle, stan_file = "missing.stan", data = list())
    )
    expect_length(Filter(inherits_warning, first), 1L)

    second <- collect_conditions(
      bg_fit_stan(handle = handle, stan_file = "missing.stan", data = list())
    )
    expect_length(Filter(inherits_warning, second), 0L)
    # The second call still mapped the argument through to validation.
    expect_length(Filter(inherits_error, second), 1L)
  })
})
