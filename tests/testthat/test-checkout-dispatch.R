test_that("daemon resolution requires an installed copy at the executing path", {
  executing <- getNamespaceInfo(asNamespace("bayesgrove"), "path")
  probe <- bayesgrove:::bg_daemons_resolve_by_name
  # Substitute only the library lookup, including the documented no-install
  # result. The production path comparison still runs in every case.
  lookup <- new.env(parent = environment(probe))
  environment(probe) <- lookup
  lookup$find.package <- function(...) character()
  expect_identical(probe(), FALSE)
  lookup$find.package <- function(...) tempfile("other-install-")
  expect_identical(probe(), FALSE)
  lookup$find.package <- function(...) executing
  expect_identical(probe(), TRUE)
})

test_that("checkout fingerprints track source edits, additions, and removals", {
  path <- withr::local_tempdir()
  dir.create(file.path(path, "R"))
  source <- file.path(path, "R", "worker.R")
  writeLines("worker <- function() 1", source)
  fingerprint <- bayesgrove:::bg_checkout_fingerprint
  original <- fingerprint(path)
  expect_identical(fingerprint(path), original)

  # Pin mtimes so this test does not depend on filesystem clock resolution.
  old_time <- file.info(source)$mtime
  writeLines("worker <- function() 12345", source)
  Sys.setFileTime(source, old_time)
  edited <- fingerprint(path)
  expect_false(identical(edited, original))
  Sys.setFileTime(source, old_time + 2)
  touched <- fingerprint(path)
  expect_false(identical(touched, edited))

  extra <- file.path(path, "R", "extra.R")
  writeLines("extra <- TRUE", extra)
  expect_false(identical(fingerprint(path), touched))
  unlink(extra)
  expect_identical(fingerprint(path), touched)
  file.rename(source, file.path(path, "R", "renamed.R"))
  expect_false(identical(fingerprint(path), touched))
})

test_that("checkout fingerprints use DESCRIPTION when source files are absent", {
  path <- withr::local_tempdir()
  description <- file.path(path, "DESCRIPTION")
  writeLines("Package: example\nVersion: 1.0", description)
  original <- bayesgrove:::bg_checkout_fingerprint(path)
  expect_identical(bayesgrove:::bg_checkout_fingerprint(path), original)
  writeLines("Package: example\nVersion: 2.0", description)
  expect_false(identical(bayesgrove:::bg_checkout_fingerprint(path), original))
})

test_that("checkout dispatch reloads once per source state and returns artifacts", {
  skip_if_not_installed("purrr", minimum_version = "1.1.0")
  skip_if_not_installed("carrier")
  skip_if_not_installed("pkgload")
  fingerprint <- "first-source-state"
  loads <- character()
  state <- new.env(parent = emptyenv())
  local_mocked_bindings(
    bg_daemons_resolve_by_name = function() FALSE,
    bg_checkout_fingerprint = function(path) fingerprint,
    bg_daemon_state = state,
    .package = "bayesgrove"
  )
  # Run the actual carrier crate in this process so covr can observe the
  # bootstrap branch even from an installed package. Real-daemon tests in
  # test-parallel-execution.R separately check serialization and isolation.
  local_mocked_bindings(
    map = function(.x, .f, ...) lapply(.x, .f, ...),
    .package = "purrr"
  )
  local_mocked_bindings(
    load_all = function(path, ...) {
      loads <<- c(loads, path)
    },
    .package = "pkgload"
  )
  path <- withr::local_tempdir()
  handle <- bg_init(path)
  withr::defer(bg_close(handle))
  bg_register_node_kind(handle, "data", executor = function(node, inputs) {
    list(label = node$label)
  })
  wave <- c(
    bg_add_node(handle, "data", label = "first"),
    bg_add_node(handle, "data", label = "second")
  )
  graph <- bg_read_graph(handle)
  plan <- bg_plan(handle)
  dispatch <- function() {
    bayesgrove:::bg_dispatch_wave_parallel(handle, wave, plan, graph)
  }

  first <- dispatch()
  expect_length(loads, 1L)
  expect_identical(state$checkout_fingerprint, fingerprint)
  expect_identical(
    loads[[1]],
    normalizePath(
      getNamespaceInfo(asNamespace("bayesgrove"), "path"),
      mustWork = FALSE
    )
  )
  expect_true(all(vapply(
    first,
    function(x) x$ok && x$worker_origin,
    logical(1)
  )))
  expect_identical(vapply(first, `[[`, character(1), "node_id"), wave)
  results <- lapply(first, function(x) {
    bayesgrove:::bg_fetch_artifact(handle, x$ref)
  })
  expect_identical(results, list(list(label = "first"), list(label = "second")))

  again <- dispatch()
  expect_length(loads, 1L)
  expect_identical(lapply(again, `[[`, "ref"), lapply(first, `[[`, "ref"))

  fingerprint <- "second-source-state"
  dispatch()
  expect_length(loads, 2L)
  expect_identical(state$checkout_fingerprint, fingerprint)
})
