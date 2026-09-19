describe("portable bundle sources (referenced-sources policy)", {
  it("copies an out-of-project Stan program and relocates the reference", {
    proj <- withr::local_tempdir()
    src_root <- withr::local_tempdir()

    stan_path <- file.path(src_root, "model.stan")
    writeLines("model { x ~ normal(0, 1); }", stan_path)
    # Siblings of the referenced file must stay out of the archive.
    writeLines("hunter2", file.path(src_root, "secret_token.txt"))
    dir.create(file.path(src_root, "junk"))
    writeLines("unreferenced", file.path(src_root, "junk", "data.csv"))

    handle <- bg_init(proj)
    bg_register_node_kind(handle, "fit")
    node_id <- bg_add_node(
      handle,
      kind = "fit",
      params = list(stan_file = stan_path)
    )
    bundle_path <- file.path(withr::local_tempdir(), "sources.tar.gz")
    expect_no_warning(bg_bundle(handle, path = bundle_path))
    unlink(src_root, recursive = TRUE)

    extract_dir <- withr::local_tempdir()
    utils::untar(bundle_path, exdir = extract_dir)
    restored_proj <- file.path(extract_dir, basename(proj))

    manifest <- jsonlite::read_json(
      file.path(restored_proj, ".bayesgrove", "bundle_manifest.json"),
      simplifyVector = FALSE
    )
    expect_equal(manifest$source_policy$name, "referenced-sources")
    expect_equal(manifest$source_policy$version, 1L)
    expect_equal(unlist(manifest$source_policy$approved_keys), "stan_file")

    entry <- manifest$source_policy$relocation[[stan_path]]
    expect_false(is.null(entry))
    expect_match(entry$archived, "^bundle_sources/stan_file/src_")

    # The relocated file exists inside the restored project, with its
    # content hash recorded in the relocation table.
    archived_abs <- file.path(restored_proj, entry$archived)
    expect_true(file.exists(archived_abs))
    expect_equal(
      entry$files[[entry$archived]],
      digest::digest(file = archived_abs, algo = "sha256")
    )

    # The archived graph points at the archived path, not the deleted one.
    graph <- jsonlite::read_json(
      file.path(restored_proj, ".bayesgrove", "graph", "graph.json"),
      simplifyVector = FALSE
    )
    expect_equal(graph$nodes[[node_id]]$params$stan_file, entry$archived)

    # Security boundary: unreferenced siblings never enter the archive.
    archived_files <- list.files(
      restored_proj,
      recursive = TRUE,
      all.files = TRUE
    )
    expect_false(any(grepl("secret_token", archived_files, fixed = TRUE)))
    expect_false(any(grepl("junk", archived_files, fixed = TRUE)))
    outside_meta <- archived_files[!startsWith(archived_files, ".bayesgrove")]
    expect_true(all(startsWith(outside_meta, "bundle_sources/")))
  })

  it("copies the #include closure preserving relative structure", {
    proj <- withr::local_tempdir()
    # The closure spans a directory above the main program's directory, so
    # the archive slot is rooted at the closure's common ancestor.
    src_root <- withr::local_tempdir()
    models_dir <- file.path(src_root, "models")
    dir.create(file.path(models_dir, "parts"), recursive = TRUE)
    dir.create(file.path(src_root, "shared"), recursive = TRUE)

    main_stan <- file.path(models_dir, "main.stan")
    writeLines(
      c(
        "data { int<lower=0> N; }",
        "parameters { real theta; }",
        "model {",
        "  #include \"parts/priors.stan\"",
        "}",
        "generated quantities {",
        "  #include \"../shared/transform.stan\"",
        "}"
      ),
      main_stan
    )
    writeLines(
      "theta ~ normal(0, 1);",
      file.path(models_dir, "parts", "priors.stan")
    )
    writeLines(
      "real theta_sq = theta * theta;",
      file.path(src_root, "shared", "transform.stan")
    )

    handle <- bg_init(proj)
    bg_register_node_kind(handle, "fit")
    bg_add_node(handle, kind = "fit", params = list(stan_file = main_stan))
    bundle_path <- file.path(withr::local_tempdir(), "includes.tar.gz")
    bg_bundle(handle, path = bundle_path)
    unlink(src_root, recursive = TRUE)

    extract_dir <- withr::local_tempdir()
    utils::untar(bundle_path, exdir = extract_dir)
    restored_proj <- file.path(extract_dir, basename(proj))

    manifest <- jsonlite::read_json(
      file.path(restored_proj, ".bayesgrove", "bundle_manifest.json"),
      simplifyVector = FALSE
    )
    entry <- manifest$source_policy$relocation[[main_stan]]
    expect_false(is.null(entry))
    expect_length(entry$files, 3L)

    restored_main <- file.path(restored_proj, entry$archived)
    expect_true(file.exists(restored_main))

    # stanc3 resolves includes relative to the including file: after the
    # move, the same relative positions must resolve inside the archive.
    expect_true(file.exists(file.path(
      dirname(restored_main),
      "parts",
      "priors.stan"
    )))
    expect_true(file.exists(normalizePath(
      file.path(dirname(restored_main), "../shared/transform.stan"),
      mustWork = FALSE
    )))

    for (archived in names(entry$files)) {
      expect_equal(
        entry$files[[archived]],
        digest::digest(
          file = file.path(restored_proj, archived),
          algo = "sha256"
        )
      )
    }
  })

  it("keeps referenced project files outside .bayesgrove portable too", {
    proj <- withr::local_tempdir()
    dir.create(file.path(proj, "models"))
    in_project <- file.path(proj, "models", "local.stan")
    writeLines("model { y ~ normal(0, 1); }", in_project)

    handle <- bg_init(proj)
    bg_register_node_kind(handle, "fit")
    node_id <- bg_add_node(
      handle,
      kind = "fit",
      params = list(stan_file = in_project)
    )
    bundle_path <- file.path(withr::local_tempdir(), "inproject.tar.gz")
    bg_bundle(handle, path = bundle_path)
    # Remove the model from the original project directory.
    unlink(file.path(proj, "models"), recursive = TRUE)

    extract_dir <- withr::local_tempdir()
    utils::untar(bundle_path, exdir = extract_dir)
    restored_proj <- file.path(extract_dir, basename(proj))

    manifest <- jsonlite::read_json(
      file.path(restored_proj, ".bayesgrove", "bundle_manifest.json"),
      simplifyVector = FALSE
    )
    entry <- manifest$source_policy$relocation[[in_project]]
    expect_false(is.null(entry))
    expect_true(file.exists(file.path(restored_proj, entry$archived)))

    graph <- jsonlite::read_json(
      file.path(restored_proj, ".bayesgrove", "graph", "graph.json"),
      simplifyVector = FALSE
    )
    expect_equal(graph$nodes[[node_id]]$params$stan_file, entry$archived)
  })

  it("leaves package-shipped Stan programs untouched", {
    proj <- withr::local_tempdir()
    stan_path <- system.file("stan", "normal_iid.stan", package = "bayesgrove")

    handle <- bg_init(proj)
    bg_register_node_kind(handle, "fit")
    node_id <- bg_add_node(
      handle,
      kind = "fit",
      params = list(stan_file = stan_path)
    )

    bundle_path <- file.path(withr::local_tempdir(), "pkgmodel.tar.gz")
    expect_no_warning(bg_bundle(handle, path = bundle_path))

    extract_dir <- withr::local_tempdir()
    utils::untar(bundle_path, exdir = extract_dir)
    restored_proj <- file.path(extract_dir, basename(proj))

    manifest <- jsonlite::read_json(
      file.path(restored_proj, ".bayesgrove", "bundle_manifest.json"),
      simplifyVector = FALSE
    )
    expect_equal(manifest$source_policy$name, "referenced-sources")
    expect_length(manifest$source_policy$relocation, 0L)

    graph <- jsonlite::read_json(
      file.path(restored_proj, ".bayesgrove", "graph", "graph.json"),
      simplifyVector = FALSE
    )
    expect_equal(graph$nodes[[node_id]]$params$stan_file, stan_path)
    expect_false(dir.exists(file.path(restored_proj, "bundle_sources")))
  })

  it("warns when a referenced source is missing", {
    proj <- withr::local_tempdir()
    handle <- bg_init(proj)
    bg_register_node_kind(handle, "fit")
    bg_add_node(
      handle,
      kind = "fit",
      params = list(stan_file = file.path(tempdir(), "no_such_model.stan"))
    )
    expect_warning(
      bg_bundle(handle),
      "Bundle is missing referenced source file"
    )
  })

  it("executes relocated references from the restored project", {
    proj <- withr::local_tempdir()
    src_root <- withr::local_tempdir()
    stan_path <- file.path(src_root, "model.stan")
    writeLines("model { x ~ normal(0, 1); }", stan_path)

    handle <- bg_init(proj)
    bg_register_node_kind(
      handle,
      "fit",
      executor = function(node, inputs) {
        node$params$stan_file
      }
    )
    node_id <- bg_add_node(
      handle,
      kind = "fit",
      params = list(stan_file = stan_path)
    )
    bundle_path <- file.path(withr::local_tempdir(), "run.tar.gz")
    bg_bundle(handle, path = bundle_path)
    unlink(src_root, recursive = TRUE)

    extract_dir <- withr::local_tempdir()
    utils::untar(bundle_path, exdir = extract_dir)
    restored_proj <- file.path(extract_dir, basename(proj))
    restored <- bg_open(restored_proj)
    bg_register_node_kind(
      restored,
      "fit",
      executor = function(node, inputs) {
        node$params$stan_file
      }
    )

    # Fingerprints are stable across working directories: relative archived
    # references resolve against the project root, not the process wd.
    manifest <- list(r = "4.5.0", bayesgrove = "0.7.0")
    fp_here <- bg_compute_fingerprint(
      restored,
      node_id,
      environment_manifest = manifest
    )
    other_wd <- withr::local_tempdir()
    fp_there <- withr::with_dir(
      other_wd,
      bg_compute_fingerprint(
        restored,
        node_id,
        environment_manifest = manifest
      )
    )
    expect_equal(fp_here, fp_there)

    run <- bg_run(restored, targets = node_id)
    expect_equal(run$status, "succeeded")

    # The executor saw an absolute path under the restored project root.
    result <- bg_result(restored, node_id)
    expect_true(file.exists(result))
    expect_true(startsWith(
      normalizePath(result, mustWork = FALSE),
      normalizePath(restored_proj, mustWork = FALSE)
    ))
    expect_match(result, "bundle_sources")
    bg_close(restored)
  })

  it("reruns a model from a restored bundle end to end", {
    skip_if_not_installed("cmdstanr")

    proj <- withr::local_tempdir()
    src_root <- withr::local_tempdir()
    dir.create(file.path(src_root, "scales"))

    stan_path <- file.path(src_root, "portable_model.stan")
    writeLines(
      c(
        "data {",
        "  int<lower=0> N;",
        "  array[N] real y;",
        "}",
        "parameters {",
        "  real mu;",
        "  real<lower=0> sigma;",
        "}",
        "model {",
        "  #include \"scales/sigma_scale.stan\"",
        "  mu ~ normal(0, 5);",
        "  sigma ~ cauchy(0, sigma_scale);",
        "  y ~ normal(mu, sigma);",
        "}"
      ),
      stan_path
    )
    writeLines(
      "real sigma_scale = 5;",
      file.path(src_root, "scales", "sigma_scale.stan")
    )

    handle <- bg_init(proj)
    bg_use_cmdstanr(handle)
    data_node <- bg_add_node(handle, kind = "stan_data", label = "data")
    bg_set_node_data(handle, data_node, list(N = 20L, y = rnorm(20)))
    fit_node <- bg_add_node(
      handle,
      kind = "cmdstanr_fit",
      label = "fit",
      inputs = data_node,
      params = list(
        stan_file = stan_path,
        chains = 1L,
        iter_warmup = 50L,
        iter_sampling = 50L,
        seed = 11L
      )
    )
    bundle_path <- file.path(withr::local_tempdir(), "e2e.tar.gz")
    bg_bundle(handle, path = bundle_path)
    bg_close(handle)

    # Remove both the original sources and the original project directory.
    unlink(src_root, recursive = TRUE)
    unlink(proj, recursive = TRUE)
    extract_dir <- withr::local_tempdir()
    utils::untar(bundle_path, exdir = extract_dir)
    restored <- bg_open(file.path(extract_dir, basename(proj)))

    run <- bg_run(restored, targets = fit_node)
    expect_equal(run$status, "succeeded")
    # cmdstanr fits are R6 objects; inherits() covers the class chain.
    expect_true(inherits(bg_result(restored, fit_node), "CmdStanMCMC"))
    bg_close(restored)
  })
})
