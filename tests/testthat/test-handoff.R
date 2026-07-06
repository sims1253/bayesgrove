describe("Handoff and Export Layer", {
  it("generates a bundle archive", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      "data"
    })
    n_data <- bg_add_node(handle, "data")

    bg_run(handle)

    bundle_path <- expect_no_warning(bg_bundle(handle))
    expect_true(file.exists(bundle_path))
    expect_true(grepl("\\.tar\\.gz$", bundle_path))

    # We should be able to un-tar it and find a bundle manifest
    extract_dir <- file.path(withr::local_tempdir(), "extract")
    dir.create(extract_dir)
    utils::untar(bundle_path, exdir = extract_dir)

    proj_name <- basename(tmp)
    expect_true(dir.exists(file.path(extract_dir, proj_name, ".bayesgrove")))

    manifest_path <- file.path(
      extract_dir,
      proj_name,
      ".bayesgrove",
      "bundle_manifest.json"
    )
    expect_true(file.exists(manifest_path))

    manifest <- jsonlite::read_json(manifest_path)
    expect_equal(manifest$schema_name, "bg_bundle_manifest")
  })

  it("exports a markdown report", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data_source")
    bg_register_node_kind(handle, "fit")

    n_data <- bg_add_node(handle, kind = "data_source", label = "A")
    n_fit <- bg_add_node(handle, kind = "fit", label = "B", inputs = n_data)

    gate <- bg_add_gate(
      handle,
      from = n_data,
      to = n_fit,
      prompt = "OK?",
      alternatives = c("yes", "no"),
      refs = list(list(citekey = "demo2026", note = "Workflow checkpoint"))
    )
    bg_answer_gate(
      handle,
      gate$id,
      "yes",
      rationale = "Because it is good",
      evidence = n_data
    )

    report_path <- bg_export_report(
      handle,
      path = "my_report.md",
      format = "md"
    )
    expect_true(file.exists(report_path))

    content <- readLines(report_path)

    # Check for nodes
    expect_true(any(grepl("data_source.*A", content)))
    expect_true(any(grepl("fit.*B", content)))

    # Check for decisions
    expect_true(any(grepl("Because it is good", content, fixed = TRUE)))
    expect_true(any(grepl("Gate edge", content, fixed = TRUE)))
    expect_true(any(grepl("Evidence nodes", content, fixed = TRUE)))
    expect_true(any(grepl("References", content, fixed = TRUE)))
  })

  it("supports html reports and modern bundle data policies", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      "data"
    })
    bg_add_node(handle, "data", label = "A")
    bg_run(handle)

    bundle_path <- expect_no_warning(
      bg_bundle(handle, include_data = "copy")
    )
    expect_true(file.exists(bundle_path))

    report_path <- bg_export_report(
      handle,
      path = "workflow.html",
      format = "html"
    )
    expect_true(file.exists(report_path))

    content <- paste(readLines(report_path), collapse = "\n")
    expect_true(grepl("<html>", content, fixed = TRUE))
    expect_true(grepl("bayesgrove Workflow Report", content, fixed = TRUE))
  })

  it("bundle manifest carries a reproducibility manifest", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(result = 1)
    })
    bg_add_node(handle, kind = "data", label = "A")
    bg_run(handle)

    bundle_path <- file.path(tmp, "test_bundle.tar.gz")
    bg_bundle(handle, bundle_path, include_fits = TRUE)

    # Extract and read the manifest into a separate tempdir (outside the
    # project dir, to avoid any path collision with the project itself).
    ex <- withr::local_tempdir()
    tar_bin <- unname(Sys.which("tar"))
    if (nzchar(tar_bin)) {
      system2(
        tar_bin,
        c("-xzf", bundle_path, "-C", ex),
        stdout = FALSE,
        stderr = FALSE
      )
    } else {
      utils::untar(bundle_path, exdir = ex)
    }
    manifest_path <- list.files(
      ex,
      pattern = "bundle_manifest.json$",
      recursive = TRUE,
      all.files = TRUE,
      full.names = TRUE
    )
    expect_length(manifest_path, 1L)
    manifest <- jsonlite::fromJSON(manifest_path[[1]])
    expect_equal(manifest$schema_name, "bg_bundle_manifest")
    # M8 item 5: reproducibility section captures R version + platform + pkgs.
    expect_true(!is.null(manifest$reproducibility))
    expect_match(manifest$reproducibility$r_version, "R version")
    expect_true(nzchar(manifest$reproducibility$platform))
    expect_true(is.character(manifest$reproducibility$packages))
    expect_gte(length(manifest$reproducibility$packages), 1L)
  })

  it("bundles the workflow state (summaries + registries)", {
    # Regression: bundles used to omit .bayesgrove/workflow entirely, so a
    # restored project lost its summaries, branches, and goals — the protocol
    # half of the audit trail.
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(
        result = 1,
        summaries = list(list(
          summary_kind = "custom_check",
          passed = TRUE,
          severity = "ok",
          metrics = list(x = 1)
        ))
      )
    })
    bg_register_summary_kind(handle, "custom_check")
    bg_add_node(handle, kind = "data", label = "A")
    bg_run(handle)
    expect_gte(length(bg_read_summaries(handle)), 1L)

    bundle_path <- file.path(tmp, "wf_bundle.tar.gz")
    bg_bundle(handle, bundle_path)

    ex <- withr::local_tempdir()
    utils::untar(bundle_path, exdir = ex)
    summaries_path <- list.files(
      ex,
      pattern = "summaries.jsonl$",
      recursive = TRUE,
      all.files = TRUE,
      full.names = TRUE
    )
    expect_length(summaries_path, 1L)
    restored <- bg_open(dirname(dirname(dirname(summaries_path[[1]]))))
    expect_gte(length(bg_read_summaries(restored)), 1L)
    bg_close(restored)
  })

  it("warns on open when the bundle manifest environment mismatches", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_close(handle)

    manifest <- list(
      schema_name = "bg_bundle_manifest",
      schema_version = 1,
      reproducibility = list(
        r_version = "R version 0.0.1 (1900-01-01)",
        platform = "imaginary-arch",
        cmdstan = NULL
      )
    )
    jsonlite::write_json(
      manifest,
      file.path(tmp, ".bayesgrove", "bundle_manifest.json"),
      auto_unbox = TRUE
    )

    expect_warning(
      bg_open(tmp, readonly = TRUE),
      "different environment"
    )
  })

  it("embeds an unescaped mermaid block in the html report", {
    # Regression: the mermaid <div>/<script> used to be appended before
    # HTML-escaping, so the diagram rendered as literal text.
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      "data"
    })
    bg_add_node(handle, "data", label = "A")
    bg_run(handle)

    report_path <- bg_export_report(handle, path = "r.html", format = "html")
    content <- paste(readLines(report_path), collapse = "\n")
    expect_true(grepl('<div class="mermaid">', content, fixed = TRUE))
    expect_false(grepl(
      "&lt;div class=&quot;mermaid&quot;&gt;",
      content,
      fixed = TRUE
    ))

    md_path <- bg_export_report(handle, path = "r.md", format = "md")
    md <- readLines(md_path)
    expect_true(any(grepl("^```mermaid$", md)))
  })
})
