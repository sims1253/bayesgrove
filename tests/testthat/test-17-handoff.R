describe("Handoff and Export Layer", {
  it("generates a bundle archive", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      "data"
    })
    n_data <- bg_add_node(handle, "data")

    bg_run(handle, mode = "sync")

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
      options = c("yes", "no"),
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
    bg_run(handle, mode = "sync")

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
    expect_true(grepl("BayesGrove Workflow Report", content, fixed = TRUE))
  })
})
