describe("bg_api_boundary()", {
  it("returns a data.frame with required columns", {
    result <- bg_api_boundary()
    expect_s3_class(result, "data.frame")
    expect_named(result, c("fn", "classification", "note", "remote_accessible"))
  })

  it("filters by function name when fn is provided", {
    result <- bg_api_boundary("bg_run")
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$fn, "bg_run")
  })

  it("returns empty data.frame with warning for unknown function", {
    expect_warning(
      result <- bg_api_boundary("nonexistent_function_xyz"),
      "not found"
    )
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 0)
  })

  it("errors on non-character fn argument", {
    expect_error(bg_api_boundary(123), "must be a single character string")
    expect_error(
      bg_api_boundary(c("a", "b")),
      "must be a single character string"
    )
  })
})

describe("API boundary registry completeness", {
  it("covers all exported bg_* functions exactly once", {
    # Get all exported bg_* functions from the namespace
    ns <- asNamespace("bayesgrove")
    ns_exports <- getNamespaceExports(ns)
    bg_exports <- sort(ns_exports[grepl("^bg_", ns_exports)])

    # Get all functions in the registry
    registry <- bg_api_boundary()
    registry_fns <- sort(registry$fn)

    # Check for missing functions (exported but not in registry)
    missing <- setdiff(bg_exports, registry_fns)
    expect_equal(
      length(missing),
      0,
      info = paste(
        "Exported functions missing from registry:",
        paste(missing, collapse = ", ")
      )
    )

    # Check for extra functions (in registry but not exported)
    extra <- setdiff(registry_fns, bg_exports)
    expect_equal(
      length(extra),
      0,
      info = paste(
        "Registry contains non-exported functions:",
        paste(extra, collapse = ", ")
      )
    )

    # Check for duplicates
    expect_equal(
      length(registry_fns),
      length(unique(registry_fns)),
      info = "Registry contains duplicate function entries"
    )
  })
})

describe("API boundary registry integrity", {
  it("has only valid classification values", {
    registry <- bg_api_boundary()
    valid_class <- c("stable", "experimental", "internal_exported")
    invalid <- setdiff(registry$classification, valid_class)
    expect_equal(
      length(invalid),
      0,
      info = paste("Invalid classifications:", paste(invalid, collapse = ", "))
    )
  })

  it("has logical remote_accessible values with no NA", {
    registry <- bg_api_boundary()
    expect_true(is.logical(registry$remote_accessible))
    expect_false(anyNA(registry$remote_accessible))
  })

  it("has non-empty notes for all functions", {
    registry <- bg_api_boundary()
    empty_notes <- registry$fn[!nzchar(trimws(registry$note))]
    expect_equal(
      length(empty_notes),
      0,
      info = paste(
        "Functions with empty notes:",
        paste(empty_notes, collapse = ", ")
      )
    )
  })

  it("passes internal validation helper", {
    expect_true(bg_validate_api_boundary_registry())
  })
})

describe("API boundary registry output shape stability", {
  it("returns consistent column types", {
    result <- bg_api_boundary()

    expect_type(result$fn, "character")
    expect_type(result$classification, "character")
    expect_type(result$note, "character")
    expect_type(result$remote_accessible, "logical")
  })

  it("returns rows sorted by function name", {
    result <- bg_api_boundary()
    expect_equal(result$fn, sort(result$fn))
  })
})

describe("API boundary classification distribution", {
  it("has a meaningful distribution of classifications", {
    registry <- bg_api_boundary()
    counts <- table(registry$classification)

    # We expect at least some stable functions
    expect_gte(counts[["stable"]], 20)

    # We expect at least some experimental functions
    expect_gte(counts[["experimental"]], 1)

    # We expect at least some internal_exported functions
    expect_gte(counts[["internal_exported"]], 5)
  })

  it("marks the bounded IPC command surface as remote_accessible", {
    registry <- bg_api_boundary()
    remote <- sort(registry$fn[registry$remote_accessible])

    expect_equal(
      remote,
      sort(c(
        "bg_add_node",
        "bg_answer_gate",
        "bg_branch_lineage",
        "bg_cancel",
        "bg_connect",
        "bg_execute_action",
        "bg_extension_registry",
        "bg_list_branches",
        "bg_next_actions",
        "bg_record_decision",
        "bg_remove_node",
        "bg_snapshot",
        "bg_status",
        "bg_submit",
        "bg_update_node"
      ))
    )
  })

  it("keeps the server registry aligned with remote_accessible flags", {
    expect_true(
      exists(
        "bg_validate_remote_command_boundary",
        envir = asNamespace("bayesgrove")
      ),
      info = "Internal validator function bg_validate_remote_command_boundary must exist"
    )
    validator <- getFromNamespace(
      "bg_validate_remote_command_boundary",
      "bayesgrove"
    )
    expect_true(validator())
  })
})
