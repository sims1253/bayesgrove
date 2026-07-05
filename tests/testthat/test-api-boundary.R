describe("bg_api_boundary()", {
  it("returns a data.frame with required columns", {
    result <- bg_api_boundary()
    expect_s3_class(result, "data.frame")
    expect_named(result, c("fn", "classification"))
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
    valid_class <- c("stable", "experimental", "internal")
    invalid <- setdiff(registry$classification, valid_class)
    expect_equal(
      length(invalid),
      0,
      info = paste("Invalid classifications:", paste(invalid, collapse = ", "))
    )
  })

  it("has no NA or empty fn/classification values", {
    registry <- bg_api_boundary()
    expect_false(anyNA(registry$fn))
    expect_true(all(nzchar(registry$fn)))
    expect_false(anyNA(registry$classification))
    expect_true(all(nzchar(registry$classification)))
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

    # We expect at least some internal functions
    expect_gte(counts[["internal"]], 1)
  })
})
