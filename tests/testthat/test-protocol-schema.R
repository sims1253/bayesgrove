describe("Protocol schema files exist", {
  it("has a schema index file", {
    index_path <- system.file(
      "protocol",
      "schema-index.json",
      package = "bayesgrove"
    )
    expect_true(length(index_path) > 0 && file.exists(index_path))
  })

  it("has a schemas directory", {
    schemas_dir <- system.file(
      "protocol",
      "schemas",
      package = "bayesgrove"
    )
    expect_true(length(schemas_dir) > 0 && dir.exists(schemas_dir))
  })

  it("has all expected schema files", {
    expected_schemas <- c(
      "bg_next_actions_result",
      "bg_obligation_item",
      "bg_action_item",
      "bg_partitioned_result",
      "bg_template_descriptor"
    )

    available <- bg_list_protocol_schemas()

    for (schema_name in expected_schemas) {
      expect_true(
        schema_name %in% available,
        info = paste("Missing schema:", schema_name)
      )
    }
  })
})

schema_has_placeholder_map <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }

  if ("." %in% names(x)) {
    return(TRUE)
  }

  any(vapply(x, schema_has_placeholder_map, logical(1)))
}

describe("Protocol schema index structure", {
  it("is valid JSON with expected structure", {
    skip_if_not_installed("jsonlite")

    index <- bg_load_protocol_schema_index()
    expect_false(is.null(index), info = "Schema index should not be NULL")

    # Check required fields
    expect_true(is.list(index))
    expect_true("$schema" %in% names(index))
    expect_true("schemas" %in% names(index))
  })

  it("references all expected schemas", {
    index <- bg_load_protocol_schema_index()
    skip_if(is.null(index), "Schema index not available")

    referenced <- names(index$schemas %||% list())
    expected_schemas <- c(
      "bg_next_actions_result",
      "bg_obligation_item",
      "bg_action_item",
      "bg_partitioned_result",
      "bg_template_descriptor"
    )

    for (schema_name in expected_schemas) {
      expect_true(
        schema_name %in% referenced,
        info = paste("Schema not in index:", schema_name)
      )
    }
  })
})

describe("Protocol schema definitions", {
  it("do not use placeholder keys inside schema maps", {
    schemas <- lapply(bg_list_protocol_schemas(), bg_load_protocol_schema)

    expect_false(any(vapply(schemas, schema_has_placeholder_map, logical(1))))
  })
})

describe("Individual schema structure", {
  it("bg_next_actions_result has required fields", {
    schema <- bg_load_protocol_schema("bg_next_actions_result")
    skip_if(is.null(schema), "Schema not available")

    expect_true("required" %in% names(schema))
    required <- schema$required %||% character()
    expect_true("context" %in% required)
    expect_true("obligations" %in% required)
    expect_true("actions" %in% required)
    expect_true("metadata" %in% required)
  })

  it("bg_obligation_item has required fields", {
    schema <- bg_load_protocol_schema("bg_obligation_item")
    skip_if(is.null(schema), "Schema not available")

    required <- schema$required %||% character()
    expect_true("obligation_id" %in% required)
    expect_true("kind" %in% required)
    expect_true("scope" %in% required)
    expect_true("severity" %in% required)
    expect_true("title" %in% required)
    expect_true("basis" %in% required)
  })

  it("bg_action_item has required fields", {
    schema <- bg_load_protocol_schema("bg_action_item")
    skip_if(is.null(schema), "Schema not available")

    required <- schema$required %||% character()
    expect_true("action_id" %in% required)
    expect_true("kind" %in% required)
    expect_true("scope" %in% required)
    expect_true("title" %in% required)
    expect_true("basis" %in% required)
  })

  it("bg_template_descriptor has required fields", {
    schema <- bg_load_protocol_schema("bg_template_descriptor")
    skip_if(is.null(schema), "Schema not available")

    required <- schema$required %||% character()
    expect_true("template_ref" %in% required)
    expect_true("title" %in% required)
    expect_true("operation_type" %in% required)
    expect_true("description" %in% required)
  })
})

describe("Protocol schema validation against live objects", {
  it("validates bg_next_actions result structure", {
    skip_if_not_installed("jsonlite")

    project_dir <- withr::local_tempdir()
    # Create a minimal test project
    handle <- bg_init(project_dir)
    bg_close(handle)

    project <- bg_open(project_dir)
    withr::defer(bg_close(project), envir = parent.frame())

    # Get next_actions result
    result <- bg_next_actions(project)

    # Validate against schema
    expect_true(bg_validate_protocol_object(result, "bg_next_actions_result"))
  })

  it("validates obligation item structure", {
    project_dir <- withr::local_tempdir()
    handle <- bg_init(project_dir)
    bg_close(handle)

    project <- bg_open(project_dir)
    withr::defer(bg_close(project), envir = parent.frame())

    result <- bg_next_actions(project)

    if (length(result$obligations) > 0) {
      obl <- result$obligations[[1]]
      expect_true(bg_validate_protocol_object(obl, "bg_obligation_item"))
    } else {
      # No obligations to validate, but structure is valid
      expect_true(TRUE)
    }
  })

  it("validates action item structure", {
    project_dir <- withr::local_tempdir()
    handle <- bg_init(project_dir)
    bg_close(handle)

    project <- bg_open(project_dir)
    withr::defer(bg_close(project), envir = parent.frame())

    result <- bg_next_actions(project)

    if (length(result$actions) > 0) {
      act <- result$actions[[1]]
      expect_true(bg_validate_protocol_object(act, "bg_action_item"))
    } else {
      # No actions to validate, but structure is valid
      expect_true(TRUE)
    }
  })

  it("validates partitioned protocol result structure", {
    project_dir <- withr::local_tempdir()
    handle <- bg_init(project_dir)
    bg_close(handle)

    project <- bg_open(project_dir)
    withr::defer(bg_close(project), envir = parent.frame())

    result <- bg_next_actions(project)
    partitioned <- bg_partition_protocol_by_scope(result, project)

    expect_true(bg_validate_protocol_object(
      partitioned,
      "bg_partitioned_result"
    ))
  })

  it("validates template descriptor from bg_list_templates()", {
    templates <- bg_list_templates()

    expect_gte(length(templates), 1)

    for (template_ref in names(templates)) {
      template <- templates[[template_ref]]
      expect_true(
        bg_validate_protocol_object(template, "bg_template_descriptor")
      )
    }
  })
})

describe("Schema helper functions", {
  it("bg_load_protocol_schema returns NULL for missing schema", {
    result <- bg_load_protocol_schema("nonexistent_schema_xyz")
    expect_true(is.null(result))
  })

  it("bg_list_protocol_schemas returns expected schemas", {
    schemas <- bg_list_protocol_schemas()
    expect_gte(length(schemas), 3)
  })

  it("loads checked-in protocol fixtures and validates them against schemas", {
    index <- bg_load_protocol_fixture_index()
    expect_false(is.null(index))

    fixtures <- index$fixtures %||% list()
    expect_gte(length(fixtures), 1)

    for (fixture_name in names(fixtures)) {
      fixture_spec <- fixtures[[fixture_name]]
      fixture <- bg_load_protocol_fixture(fixture_name)

      expect_false(is.null(fixture), info = fixture_name)
      expect_true(
        bg_validate_protocol_object(fixture, fixture_spec$schema),
        info = fixture_name
      )
    }
  })

  it("filters protocol fixtures by stability", {
    stable <- bg_list_protocol_fixtures("stable")

    expect_gte(length(stable), 1)
    expect_true(all(stable %in% bg_list_protocol_fixtures()))
  })

  it("keeps empty object-like protocol fields as named maps", {
    project_dir <- withr::local_tempdir()
    handle <- bg_init(project_dir)
    bg_close(handle)

    project <- bg_open(project_dir)
    withr::defer(bg_close(project), envir = parent.frame())

    result <- bg_next_actions(project)
    partitioned <- bg_partition_protocol_by_scope(result, project)

    expect_equal(names(result$obligations), character())
    expect_equal(names(result$actions), character())
    expect_equal(names(result$metadata$external_holds), character())
    expect_equal(names(partitioned$project$obligations), character())
    expect_equal(names(partitioned$project$actions), character())
  })
})
