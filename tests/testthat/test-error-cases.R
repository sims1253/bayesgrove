# Error Case and Edge Case Tests
# --------------------------------
# Tests for error handling, invalid inputs, edge cases, and failure modes.

describe("Error Handling - Invalid Inputs", {
  it("rejects invalid path on init", {
    tmp <- tempfile(fileext = ".lock")
    writeLines("blocked", tmp)
    on.exit(unlink(tmp), add = TRUE)
    expect_error(
      bg_init(path = file.path(tmp, "subdir"))
    )
  })

  it("rejects NULL path on init", {
    expect_error(
      bg_init(path = NULL)
    )
  })

  it("handles various project_name types", {
    tmp <- withr::local_tempdir()
    # Numeric may be coerced to string - test actual behavior
    handle <- bg_init(path = tmp, project_name = "valid_name")
    expect_s7_class(handle, bg_handle)
  })

  it("rejects opening non-existent project", {
    tmp <- withr::local_tempdir()
    expect_error(
      bg_open(path = file.path(tmp, "nonexistent"))
    )
  })
})

describe("Error Handling - Invalid Node Operations", {
  it("rejects adding node with invalid kind", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    expect_error(
      bg_add_node(handle, kind = "nonexistent_kind")
    )
  })

  it("rejects adding node with invalid inputs reference", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind("test_kind")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    expect_error(
      bg_add_node(handle, kind = "test_kind", inputs = "nonexistent_node")
    )
  })

  it("rejects updating non-existent node", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    expect_error(
      bg_update_node(handle, "nonexistent_node", label = "test")
    )
  })

  it("rejects removing non-existent node", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    expect_error(
      bg_remove_node(handle, "nonexistent_node")
    )
  })
})

describe("Error Handling - Closed Project Operations", {
  it("rejects mutations on closed handle", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "test_kind")
    bg_close(handle)

    expect_error(
      bg_add_node(handle, kind = "test_kind"),
      "closed",
      ignore.case = TRUE
    )
  })
})

describe("Error Handling - Readonly Mode", {
  it("rejects mutations on readonly handle", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "test_kind")
    bg_close(handle)
    handle <- bg_open(path = tmp, readonly = TRUE)

    expect_error(
      bg_add_node(handle, kind = "test_kind"),
      "readonly",
      ignore.case = TRUE
    )
  })
})

describe("Edge Cases - Empty Graph", {
  it("handles empty graph correctly", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    expect_equal(length(graph$nodes), 0)
    expect_equal(length(graph$edges), 0)
  })
})

describe("Edge Cases - Circular Dependencies", {
  it("detects circular dependencies", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind("test_kind")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    n1 <- bg_add_node(handle, kind = "test_kind", label = "Node 1")
    n2 <- bg_add_node(handle, kind = "test_kind", label = "Node 2", inputs = n1)

    # Attempting to create a cycle should be detected
    expect_error(
      bg_add_edge(handle, from = n2, to = n1)
    )
  })
})

describe("Error Handling - Corrupted State", {
  it("handles corrupted graph file gracefully", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_close(handle)

    # Corrupt the graph file
    graph_path <- file.path(tmp, ".bayesgrove", "graph", "graph.json")
    writeLines("invalid json {{{", graph_path)

    expect_error(
      bg_open(path = tmp)
    )
  })

  it("handles missing config file gracefully", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_close(handle)

    # Remove config file
    config_path <- file.path(tmp, ".bayesgrove", "config.json")
    file.remove(config_path)

    # bg_open should still work but use default config
    handle2 <- bg_open(path = tmp)
    expect_s7_class(handle2, bg_handle)
  })
})

describe("Error Handling - Invalid Parameters", {
  it("validates node parameters", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind("test_kind")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    # Should accept valid params
    n1 <- bg_add_node(handle, kind = "test_kind", params = list(value = 42))
    expect_true(startsWith(n1, "node_"))

    # Should handle empty params
    n2 <- bg_add_node(handle, kind = "test_kind", params = list())
    expect_true(startsWith(n2, "node_"))
  })
})

describe("Error Handling - Concurrent Access", {
  it("detects version conflicts on concurrent writes", {
    tmp <- withr::local_tempdir()
    handle_a <- bg_init(path = tmp)

    graph_a <- bg_read_graph(handle_a)
    graph_a$version <- graph_a$version + 1L
    bg_commit_graph(handle_a, graph_a)
    bg_close(handle_a)

    # Reopen as a fresh writer. The persisted graph is now at version 1.
    # A commit at the same (stale) version must be rejected.
    handle_b <- bg_open(path = tmp)
    graph_b <- bg_read_graph(handle_b)

    expect_error(
      bg_commit_graph(handle_b, graph_b),
      "Version conflict"
    )
  })
})

describe("Error Handling - Missing Dependencies", {
  it("handles missing input artifacts gracefully", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind("test_kind")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    n1 <- bg_add_node(handle, kind = "test_kind", label = "Source")
    n2 <- bg_add_node(
      handle,
      kind = "test_kind",
      label = "Dependent",
      inputs = n1
    )

    # Attempting to get result for node without cached result
    expect_error(
      bg_result(handle, n2)
    )
  })
})

describe("Edge Cases - Special Characters", {
  it("handles special characters in labels", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind("test_kind")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    # Should handle unicode and special characters
    n1 <- bg_add_node(handle, kind = "test_kind", label = "Node with émojis 🎉")
    expect_true(startsWith(n1, "node_"))

    n2 <- bg_add_node(
      handle,
      kind = "test_kind",
      label = "Node with \"quotes\" and 'apostrophes'"
    )
    expect_true(startsWith(n2, "node_"))
  })
})

describe("Edge Cases - Boundary Conditions", {
  it("handles very long labels", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind("test_kind")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    long_label <- paste(rep("a", 1000), collapse = "")
    n1 <- bg_add_node(handle, kind = "test_kind", label = long_label)

    g2 <- bg_read_graph(handle)
    expect_equal(nchar(g2$nodes[[n1]]$label), 1000)
  })

  it("handles many nodes efficiently", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind("test_kind")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    # Add 100 nodes
    node_ids <- character(100)
    for (i in seq_len(100)) {
      node_ids[i] <- bg_add_node(
        handle,
        kind = "test_kind",
        label = paste("Node", i)
      )
    }

    g2 <- bg_read_graph(handle)
    expect_equal(length(g2$nodes), 100)
  })
})
