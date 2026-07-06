describe("Graph Node Management", {
  it("can add and connect nodes", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    # Register a dummy kind for testing
    # We might need to mock or register a kind if dagriculture validates it.
    # Let's read the graph, add a kind to the registry directly, and save.
    # Or use a low-level mutation just for the test.
    graph <- bg_read_graph(handle)
    graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind("test_kind")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    # Add node 1
    n1 <- bg_add_node(handle, kind = "test_kind", label = "Node 1")
    expect_true(startsWith(n1, "node_"))

    # Add node 2
    n2 <- bg_add_node(handle, kind = "test_kind", label = "Node 2", inputs = n1)
    expect_true(startsWith(n2, "node_"))

    # Verify via raw graph
    graph2 <- bg_read_graph(handle)
    expect_equal(length(graph2$nodes), 2)
    expect_equal(length(graph2$edges), 1)

    expect_equal(graph2$nodes[[n1]]$label, "Node 1")
    expect_equal(graph2$nodes[[n2]]$label, "Node 2")

    edge <- graph2$edges[[names(graph2$edges)[1]]]
    expect_equal(edge$from, n1)
    expect_equal(edge$to, n2)
  })

  it("can update and remove graph components", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind("test_kind")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    n1 <- bg_add_node(handle, kind = "test_kind", label = "Node A")

    # Update
    bg_update_node(handle, n1, label = "Node B")
    g2 <- bg_read_graph(handle)
    expect_equal(g2$nodes[[n1]]$label, "Node B")

    # Remove
    bg_remove_node(handle, n1)
    g3 <- bg_read_graph(handle)
    expect_equal(length(g3$nodes), 0)
  })

  it("bg_update_node merges params by default and preserves the others", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["fit_kind"]] <- dagriculture::dagri_kind("fit_kind")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    n1 <- bg_add_node(handle, kind = "fit_kind", label = "fit")
    # Establish a multi-param node: set chains, seed, and a stan_file.
    bg_update_node(
      handle,
      n1,
      params = list(chains = 4L, seed = 123L, stan_file = "model.stan")
    )
    g1 <- bg_read_graph(handle)
    expect_equal(g1$nodes[[n1]]$params$chains, 4L)

    # Updating just stan_file must NOT wipe chains and seed.
    bg_update_node(handle, n1, params = list(stan_file = "new_model.stan"))
    g2 <- bg_read_graph(handle)
    expect_equal(g2$nodes[[n1]]$params$stan_file, "new_model.stan")
    expect_equal(g2$nodes[[n1]]$params$chains, 4L)
    expect_equal(g2$nodes[[n1]]$params$seed, 123L)

    # replace = TRUE does a wholesale replacement.
    bg_update_node(
      handle,
      n1,
      params = list(stan_file = "only.stan"),
      replace = TRUE
    )
    g3 <- bg_read_graph(handle)
    expect_equal(g3$nodes[[n1]]$params$stan_file, "only.stan")
    expect_null(g3$nodes[[n1]]$params$chains)
    expect_null(g3$nodes[[n1]]$params$seed)

    # metadata merges the same way.
    bg_update_node(handle, n1, metadata = list(author = "a"))
    bg_update_node(handle, n1, metadata = list(version = 2L))
    g4 <- bg_read_graph(handle)
    expect_equal(g4$nodes[[n1]]$metadata$author, "a")
    expect_equal(g4$nodes[[n1]]$metadata$version, 2L)
  })
})
