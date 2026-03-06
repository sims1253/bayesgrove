describe("Graph Branching", {
  it("creates a clone of a node with upstream edges", {
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
      label = "Transform",
      inputs = n1
    )

    # Branch n2
    branch <- bg_branch(handle, n2, label = "Transform Branch")
    n3 <- branch$root_node_id
    expect_true(startsWith(n3, "node_"))
    expect_true(startsWith(branch$branch_id, "branch:"))

    g2 <- bg_read_graph(handle)

    # The new node should exist and have the new label
    expect_equal(g2$nodes[[n3]]$label, "Transform Branch")

    # The new node should have an edge from n1
    edges_to_n3 <- Filter(function(e) e$to == n3, g2$edges)
    expect_equal(length(edges_to_n3), 1)
    expect_equal(edges_to_n3[[1]]$from, n1)
  })
})
