# Adapter boundary smoke test (Milestone 5) ----------------------------------
#
# After M5, the bg_dagri_* edge/diff helpers are thin pass-throughs to
# dagriculture (>=0.3.0), which now owns and tests them. bayesgrove keeps only a
# light smoke test here: the adapters route to the right dagriculture functions.
# Full semantic coverage lives in dagriculture's own test suite.

describe("bg_dagri_* adapters pass through to dagriculture", {
  it("routes edge traversal and diff helpers to dagriculture::dagri_*", {
    graph <- dagriculture::dagri_graph(dagriculture::dagri_registry())
    graph$registry$kinds[["source"]] <- dagriculture::dagri_kind("source")
    graph$registry$kinds[["fit"]] <- dagriculture::dagri_kind("fit")

    graph <- dagriculture::dagri_add_node(
      graph,
      "node_a",
      "source",
      label = "A"
    )
    graph <- dagriculture::dagri_add_node(graph, "node_b", "fit", label = "B")
    graph <- dagriculture::dagri_add_edge(
      graph,
      from = "node_a",
      to = "node_b",
      id = "edge_ab"
    )

    # The adapters return exactly what the underlying dagriculture functions do.
    expect_equal(
      bayesgrove:::bg_dagri_incoming_edges(graph, "node_b"),
      dagriculture::dagri_incoming_edges(graph, "node_b")
    )
    expect_equal(
      bayesgrove:::bg_dagri_outgoing_edges(graph, "node_a"),
      dagriculture::dagri_outgoing_edges(graph, "node_a")
    )

    edges <- list(
      late = list(id = "edge_z", from = "a", to = "b"),
      early = list(id = "edge_a", from = "c", to = "d")
    )
    expect_equal(
      bayesgrove:::bg_dagri_order_edges(edges),
      dagriculture::dagri_order_edges(edges)
    )
    expect_equal(
      bayesgrove:::bg_dagri_edge_ids(edges),
      dagriculture::dagri_edge_ids(edges)
    )
    expect_equal(
      bayesgrove:::bg_dagri_graph_diff(graph, graph),
      dagriculture::dagri_graph_diff(graph, graph)
    )
  })
})
