describe("dagriculture boundary helpers", {
  local_helpers <- list(
    add_node = getFromNamespace("bg_dagri_add_node", "bayesgrove"),
    add_edge = getFromNamespace("bg_dagri_add_edge", "bayesgrove"),
    incoming_edges = getFromNamespace("bg_dagri_incoming_edges", "bayesgrove"),
    outgoing_edges = getFromNamespace("bg_dagri_outgoing_edges", "bayesgrove"),
    order_edges = getFromNamespace("bg_dagri_order_edges", "bayesgrove"),
    descendants = getFromNamespace("bg_dagri_descendants", "bayesgrove"),
    recompute_state = getFromNamespace("bg_dagri_recompute_state", "bayesgrove"),
    graph_diff = getFromNamespace("bg_dagri_graph_diff", "bayesgrove")
  )

  it("exposes graph-generic incoming, outgoing, and descendant queries", {
    graph <- dagriculture::dagri_graph(dagriculture::dagri_registry())
    graph$registry$kinds[["source"]] <- dagriculture::dagri_kind("source")
    graph$registry$kinds[["fit"]] <- dagriculture::dagri_kind("fit")
    graph$registry$kinds[["ppc"]] <- dagriculture::dagri_kind("ppc")

    graph <- local_helpers$add_node(graph, "node_a", "source", label = "A")
    graph <- local_helpers$add_node(graph, "node_b", "fit", label = "B")
    graph <- local_helpers$add_node(graph, "node_c", "ppc", label = "C")
    graph <- local_helpers$add_edge(
      graph,
      from = "node_a",
      to = "node_b",
      id = "edge_b"
    )
    graph <- local_helpers$add_edge(
      graph,
      from = "node_b",
      to = "node_c",
      id = "edge_c"
    )

    incoming_b <- local_helpers$incoming_edges(graph, "node_b")
    outgoing_b <- local_helpers$outgoing_edges(graph, "node_b")
    descendants_a <- local_helpers$descendants(graph, "node_a")

    expect_equal(unname(vapply(incoming_b, `[[`, character(1), "from")), "node_a")
    expect_equal(unname(vapply(outgoing_b, `[[`, character(1), "to")), "node_c")
    expect_equal(descendants_a, c("node_b", "node_c"))
  })

  it("orders edges deterministically by edge id", {
    edges <- list(
      late = list(id = "edge_z", from = "a", to = "b"),
      early = list(id = "edge_a", from = "c", to = "d"),
      middle = list(id = "edge_m", from = "e", to = "f")
    )

    ordered <- local_helpers$order_edges(edges)

    expect_equal(
      unname(vapply(ordered, `[[`, character(1), "id")),
      c("edge_a", "edge_m", "edge_z")
    )
  })

  it("computes structural graph diffs without workflow semantics", {
    graph_before <- dagriculture::dagri_graph(dagriculture::dagri_registry())
    graph_before$registry$kinds[["source"]] <- dagriculture::dagri_kind("source")
    graph_before$registry$kinds[["fit"]] <- dagriculture::dagri_kind("fit")

    graph_before <- local_helpers$add_node(
      graph_before,
      "node_a",
      "source",
      label = "A"
    )
    graph_before <- local_helpers$add_node(
      graph_before,
      "node_b",
      "fit",
      label = "B"
    )
    graph_before <- local_helpers$add_edge(
      graph_before,
      from = "node_a",
      to = "node_b",
      id = "edge_ab"
    )

    graph_after <- local_helpers$add_node(
      graph_before,
      "node_c",
      "fit",
      label = "C"
    )
    graph_after <- local_helpers$add_edge(
      graph_after,
      from = "node_b",
      to = "node_c",
      id = "edge_bc"
    )

    diff <- local_helpers$graph_diff(graph_before, graph_after)
    reverse_diff <- local_helpers$graph_diff(graph_after, graph_before)

    expect_equal(diff$added_nodes, "node_c")
    expect_equal(diff$removed_nodes, character())
    expect_equal(diff$added_edges, "edge_bc")
    expect_equal(diff$removed_edges, character())
    expect_equal(reverse_diff$added_nodes, character())
    expect_equal(reverse_diff$removed_nodes, "node_c")
    expect_equal(reverse_diff$added_edges, character())
    expect_equal(reverse_diff$removed_edges, "edge_bc")
  })

  it("keeps state recomputation graph-generic", {
    graph <- dagriculture::dagri_graph(dagriculture::dagri_registry())
    graph$registry$kinds[["source"]] <- dagriculture::dagri_kind("source")
    graph$registry$kinds[["fit"]] <- dagriculture::dagri_kind("fit")

    graph <- local_helpers$add_node(graph, "node_a", "source", label = "A")
    graph <- local_helpers$add_node(graph, "node_b", "fit", label = "B")
    graph <- local_helpers$add_edge(
      graph,
      from = "node_a",
      to = "node_b",
      id = "edge_ab"
    )

    recomputed <- local_helpers$recompute_state(graph)

    expect_equal(recomputed$nodes[["node_a"]]$state, "ready")
    expect_equal(recomputed$nodes[["node_b"]]$state, "ready")
  })
})
