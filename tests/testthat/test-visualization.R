# Visualization (Milestone 7) ------------------------------------------------

describe("bg_graph_mermaid (Milestone 7)", {
  it("emits a Mermaid flowchart with node labels and state classes", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "data", executor = function(node, inputs) 1)
    a <- bg_add_node(handle, kind = "data", label = "A")
    b <- bg_add_node(handle, kind = "data", label = "B", inputs = a)

    text <- bg_graph_mermaid(handle)
    # Flowchart header, both node ids present, labels rendered, state classes.
    expect_match(text, "^flowchart TD")
    expect_true(a %in% strsplit(text, "\n")[[1]][[3]] || grepl(a, text))
    expect_true(grepl("A", text) && grepl("B", text))
    expect_true(grepl("ready", text))
  })

  it("respects the direction argument", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "data", executor = function(node, inputs) 1)
    bg_add_node(handle, kind = "data", label = "A")

    text <- bg_graph_mermaid(handle, direction = "LR")
    expect_match(text, "^flowchart LR")
  })
})

describe("bg_plot dispatch (Milestone 7)", {
  it("aborts for a node kind without a plot method", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "data", executor = function(node, inputs) 1)
    n <- bg_add_node(handle, kind = "data", label = "A")
    bg_run(handle)
    expect_error(
      bg_plot(handle, n),
      class = "rlang_error"
    )
  })

  it("aborts when the node has no artifact yet", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "data", executor = function(node, inputs) 1)
    n <- bg_add_node(handle, kind = "data", label = "A")
    expect_error(
      bg_plot(handle, n),
      class = "rlang_error"
    )
  })
})
