describe("Decision and Gate Layer", {
  it("can add a gate to an edge and track it as pending", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind("test_kind")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    n1 <- bg_add_node(handle, kind = "test_kind", label = "Node 1")
    n2 <- bg_add_node(handle, kind = "test_kind", label = "Node 2", inputs = n1)

    # Add a gate
    gate <- bg_add_gate(
      project = handle,
      from = n1,
      to = n2,
      prompt = "Should we proceed?",
      options = c("yes", "no")
    )

    expect_true(startsWith(gate$id, "gate_"))
    expect_equal(gate$prompt, "Should we proceed?")

    # Check pending gates list
    pending <- bg_pending_gates(handle)
    expect_length(pending, 1)
    expect_equal(pending[[gate$id]]$from_node_id, n1)
    expect_equal(pending[[gate$id]]$to_node_id, n2)

    # Check graph contains structural gate
    g2 <- bg_read_graph(handle)
    expect_true(gate$id %in% names(g2$gates))
    expect_equal(g2$gates[[gate$id]]$status, "pending")
  })

  it("can answer a gate, removing it from pending and recording the decision", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)
    graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind("test_kind")
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    n1 <- bg_add_node(handle, kind = "test_kind", label = "Node 1")
    n2 <- bg_add_node(handle, kind = "test_kind", label = "Node 2", inputs = n1)

    gate <- bg_add_gate(
      project = handle,
      from = n1,
      to = n2,
      prompt = "Should we proceed?",
      options = c("yes", "no")
    )

    # Must fail without rationale
    expect_error(
      bg_answer_gate(handle, id = gate$id, choice = "yes", rationale = ""),
      "Rationale is required"
    )

    # Must fail with invalid choice
    expect_error(
      bg_answer_gate(
        handle,
        id = gate$id,
        choice = "maybe",
        rationale = "Because"
      ),
      "valid options"
    )

    # Valid answer
    decision <- bg_answer_gate(
      handle,
      id = gate$id,
      choice = "yes",
      rationale = "looks good"
    )

    expect_equal(decision$choice, "yes")
    expect_equal(decision$rationale, "looks good")

    # Pending list should be empty
    pending <- bg_pending_gates(handle)
    expect_length(pending, 0)

    # Graph structural gate should be resolved
    g2 <- bg_read_graph(handle)
    expect_equal(g2$gates[[gate$id]]$status, "resolved")
  })

  it("can record an explicit manual decision", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    decision <- bg_record_decision(
      handle,
      scope = "project",
      prompt = "Why this prior?",
      choice = "Normal(0, 1)",
      rationale = "Standard weakly informative."
    )

    expect_true(startsWith(decision$decision_id, "dec_"))

    log_path <- file.path(tmp, ".bayesgrove", "decisions", "decisions.jsonl")
    expect_true(file.exists(log_path))

    lines <- readLines(log_path)
    expect_length(lines, 1)

    record <- jsonlite::fromJSON(lines[1])
    expect_equal(record$choice, "Normal(0, 1)")
    expect_equal(record$rationale, "Standard weakly informative.")
  })
})
