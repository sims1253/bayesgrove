describe("Decision and Gate Layer", {
  it("rejects a gate whose edge is missing without changing project state", {
    handle <- bg_init(path = withr::local_tempdir())
    bg_register_node_kind(handle, "data")
    from <- bg_add_node(handle, "data")
    to <- bg_add_node(handle, "data", inputs = from)
    gate <- bg_add_gate(handle, from, to, "Proceed?", c("yes", "no"))
    graph <- bg_read_graph(handle)
    graph$edges[[gate$edge_id]] <- NULL
    graph$version <- graph$version + 1L
    bg_commit_graph(handle, graph)

    expect_error(
      bg_answer_gate(handle, gate$id, "yes", rationale = "Reviewed"),
      "no longer exists in the graph"
    )
    expect_equal(bg_read_graph(handle), graph)
    expect_length(bg_read_decisions(handle), 0)
    expect_true(gate$id %in% names(bg_read_gate_specs(handle)))
  })

  it("validates the handle before reading decisions", {
    expect_error(
      bg_read_decisions(list(path = tempdir())),
      "`project` must be a <bayesgrove::bg_handle>"
    )
  })

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
      alternatives = c("yes", "no")
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
      alternatives = c("yes", "no")
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
    expect_equal(decision$alternatives, "no")
    expect_equal(decision$metadata$from_node_id, n1)
    expect_equal(decision$metadata$to_node_id, n2)

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
    expect_equal(record$schema_name, "bg_decision_entry")
    expect_equal(record$schema_version, 1)
    expect_equal(record$project_id, handle@project_id)
    expect_equal(record$choice, "Normal(0, 1)")
    expect_equal(record$rationale, "Standard weakly informative.")
  })
})

describe("Monotonic seq for JSONL records (Phase 2.5)", {
  it("stamps each decision record with an incrementing seq", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    decisions <- list()
    for (i in 1:3) {
      bg_record_decision(
        handle,
        scope = "project",
        prompt = sprintf("Decision %d", i),
        choice = sprintf("choice_%d", i),
        rationale = "test"
      )
    }

    records <- bg_read_decisions(handle)
    seqs <- vapply(records, function(r) r$seq %||% NA_integer_, integer(1))

    expect_equal(unname(seqs), c(1L, 2L, 3L))
  })

  it("resolves two decisions in the same second to the later seq", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    # Record two decisions back to back; they will share a created_at second
    # but have distinct seq values.
    bg_record_decision(
      handle,
      scope = "project",
      prompt = "first",
      choice = "a",
      rationale = "earlier"
    )
    bg_record_decision(
      handle,
      scope = "project",
      prompt = "second",
      choice = "b",
      rationale = "later"
    )

    records <- bg_read_decisions(handle)
    latest_idx <- bg_latest_index_by_seq(records)
    expect_length(latest_idx, 1)
    expect_equal(records[[latest_idx]]$choice, "b")
  })

  it("stamps seq on summaries and jobs too (single chokepoint)", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(result = 1)
    })
    n1 <- bg_add_node(handle, kind = "data", label = "A")
    bg_run(handle, targets = n1)

    summaries <- bg_read_summaries(handle)
    if (length(summaries) > 0) {
      summary_seqs <- vapply(
        summaries,
        function(s) s$seq %||% NA_integer_,
        integer(1)
      )
      expect_true(!anyNA(summary_seqs))
    }

    jobs <- bg_jobs(handle)
    if (length(jobs) > 0) {
      job_seqs <- vapply(
        jobs,
        function(j) j$seq %||% NA_integer_,
        integer(1)
      )
      expect_true(!anyNA(job_seqs))
    }
  })
})
