# Property-based tests (hedgehog) with plain-testthat fallbacks --------------
#
# The hedgehog describes check the invariants across random inputs when
# hedgehog is available. The plain fallback describe below runs the two core
# fingerprint invariants everywhere, including environments without hedgehog,
# so the properties contribute coverage instead of silent skips.

if (requireNamespace("hedgehog", quietly = TRUE)) {
  library(hedgehog)
}

describe("Fingerprint properties (plain testthat fallback)", {
  it("computes identical fingerprints for equal parameters in any order", {
    set.seed(20260921)

    for (i in seq_len(12)) {
      tmp <- withr::local_tempdir()
      handle <- bg_init(path = tmp)
      bg_register_node_kind(handle, "test_kind")

      params <- lapply(seq_len(sample.int(5, 1L)), function(.) {
        if (runif(1) < 0.5) {
          sample.int(100, 1L)
        } else {
          paste0("v", letters[sample.int(26, 1L)])
        }
      })
      names(params) <- paste0("param_", seq_along(params))

      n1 <- bg_add_node(handle, kind = "test_kind", params = params)
      fp1 <- bg_compute_fingerprint(handle, n1)

      shuffled_params <- params[sample(seq_along(params))]
      n2 <- bg_add_node(handle, kind = "test_kind", params = shuffled_params)
      fp2 <- bg_compute_fingerprint(handle, n2)

      expect_equal(fp1, fp2, info = paste("iteration", i))
    }
  })

  it("changes the fingerprint when a node's parameters change", {
    set.seed(42)

    for (i in seq_len(12)) {
      tmp <- withr::local_tempdir()
      handle <- bg_init(path = tmp)
      bg_register_node_kind(handle, "test_kind")

      vals <- sample.int(1000, 2L)
      n1 <- bg_add_node(
        handle,
        kind = "test_kind",
        params = list(value = vals[[1]])
      )
      fp1 <- bg_compute_fingerprint(handle, n1)

      bg_update_node(handle, n1, params = list(value = vals[[2]]))
      fp2 <- bg_compute_fingerprint(handle, n1)

      expect_true(fp1 != fp2, info = paste("iteration", i))
    }
  })
})

describe("Property: Fingerprints (hedgehog)", {
  skip_if_not_installed("hedgehog")

  it("computes the exact same fingerprint regardless of parameter order", {
    gen_params <- gen.list(
      gen.element(c(
        gen.int(100),
        gen.c(gen.c(gen.alpha())),
        gen.c(gen.int(10))
      )),
      from = 1,
      to = 5
    )

    gen_named_params <- gen.map(
      function(l) {
        names(l) <- paste0("param_", seq_along(l))
        l
      },
      gen_params
    )

    forall(gen_named_params, function(params) {
      tmp <- withr::local_tempdir()
      handle <- bg_init(path = tmp)
      bg_register_node_kind(handle, "test_kind")

      n1 <- bg_add_node(handle, "test_kind", params = params)
      fp1 <- bg_compute_fingerprint(handle, n1)

      shuffled_indices <- sample(seq_along(params))
      shuffled_params <- params[shuffled_indices]

      n2 <- bg_add_node(handle, "test_kind", params = shuffled_params)
      fp2 <- bg_compute_fingerprint(handle, n2)

      expect_equal(fp1, fp2)
    })
  })

  it("produces different fingerprints for different params", {
    # Disjoint alphabets guarantee the two strings differ for every sample,
    # so the property never runs vacuously.
    forall(
      list(
        a = gen.c(gen.element(letters), 3),
        b = gen.c(gen.element(LETTERS), 3)
      ),
      function(a, b) {
        tmp <- withr::local_tempdir()
        handle <- bg_init(path = tmp)
        bg_register_node_kind(handle, "test_kind")

        n1 <- bg_add_node(
          handle,
          kind = "test_kind",
          params = list(value = paste(a, collapse = ""))
        )

        f1 <- bg_compute_fingerprint(handle, n1)

        bg_update_node(
          handle,
          n1,
          params = list(value = paste(b, collapse = ""))
        )

        f2 <- bg_compute_fingerprint(handle, n1)

        expect_true(f1 != f2)
      }
    )
  })
})

describe("Property: Graph Invariants (hedgehog)", {
  skip_if_not_installed("hedgehog")

  it("maintains node count after operations", {
    forall(
      list(n = gen.element(1:10)),
      function(n) {
        tmp <- withr::local_tempdir()
        handle <- bg_init(path = tmp)

        graph <- bg_read_graph(handle)
        graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind(
          "test_kind"
        )
        graph$version <- graph$version + 1L
        bg_commit_graph(handle, graph)

        initial_count <- length(bg_read_graph(handle)$nodes)

        # Add nodes
        node_ids <- character(n)
        for (i in seq_len(n)) {
          node_ids[i] <- bg_add_node(handle, kind = "test_kind")
        }

        mid_count <- length(bg_read_graph(handle)$nodes)
        expect_equal(mid_count, initial_count + n)

        # Remove nodes
        for (nid in node_ids) {
          bg_remove_node(handle, nid)
        }

        final_count <- length(bg_read_graph(handle)$nodes)
        expect_equal(final_count, initial_count)
      }
    )
  })

  it("maintains edge consistency", {
    # n >= 2 so every sample builds at least one edge and the property never
    # runs vacuously.
    forall(
      list(n = gen.element(2:10)),
      function(n) {
        tmp <- withr::local_tempdir()
        handle <- bg_init(path = tmp)

        graph <- bg_read_graph(handle)
        graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind(
          "test_kind"
        )
        graph$version <- graph$version + 1L
        bg_commit_graph(handle, graph)

        # Add nodes
        node_ids <- character(n)
        for (i in seq_len(n)) {
          node_ids[i] <- bg_add_node(handle, kind = "test_kind")
        }

        # Add edges
        for (i in seq_len(n - 1)) {
          bg_add_node(handle, kind = "test_kind", inputs = node_ids[i])
        }

        # Verify all edges connect existing nodes
        g <- bg_read_graph(handle)
        for (e in g$edges) {
          expect_true(e$from %in% names(g$nodes))
          expect_true(e$to %in% names(g$nodes))
        }
      }
    )
  })

  it("preserves graph version monotonicity", {
    forall(
      list(ops = gen.c(gen.element(c("add", "update")), 5)),
      function(ops) {
        tmp <- withr::local_tempdir()
        handle <- bg_init(path = tmp)

        graph <- bg_read_graph(handle)
        graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind(
          "test_kind"
        )
        graph$version <- graph$version + 1L
        bg_commit_graph(handle, graph)

        versions <- integer(length(ops) + 1)
        versions[1] <- bg_read_graph(handle)$version

        node_id <- NULL
        for (i in seq_along(ops)) {
          if (ops[i] == "add") {
            node_id <- bg_add_node(handle, kind = "test_kind")
          } else if (!is.null(node_id)) {
            bg_update_node(handle, node_id, label = paste("Label", i))
          }
          versions[i + 1] <- bg_read_graph(handle)$version
        }

        expect_true(all(diff(versions) >= 0))
      }
    )
  })
})

describe("Property: Label Handling (hedgehog)", {
  skip_if_not_installed("hedgehog")

  it("preserves label exactly", {
    # No blank characters in the generator, so every sample has a usable label.
    forall(
      list(s = gen.c(gen.element(c(letters, LETTERS, "-", "_")), 20)),
      function(s) {
        label <- paste(s, collapse = "")

        tmp <- withr::local_tempdir()
        handle <- bg_init(path = tmp)

        graph <- bg_read_graph(handle)
        graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind(
          "test_kind"
        )
        graph$version <- graph$version + 1L
        bg_commit_graph(handle, graph)

        n1 <- bg_add_node(handle, kind = "test_kind", label = label)

        g <- bg_read_graph(handle)
        expect_identical(g$nodes[[n1]]$label, label)
      }
    )
  })
})

describe("Property: Params Handling (hedgehog)", {
  skip_if_not_installed("hedgehog")

  it("preserves numeric params", {
    forall(
      list(x = gen.unif(-1000, 1000), y = gen.unif(0, 1000)),
      function(x, y) {
        tmp <- withr::local_tempdir()
        handle <- bg_init(path = tmp)

        graph <- bg_read_graph(handle)
        graph$registry$kinds[["test_kind"]] <- dagriculture::dagri_kind(
          "test_kind"
        )
        graph$version <- graph$version + 1L
        bg_commit_graph(handle, graph)

        n1 <- bg_add_node(
          handle,
          kind = "test_kind",
          params = list(x = x, y = y)
        )

        g <- bg_read_graph(handle)
        # Allow for JSON serialization precision (jsonlite defaults to ~6 significant digits)
        # Using 1e-3 tolerance for 3 decimal places
        expect_true(abs(g$nodes[[n1]]$params$x - x) < 1e-3)
        expect_true(abs(g$nodes[[n1]]$params$y - y) < 1e-3)
      }
    )
  })
})
