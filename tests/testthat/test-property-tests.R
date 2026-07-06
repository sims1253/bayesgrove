# Property-Based Tests using Hedgehog
# ------------------------------------
# Tests for invariants and properties that should hold across all inputs.

# Skip all tests in this file if hedgehog is not available
testthat::skip_if_not_installed("hedgehog")

library(hedgehog)

describe("Property: Fingerprint Determinism", {
  it("produces identical fingerprints for identical inputs", {
    forall(
      list(a = gen.c(gen.element(letters), 5), b = gen.unif(1, 100)),
      function(a, b) {
        tmp <- withr::local_tempdir()
        handle <- bg_init(path = tmp)

        bg_register_node_kind(handle, "test_kind")

        n1 <- bg_add_node(
          handle,
          kind = "test_kind",
          params = list(label = paste(a, collapse = ""), value = b)
        )

        f1 <- bg_compute_fingerprint(handle, n1)
        f2 <- bg_compute_fingerprint(handle, n1)

        expect_identical(f1, f2)
      }
    )
  })

  it("produces different fingerprints for different params", {
    forall(
      list(
        a = gen.c(gen.element(letters), 3),
        b = gen.c(gen.element(letters), 3)
      ),
      function(a, b) {
        # Skip if strings are identical
        if (identical(a, b)) {
          return(TRUE)
        }

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

describe("Property: Graph Invariants", {
  it("maintains node count after operations", {
    forall(
      list(n = gen.int(10)),
      function(n) {
        if (n < 1) {
          return(TRUE)
        }

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
    forall(
      list(n = gen.int(10)),
      function(n) {
        if (n < 2) {
          expect_true(TRUE) # Trivially true for n < 2
          return()
        }

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

describe("Property: Label Handling", {
  it("preserves label exactly", {
    forall(
      list(s = gen.c(gen.element(c(letters, LETTERS, " ", "-", "_")), 20)),
      function(s) {
        label <- paste(s, collapse = "")
        if (nchar(trimws(label)) == 0) {
          return(TRUE)
        } # Skip empty labels

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

describe("Property: Params Handling", {
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
