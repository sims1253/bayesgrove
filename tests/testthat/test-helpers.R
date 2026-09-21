describe("CLI helper output", {
  it("suppresses informational chatter when quiet mode is enabled", {
    quiet_inform <- getFromNamespace("bg_cli_inform", "bayesgrove")

    expect_no_message(
      withr::with_options(
        list(bayesgrove.quiet_inform = TRUE),
        quiet_inform("This should stay quiet.")
      )
    )
  })

  it("emits messages when quiet mode is disabled", {
    quiet_inform <- getFromNamespace("bg_cli_inform", "bayesgrove")

    expect_message(
      withr::with_envvar(
        c(TESTTHAT = "false"),
        withr::with_options(
          list(bayesgrove.quiet_inform = FALSE),
          quiet_inform("This should be visible.")
        )
      ),
      regexp = "This should be visible."
    )
  })

  it("evaluates cli glue in the caller environment", {
    quiet_inform <- getFromNamespace("bg_cli_inform", "bayesgrove")
    run_id <- "run_test123"

    expect_message(
      withr::with_envvar(
        c(TESTTHAT = "false"),
        quiet_inform("Starting run {.val {run_id}}.")
      ),
      regexp = "run_test123"
    )
  })
})

describe("bg_require_rationale", {
  it("rejects null, NA, wrong-length, non-character, and blank rationales", {
    for (bad in list(NULL, NA_character_, c("first", "second"), 3, "   ")) {
      expect_error(
        bayesgrove:::bg_require_rationale(bad),
        "Rationale is required",
        class = "rlang_error"
      )
    }
  })

  it("returns the trimmed rationale when one is supplied", {
    expect_equal(
      bayesgrove:::bg_require_rationale("  because of prior checks  "),
      "because of prior checks"
    )
  })
})

describe("bg_new_id", {
  it("draws ids from a 64-bit space, wider than the old 32-bit digest", {
    ids <- replicate(200, bayesgrove:::bg_new_id("node"))

    # 16 hex chars of digest (the old generator emitted 8, i.e. 32 bits).
    expect_true(all(grepl("^node_[0-9a-f]{16}$", ids)))
    expect_false(any(grepl("^node_[0-9a-f]{8}$", ids)))
    # 200 draws from 2^64 must be distinct (a repeated value would mean the
    # generator collapsed back to a narrow space).
    expect_true(length(unique(ids)) == 200L)
  })
})

describe("bg_new_unique_id", {
  it("regenerates when a drawn id is already taken", {
    draws <- 0L
    local_mocked_bindings(bg_new_id = function(prefix) {
      draws <<- draws + 1L
      if (draws == 1L) "node_taken" else "node_fresh"
    })

    id <- bayesgrove:::bg_new_unique_id("node", "node_taken")
    expect_equal(id, "node_fresh")
    expect_equal(draws, 2L)
  })

  it("returns the first draw when it does not collide", {
    local_mocked_bindings(bg_new_id = function(prefix) "node_free")

    expect_equal(
      bayesgrove:::bg_new_unique_id("node", c("node_a", "node_b")),
      "node_free"
    )
  })

  it("aborts once the draw budget is exhausted", {
    local_mocked_bindings(bg_new_id = function(prefix) "node_taken")

    expect_error(
      bayesgrove:::bg_new_unique_id("node", "node_taken", max_attempts = 3L),
      "Failed to draw a unique"
    )
  })
})
