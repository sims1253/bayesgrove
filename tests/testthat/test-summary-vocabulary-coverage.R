# Vocabulary / coverage test (Milestone 4 item 4) ----------------------------
#
# Every literal summary_kinds string referenced by a pack provider must be in
# bg_summary_vocabulary(). Dynamic lookups (function calls on the RHS) are
# skipped; this catches the typo / dead-end class of bug the milestone targets.

describe("summary_kinds vocabulary coverage (Milestone 4)", {
  it("every literal summary_kinds = \"...\" in the pack sources is known", {
    pack_files <- list.files(
      pattern = "^workflow-packs.*\\.R$",
      path = system.file("R", package = "bayesgrove"),
      full.names = FALSE
    )
    # When running under devtools, source lives at R/ not inst; read both.
    src_dir <- if (nzchar(Sys.getenv("DEVTOOLS_LOAD"))) {
      "R"
    } else {
      system.file("R", package = "bayesgrove")
    }
    if (!dir.exists(src_dir)) {
      src_dir <- system.file("R", package = "bayesgrove")
    }
    pack_files <- list.files(
      pattern = "^workflow-packs.*\\.R$",
      path = src_dir,
      full.names = TRUE
    )

    # Extract summary_kinds = "<literal>" or summary_kinds = c("<literal>", ...)
    literals <- character()
    pat <- "summary_kinds[[:space:]]*=[[:space:]]*(.+)"
    for (f in pack_files) {
      lines <- readLines(f, warn = FALSE)
      for (line in lines) {
        m <- regmatches(line, regexec(pat, line))[[1]]
        if (length(m) > 1L) {
          rhs <- m[[2]]
          # Only scan RHS that is purely literal (string or c(...) of strings),
          # not a function call like bg_pack_selection_summary_kinds().
          if (!grepl("[a-zA-Z_][a-zA-Z0-9_.]*\\(", rhs)) {
            quoted <- regmatches(rhs, gregexpr('"[^"]+"', rhs))[[1]]
            literals <- c(literals, gsub('"', "", quoted))
          }
        }
      }
    }

    known <- names(bg_summary_vocabulary())
    unknown <- setdiff(unique(literals), known)
    expect_length(unknown, 0L)
  })

  it("every built-in node kind's emitted summary kinds are in the vocabulary", {
    # The diagnostic executors added in M4 must emit summary kinds that exist in
    # the vocabulary. loo_pit -> loo_pit_calibration; sbc -> sbc_result; ppc ->
    # posterior_predictive_check. These are the kinds packs match on.
    vocab <- names(bg_summary_vocabulary())
    expect_true("loo_pit_calibration" %in% vocab)
    expect_true("sbc_result" %in% vocab)
    expect_true("posterior_predictive_check" %in% vocab)
  })

  it("bg_summary_vocabulary documents which kinds a built-in executor emits", {
    # The vocab descriptors carry an `emitted_by` field; the M4 kinds must
    # reference their built-in executors so the ?bg_summary_vocabulary table
    # closes the "what produces this evidence?" loop.
    vocab <- bg_summary_vocabulary()
    expect_match(vocab[["loo_pit_calibration"]]$emitted_by, "loo_pit")
    expect_match(vocab[["sbc_result"]]$emitted_by, "sbc")
    expect_match(vocab[["posterior_predictive_check"]]$emitted_by, "ppc")
  })
})
