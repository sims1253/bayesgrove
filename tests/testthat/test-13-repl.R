describe("Interactive REPL", {
  it("aborts when run non-interactively", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    # testthat runs non-interactively by default, so it should error
    expect_error(bg_repl(handle), "must be run in an interactive R session")
  })
})
