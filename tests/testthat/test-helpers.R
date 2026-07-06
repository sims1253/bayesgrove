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
