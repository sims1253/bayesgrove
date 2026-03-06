describe("Async Execution Layer", {
  skip_if_async_package_unavailable <- function() {
    installed_path <- tryCatch(find.package("bayesgrove"), error = function(e) {
      ""
    })
    if (!nzchar(installed_path)) {
      skip("async worker tests require an installed bayesgrove package")
    }
  }

  it("can submit jobs via callr and wait for completion", {
    skip_if_not_installed("callr")
    skip_if_async_package_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(
      handle,
      "slow_data",
      executor = function(node, inputs) {
        42
      }
    )

    n1 <- bg_add_node(handle, "slow_data", label = "A")

    # Submit async
    run_res <- bg_submit(handle, backend = "callr")
    expect_equal(run_res$status, "running")
    expect_length(run_res$job_ids, 1)

    # Wait for completion
    tryCatch(
      {
        bg_wait(handle, run_res$run_id, timeout = 10)
      },
      error = function(e) {
        cat("JOB STATUS AT TIMEOUT:\n")
        print(bg_jobs(handle))
        cat(
          "STDOUT:\n",
          readLines(file.path(
            tmp,
            ".bayesgrove",
            "runs",
            paste0(run_res$job_ids[1], "_stdout.log")
          )),
          "\n"
        )
        cat(
          "STDERR:\n",
          readLines(file.path(
            tmp,
            ".bayesgrove",
            "runs",
            paste0(run_res$job_ids[1], "_stderr.log")
          )),
          "\n"
        )
        cat(
          "DEBUG:\n",
          readLines(file.path(
            tmp,
            ".bayesgrove",
            "runs",
            paste0(run_res$job_ids[1], "_debug.txt")
          )),
          "\n"
        )
        stop(e)
      }
    )

    # Verify job status
    jobs <- bg_jobs(handle)
    expect_equal(jobs[[run_res$job_ids[1]]]$status, "succeeded")

    # Verify result can be fetched
    res <- bg_result(handle, n1)
    expect_equal(res, 42)
  })

  it("handles failed async jobs gracefully", {
    skip_if_not_installed("callr")
    skip_if_async_package_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(
      handle,
      "fail_node",
      executor = function(node, inputs) {
        Sys.sleep(0.5)
        stop("Intentional failure")
      }
    )

    n1 <- bg_add_node(handle, "fail_node", label = "A")

    run_res <- bg_submit(handle, backend = "callr")
    bg_wait(handle, run_res$run_id, timeout = 10)

    jobs <- bg_jobs(handle)
    expect_equal(jobs[[run_res$job_ids[1]]]$status, "failed")
    expect_equal(
      jobs[[run_res$job_ids[1]]]$error$message,
      "Intentional failure"
    )
  })

  it("can submit jobs via mirai and wait for completion", {
    skip_if_not_installed("mirai")
    skip_if_async_package_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(
      handle,
      "fast_data",
      executor = function(node, inputs) {
        42
      }
    )

    n1 <- bg_add_node(handle, "fast_data", label = "A")

    run_res <- bg_submit(handle, backend = "mirai")

    tryCatch(
      {
        bg_wait(handle, run_res$run_id, timeout = 10)
      },
      error = function(e) {
        stop(e)
      }
    )

    jobs <- bg_jobs(handle)
    expect_equal(jobs[[run_res$job_ids[1]]]$status, "succeeded")

    res <- bg_result(handle, n1)
    expect_equal(res, 42)
  })

  it("can cancel jobs", {
    skip_if_not_installed("callr")
    skip_if_async_package_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(
      handle,
      "very_slow",
      executor = function(node, inputs) {
        Sys.sleep(10)
        42
      }
    )

    n1 <- bg_add_node(handle, "very_slow", label = "A")

    run_res <- bg_submit(handle, backend = "callr")

    # Wait a tiny bit for it to start
    Sys.sleep(0.5)

    # Cancel it
    bg_cancel(handle, run_res$run_id)

    jobs <- bg_jobs(handle)
    expect_equal(jobs[[run_res$job_ids[1]]]$status, "cancelled")
  })
})
