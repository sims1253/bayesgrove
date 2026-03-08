describe("Async Execution Layer", {
  skip_if_async_package_unavailable <- function() {
    installed_path <- tryCatch(find.package("bayesgrove"), error = function(e) {
      ""
    })
    if (!nzchar(installed_path)) {
      skip("async worker tests require an installed bayesgrove package")
    }
  }

  skip_if_mirai_daemon_unavailable <- function(compute = "bayesgrove-test") {
    started_daemon <- FALSE
    ready <- tryCatch(
      {
        if (mirai::status(.compute = compute)$daemons == 0) {
          mirai::daemons(1, .compute = compute)
          started_daemon <- TRUE
        }
        TRUE
      },
      error = function(e) e
    )

    if (inherits(ready, "error")) {
      skip(paste(
        "mirai daemon unavailable in this environment:",
        ready$message
      ))
    }

    if (started_daemon) {
      withr::defer(
        mirai::daemons(0, .compute = compute),
        envir = parent.frame()
      )
    }
  }

  it("can submit jobs via mirai and wait for completion", {
    skip_if_not_installed("mirai")
    skip_if_async_package_unavailable()
    skip_if_mirai_daemon_unavailable()

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
    skip_if_not_installed("mirai")
    skip_if_async_package_unavailable()
    skip_if_mirai_daemon_unavailable()

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

    run_res <- bg_submit(handle, backend = "mirai")

    # Wait a tiny bit for it to start
    Sys.sleep(0.5)

    # Cancel it
    bg_cancel(handle, run_res$run_id)

    jobs <- bg_jobs(handle)
    expect_equal(jobs[[run_res$job_ids[1]]]$status, "cancelled")
  })

  it("returns a consistent handle when nothing needs execution", {
    skip_if_not_installed("mirai")
    skip_if_async_package_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(
      handle,
      "source",
      executor = function(node, inputs) {
        42
      }
    )

    node_id <- bg_add_node(handle, "source", label = "A")
    bg_run(handle, mode = "sync")

    run_res <- bg_submit(handle, backend = "mirai")

    expect_s3_class(run_res, "bg_run_handle")
    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$mode, "async")
    expect_equal(run_res$targets, node_id)
    expect_equal(run_res$job_ids, character(0))
    expect_null(run_res$submitted_at)
    expect_null(run_res$started_at)
    expect_null(run_res$finished_at)
    expect_equal(run_res$summary$total_jobs, 0L)
    expect_equal(run_res$metadata, list())
  })

  it("handles failed async jobs gracefully", {
    skip_if_not_installed("mirai")
    skip_if_async_package_unavailable()
    skip_if_mirai_daemon_unavailable()

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

    bg_add_node(handle, "fail_node", label = "A")

    run_res <- bg_submit(handle, backend = "mirai")
    bg_wait(handle, run_res$run_id, timeout = 10)

    jobs <- bg_jobs(handle)
    expect_equal(jobs[[run_res$job_ids[1]]]$status, "failed")
    expect_equal(
      jobs[[run_res$job_ids[1]]]$error$message,
      "Intentional failure"
    )
  })
})
