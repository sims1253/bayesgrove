describe("Job Tracking Layer", {
  it("can log and read jobs", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    # Empty jobs list initially
    jobs <- bg_jobs(handle)
    expect_length(jobs, 0)

    j1 <- bg_create_job(handle, run_id = "r1", node_id = "n1")

    jobs <- bg_jobs(handle)
    expect_length(jobs, 1)
    expect_equal(jobs[[j1$job_id]]$status, "queued")
    expect_equal(jobs[[j1$job_id]]$node_id, "n1")

    # Update job
    bg_update_job(handle, j1$job_id, status = "running")

    jobs_updated <- bg_jobs(handle)
    expect_length(jobs_updated, 1) # Still 1 job, just updated
    expect_equal(jobs_updated[[j1$job_id]]$status, "running")
  })

  it("can filter jobs by status", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    j1 <- bg_create_job(handle, run_id = "r1", node_id = "n1")
    j2 <- bg_create_job(handle, run_id = "r1", node_id = "n2")

    bg_update_job(handle, j2$job_id, status = "succeeded")

    queued_jobs <- bg_jobs(handle, status = "queued")
    succ_jobs <- bg_jobs(handle, status = "succeeded")

    expect_length(queued_jobs, 1)
    expect_equal(names(queued_jobs), j1$job_id)

    expect_length(succ_jobs, 1)
    expect_equal(names(succ_jobs), j2$job_id)
  })

  it("keeps seq stamps monotonic after a cold cache on an existing log", {
    # Regression: bg_log_job used to reset the cached line count to 1 when the
    # cache was cold on a file with pre-existing records, so the NEXT append
    # got a duplicate seq and broke last-record-wins ordering.
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_create_job(handle, run_id = "r1", node_id = "n1")
    bg_create_job(handle, run_id = "r1", node_id = "n2")

    # Simulate a fresh handle: drop the in-memory jobs cache.
    handle@.state$jobs_cache <- NULL

    bg_create_job(handle, run_id = "r1", node_id = "n3")
    bg_create_job(handle, run_id = "r1", node_id = "n4")

    lines <- readLines(bg_jobs_log_path(handle), warn = FALSE)
    seqs <- vapply(
      lines,
      function(line) jsonlite::fromJSON(line)$seq,
      integer(1),
      USE.NAMES = FALSE
    )
    expect_equal(seqs, 1:4)
  })

  it("counts lines correctly when records contain quote characters", {
    # Regression: count.fields-based line counting misparsed JSON's escaped
    # quotes (\"), merging lines and undercounting, which corrupted seq
    # stamps. Error messages with quotes are the realistic trigger.
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    j1 <- bg_create_job(handle, run_id = "r1", node_id = "n1")
    bg_update_job(
      handle,
      j1$job_id,
      status = "failed",
      error = list(message = 'missing closing " in Stan program')
    )

    # Force a re-count from disk, then append again: seq must stay monotonic.
    handle@.state$jobs_cache <- NULL
    bg_create_job(handle, run_id = "r1", node_id = "n2")

    lines <- readLines(bg_jobs_log_path(handle), warn = FALSE)
    seqs <- vapply(
      lines,
      function(line) jsonlite::fromJSON(line)$seq,
      integer(1),
      USE.NAMES = FALSE
    )
    expect_equal(seqs, seq_along(lines))
  })
})
