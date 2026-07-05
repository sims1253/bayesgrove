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
})
