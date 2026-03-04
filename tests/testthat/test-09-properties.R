describe("Fingerprint Properties (Developer Tests)", {
  it("computes the exact same fingerprint regardless of parameter order", {
    skip_if_not_installed("hedgehog")
    library(hedgehog)

    # Generate a list of random parameters
    gen_params <- gen.list(
      gen.element(c(
        gen.int(100),
        gen.c(gen.c(gen.alpha())),
        gen.c(gen.int(10))
      )),
      from = 1,
      to = 5
    )

    # Make them named lists
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

      # Original order
      n1 <- bg_add_node(handle, "test_kind", params = params)
      fp1 <- bg_compute_fingerprint(handle, n1)

      # Shuffled order
      shuffled_indices <- sample(seq_along(params))
      shuffled_params <- params[shuffled_indices]

      n2 <- bg_add_node(handle, "test_kind", params = shuffled_params)
      fp2 <- bg_compute_fingerprint(handle, n2)

      expect_equal(fp1, fp2)
    })
  })

  it("produces distinct fingerprints when parameters differ", {
    skip_if_not_installed("hedgehog")
    library(hedgehog)

    gen_val <- gen.int(1000)

    forall(gen.list(gen_val, from = 2, to = 2), function(vals) {
      # Ensure they are actually different
      if (vals[[1]] == vals[[2]]) {
        return(TRUE)
      }

      tmp <- withr::local_tempdir()
      handle <- bg_init(path = tmp)
      bg_register_node_kind(handle, "test_kind")

      n1 <- bg_add_node(handle, "test_kind", params = list(x = vals[[1]]))
      n2 <- bg_add_node(handle, "test_kind", params = list(x = vals[[2]]))

      fp1 <- bg_compute_fingerprint(handle, n1)
      fp2 <- bg_compute_fingerprint(handle, n2)

      expect_false(fp1 == fp2)
    })
  })
})
