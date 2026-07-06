describe("Fingerprint Properties (Developer Tests)", {
  it("computes the exact same fingerprint regardless of parameter order", {
    skip_if_not_installed("hedgehog")
    requireNamespace("hedgehog", quietly = TRUE)

    gen_params <- hedgehog::gen.list(
      hedgehog::gen.element(c(
        hedgehog::gen.int(100),
        hedgehog::gen.c(hedgehog::gen.c(hedgehog::gen.alpha())),
        hedgehog::gen.c(hedgehog::gen.int(10))
      )),
      from = 1,
      to = 5
    )

    gen_named_params <- hedgehog::gen.map(
      function(l) {
        names(l) <- paste0("param_", seq_along(l))
        l
      },
      gen_params
    )

    hedgehog::forall(gen_named_params, function(params) {
      tmp <- withr::local_tempdir()
      handle <- bg_init(path = tmp)
      bg_register_node_kind(handle, "test_kind")

      n1 <- bg_add_node(handle, "test_kind", params = params)
      fp1 <- bg_compute_fingerprint(handle, n1)

      shuffled_indices <- sample(seq_along(params))
      shuffled_params <- params[shuffled_indices]

      n2 <- bg_add_node(handle, "test_kind", params = shuffled_params)
      fp2 <- bg_compute_fingerprint(handle, n2)

      expect_equal(fp1, fp2)
    })
  })

  it("produces distinct fingerprints when parameters differ", {
    skip_if_not_installed("hedgehog")
    requireNamespace("hedgehog", quietly = TRUE)

    gen_val <- hedgehog::gen.int(1000)

    hedgehog::forall(
      hedgehog::gen.list(gen_val, from = 2, to = 2),
      function(vals) {
        if (vals[[1]] == vals[[2]]) {
          expect_true(TRUE)
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
      }
    )
  })
})
