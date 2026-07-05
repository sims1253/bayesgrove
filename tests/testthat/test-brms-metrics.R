describe("bg_brms_nuts_metrics (brms backend)", {
  it("computes per-chain E-BFMI from energy__ and takes the min", {
    set.seed(1)
    # Chain 1: a slow random walk in energy -> low E-BFMI (well below 0.3).
    # Chain 2: i.i.d. normal energies -> E-BFMI near 2 (healthy).
    n <- 200
    e1 <- cumsum(rnorm(n, 0, 1))
    e2 <- rnorm(n, 0, 1)

    np <- data.frame(
      Chain = rep(c(1L, 2L), each = n),
      Iteration = rep(seq_len(n), 2),
      Parameter = factor(rep("energy__", 2 * n)),
      Value = c(e1, e2),
      stringsAsFactors = FALSE
    )

    m <- bayesgrove:::bg_brms_nuts_metrics(np)

    # Healthy chain's E-BFMI is large; random-walk chain's is tiny. The min
    # must come from the sick chain and land below the 0.3 severity threshold.
    expect_true(is.finite(m$e_bfmi))
    expect_lt(m$e_bfmi, 0.3)

    # Sanity: the two per-chain values straddle the threshold. Recompute them
    # directly to confirm the min is the sick chain's, not an accident.
    ebfmi <- function(e) sum(diff(e)^2) / ((length(e) - 1) * stats::var(e))
    c1 <- ebfmi(e1)
    c2 <- ebfmi(e2)
    expect_lt(c1, 0.3)
    expect_gt(c2, 0.3)
    expect_equal(m$e_bfmi, min(c(c1, c2)))

    # diff(E) is iteration-order sensitive: shuffling the input rows within each
    # chain must NOT change the result, because bg_brms_nuts_metrics sorts by
    # Chain then Iteration before splitting.
    shuffled_idx <- sample(seq_len(nrow(np)))
    np_shuffled <- np[shuffled_idx, , drop = FALSE]
    m_shuffled <- bayesgrove:::bg_brms_nuts_metrics(np_shuffled)
    expect_equal(m_shuffled$e_bfmi, m$e_bfmi)
  })

  it("parameterizes the treedepth threshold via max_treedepth", {
    np <- data.frame(
      Chain = rep(1L, 4L),
      Iteration = 1:4,
      Parameter = factor(rep("treedepth__", 4L)),
      Value = c(9, 10, 11, 12),
      stringsAsFactors = FALSE
    )

    # With the default cap of 10: values 10, 11, 12 hit -> 3 hits.
    m_default <- bayesgrove:::bg_brms_nuts_metrics(np)
    expect_equal(m_default$max_treedepth_hits, 3L)

    # With a raised cap of 12: only the single value of 12 hits -> 1 hit.
    m_raised <- bayesgrove:::bg_brms_nuts_metrics(np, max_treedepth = 12)
    expect_equal(m_raised$max_treedepth_hits, 1L)
  })

  it("counts divergences from the divergent__ sampler parameter", {
    np <- data.frame(
      Chain = c(1L, 1L, 2L),
      Iteration = 1:3,
      Parameter = factor(c("divergent__", "divergent__", "divergent__")),
      Value = c(0, 1, 0),
      stringsAsFactors = FALSE
    )
    m <- bayesgrove:::bg_brms_nuts_metrics(np)
    expect_equal(m$divergences, 1L)
  })

  it("returns zeroed metrics when nuts_params is NULL", {
    m <- bayesgrove:::bg_brms_nuts_metrics(NULL)
    expect_equal(m$divergences, 0L)
    expect_equal(m$max_treedepth_hits, 0L)
    expect_equal(m$e_bfmi, Inf)
  })
})

describe("bg_brms_build_priors", {
  skip_if_not_installed("brms")

  it("returns NULL when no priors are given", {
    expect_null(bayesgrove:::bg_brms_build_priors(NULL))
  })

  it("builds a single brmsprior from a character string", {
    p <- bayesgrove:::bg_brms_build_priors("student_t(3, 0, 5)")
    expect_true(inherits(p, "brmsprior"))
  })

  it("builds a single brmsprior from a named set_prior args list", {
    p <- bayesgrove:::bg_brms_build_priors(
      list(prior = "normal(0, 1)", class = "b")
    )
    expect_true(inherits(p, "brmsprior"))
    expect_equal(p$prior, "normal(0, 1)")
    expect_equal(p$class, "b")
  })

  it("combines multiple priors from an unnamed list of specs", {
    ps <- bayesgrove:::bg_brms_build_priors(list(
      list(prior = "normal(0, 1)", class = "b"),
      "student_t(3, 0, 5)"
    ))
    # brmsprior subclasses data.frame; length() is the column count, so use nrow.
    expect_s3_class(ps, "brmsprior")
    expect_equal(nrow(ps), 2)
    expect_equal(ps$class[[1]], "b")
    expect_equal(ps$prior[[2]], "student_t(3, 0, 5)")
  })

  it("passes an already-built brmsprior through unchanged", {
    original <- brms::set_prior("normal(0, 2)", class = "b")
    p <- bayesgrove:::bg_brms_build_priors(original)
    expect_identical(p, original)
  })
})
