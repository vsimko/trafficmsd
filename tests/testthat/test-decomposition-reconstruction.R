test_that("decomposition returns a correctly shaped finite coefficient matrix", {
  fixtures <- list(
    constant = rep(2, 32),
    impulse = c(1, rep(0, 31)),
    alternating = rep(c(0, 2), 16),
    sparse = c(rep(0, 10), 3, rep(0, 21))
  )

  for (x in fixtures) {
    d <- decompose_traffic(x)
    expect_equal(d$length, length(x))
    expect_equal(ncol(d$coef), length(x))
    expect_equal(nrow(d$coef), d$maxscale + 1)
    expect_true(all(is.finite(d$coef)))
  }
})

test_that("reconstruction returns finite output with original dimensions", {
  x <- rep(c(0, 4, 0, 2), 8)
  d <- decompose_traffic(x)
  reconstructed <- reconstruct_traffic(d)

  expect_length(reconstructed, length(x))
  expect_true(all(is.finite(reconstructed)))
  expect_lt(abs(sum(reconstructed) - sum(x)) / sum(x), 0.35)
})

test_that("interval counting handles empty and separated runs", {
  expect_equal(get_interval_count(numeric()), 0)
  expect_equal(get_interval_count(c(0, 1, 1, 0, 2, 2, 0, 3)), 3)
  expect_equal(get_interval_count(rep(0, 8)), 0)
})

test_that("total interval count validates and counts coefficient matrices", {
  x <- rep(c(0, 4, 0, 2), 8)
  coefficients <- decompose_traffic(x)$coef
  expect_gte(get_total_intervals(coefficients), 0)
  expect_error(get_total_intervals(matrix(1, nrow = 1, ncol = 8)))
})
