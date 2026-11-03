test_that("estimate_clusters handles constant and low-cardinality data", {
  expect_equal(estimate_clusters(rep(0, 64)), 1)
  expect_equal(estimate_clusters(rep(3.5, 64)), 1)
  expect_equal(estimate_clusters(rep(c(1, 2), 32)), 1)
})

test_that("estimate_clusters validates arguments and data", {
  expect_error(estimate_clusters(c(1, NA_real_)))
  expect_error(estimate_clusters(c(1, Inf)))
  expect_error(estimate_clusters(1:64, max.cl = 2))
  expect_error(estimate_clusters(1:64, max.cl = 30))
  expect_error(estimate_clusters(1:64, cutoff = 0))
  expect_error(estimate_clusters(1:64, cutoff = 1))
  expect_error(estimate_clusters(1:64, nstart = 0))
})

test_that("cluster_by_amplitude is reproducible and preserves RNG state", {
  x <- rep(seq_len(8), each = 8)
  set.seed(123)
  caller_seed <- .Random.seed

  fit1 <- cluster_by_amplitude(x, num.cl = 3, seed = 42, nstart = 10)
  expect_identical(.Random.seed, caller_seed)
  fit2 <- cluster_by_amplitude(x, num.cl = 3, seed = 42, nstart = 10)

  expect_equal(fit1$cfit$cluster, fit2$cfit$cluster)
  expect_equal(fit1$cfit$centers, fit2$cfit$centers)
  expect_equal(fit1$num.cl, 3)
  expect_length(fit1$thdata, length(x))
  expect_length(fit1$abserrors, length(x))
  expect_gte(fit1$MAE, 0)
})

test_that("cluster_by_amplitude rejects invalid arguments", {
  expect_error(cluster_by_amplitude(1:10, num.cl = 0))
  expect_error(cluster_by_amplitude(1:10, num.cl = 2.5))
  expect_error(cluster_by_amplitude(c(1, NA_real_), num.cl = 1))
  expect_error(cluster_by_amplitude(1:10, num.cl = 2, nstart = 0))
})
