test_that("interval counting APIs handle valid and invalid dimensions", {
  expect_equal(get_interval_count(numeric()), 0L)
  expect_equal(get_interval_count(c(0, 1, 1, 0, 2, 0, 2)), 3)
  expect_equal(get_total_intervals(matrix(c(0, 1, 1, 0, 2, 0, 2, 0), nrow = 2)), 3)
  expect_error(get_total_intervals(matrix(1:4, nrow = 1)))
  expect_error(get_total_intervals(matrix(numeric(), nrow = 2, ncol = 0)))
})

test_that("decomposition and padding enforce documented boundaries", {
  expect_error(pad_power2(1:15), "at least 16")
  expect_error(decompose_traffic(1:15), "at least 16")
  expect_error(decompose_traffic(rep(1, 16), cutoff.fraction = 0), "cutoff.fraction")
  expect_error(decompose_traffic(rep(1, 16), cutoff.fraction = 1), "cutoff.fraction")
  expect_equal(pad_power2(1:32), 1:32)
})

test_that("clustering validates random initialization controls", {
  signal <- rep(seq_len(8), each = 8)
  expect_error(cluster_by_amplitude(signal, 3, seed = -1), "seed")
  expect_error(cluster_by_amplitude(signal, 3, nstart = 0), "nstart")
  expect_error(estimate_clusters(signal, seed = -1), "seed")
  expect_error(estimate_clusters(signal, nstart = 0), "nstart")
})

test_that("CSV reader rejects missing schema and invalid options", {
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)

  write.csv(data.frame(timestamp = 1:16, bytes = 1:16), file, row.names = FALSE)
  expect_error(read_traffic(file), "Time and Length")

  write.csv(data.frame(Time = 1:16, Length = 1:16), file, row.names = FALSE)
  expect_error(read_traffic(file, use.power2len = NA), "use.power2len")
  expect_error(read_traffic(file, use.expfactor = 0), "use.expfactor")
  expect_error(read_traffic(file, aggregation_function = 1), "aggregation_function")
})

test_that("reconstruction accepts both documented input forms", {
  decomposition <- decompose_traffic(sample32)
  expect_equal(
    reconstruct_traffic(decomposition),
    reconstruct_traffic(decomposition$coef)
  )
})
