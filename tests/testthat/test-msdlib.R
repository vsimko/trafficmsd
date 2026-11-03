context("plotting")
suppressMessages(library(dplyr))

pdf(NULL) # suppress generating any PDFs

test_that("Padding signal to power of two", {
   expect_equal( pad_power2(1:16) , 1:16 )
   expect_equal( pad_power2(1:17), c(1:17, rep(0, 15)) )
})

test_that("Plotting of spectrum", {
  1:32 %>% decompose_traffic %>% plot_spectrum
})

test_that("Plotting of traffic", {
  plot_traffic(1:32)
  plot_traffic(1:32, 2:33)
})

test_that("Plotting of scaleogram", {
  1:32 %>% plot_scaleogram
})

test_that("Reading traffic from CSV file", {

  LEN <- 17
  LEN2 <- 32
  FNAME <- "testsample.csv"

  write.csv(
    data.frame(Time = 1:LEN, Length = sample(LEN)),
    file = FNAME )

  FNAME %>% read_traffic(use.power2len = FALSE) -> ts
  FNAME %>% read_traffic -> ts2
  FNAME %>% unlink

  expect_equal(length(ts), LEN)
  expect_equal(length(ts2), LEN2)
})

test_that("Estimating number of clusters", {

  sample(100) %>% rep(10) -> data

  data %>% estimate_clusters -> E

  expect_true(E < 11)
  expect_true(E > 4)

  expect_error(data %>% estimate_clusters(max.cl = -1))
  expect_error(data %>% estimate_clusters(max.cl =  0))
  expect_error(data %>% estimate_clusters(max.cl =  1))
  expect_error(data %>% estimate_clusters(max.cl =  2))
  expect_error(data %>% estimate_clusters(max.cl = 30))
  expect_error(data %>% estimate_clusters(cutoff = -1))
  expect_error(data %>% estimate_clusters(cutoff =  0))

  data %>% estimate_clusters(max.cl = 29) %>% expect_gt(2)
})

test_that("Clustering by amplitude", {
  sample32 %>% cluster_by_amplitude(num.cl = 4) -> cl
  expect_equal(cl$num.cl, 4)
  expect_equal(cl$data, sample32)
  expect_equal(length(cl$thdata), length(sample32))
  expect_equal(length(cl$abserrors), length(sample32))
  expect_true(is.numeric(cl$MAE))
})

test_that("Model size", {
  sample32 %>% decompose_traffic %>% .$coef %>%
    get_total_intervals %>% expect_equal(6)

  pad_power2(sample(100)) %>% decompose_traffic %>% .$coef %>%
    get_total_intervals %>% expect_gt(10)
})

test_that("Reconstruction", {

  # preparing
  sample32 %>% decompose_traffic %>% reconstruct_traffic -> out
  sample32 %>% sum -> s1 # sum of original signal
  out %>% sum -> s2 # sum of reconstructed signal

  # testing
  expect_equal(length(sample32), length(out))
  expect_lt(abs(s1 - s2) / s1, .2)
})

test_that("print_stats", {
  cmd <- function() {
    sample32 %>% decompose_traffic %>% print_stats
  }
  expect_output(cmd(), "Total number of intervals: *6")
})

test_that("plot_traffic_densities", {
  plot_traffic_densities(list(sample_regsignal, sample_vidstream))
})


test_that("Signal validation and padding preserve numeric data", {
  expect_error(decompose_traffic(1:17), "power of two")
  expect_error(decompose_traffic(c(rep(1, 15), NA_real_)), "finite")
  expect_error(decompose_traffic(c(rep(1, 15), Inf)), "finite")
  expect_equal(pad_power2(rep(1.25, 17)), c(rep(1.25, 17), rep(0, 15)))
  expect_equal(sum(decompose_traffic(rep(1.25, 16))$coef), 20)
})

test_that("Cluster estimation handles degenerate signals", {
  expect_equal(estimate_clusters(rep(0, 64)), 1)
  expect_equal(estimate_clusters(rep(3.5, 64)), 1)
  expect_error(estimate_clusters(c(1, NA_real_)), "finite")
  expect_error(estimate_clusters(rep(1, 64), max.cl = 2))
})

test_that("Clustering is reproducible and does not change caller RNG state", {
  set.seed(123)
  before <- .Random.seed
  a <- cluster_by_amplitude(rep(1:8, each = 8), 3, seed = 42)
  expect_identical(.Random.seed, before)
  b <- cluster_by_amplitude(rep(1:8, each = 8), 3, seed = 42)
  expect_equal(a$cfit$cluster, b$cfit$cluster)
  expect_equal(a$cfit$centers, b$cfit$centers)
  expect_error(cluster_by_amplitude(1:10, 0))
  expect_error(cluster_by_amplitude(1:10, 2.5))
})

test_that("Decomposition preserves fractional input precision", {
  signal <- rep(1.25, 32)
  result <- decompose_traffic(signal)
  expect_type(result$coef, "double")
  expect_equal(sum(result$coef), sum(signal), tolerance = 1e-10)
})
