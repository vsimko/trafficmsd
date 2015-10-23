library(testthat)
library(assertthat)
library(scales)
library(reshape2)
suppressMessages(library(fields))
suppressMessages(library(zoo))
suppressMessages(library(biwavelet))
suppressMessages(library(dplyr))

pdf(NULL)

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
  
  LEN = 17
  LEN2 = 32
  FNAME = 'testsample.csv'
  
  write.csv(
    data.frame(Time=1:LEN, Length=sample(LEN)),
    file=FNAME )
  
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
  
  data %>% estimate_clusters(max.cl = 29) %>% expect_more_than(2)
  
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
  sample32 %>% decompose_traffic %>% .$coef %>% get_total_intervals %>% expect_equal(6)
  sample(100) %>% decompose_traffic %>% .$coef %>% get_total_intervals %>% expect_more_than(10)
})

test_that("Reconstruction", {
  
  # preparing
  sample32 %>% decompose_traffic %>% reconstruct_traffic -> out
  sample32 %>% sum -> s1 # sum of original signal
  out %>% sum -> s2 # sum of reconstructed signal
  
  # testing
  expect_equal(length(sample32), length(out))
  expect_less_than( abs(s1 - s2) / s1, .2)
  
})
