test_that("pad_power2 pads without changing existing observations", {
  x <- seq(1, 17) + 0.25
  padded <- pad_power2(x)
  expect_length(padded, 32)
  expect_equal(padded[seq_along(x)], x)
  expect_equal(tail(padded, 15), rep(0, 15))
  expect_equal(pad_power2(1:16), 1:16)
})

test_that("pad_power2 rejects invalid signals", {
  expect_error(pad_power2(1:10))
  expect_error(pad_power2(c(rep(1, 15), NA_real_)))
  expect_error(pad_power2(c(rep(1, 15), Inf)))
  expect_error(pad_power2(letters[1:16]))
})

test_that("decompose_traffic validates signal shape and values", {
  expect_error(decompose_traffic(1:15))
  expect_error(decompose_traffic(1:17), "power of two")
  expect_error(decompose_traffic(c(rep(1, 15), NA_real_)), "finite")
  expect_error(decompose_traffic(c(rep(1, 15), Inf)), "finite")
  expect_error(decompose_traffic(1:32, cutoff.fraction = 0))
  expect_error(decompose_traffic(1:32, cutoff.fraction = 1))
})

test_that("decomposition retains fractional numeric input", {
  x <- rep(1.25, 32)
  d <- decompose_traffic(x)
  expect_type(d$coef, "double")
  expect_equal(d$length, length(x))
  expect_equal(ncol(d$coef), length(x))
  expect_true(all(is.finite(d$coef)))
})

test_that("read_traffic validates CSV schema and options", {
  valid <- tempfile(fileext = ".csv")
  invalid <- tempfile(fileext = ".csv")
  on.exit(unlink(c(valid, invalid)))

  utils::write.csv(data.frame(Time = 1:17, Length = seq_len(17)), valid,
                   row.names = FALSE)
  utils::write.csv(data.frame(timestamp = 1:17, bytes = seq_len(17)), invalid,
                   row.names = FALSE)

  expect_length(read_traffic(valid, use.power2len = FALSE), 17)
  expect_length(read_traffic(valid), 32)
  expect_error(read_traffic(invalid), "Time and Length")
  expect_error(read_traffic(valid, use.expfactor = 0))
  expect_error(read_traffic(valid, use.power2len = NA))
})
