test_that("compute_emits uses globally aligned center-left positions", {
  n <- 32L
  max_scale <- 5L
  coef <- matrix(seq_len((max_scale + 1L) * n),
                 nrow = max_scale + 1L, ncol = n)
  emitted <- trafficmsd:::compute_emits(coef)

  expected <- matrix(0, nrow = max_scale + 1L, ncol = n)
  expected[1L, ] <- coef[1L, ]

  for (scale in seq_len(max_scale)) {
    window_length <- 2L^scale
    emission_indices <- seq.int(
      from = window_length / 2L,
      to = n,
      by = window_length
    )
    expected[scale + 1L, emission_indices] <-
      coef[scale + 1L, emission_indices]
  }

  expect_equal(emitted, expected)
})

test_that("compute_emits adds simultaneous emissions from different scales", {
  n <- 32L
  coef <- matrix(1, nrow = 6L, ncol = n)
  emitted <- trafficmsd:::compute_emits(coef)
  reconstructed <- colSums(emitted)

  # One-based sample 2 (zero-based 1): scales 0, 1, and 2.
  expect_equal(reconstructed[2L], 3)
  # One-based sample 4 (zero-based 3): scales 0, 1, and 3.
  expect_equal(reconstructed[4L], 3)
  # One-based sample 16 (zero-based 15): scales 0, 1, and 5.
  expect_equal(reconstructed[16L], 3)
  # One-based sample 32 (zero-based 31): scales 0 and 1.
  expect_equal(reconstructed[32L], 2)
})

test_that("compute_emits retains every scale-zero residual sample", {
  coef <- matrix(0, nrow = 6L, ncol = 32L)
  coef[1L, ] <- seq_len(32L) / 4
  emitted <- trafficmsd:::compute_emits(coef)

  expect_equal(emitted[1L, ], coef[1L, ])
  expect_equal(emitted[-1L, ], matrix(0, nrow = 5L, ncol = 32L))
})
