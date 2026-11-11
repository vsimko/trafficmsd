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

test_that("compute_emits preserves disjoint higher-scale emission phases", {
  n <- 32L
  coef <- matrix(1, nrow = 6L, ncol = n)
  emitted <- trafficmsd:::compute_emits(coef)
  reconstructed <- colSums(emitted)

  # Zero-based sample 1 is emitted by scales 0 and 2.
  expect_equal(reconstructed[2L], 2)
  # Zero-based sample 3 is emitted by scales 0 and 3.
  expect_equal(reconstructed[4L], 2)
  # Zero-based sample 15 is emitted by scales 0 and 5.
  expect_equal(reconstructed[16L], 2)
  # Zero-based sample 31 is emitted only by scale 0.
  expect_equal(reconstructed[32L], 1)
})

test_that("compute_emits retains every scale-zero residual sample", {
  coef <- matrix(0, nrow = 6L, ncol = 32L)
  coef[1L, ] <- seq_len(32L) / 4
  emitted <- trafficmsd:::compute_emits(coef)

  expect_equal(emitted[1L, ], coef[1L, ])
  expect_equal(emitted[-1L, ], matrix(0, nrow = 5L, ncol = 32L))
})
