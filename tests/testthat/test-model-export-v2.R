test_that("model v2 omits zero residuals without changing reconstruction", {
  x <- rep(c(0, 4, 0, 2), 8)
  d <- decompose_traffic(x)
  v1 <- export_model_v1(
    d, length(x), "sparse-v1-control", "synthetic"
  )
  v2 <- export_model_v2(
    d, length(x), "sparse-v2", "synthetic"
  )

  expect_identical(v2$schema_version, "2.0")
  expect_true(all(vapply(v2$components, function(component) {
    component$scale_index != 0L || component$amplitude != 0
  }, logical(1))))

  reconstruct_components <- function(model) {
    out <- numeric(model$coverage$decomposition_sample_count)
    for (component in model$components) {
      index <- component$emission_sample_index + 1L
      out[index] <- out[index] + component$amplitude
    }
    out
  }
  expect_equal(reconstruct_components(v2), reconstruct_components(v1))
  expect_lt(length(v2$components), length(v1$components))
})

test_that("model v2 retains non-zero residuals and higher-scale components", {
  x <- pad_power2(c(0, 3, rep(0, 30), 2))
  d <- decompose_traffic(x)

  # The chosen signal can be represented entirely by higher-scale emissions.
  # Inject one controlled residual coefficient to exercise v2's non-zero
  # scale-zero preservation contract independently of that data-dependent fit.
  d$coef[1L, 1L] <- d$coef[1L, 1L] + 1

  v1 <- export_model_v1(d, length(x), "v1", "synthetic")
  v2 <- export_model_v2(d, length(x), "v2", "synthetic")
  v1_reconstruction <- numeric(d$length)
  v2_reconstruction <- numeric(d$length)
  for (component in v1$components) {
    i <- component$emission_sample_index + 1L
    v1_reconstruction[i] <- v1_reconstruction[i] + component$amplitude
  }
  for (component in v2$components) {
    i <- component$emission_sample_index + 1L
    v2_reconstruction[i] <- v2_reconstruction[i] + component$amplitude
  }
  expect_equal(v2_reconstruction, v1_reconstruction)
  expect_true(any(vapply(v2$components, function(component) {
    component$scale_index == 0L && component$amplitude > 0
  }, logical(1))))
  expect_true(any(vapply(v2$components, function(component) {
    component$scale_index > 0L
  }, logical(1))))
})

test_that("model v2 writes versioned sparse JSON", {
  skip_if_not_installed("jsonlite")
  d <- decompose_traffic(rep(c(0, 3), 16))
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  model <- export_model_v2(
    d, 32, "sparse-json", "synthetic", path = path
  )
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(parsed$schema_version, "2.0")
  expect_length(parsed$components, length(model$components))
})
