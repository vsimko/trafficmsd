test_that("model v1 exporter follows legacy emission positions", {
  x <- rep(c(0, 4, 0, 2), 8)
  d <- decompose_traffic(x)
  model <- export_model_v1(
    d,
    observed_sample_count = length(x),
    model_id = "r-golden-periodic-32",
    dataset_id = "synthetic-periodic-32"
  )

  expect_equal(model$coverage$observed_sample_count, length(x))
  expect_equal(model$coverage$decomposition_sample_count, length(x))
  expect_equal(model$coverage$padding_sample_count, 0L)
  expect_equal(model$extraction$cutoff_fraction, d$cutoff.fraction)

  residual <- Filter(function(component) component$scale_index == 0L,
                     model$components)
  expect_length(residual, length(x))
  expect_equal(
    vapply(residual, function(component) component$window_start_sample, integer(1)),
    0:(length(x) - 1L)
  )

  higher <- Filter(function(component) component$scale_index > 0L,
                   model$components)
  for (component in higher) {
    expect_equal(
      component$emission_sample_index,
      component$window_start_sample + component$window_length_samples / 2L - 1L
    )
    expect_equal(
      component$window_start_sample %% component$window_length_samples,
      0
    )
  }

  reconstructed <- numeric(length(x))
  for (component in model$components) {
    index <- component$emission_sample_index + 1L
    reconstructed[index] <- reconstructed[index] + component$amplitude
  }
  expect_equal(reconstructed, as.numeric(reconstruct_traffic(d)))
})

test_that("model v1 exporter records explicit unknown units and rejects invalid metadata", {
  d <- decompose_traffic(rep(c(0, 1), 16))
  model <- export_model_v1(
    d,
    observed_sample_count = 32,
    model_id = "unknown-units",
    dataset_id = "synthetic"
  )
  expect_identical(model$observation$time_unit, "unknown")
  expect_identical(model$observation$time_unit_status, "unknown")
  expect_null(model$observation$value_unit)
  expect_identical(model$observation$value_unit_status, "unknown")
  expect_error(
    export_model_v1(d, 32, "bad", "synthetic", cutoff_fraction = 1),
    "cutoff_fraction"
  )
  expect_error(
    export_model_v1(d, 32, "bad", "synthetic", value_unit = "bytes"),
    "verification statuses"
  )
})

test_that("model v1 exporter writes valid JSON when jsonlite is installed", {
  skip_if_not_installed("jsonlite")
  d <- decompose_traffic(rep(c(0, 3), 16))
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  export_model_v1(d, 32, "json-output", "synthetic", path = path)
  expected <- export_model_v1(d, 32, "json-output", "synthetic")
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(parsed$schema_version, "1.0")
  expect_identical(parsed$model_kind, "multiscale_reconstruction")
  expect_length(parsed$components, length(expected$components))
})

test_that("model v1 exporter distinguishes observations from zero padding", {
  observed <- rep(c(0, 5, 0, 1), 5)
  padded <- pad_power2(observed)
  d <- decompose_traffic(padded)
  model <- export_model_v1(
    d,
    observed_sample_count = length(observed),
    model_id = "padded-observation",
    dataset_id = "synthetic-padded"
  )

  expect_equal(model$coverage$observed_sample_count, length(observed))
  expect_equal(model$coverage$decomposition_sample_count, length(padded))
  expect_equal(model$coverage$padding_sample_count,
               length(padded) - length(observed))
  expect_length(
    Filter(function(component) component$scale_index == 0L,
           model$components),
    length(padded)
  )
})
