test_that("Rust CLI adapter validates input before launching the process", {
  expect_error(decompose_traffic_rust(rep(-1, 16)), "non-negative")
  expect_error(decompose_traffic_rust(rep(1, 17)), "power of two")
  expect_error(decompose_traffic_rust(rep(1, 16), sampling_interval = 0),
               "sampling_interval")
})

test_that("Rust CLI adapter reports a missing executable clearly", {
  old <- Sys.getenv("TRAFFICMSD_CLI", unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("TRAFFICMSD_CLI") else
      Sys.setenv(TRAFFICMSD_CLI = old)
  }, add = TRUE)
  Sys.setenv(TRAFFICMSD_CLI = file.path(tempdir(), "missing-trafficmsd"))
  expect_error(decompose_traffic_rust(rep(1, 16)),
               "does not name an existing executable")
})

test_that("Rust CLI adapter agrees with the R reference when a CLI is available", {
  cli <- Sys.getenv("TRAFFICMSD_CLI", unset = "")
  if (!nzchar(cli)) cli <- Sys.which("trafficmsd")
  skip_if(!nzchar(cli), "Rust CLI is not available on PATH or TRAFFICMSD_CLI")
  signal <- rep(c(0, 4, 0, 2), 8)
  rust <- decompose_traffic_rust(signal, value_unit = "bytes")
  reference <- decompose_traffic(signal)
  expect_equal(rust$coef, reference$coef, tolerance = 1e-12)
  expect_equal(reconstruct_traffic_rust(rust, value_unit = "bytes"),
               reconstruct_traffic(reference), tolerance = 1e-12)
})

test_that("R adapter rejects an incompatible CLI version with guidance", {
  skip_on_os("windows")
  old <- Sys.getenv("TRAFFICMSD_CLI", unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("TRAFFICMSD_CLI") else
      Sys.setenv(TRAFFICMSD_CLI = old)
  }, add = TRUE)
  fake_cli <- tempfile("trafficmsd-wrong-version-")
  writeLines(c("#!/bin/sh", "echo 'trafficmsd 99.99.99'"), fake_cli)
  Sys.chmod(fake_cli, mode = "0755")
  Sys.setenv(TRAFFICMSD_CLI = fake_cli)
  expect_error(
    decompose_traffic_rust(rep(1, 16)),
    "Incompatible TrafficMSD CLI"
  )
})
