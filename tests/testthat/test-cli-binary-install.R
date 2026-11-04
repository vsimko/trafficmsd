test_that("prebuilt CLI target matches supported host platforms", {
  target <- .trafficmsd_cli_target()
  expect_true(target %in% c(
    "x86_64-pc-windows-msvc",
    "x86_64-unknown-linux-gnu",
    "aarch64-unknown-linux-gnu",
    "x86_64-apple-darwin",
    "aarch64-apple-darwin"
  ))
})

test_that("CLI installer rejects invalid release selectors before network access", {
  expect_error(install_trafficmsd_cli(version = "../latest"), "version")
  expect_error(install_trafficmsd_cli(repo = "not-a-repository"), "owner/name")
})
