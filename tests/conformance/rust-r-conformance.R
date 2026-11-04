#!/usr/bin/env Rscript

# Cross-language conformance checks for deterministic decomposition and
# reconstruction. The fixtures are synthetic and do not replace measured-trace
# validation or the provenance requirements in issue #41.

args <- commandArgs(trailingOnly = TRUE)
cli <- if (length(args) >= 1L) args[[1L]] else file.path("target", "release", "trafficmsd")
if (!file.exists(cli)) stop("TrafficMSD CLI binary not found: ", cli)
if (!requireNamespace("trafficmsd", quietly = TRUE)) stop("Install trafficmsd first.")
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first.")

cli <- normalizePath(cli, mustWork = TRUE)
work <- tempfile("trafficmsd-rust-r-conformance-")
dir.create(work)

signals <- list(
  constant_16 = rep(8, 16),
  zero_32 = rep(0, 32),
  alternating_32 = rep(c(0, 2), 16),
  monotonic_32 = as.numeric(seq_len(32)),
  sparse_32 = c(rep(0, 10), 3, rep(0, 21)),
  periodic_32 = rep(c(0, 4, 0, 2), 8)
)
cutoffs <- c(0.004, 0.15)
tolerance <- 1e-12
checks <- 0L

write_config <- function(path, command, input, output = NULL, cutoff = NULL) {
  config <- list(
    schema_version = 1L,
    command = command,
    input_csv = input,
    signal = list(
      sampling_interval = 1,
      time_unit = "s",
      value_unit = "bytes"
    ),
    parameters = list(cutoff_fraction = cutoff)
  )
  if (!is.null(output)) config$output_csv <- output
  jsonlite::write_json(config, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

run_cli <- function(command, config_path) {
  status <- system2(cli, c(command, "--config", shQuote(config_path)))
  if (is.null(status)) 0L else as.integer(status)
}

compare_numeric <- function(reference, candidate, label) {
  if (length(reference) != length(candidate)) {
    stop(label, ": length mismatch (", length(reference), " vs ",
         length(candidate), ").")
  }
  difference <- max(abs(as.numeric(reference) - as.numeric(candidate)))
  scale <- max(1, max(abs(as.numeric(reference))))
  if (!is.finite(difference) || difference > tolerance * scale) {
    stop(label, ": max absolute difference ", format(difference),
         " exceeds relative tolerance ", tolerance, ".")
  }
  invisible(difference)
}

for (signal_name in names(signals)) {
  signal <- signals[[signal_name]]
  input_path <- file.path(work, paste0(signal_name, "-input.csv"))
  write.csv(
    data.frame(sample_index = 0:(length(signal) - 1L), value = signal),
    input_path,
    row.names = FALSE
  )

  for (cutoff in cutoffs) {
    case_id <- paste0(signal_name, "-cutoff-", gsub("\\.", "_", as.character(cutoff)))
    coefficient_path <- file.path(work, paste0(case_id, "-coefficients.csv"))
    config_path <- file.path(work, paste0(case_id, "-decompose.json"))
    write_config(
      config_path,
      "decompose",
      basename(input_path),
      basename(coefficient_path),
      cutoff
    )

    reference <- trafficmsd::decompose_traffic(signal, cutoff.fraction = cutoff)
    status <- run_cli("decompose", config_path)
    if (status != 0L) stop(case_id, ": CLI decompose exited with code ", status, ".")

    table <- read.csv(coefficient_path, check.names = FALSE)
    expected_rows <- nrow(reference$coef) * ncol(reference$coef)
    if (nrow(table) != expected_rows) {
      stop(case_id, ": coefficient table has ", nrow(table),
           " rows; expected ", expected_rows, ".")
    }
    actual <- matrix(NA_real_, nrow = nrow(reference$coef), ncol = ncol(reference$coef))
    actual[cbind(table$scale + 1L, table$sample_index + 1L)] <- table$coefficient
    if (anyNA(actual)) stop(case_id, ": coefficient table is incomplete.")
    compare_numeric(reference$coef, actual, paste(case_id, "decomposition"))
    checks <- checks + 1L

    repeated_path <- file.path(work, paste0(case_id, "-coefficients-repeat.csv"))
    repeated_config <- file.path(work, paste0(case_id, "-repeat.json"))
    write_config(
      repeated_config,
      "decompose",
      basename(input_path),
      basename(repeated_path),
      cutoff
    )
    status <- run_cli("decompose", repeated_config)
    if (status != 0L) stop(case_id, ": repeated CLI decompose failed.")
    first_bytes <- readBin(coefficient_path, what = "raw", n = file.info(coefficient_path)$size)
    repeat_bytes <- readBin(repeated_path, what = "raw", n = file.info(repeated_path)$size)
    if (!identical(first_bytes, repeat_bytes)) {
      stop(case_id, ": fixed-input decomposition is not byte-for-byte deterministic.")
    }
    checks <- checks + 1L

    reconstructed_path <- file.path(work, paste0(case_id, "-reconstructed.csv"))
    reconstruct_config <- file.path(work, paste0(case_id, "-reconstruct.json"))
    write_config(
      reconstruct_config,
      "reconstruct",
      basename(coefficient_path),
      basename(reconstructed_path)
    )
    status <- run_cli("reconstruct", reconstruct_config)
    if (status != 0L) stop(case_id, ": CLI reconstruct exited with code ", status, ".")
    reconstructed_table <- read.csv(reconstructed_path, check.names = FALSE)
    if (!identical(as.integer(reconstructed_table$sample_index),
                   as.integer(0:(length(signal) - 1L)))) {
      stop(case_id, ": reconstructed sample indices are not contiguous.")
    }
    reference_reconstruction <- trafficmsd::reconstruct_traffic(reference)
    compare_numeric(
      reference_reconstruction,
      reconstructed_table$value,
      paste(case_id, "reconstruction")
    )
    checks <- checks + 1L
  }
}

# Traffic volume is non-negative by domain definition. The legacy R API
# historically accepted negative values, while the Rust core rejects them.
# This is an explicit validation tightening, not a numerical conformance case.
negative <- rep(1, 16)
negative[[1L]] <- -1
negative_path <- file.path(work, "negative.csv")
negative_config <- file.path(work, "negative.json")
write.csv(
  data.frame(sample_index = 0:15, value = negative),
  negative_path,
  row.names = FALSE
)
write_config(negative_config, "decompose", basename(negative_path),
             "negative-coefficients.csv", 0.004)
status <- run_cli("decompose", negative_config)
if (status != 3L) {
  stop("negative traffic input must be rejected with exit code 3; got ", status, ".")
}
checks <- checks + 1L

cat("R/Rust conformance passed: ", checks,
    " checks; relative tolerance ", tolerance, ".\n", sep = "")
