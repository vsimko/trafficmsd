#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: analyze-cutoff-sensitivity.R TRACE.csv MANIFEST.json OUTPUT_DIR",
       call. = FALSE)
}
trace_path <- args[[1L]]
manifest_path <- args[[2L]]
output_dir <- args[[3L]]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (package in c("trafficmsd", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required package is not installed: ", package, call. = FALSE)
  }
}

manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
trace <- utils::read.csv(trace_path, stringsAsFactors = FALSE)
if (!identical(names(trace), c("sample_index", "value")) ||
    nrow(trace) < 16L || any(!is.finite(trace$value)) ||
    any(trace$value < 0) ||
    !identical(as.integer(trace$sample_index), seq.int(0L, nrow(trace) - 1L))) {
  stop("Trace CSV must contain at least 16 finite, non-negative, contiguous samples.",
       call. = FALSE)
}

signal <- as.numeric(trace$value)
observed_count <- length(signal)
padded_signal <- trafficmsd::pad_power2(signal)
cutoffs <- c(0.0001, 0.00025, 0.0005, 0.001, 0.002, 0.004, 0.008, 0.016)
threshold <- as.numeric(stats::quantile(signal, 0.95, names = FALSE, type = 7))

fidelity_metrics <- function(source, estimate) {
  difference <- estimate - source
  source_total <- sum(source)
  list(
    mae = mean(abs(difference)),
    rmse = sqrt(mean(difference^2)),
    bias = mean(difference),
    relative_l1_error = if (source_total == 0) NULL else
      sum(abs(difference)) / source_total,
    volume_error = if (source_total == 0) NULL else
      (sum(estimate) - source_total) / source_total
  )
}

acf_at_lag <- function(x, lag) {
  centered <- x - mean(x)
  denominator <- sum(centered^2)
  if (denominator == 0) return(NA_real_)
  n <- length(centered)
  sum(centered[seq_len(n - lag)] * centered[(lag + 1L):n]) / denominator
}

run_lengths <- function(x, fixed_threshold) {
  runs <- rle(x > fixed_threshold)
  as.integer(runs$lengths[runs$values])
}

constant_mean <- rep(mean(signal), observed_count)
constant_mean_metrics <- fidelity_metrics(signal, constant_mean)
eligible_lags <- c(1L, 2L, 4L, 8L, 16L, 32L, 64L, 128L, 256L)
eligible_lags <- eligible_lags[eligible_lags < observed_count / 4]
source_acf <- vapply(eligible_lags, function(lag) acf_at_lag(signal, lag), numeric(1))
source_runs <- run_lengths(signal, threshold)

rows <- lapply(cutoffs, function(cutoff) {
  decomposition <- trafficmsd::decompose_traffic(
    padded_signal, cutoff.fraction = cutoff
  )
  reconstructed_padded <- as.numeric(
    trafficmsd::reconstruct_traffic(decomposition)
  )
  reconstructed <- reconstructed_padded[seq_len(observed_count)]
  metrics <- fidelity_metrics(signal, reconstructed)
  reconstruction_acf <- vapply(
    eligible_lags, function(lag) acf_at_lag(reconstructed, lag), numeric(1)
  )
  acf_differences <- abs(source_acf - reconstruction_acf)
  reconstruction_runs <- run_lengths(reconstructed, threshold)

  model_path <- tempfile(fileext = ".json")
  model <- trafficmsd::export_model_v1(
    decomposition,
    observed_sample_count = observed_count,
    model_id = paste0("westermo-right-cutoff-", format(cutoff, scientific = FALSE)),
    dataset_id = manifest$dataset_id,
    path = model_path,
    time_unit = "second",
    time_unit_status = "verified",
    sampling_interval = as.numeric(manifest$sampling_interval),
    time_origin = 0,
    value_unit = "byte",
    value_unit_status = "verified",
    cutoff_fraction = cutoff,
    license_status = "verified",
    source_reference = manifest$source_reference,
    license_reference = manifest$license_reference,
    preprocessing = c(
      "Westermo right observation-point archive only",
      "timestamp = frame.time_epoch; value = frame.cap_len",
      "sum captured frame lengths in left-closed/right-open fixed-width bins",
      "bin origin = first packet timestamp; empty bins = zero",
      "no deduplication; no application-payload interpretation"
    )
  )
  model_bytes <- as.numeric(file.info(model_path)$size)
  unlink(model_path)
  scale_zero_count <- sum(vapply(model$components, function(component) {
    identical(as.integer(component$scale_index), 0L)
  }, logical(1)))

  data.frame(
    cutoff_fraction = cutoff,
    mae = metrics$mae,
    rmse = metrics$rmse,
    bias = metrics$bias,
    relative_l1_error = metrics$relative_l1_error,
    volume_error = metrics$volume_error,
    mean_absolute_acf_difference = if (all(is.na(acf_differences))) {
      NA_real_
    } else {
      mean(acf_differences, na.rm = TRUE)
    },
    source_burst_run_count = length(source_runs),
    reconstruction_burst_run_count = length(reconstruction_runs),
    source_burst_max_run = if (length(source_runs)) max(source_runs) else NA_integer_,
    reconstruction_burst_max_run = if (length(reconstruction_runs)) {
      max(reconstruction_runs)
    } else {
      NA_integer_
    },
    component_count = length(model$components),
    scale_zero_residual_count = scale_zero_count,
    serialized_model_bytes = model_bytes,
    model_size_ratio = model_bytes / (8 * observed_count)
  )
})

results <- do.call(rbind, rows)
utils::write.csv(results, file.path(output_dir, "cutoff-sensitivity.csv"),
                 row.names = FALSE, na = "")
report <- list(
  schema_version = "1.0",
  experiment_id = "westermo-right-cutoff-sensitivity-v1",
  evaluation_scope = "full_trace_diagnostic",
  dataset_id = manifest$dataset_id,
  dataset_release = manifest$dataset_release,
  archive_sha256 = manifest$archive_sha256,
  trace_sha256 = manifest$trace_sha256,
  observation_point = manifest$observation_point,
  packet_count = manifest$packet_count,
  sample_count = observed_count,
  sampling_interval = as.numeric(manifest$sampling_interval),
  value_unit = "captured_frame_bytes",
  cutoff_fractions = as.list(cutoffs),
  constant_mean_baseline = constant_mean_metrics,
  source_burst_threshold = threshold,
  source_burst_run_count = length(source_runs),
  source_burst_max_run = if (length(source_runs)) max(source_runs) else NULL,
  results = lapply(seq_len(nrow(results)), function(index) {
    as.list(results[index, , drop = FALSE])
  }),
  interpretation = paste(
    "All cutoffs are compared on the same full trace for exploratory sensitivity only.",
    "This is not a held-out estimate and must not be used to select a final cutoff",
    "for a test set. The constant-mean baseline uses the full-trace mean."
  )
)
jsonlite::write_json(report, file.path(output_dir, "cutoff-sensitivity.json"),
                     auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
message("Cutoff sensitivity analysis completed: ", output_dir)
