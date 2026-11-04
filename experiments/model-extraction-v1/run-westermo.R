#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: run-westermo.R TRACE.csv MANIFEST.json OUTPUT_DIR", call. = FALSE)
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
decomposition <- trafficmsd::decompose_traffic(padded_signal, cutoff.fraction = 0.004)
reconstructed_padded <- as.numeric(trafficmsd::reconstruct_traffic(decomposition))
reconstructed <- reconstructed_padded[seq_len(observed_count)]
errors <- reconstructed - signal

model <- trafficmsd::export_model_v1(
  decomposition,
  observed_sample_count = observed_count,
  model_id = "westermo-right-captured-frame-bytes-v1",
  dataset_id = manifest$dataset_id,
  path = file.path(output_dir, "model-v1.json"),
  time_unit = "second",
  time_unit_status = "verified",
  sampling_interval = as.numeric(manifest$sampling_interval),
  time_origin = as.numeric(manifest$model_time_origin),
  value_unit = "byte",
  value_unit_status = "verified",
  cutoff_fraction = decomposition$cutoff.fraction,
  license_status = "verified",
  source_reference = manifest$source_reference,
  license_reference = manifest$license_reference,
  preprocessing = c(
    "Westermo right observation-point archive only",
    "all PCAP/PCAPNG members in the selected archive",
    "timestamp = frame.time_epoch; value = frame.cap_len",
    "sum captured frame lengths in left-closed/right-open fixed-width bins",
    "bin origin = first packet timestamp; empty bins = zero",
    "no deduplication; no application-payload interpretation"
  )
)

acf_at_lag <- function(x, lag) {
  centered <- x - mean(x)
  denominator <- sum(centered^2)
  if (denominator == 0) return(0)
  n <- length(centered)
  sum(centered[seq_len(n - lag)] * centered[(lag + 1L):n]) / denominator
}
quantile_list <- function(x) {
  values <- stats::quantile(x, probs = c(0.50, 0.90, 0.95, 0.99),
                            names = FALSE, type = 7)
  as.list(stats::setNames(as.numeric(values), c("p50", "p90", "p95", "p99")))
}
wasserstein_1 <- function(x, y) mean(abs(sort(x) - sort(y)))
ks_distance <- function(x, y) {
  points <- sort(unique(c(x, y)))
  max(abs(vapply(points, function(value) mean(x <= value), numeric(1)) -
          vapply(points, function(value) mean(y <= value), numeric(1))))
}
run_lengths <- function(x, threshold) {
  runs <- rle(x > threshold)
  as.integer(runs$lengths[runs$values])
}
optional_quantile <- function(x, probability) {
  if (length(x) == 0L) return(NULL)
  as.numeric(stats::quantile(x, probability, names = FALSE, type = 7))
}
optional_max <- function(x) if (length(x) == 0L) NULL else as.integer(max(x))

eligible_lags <- c(1L, 2L, 4L, 8L, 16L, 32L, 64L, 128L, 256L)
eligible_lags <- eligible_lags[eligible_lags < observed_count / 4]
acf_rows <- lapply(eligible_lags, function(lag) {
  source_acf <- acf_at_lag(signal, lag)
  reconstruction_acf <- acf_at_lag(reconstructed, lag)
  list(lag = as.integer(lag), source_acf = source_acf,
       reconstruction_acf = reconstruction_acf,
       absolute_difference = abs(source_acf - reconstruction_acf))
})
acf_differences <- vapply(acf_rows, function(row) row$absolute_difference, numeric(1))

threshold <- as.numeric(stats::quantile(signal, 0.95, names = FALSE, type = 7))
source_runs <- run_lengths(signal, threshold)
reconstruction_runs <- run_lengths(reconstructed, threshold)
source_volume <- sum(signal)
relative_l1 <- if (source_volume == 0) NULL else sum(abs(errors)) / source_volume
volume_error <- if (source_volume == 0) NULL else
  (sum(reconstructed) - source_volume) / source_volume
model_json <- jsonlite::toJSON(model, auto_unbox = TRUE, pretty = TRUE,
                               null = "null", digits = NA)
serialized_model_bytes <- nchar(as.character(model_json), type = "bytes")
components <- model$components
scale_zero_count <- sum(vapply(components, function(component) {
  identical(as.integer(component$scale_index), 0L)
}, logical(1)))

metrics <- list(
  fidelity = list(
    mae = mean(abs(errors)), rmse = sqrt(mean(errors^2)), bias = mean(errors),
    relative_l1_error = relative_l1, volume_error = volume_error
  ),
  distribution = list(
    source_mean = mean(signal), reconstruction_mean = mean(reconstructed),
    source_sd = stats::sd(signal), reconstruction_sd = stats::sd(reconstructed),
    source_quantiles = quantile_list(signal),
    reconstruction_quantiles = quantile_list(reconstructed),
    wasserstein_1 = wasserstein_1(signal, reconstructed),
    ks_distance = ks_distance(signal, reconstructed)
  ),
  temporal = list(
    acf_differences = acf_rows,
    mean_absolute_acf_difference = if (length(acf_differences) == 0L) NULL else
      mean(acf_differences)
  ),
  bursts = list(
    threshold = threshold,
    source_exceedance_fraction = mean(signal > threshold),
    reconstruction_exceedance_fraction = mean(reconstructed > threshold),
    source_run_count = length(source_runs),
    reconstruction_run_count = length(reconstruction_runs),
    source_median_run_length = optional_quantile(source_runs, 0.50),
    reconstruction_median_run_length = optional_quantile(reconstruction_runs, 0.50),
    source_p95_run_length = optional_quantile(source_runs, 0.95),
    reconstruction_p95_run_length = optional_quantile(reconstruction_runs, 0.95),
    source_max_run_length = optional_max(source_runs),
    reconstruction_max_run_length = optional_max(reconstruction_runs)
  ),
  compactness = list(
    component_count = length(components),
    scale_zero_residual_count = as.integer(scale_zero_count),
    serialized_model_bytes = as.integer(serialized_model_bytes),
    source_float64_bytes = as.integer(8L * observed_count),
    model_size_ratio = serialized_model_bytes / (8 * observed_count)
  )
)

commit <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[[1L]],
  error = function(error) "local-unknown"
)
result <- list(
  schema_version = "1.0",
  experiment_id = "westermo-right-model-extraction-v1",
  software = list(
    trafficmsd_version = as.character(utils::packageVersion("trafficmsd")),
    trafficmsd_commit = if (length(commit) == 1L && nchar(commit) >= 7L) commit else "local-unknown",
    model_schema_version = "1.0"
  ),
  dataset = list(
    dataset_id = manifest$dataset_id,
    source_reference = manifest$source_reference,
    license_status = "verified",
    source_files = list(list(file_id = manifest$archive,
                             sha256 = manifest$archive_sha256))
  ),
  preprocessing = list(
    quantity = "sum_of_packet_lengths",
    aggregation = "sum",
    sampling_interval = as.numeric(manifest$sampling_interval),
    time_unit = "second",
    value_unit = "captured_frame_bytes",
    bin_convention = "left_closed_right_open",
    selection_policy = "all packets from all PCAP members in the right observation-point archive only",
    preprocessing_version = "westermo-pcap-binning-v1"
  ),
  configuration = list(
    cutoff_fraction = decomposition$cutoff.fraction,
    evaluation_split = "full_trace_diagnostic",
    training_range = NULL,
    test_range = NULL,
    random_seed = NULL
  ),
  results = list(list(
    trace_id = "westermo-right-captured-frame-bytes-v1",
    status = "success",
    sample_count = observed_count,
    metrics = metrics
  )),
  summary = list(
    successful_traces = 1L,
    failed_traces = 0L,
    metric_medians = list(
      mae = metrics$fidelity$mae, rmse = metrics$fidelity$rmse,
      relative_l1_error = metrics$fidelity$relative_l1_error,
      volume_error = metrics$fidelity$volume_error,
      wasserstein_1 = metrics$distribution$wasserstein_1,
      mean_absolute_acf_difference = metrics$temporal$mean_absolute_acf_difference,
      model_size_ratio = metrics$compactness$model_size_ratio
    ),
    metric_iqrs = list(
      mae = 0, rmse = 0,
      relative_l1_error = if (is.null(relative_l1)) NULL else 0,
      volume_error = if (is.null(volume_error)) NULL else 0,
      wasserstein_1 = 0,
      mean_absolute_acf_difference = if (length(acf_differences) == 0L) NULL else 0,
      model_size_ratio = 0
    )
  )
)

jsonlite::write_json(result, file.path(output_dir, "evaluation-result-v1.json"),
                     auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
utils::write.csv(data.frame(sample_index = 0:(observed_count - 1L), value = signal),
                 file.path(output_dir, "source-signal.csv"), row.names = FALSE)
utils::write.csv(data.frame(sample_index = 0:(observed_count - 1L), value = reconstructed),
                 file.path(output_dir, "reconstructed-signal.csv"), row.names = FALSE)
message("Westermo model extraction completed: ", output_dir)
