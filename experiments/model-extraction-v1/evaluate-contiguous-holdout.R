#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: evaluate-contiguous-holdout.R TRACE.csv MANIFEST.json OUTPUT_DIR",
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
    nrow(trace) < 80L || any(!is.finite(trace$value)) ||
    any(trace$value < 0) ||
    !identical(as.integer(trace$sample_index), seq.int(0L, nrow(trace) - 1L))) {
  stop("Trace CSV must contain at least 80 finite, non-negative, contiguous samples.",
       call. = FALSE)
}

signal <- as.numeric(trace$value)
n <- length(signal)

# Freeze the split before evaluating any cutoff: first 60% train, next 20%
# validation, final 20% test. All ranges are zero-based and half-open.
train_end <- floor(0.60 * n)
validation_end <- floor(0.80 * n)
train <- signal[seq_len(train_end)]
validation <- signal[(train_end + 1L):validation_end]
test <- signal[(validation_end + 1L):n]
if (min(length(train), length(validation), length(test)) < 16L) {
  stop("Each contiguous split must contain at least 16 samples.", call. = FALSE)
}

training_mean <- mean(train)
burst_threshold <- as.numeric(stats::quantile(
  train, 0.95, names = FALSE, type = 7
))
cutoffs <- c(0.0001, 0.00025, 0.0005, 0.001, 0.002, 0.004, 0.008, 0.016)

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
  if (denominator == 0) return(0)
  sum(centered[seq_len(length(x) - lag)] *
        centered[(lag + 1L):length(x)]) / denominator
}

run_lengths <- function(x, threshold) {
  runs <- rle(x > threshold)
  as.integer(runs$lengths[runs$values])
}

optional_max <- function(x) {
  if (length(x) == 0L) NULL else as.integer(max(x))
}

evaluate_segment <- function(source, cutoff, segment_name) {
  padded <- trafficmsd::pad_power2(source)
  decomposition <- trafficmsd::decompose_traffic(
    padded, cutoff.fraction = cutoff
  )
  reconstructed <- as.numeric(
    trafficmsd::reconstruct_traffic(decomposition)
  )[seq_len(length(source))]
  model_path <- tempfile(fileext = ".json")
  on.exit(unlink(model_path), add = TRUE)
  model <- trafficmsd::export_model_v1(
    decomposition,
    observed_sample_count = length(source),
    model_id = paste0("westermo-right-", segment_name, "-cutoff-",
                      format(cutoff, scientific = FALSE)),
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
      "one-second left-closed/right-open captured-frame-byte bins",
      "contiguous segment independently padded to a power of two",
      "no application-payload interpretation"
    )
  )

  lags <- c(1L, 2L, 4L, 8L, 16L, 32L)
  lags <- lags[lags < length(source) / 4]
  acf_differences <- lapply(lags, function(lag) {
    source_acf <- acf_at_lag(source, lag)
    reconstructed_acf <- acf_at_lag(reconstructed, lag)
    list(
      lag = as.integer(lag),
      source_acf = source_acf,
      reconstruction_acf = reconstructed_acf,
      absolute_difference = abs(source_acf - reconstructed_acf)
    )
  })
  acf_values <- vapply(
    acf_differences, function(item) item$absolute_difference, numeric(1)
  )
  source_runs <- run_lengths(source, burst_threshold)
  reconstructed_runs <- run_lengths(reconstructed, burst_threshold)
  baseline <- rep(training_mean, length(source))
  model_metrics <- fidelity_metrics(source, reconstructed)
  baseline_metrics <- fidelity_metrics(source, baseline)
  identity_metrics <- fidelity_metrics(source, source)

  list(
    sample_count = length(source),
    cutoff_fraction = cutoff,
    trafficmsd = model_metrics,
    identity_baseline = list(
      type = "raw observed segment (reconstruction upper-bound control)",
      fidelity = identity_metrics,
      mean_absolute_acf_difference = 0,
      burst_run_count = length(source_runs),
      burst_max_run_length = optional_max(source_runs)
    ),
    training_mean_baseline = list(
      value = training_mean,
      fidelity = baseline_metrics
    ),
    comparison = list(
      mae_improvement_vs_training_mean =
        baseline_metrics$mae - model_metrics$mae,
      rmse_improvement_vs_training_mean =
        baseline_metrics$rmse - model_metrics$rmse,
      mae_improvement_vs_identity =
        identity_metrics$mae - model_metrics$mae,
      rmse_improvement_vs_identity =
        identity_metrics$rmse - model_metrics$rmse
    ),
    temporal = list(
      acf_differences = acf_differences,
      mean_absolute_acf_difference =
        if (length(acf_values)) mean(acf_values) else NULL
    ),
    bursts = list(
      threshold_from_training_p95 = burst_threshold,
      source_run_count = length(source_runs),
      reconstruction_run_count = length(reconstructed_runs),
      source_max_run_length = optional_max(source_runs),
      reconstruction_max_run_length = optional_max(reconstructed_runs)
    ),
    compactness = list(
      component_count = length(model$components),
      scale_zero_residual_count = sum(vapply(model$components, function(item) {
        identical(as.integer(item$scale_index), 0L)
      }, logical(1))),
      serialized_model_bytes = as.integer(file.info(model_path)$size),
      source_float64_bytes = as.integer(8L * length(source)),
      model_size_ratio = as.numeric(file.info(model_path)$size) /
        (8 * length(source))
    )
  )
}

validation_rows <- lapply(cutoffs, function(cutoff) {
  result <- evaluate_segment(validation, cutoff, "validation")
  data.frame(
    cutoff_fraction = cutoff,
    mae = result$trafficmsd$mae,
    rmse = result$trafficmsd$rmse,
    training_mean_mae = result$training_mean_baseline$fidelity$mae,
    training_mean_rmse = result$training_mean_baseline$fidelity$rmse,
    mean_absolute_acf_difference = result$temporal$mean_absolute_acf_difference,
    reconstruction_burst_run_count = result$bursts$reconstruction_run_count,
    reconstruction_burst_max_run = result$bursts$reconstruction_max_run_length,
    model_size_ratio = result$compactness$model_size_ratio
  )
})
validation_table <- do.call(rbind, validation_rows)

# Primary selection criterion is validation MAE only; cutoff is the tie-breaker.
# The test segment is not evaluated until this choice is frozen.
selected_index <- order(
  validation_table$mae, validation_table$cutoff_fraction
)[1L]
selected_cutoff <- validation_table$cutoff_fraction[selected_index]
selected_validation <- evaluate_segment(
  validation, selected_cutoff, "validation-selected"
)
test_result <- evaluate_segment(test, selected_cutoff, "test")

result <- list(
  schema_version = "1.0",
  experiment_id = "westermo-right-contiguous-holdout-v1",
  evaluation_scope = "contiguous_holdout",
  dataset_id = manifest$dataset_id,
  dataset_release = manifest$dataset_release,
  archive_sha256 = manifest$archive_sha256,
  trace_sha256 = manifest$trace_sha256,
  observation_point = manifest$observation_point,
  sample_count = n,
  split = list(
    policy = "contiguous 60/20/20 split; no shuffling",
    index_convention = "zero-based half-open",
    train_range = list(0L, as.integer(train_end)),
    validation_range = list(as.integer(train_end), as.integer(validation_end)),
    test_range = list(as.integer(validation_end), as.integer(n)),
    train_sample_count = length(train),
    validation_sample_count = length(validation),
    test_sample_count = length(test)
  ),
  baseline = list(
    type = "constant mean estimated on training segment only",
    training_mean = training_mean,
    burst_threshold = burst_threshold,
    burst_threshold_source = "training segment p95"
  ),
  cutoff_selection = list(
    candidates = as.list(cutoffs),
    primary_metric = "validation MAE",
    tie_breaker = "smallest cutoff",
    selected_cutoff_fraction = selected_cutoff,
    validation_sweep = lapply(seq_len(nrow(validation_table)), function(i) {
      as.list(validation_table[i, , drop = FALSE])
    })
  ),
  validation = selected_validation,
  test = test_result,
  interpretation = paste(
    "The decomposition is a deterministic encoding of each evaluated segment,",
    "not a forecast trained on the training segment. The training segment is",
    "used only to estimate the constant-mean control and burst threshold.",
    "The cutoff is selected on validation MAE and frozen before the final test",
    "segment is evaluated. Test metrics assess whether this fixed extraction",
    "setting transfers to a later contiguous segment of the same capture."
  )
)

jsonlite::write_json(
  result,
  file.path(output_dir, "contiguous-holdout-evaluation.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA
)
utils::write.csv(
  validation_table,
  file.path(output_dir, "contiguous-holdout-validation-sweep.csv"),
  row.names = FALSE
)
message(
  "Contiguous holdout evaluation completed; selected cutoff=",
  selected_cutoff, "; test MAE=", test_result$trafficmsd$mae,
  "; training-mean MAE=", test_result$training_mean_baseline$fidelity$mae
)
