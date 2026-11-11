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
  if (denominator == 0) return(NA_real_)
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

optional_quantile <- function(x, probability) {
  if (length(x) == 0L) NULL else
    as.numeric(stats::quantile(x, probability, names = FALSE, type = 7))
}

wasserstein_1 <- function(x, y) {
  mean(abs(sort(x) - sort(y)))
}

quantile_summary <- function(x) {
  values <- stats::quantile(
    x, probs = c(0.50, 0.90, 0.95, 0.99), names = FALSE, type = 7
  )
  as.list(stats::setNames(as.numeric(values), c("p50", "p90", "p95", "p99")))
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

  sparse_model_path <- file.path(output_dir, paste0("model-v2-", segment_name, ".json"))
  sparse_model <- trafficmsd::export_model_v2(
    decomposition,
    observed_sample_count = length(source),
    model_id = paste0("westermo-right-", segment_name, "-sparse-v2-cutoff-",
                      format(cutoff, scientific = FALSE)),
    dataset_id = manifest$dataset_id,
    path = sparse_model_path,
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
      "zero scale-zero residuals omitted; absent residual means zero",
      "no application-payload interpretation"
    )
  )
  sparse_reconstructed <- numeric(length(source) +
    (length(padded) - length(source)))
  for (component in sparse_model$components) {
    index <- component$emission_sample_index + 1L
    sparse_reconstructed[index] <- sparse_reconstructed[index] +
      component$amplitude
  }
  if (!isTRUE(all.equal(
    sparse_reconstructed,
    as.numeric(trafficmsd::reconstruct_traffic(decomposition)),
    tolerance = 0
  ))) {
    stop("Sparse model v2 reconstruction differs from model v1.", call. = FALSE)
  }

  lags <- c(1L, 2L, 4L, 8L, 16L, 32L, 64L, 128L, 256L)
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
    distribution = list(
      source_mean = mean(source),
      reconstruction_mean = mean(reconstructed),
      source_sd = stats::sd(source),
      reconstruction_sd = stats::sd(reconstructed),
      source_cv = if (mean(source) == 0) NULL else stats::sd(source) / mean(source),
      reconstruction_cv = if (mean(reconstructed) == 0) NULL else {
        stats::sd(reconstructed) / mean(reconstructed)
      },
      source_quantiles = quantile_summary(source),
      reconstruction_quantiles = quantile_summary(reconstructed),
      wasserstein_1 = wasserstein_1(source, reconstructed)
    ),
    temporal = list(
      acf_differences = acf_differences,
      mean_absolute_acf_difference =
        if (length(acf_values) && any(is.finite(acf_values))) {
          mean(acf_values, na.rm = TRUE)
        } else {
          NULL
        }
    ),
    bursts = list(
      threshold_from_training_p95 = burst_threshold,
      exceedance_fraction_difference = abs(
        mean(reconstructed > burst_threshold) -
          mean(source > burst_threshold)
      ),
      source_run_count = length(source_runs),
      reconstruction_run_count = length(reconstructed_runs),
      source_median_run_length = optional_quantile(source_runs, 0.50),
      reconstruction_median_run_length = optional_quantile(reconstructed_runs, 0.50),
      source_p95_run_length = optional_quantile(source_runs, 0.95),
      reconstruction_p95_run_length = optional_quantile(reconstructed_runs, 0.95),
      median_run_length_difference = if (
        length(source_runs) && length(reconstructed_runs)
      ) {
        abs(
          optional_quantile(source_runs, 0.50) -
            optional_quantile(reconstructed_runs, 0.50)
        )
      } else {
        NULL
      },
      p95_run_length_difference = if (
        length(source_runs) && length(reconstructed_runs)
      ) {
        abs(
          optional_quantile(source_runs, 0.95) -
            optional_quantile(reconstructed_runs, 0.95)
        )
      } else {
        NULL
      },
      source_max_run_length = optional_max(source_runs),
      reconstruction_max_run_length = optional_max(reconstructed_runs)
    ),
    compactness = list(
      component_count = length(model$components),
      scale_zero_residual_count = sum(vapply(model$components, function(item) {
        identical(as.integer(item$scale_index), 0L)
      }, logical(1))),
      v1_component_count = length(model$components),
      v2_component_count = length(sparse_model$components),
      v1_serialized_model_bytes = as.integer(file.info(model_path)$size),
      v2_serialized_model_bytes = as.integer(file.info(sparse_model_path)$size),
      source_float64_bytes = as.integer(8L * length(source)),
      v1_model_size_ratio = as.numeric(file.info(model_path)$size) /
        (8 * length(source)),
      v2_model_size_ratio = as.numeric(file.info(sparse_model_path)$size) /
        (8 * length(source)),
      v2_size_reduction_fraction = 1 -
        as.numeric(file.info(sparse_model_path)$size) /
        as.numeric(file.info(model_path)$size)
    )
  )
}

validation_rows <- lapply(cutoffs, function(cutoff) {
  result <- evaluate_segment(
    validation, cutoff,
    paste0("validation-cutoff-", format(cutoff, scientific = FALSE))
  )
  data.frame(
    cutoff_fraction = cutoff,
    mae = result$trafficmsd$mae,
    rmse = result$trafficmsd$rmse,
    relative_l1_error = if (is.null(result$trafficmsd$relative_l1_error)) {
      NA_real_
    } else {
      result$trafficmsd$relative_l1_error
    },
    training_mean_mae = result$training_mean_baseline$fidelity$mae,
    training_mean_rmse = result$training_mean_baseline$fidelity$rmse,
    mean_absolute_acf_difference =
      if (is.null(result$temporal$mean_absolute_acf_difference)) {
        NA_real_
      } else {
        result$temporal$mean_absolute_acf_difference
      },
    wasserstein_1 = result$distribution$wasserstein_1,
    source_p95 = result$distribution$source_quantiles$p95,
    reconstruction_p95 = result$distribution$reconstruction_quantiles$p95,
    burst_exceedance_fraction_difference =
      result$bursts$exceedance_fraction_difference,
    reconstruction_burst_run_count = result$bursts$reconstruction_run_count,
    median_run_length_difference =
      if (is.null(result$bursts$median_run_length_difference)) {
        NA_real_
      } else {
        result$bursts$median_run_length_difference
      },
    p95_run_length_difference =
      if (is.null(result$bursts$p95_run_length_difference)) {
        NA_real_
      } else {
        result$bursts$p95_run_length_difference
      },
    reconstruction_burst_max_run =
      if (is.null(result$bursts$reconstruction_max_run_length)) {
        NA_integer_
      } else {
        result$bursts$reconstruction_max_run_length
      },
    component_count = result$compactness$v1_component_count,
    serialized_model_bytes = result$compactness$v1_serialized_model_bytes,
    model_size_ratio = result$compactness$v1_model_size_ratio
  )
})
validation_table <- do.call(rbind, validation_rows)

pareto_frontier <- function(table, objective_columns, tolerance = 1e-12) {
  values <- as.matrix(table[, objective_columns, drop = FALSE])
  if (any(!is.finite(values))) {
    stop(
      "Pareto objectives contain undefined values; choose a valid objective set.",
      call. = FALSE
    )
  }
  keep <- rep(TRUE, nrow(values))
  for (candidate in seq_len(nrow(values))) {
    for (challenger in seq_len(nrow(values))) {
      if (candidate == challenger) next
      no_worse <- all(values[challenger, ] <= values[candidate, ] + tolerance)
      strictly_better <- any(values[challenger, ] < values[candidate, ] - tolerance)
      if (no_worse && strictly_better) {
        keep[candidate] <- FALSE
        break
      }
    }
  }
  which(keep)
}

# Fidelity objectives are selected from metrics that are defined for this trace.
# Component count and exact v1 JSON bytes are always minimized as separate costs.
objective_columns <- c("component_count", "serialized_model_bytes")
if (all(is.finite(validation_table$relative_l1_error))) {
  objective_columns <- c("relative_l1_error", objective_columns)
} else {
  objective_columns <- c("mae", objective_columns)
}
if (all(is.finite(validation_table$mean_absolute_acf_difference))) {
  objective_columns <- c("mean_absolute_acf_difference", objective_columns)
}
frontier_indices <- pareto_frontier(validation_table, objective_columns)
validation_frontier <- validation_table[frontier_indices, , drop = FALSE]

# No arbitrary scalar score or unique winner is imposed. Evaluate every
# validation Pareto candidate on the untouched test segment.
test_frontier_results <- lapply(frontier_indices, function(index) {
  cutoff <- validation_table$cutoff_fraction[index]
  evaluate_segment(
    test, cutoff,
    paste0("test-frontier-cutoff-", format(cutoff, scientific = FALSE))
  )
})

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
    objective_columns = as.list(objective_columns),
    dominance_tolerance = 1e-12,
    selection_rule = paste(
      "retain all non-dominated validation candidates;",
      "no unique candidate is selected without predeclared use-case tolerances"
    ),
    selected_cutoff_fraction = NULL,
    validation_sweep = lapply(seq_len(nrow(validation_table)), function(i) {
      as.list(validation_table[i, , drop = FALSE])
    }),
    validation_pareto_frontier = lapply(seq_len(nrow(validation_frontier)), function(i) {
      as.list(validation_frontier[i, , drop = FALSE])
    })
  ),
  test_pareto_candidates = test_frontier_results,
  interpretation = paste(
    "The decomposition is a deterministic encoding of each evaluated segment,",
    "not a forecast trained on the training segment. The training segment is",
    "used only to estimate the constant-mean control and burst threshold.",
    "All cutoffs are evaluated on validation and the non-dominated candidates",
    "are evaluated on the untouched test segment. No unique winner is claimed",
    "without predeclared use-case fidelity tolerances. Test metrics assess",
    "whether the extraction trade-off transfers to a later segment."
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
  "Contiguous holdout evaluation completed; Pareto candidates=",
  length(frontier_indices), "; objectives=",
  paste(objective_columns, collapse = ", ")
)
