#' Export an R decomposition as a versioned model v1
#'
#' Converts the coefficient matrix produced by [decompose_traffic()] into the
#' deterministic reconstruction representation defined by model format v1.
#' Higher-scale coefficients are emitted only at the globally aligned
#' center-left position of each window, matching [compute_emits()]. The full
#' scale-zero residual is retained, including zero-valued samples.
#'
#' @param decomposition Result of [decompose_traffic()] or its coefficient matrix.
#' @param observed_sample_count Number of original, unpadded observations.
#' @param model_id Stable non-empty identifier for this model.
#' @param dataset_id Stable dataset or fixture identifier.
#' @param path Optional JSON output path. Requires the jsonlite package.
#' @param time_unit One of second, millisecond, microsecond, nanosecond, unknown.
#' @param time_unit_status Verification status for the time unit.
#' @param sampling_interval Positive sampling interval in the declared time unit.
#' @param time_origin Numeric origin of sample zero.
#' @param value_unit Physical unit of the observed values, or NULL if unknown.
#' @param value_unit_status Verification status for the value unit.
#' @param license_status Provenance license status.
#' @param source_reference Source URL or reference, or NULL.
#' @param license_reference License terms URL or reference, or NULL.
#' @param preprocessing Character vector describing preprocessing.
#' @return A model v1 list. If `path` is supplied, the same model is written as JSON.
#' @export
export_model_v1 <- function(decomposition,
                            observed_sample_count,
                            model_id,
                            dataset_id,
                            path = NULL,
                            time_unit = "unknown",
                            time_unit_status = "unknown",
                            sampling_interval = 1,
                            time_origin = 0,
                            value_unit = NULL,
                            value_unit_status = "unknown",
                            license_status = "unknown",
                            source_reference = NULL,
                            license_reference = NULL,
                            preprocessing = character()) {
  coef <- if (is.matrix(decomposition)) {
    decomposition
  } else if (is.list(decomposition) && is.matrix(decomposition$coef)) {
    decomposition$coef
  } else {
    stop("`decomposition` must be a coefficient matrix or decompose_traffic() result.",
         call. = FALSE)
  }

  if (!is.numeric(coef) || length(dim(coef)) != 2L ||
      nrow(coef) < 5L || ncol(coef) < 16L ||
      ncol(coef) != 2^round(log2(ncol(coef)))) {
    stop("`decomposition` must have at least 5 scales and a power-of-two width >= 16.",
         call. = FALSE)
  }
  if (anyNA(coef) || any(!is.finite(coef)) || any(coef < 0)) {
    stop("Coefficient values must be finite, non-missing, and non-negative.",
         call. = FALSE)
  }
  if (length(observed_sample_count) != 1L ||
      !is.numeric(observed_sample_count) ||
      !is.finite(observed_sample_count) ||
      observed_sample_count < 16 ||
      observed_sample_count != as.integer(observed_sample_count) ||
      observed_sample_count > ncol(coef)) {
    stop("`observed_sample_count` must be an integer from 16 to ncol(coef).",
         call. = FALSE)
  }
  if (!is.character(model_id) || length(model_id) != 1L ||
      is.na(model_id) || !nzchar(trimws(model_id)) ||
      !is.character(dataset_id) || length(dataset_id) != 1L ||
      is.na(dataset_id) || !nzchar(trimws(dataset_id))) {
    stop("`model_id` and `dataset_id` must be non-empty strings.",
         call. = FALSE)
  }
  allowed_time_units <- c("second", "millisecond", "microsecond",
                          "nanosecond", "unknown")
  if (!time_unit %in% allowed_time_units ||
      !time_unit_status %in% c("verified", "legacy_interpretation_unverified", "unknown") ||
      !value_unit_status %in% c("verified", "unknown") ||
      !license_status %in% c("verified", "unknown", "restricted", "not_redistributable")) {
    stop("One or more unit or license status values are invalid.", call. = FALSE)
  }
  if (length(sampling_interval) != 1L || !is.numeric(sampling_interval) ||
      !is.finite(sampling_interval) || sampling_interval <= 0 ||
      length(time_origin) != 1L || !is.numeric(time_origin) ||
      !is.finite(time_origin)) {
    stop("`sampling_interval` must be positive and `time_origin` finite.",
         call. = FALSE)
  }
  if (!is.null(value_unit) &&
      (!is.character(value_unit) || length(value_unit) != 1L ||
       is.na(value_unit) || !nzchar(trimws(value_unit)))) {
    stop("`value_unit` must be NULL or a non-empty string.", call. = FALSE)
  }
  if (!is.null(path) &&
      (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))) {
    stop("`path` must be NULL or a non-empty file path.", call. = FALSE)
  }
  if (!is.character(preprocessing) || anyNA(preprocessing)) {
    stop("`preprocessing` must be a character vector.", call. = FALSE)
  }

  sample_count <- ncol(coef)
  max_scale <- nrow(coef) - 1L
  components <- vector("list", sample_count)

  # Scale zero is a complete residual vector, including explicit zeros.
  for (sample_index in 0:(sample_count - 1L)) {
    components[[sample_index + 1L]] <- list(
      scale_index = 0L,
      window_start_sample = as.integer(sample_index),
      window_length_samples = 1L,
      emission_sample_index = as.integer(sample_index),
      amplitude = as.numeric(coef[1L, sample_index + 1L])
    )
  }

  # Higher-scale rows are repeated across windows in the legacy matrix.
  # Serialize one component at the center-left emission point per window.
  for (scale in seq_len(max_scale)) {
    window_length <- 2^scale
    window_starts <- seq.int(0L, sample_count - window_length,
                             by = window_length)
    for (window_start in window_starts) {
      emission_index <- window_start + window_length / 2L - 1L
      amplitude <- as.numeric(coef[scale + 1L, emission_index + 1L])
      if (amplitude != 0) {
        components[[length(components) + 1L]] <- list(
          scale_index = as.integer(scale),
          window_start_sample = as.integer(window_start),
          window_length_samples = as.integer(window_length),
          emission_sample_index = as.integer(emission_index),
          amplitude = amplitude
        )
      }
    }
  }

  model <- list(
    schema_version = "1.0",
    model_id = model_id,
    model_kind = "multiscale_reconstruction",
    provenance = list(
      dataset_id = dataset_id,
      source_reference = source_reference,
      license_status = license_status,
      license_reference = license_reference,
      extracted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      extracted_with = paste0("trafficmsd/", as.character(utils::packageVersion("trafficmsd"))),
      preprocessing = as.list(preprocessing)
    ),
    observation = list(
      quantity = "sum_of_length",
      aggregation = "sum",
      sampling_interval = as.numeric(sampling_interval),
      time_unit = time_unit,
      time_unit_status = time_unit_status,
      time_origin = as.numeric(time_origin),
      value_unit = value_unit,
      value_unit_status = value_unit_status,
      bin_convention = "left_closed_right_open"
    ),
    coverage = list(
      observed_sample_count = as.integer(observed_sample_count),
      decomposition_sample_count = as.integer(sample_count),
      empty_bin_value = 0L,
      padding_sample_count = as.integer(sample_count - observed_sample_count)
    ),
    extraction = list(
      algorithm = "trafficmsd_multiscale_decomposition",
      algorithm_version = as.character(utils::packageVersion("trafficmsd")),
      cutoff_fraction = 0.004
    ),
    components = components
  )

  if (!is.null(path)) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Writing JSON requires the 'jsonlite' package.", call. = FALSE)
    }
    jsonlite::write_json(model, path, auto_unbox = TRUE, pretty = TRUE,
                         null = "null", digits = NA)
  }

  model
}
