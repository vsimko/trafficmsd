#' Export an R decomposition as a sparse model v2
#'
#' Model v2 omits zero-valued scale-zero residual entries. Missing residual
#' samples have an implicit amplitude of zero. Higher-scale emission semantics
#' are unchanged from model v1. The v1 exporter and format remain unchanged.
#'
#' @inheritParams export_model_v1
#' @return A sparse model v2 list, optionally written as JSON.
#' @export
export_model_v2 <- function(decomposition,
                            observed_sample_count,
                            model_id,
                            dataset_id,
                            path = NULL,
                            extracted_at = NULL,
                            time_unit = "unknown",
                            time_unit_status = "unknown",
                            sampling_interval = 1,
                            time_origin = 0,
                            value_unit = NULL,
                            value_unit_status = "unknown",
                            cutoff_fraction = NULL,
                            license_status = "unknown",
                            source_reference = NULL,
                            license_reference = NULL,
                            preprocessing = character()) {
  model <- export_model_v1(
    decomposition = decomposition,
    observed_sample_count = observed_sample_count,
    model_id = model_id,
    dataset_id = dataset_id,
    path = NULL,
    extracted_at = extracted_at,
    time_unit = time_unit,
    time_unit_status = time_unit_status,
    sampling_interval = sampling_interval,
    time_origin = time_origin,
    value_unit = value_unit,
    value_unit_status = value_unit_status,
    cutoff_fraction = cutoff_fraction,
    license_status = license_status,
    source_reference = source_reference,
    license_reference = license_reference,
    preprocessing = preprocessing
  )

  # A zero-amplitude residual contributes nothing to reconstruction. In v2,
  # absence of a scale-zero component at a sample index means residual zero.
  model$schema_version <- "2.0"
  model$components <- Filter(function(component) {
    component$scale_index != 0L || component$amplitude != 0
  }, model$components)

  if (!is.null(path)) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Writing JSON requires the 'jsonlite' package.", call. = FALSE)
    }
    jsonlite::write_json(model, path, auto_unbox = TRUE, pretty = TRUE,
                         null = "null", digits = NA)
  }

  model
}
