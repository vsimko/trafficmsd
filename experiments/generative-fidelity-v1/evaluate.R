#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: evaluate.R REFERENCE.csv GENERATED_DIR OUTPUT.json", call. = FALSE)
}
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else "evaluate.R"
source(file.path(dirname(normalizePath(script_path)), "metrics.R"))

reference_path <- args[[1L]]
generated_dir <- args[[2L]]
output_path <- args[[3L]]
if (!requireNamespace("jsonlite", quietly = TRUE))
  stop("Install jsonlite before running the evaluator.", call. = FALSE)
if (!dir.exists(generated_dir))
  stop("Generated-replicate directory does not exist: ", generated_dir, call. = FALSE)

reference <- read_series(reference_path)
replicate_files <- sort(list.files(generated_dir, pattern = "^replicate-[^/]+\\.csv$", full.names = TRUE))
if (length(replicate_files) < 2L)
  stop("At least two independent generated replicate CSV files are required.", call. = FALSE)

results <- lapply(replicate_files, function(path) {
  generated <- read_series(path)
  if (length(generated) != length(reference))
    stop("Sample-count mismatch in ", path, call. = FALSE)
  list(replicate_id = basename(path), metrics = compare_realization(reference, generated))
})

result <- list(
  schema_version = "1.0",
  evaluation = "statistical_generative_fidelity",
  reference = list(file = basename(reference_path), sample_count = length(reference),
                   time_alignment = "sample_index; identical regular bins required"),
  replicate_count = length(results),
  replicate_results = results,
  replicate_metric_summary = summarize_replicates(lapply(results, `[[`, "metrics")),
  interpretation = list(
    pointwise_reconstruction_error = "not calculated; generated realizations are not expected to match individual source bins",
    replicate_percentiles = "empirical spread across supplied independent realizations, not confidence intervals",
    ks_test_p_value = "not calculated because time bins are autocorrelated"
  )
)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(result, output_path, auto_unbox = TRUE, pretty = TRUE,
                     null = "null", digits = NA)
message("Generative fidelity evaluation completed: ", output_path)
