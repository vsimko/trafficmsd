# Generate the deterministic R-produced model v1 example.
#
# Run from the repository root after installing the current package:
#   R CMD INSTALL .
#   Rscript data-raw/generate-model-v1-fixture.R
#
# The fixture intentionally uses unknown physical units. It is a synthetic
# algorithm/conformance example, not a network-trace dataset or benchmark.

if (!requireNamespace("trafficmsd", quietly = TRUE)) {
  stop("Install the current trafficmsd package first: R CMD INSTALL .")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite first: install.packages('jsonlite')")
}

signal <- rep(c(0, 4, 0, 2), 8)
decomposition <- trafficmsd::decompose_traffic(signal)

output <- file.path(
  "examples", "models", "v1", "r-generated-periodic-32.json"
)

trafficmsd::export_model_v1(
  decomposition,
  observed_sample_count = length(signal),
  model_id = "r-generated-periodic-32",
  dataset_id = "synthetic-periodic-32",
  path = output,
  preprocessing = c("none", "deterministic periodic synthetic signal")
)

message("Wrote ", output)
