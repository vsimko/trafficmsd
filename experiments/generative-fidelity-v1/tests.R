#!/usr/bin/env Rscript

source(file.path("experiments", "generative-fidelity-v1", "metrics.R"))

reference <- rep(c(rep(0, 8), rep(10, 8)), 4)
identical_result <- compare_realization(reference, reference)
stopifnot(identical_result$distribution$wasserstein_1 == 0)
stopifnot(identical_result$distribution$ks_distance == 0)
stopifnot(identical_result$temporal$mean_absolute_acf_difference == 0)
stopifnot(identical_result$bursts$absolute_exceedance_fraction_difference == 0)

# Same marginal distribution, different temporal ordering: distribution
# distances remain zero while temporal fidelity detects the change.
reordered <- rep(c(rep(0, 4), rep(10, 4)), 8)
same_marginal <- compare_realization(reference, reordered)
stopifnot(same_marginal$distribution$wasserstein_1 == 0)
stopifnot(same_marginal$distribution$ks_distance == 0)
stopifnot(is.finite(same_marginal$temporal$mean_absolute_acf_difference))
stopifnot(same_marginal$temporal$mean_absolute_acf_difference > 0)

# A change in traffic volume must be visible to marginal metrics.
scaled <- compare_realization(reference, reference * 2)
stopifnot(scaled$distribution$wasserstein_1 > 0)
stopifnot(scaled$distribution$relative_mean_difference > 0)

summary <- summarize_replicates(list(identical_result, same_marginal, scaled))
stopifnot(summary$wasserstein_1$n == 3L)
stopifnot(summary$mean_absolute_acf_difference$n >= 2L)

cat("Generative fidelity metric tests passed.\n")

# Exercise the command-line contract and JSON output with synthetic replicates.
# These fixtures test the evaluator only; they are not network evidence.
temporary <- tempfile("generative-fidelity-")
dir.create(temporary)
replicate_dir <- file.path(temporary, "replicates")
dir.create(replicate_dir)
reference_path <- file.path(temporary, "reference.csv")
output_path <- file.path(temporary, "result.json")
write_series <- function(path, values) {
  utils::write.csv(data.frame(sample_index = 0:(length(values) - 1L),
                              value = values), path, row.names = FALSE)
}
write_series(reference_path, reference)
write_series(file.path(replicate_dir, "replicate-001.csv"), reference)
write_series(file.path(replicate_dir, "replicate-002.csv"), reordered)
evaluator <- file.path("experiments", "generative-fidelity-v1", "evaluate.R")
status <- system2(file.path(R.home("bin"), "Rscript"),
                  c(evaluator, reference_path, replicate_dir, output_path))
stopifnot(identical(as.integer(status), 0L), file.exists(output_path))
json_result <- jsonlite::fromJSON(output_path, simplifyVector = FALSE)
stopifnot(identical(as.integer(json_result$replicate_count), 2L))
stopifnot(identical(json_result$evaluation, "statistical_generative_fidelity"))
unlink(temporary, recursive = TRUE)
cat("Generative fidelity CLI integration test passed.\\n")
