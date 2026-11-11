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
