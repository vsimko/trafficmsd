sample32 <- c(0, 0, 0, 0, 4, 4, 4, 0,
              2, 2, 2, 0, 0, 0, 0, 0,
              2, 2, 2, 4, 3, 3, 3, 0,
              0, 0, 0, 0, 3, 0, 0, 0)
devtools::use_data(sample32, internal = FALSE)

sample_vidstream <- read_traffic('data-raw/01-video-with-caching.csv', use.power2len = FALSE)
sample_vidstream <- as.vector(sample_vidstream)
devtools::use_data(sample_linkcap)

sample_linkcap <- read_traffic('data-raw/02-heatup-and-linkcap.csv', use.power2len = FALSE)
sample_linkcap <- as.vector(sample_linkcap)
devtools::use_data(sample_linkcap)

sample_withoutliers <- read_traffic('data-raw/03-outliers.csv', use.power2len = FALSE)
sample_withoutliers <- as.vector(sample_withoutliers)
devtools::use_data(sample_withoutliers)

sample_regsignal <- read_traffic('data-raw/04-regular-signal.csv', use.power2len = FALSE)
sample_regsignal <- as.vector(sample_regsignal)
devtools::use_data(sample_regsignal)
