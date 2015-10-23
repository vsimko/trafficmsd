#' @import assertthat
#' @import reshape2
#' @import scales
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("trafficmsd",
          utils::packageDescription("trafficmsd", field="Version"),
          "loaded." ),
    appendLF=TRUE )
}


#' Simple traffic example of 32 samples for debugging
"sample32"


#' Example of heat-up phase with large data transfer
#' 
#' Irregular data transfer with a heat-up phase containing several small data
#' transfers, followed by a continuous transfer reaching the link capacity.
"sample_linkcap"


#' Example of regular signal
"sample_regsignal"


#' Video streaming traffic
#' 
#' Data transfer during 60 minutes of video streaming with occasional caching.
"sample_vidstream"


#' Signal with several outliers.
#' 
#' An example of multiple applications transferring data in parallel.
"sample_withoutliers"