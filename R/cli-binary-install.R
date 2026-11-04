# Download and verify a prebuilt TrafficMSD CLI ----------------------------

.trafficmsd_cli_target <- function() {
  system <- Sys.info()[["sysname"]]
  machine <- Sys.info()[["machine"]]
  if (is.null(machine) || is.na(machine) || !nzchar(machine)) {
    machine <- R.version$arch
  }
  machine <- tolower(machine)
  architecture <- if (machine %in% c("x86_64", "x86-64", "amd64")) {
    "x86_64"
  } else if (machine %in% c("aarch64", "arm64")) {
    "aarch64"
  } else {
    NA_character_
  }

  if (identical(system, "Windows") && identical(architecture, "x86_64")) {
    return("x86_64-pc-windows-msvc")
  }
  if (identical(system, "Linux") && identical(architecture, "x86_64")) {
    return("x86_64-unknown-linux-gnu")
  }
  if (identical(system, "Linux") && identical(architecture, "aarch64")) {
    return("aarch64-unknown-linux-gnu")
  }
  if (identical(system, "Darwin") && identical(architecture, "x86_64")) {
    return("x86_64-apple-darwin")
  }
  if (identical(system, "Darwin") && identical(architecture, "aarch64")) {
    return("aarch64-apple-darwin")
  }
  stop(
    sprintf("No prebuilt TrafficMSD CLI is published for %s/%s.",
            system, machine),
    call. = FALSE
  )
}

.trafficmsd_download <- function(url, destination, description) {
  status <- tryCatch(
    utils::download.file(url, destination, mode = "wb", quiet = TRUE),
    error = function(error) {
      stop(sprintf("Could not download %s: %s", description, conditionMessage(error)),
           call. = FALSE)
    }
  )
  if (!identical(as.integer(status), 0L) || !file.exists(destination)) {
    stop(sprintf("Could not download %s from %s.", description, url),
         call. = FALSE)
  }
  invisible(destination)
}

#' Install the prebuilt TrafficMSD Rust CLI
#'
#' Downloads the release asset for the current operating system, verifies its
#' SHA-256 checksum against the release manifest, stores it in the user's
#' cache, and sets TRAFFICMSD_CLI for the current R session. No Rust toolchain
#' is required. A published GitHub Release is required.
#'
#' @param version Release version such as 0.1.0, or "latest".
#' @param repo GitHub repository in owner/name form.
#' @param cache_dir Optional user-writable cache directory.
#' @param force If TRUE, download and replace an existing cached executable.
#' @return The absolute path to the installed executable.
#' @export
install_trafficmsd_cli <- function(version = "latest",
                                   repo = "vsimko/trafficmsd",
                                   cache_dir = NULL,
                                   force = FALSE) {
  if (!is.character(version) || length(version) != 1L || is.na(version) ||
      !nzchar(version) ||
      (!identical(version, "latest") &&
       !grepl("^[v]?[A-Za-z0-9._-]+$", version))) {
    stop("version must be 'latest' or a release version such as '0.1.0'.",
         call. = FALSE)
  }
  if (!is.character(repo) || length(repo) != 1L || is.na(repo) ||
      !grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repo)) {
    stop("repo must be a GitHub owner/name repository identifier.",
         call. = FALSE)
  }
  if (!is.logical(force) || length(force) != 1L || is.na(force)) {
    stop("force must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(cache_dir) &&
      (!is.character(cache_dir) || length(cache_dir) != 1L ||
       is.na(cache_dir) || !nzchar(cache_dir))) {
    stop("cache_dir must be NULL or one non-empty path.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Install the jsonlite package before installing the Rust CLI.",
         call. = FALSE)
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Install the digest package to verify the Rust CLI SHA-256 checksum.",
         call. = FALSE)
  }

  target <- .trafficmsd_cli_target()
  requested_version <- sub("^v", "", version)
  release_path <- if (identical(version, "latest")) {
    "releases/latest/download"
  } else {
    paste0("releases/download/v", requested_version)
  }
  base_url <- paste0("https://github.com/", repo, "/", release_path)
  manifest_path <- tempfile("trafficmsd-manifest-", fileext = ".json")
  on.exit(unlink(manifest_path), add = TRUE)
  .trafficmsd_download(
    paste0(base_url, "/trafficmsd-cli-manifest.json"),
    manifest_path, "CLI release manifest"
  )
  manifest <- tryCatch(
    jsonlite::fromJSON(manifest_path, simplifyVector = FALSE),
    error = function(error) {
      stop("The CLI release manifest is not valid JSON.", call. = FALSE)
    }
  )
  if (!identical(as.integer(manifest$schema_version), 1L) ||
      is.null(manifest$release_version) ||
      !grepl("^[A-Za-z0-9._-]+$", manifest$release_version) ||
      is.null(manifest$targets)) {
    stop("The CLI release manifest has an unsupported or invalid schema.",
         call. = FALSE)
  }
  if (!identical(version, "latest") &&
      !identical(manifest$release_version, requested_version)) {
    stop("The downloaded CLI manifest does not match the requested release version.",
         call. = FALSE)
  }
  entry <- manifest$targets[[target]]
  if (is.null(entry) || is.null(entry$asset) || is.null(entry$sha256) ||
      !nzchar(entry$asset) ||
      !identical(basename(entry$asset), entry$asset) ||
      !grepl("^[A-Fa-f0-9]{64}$", entry$sha256)) {
    stop(sprintf("The release does not contain a valid asset for %s.", target),
         call. = FALSE)
  }

  if (is.null(cache_dir)) {
    cache_dir <- if (getRversion() >= "4.0.0") {
      tools::R_user_dir("trafficmsd", which = "cache")
    } else {
      file.path(path.expand("~"), ".cache", "R", "trafficmsd")
    }
  }
  executable_name <- if (.Platform$OS.type == "windows") {
    "trafficmsd.exe"
  } else {
    "trafficmsd"
  }
  destination_dir <- file.path(cache_dir, manifest$release_version, target)
  if (!dir.exists(destination_dir) &&
      !dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create the TrafficMSD CLI cache directory.", call. = FALSE)
  }
  destination <- file.path(destination_dir, executable_name)
  expected_sha256 <- tolower(entry$sha256)

  if (file.exists(destination) && !force) {
    cached_sha256 <- digest::digest(
      file = destination, algo = "sha256", serialize = FALSE
    )
    if (identical(tolower(cached_sha256), expected_sha256)) {
      installed <- normalizePath(destination, winslash = "/", mustWork = TRUE)
      if (.Platform$OS.type != "windows") Sys.chmod(destination, mode = "0755")
      Sys.setenv(TRAFFICMSD_CLI = installed)
      return(invisible(installed))
    }
  }

  binary_path <- tempfile("trafficmsd-cli-")
  on.exit(unlink(binary_path), add = TRUE)
  asset_url <- paste0(
    "https://github.com/", repo, "/releases/download/v",
    manifest$release_version, "/",
    utils::URLencode(entry$asset, reserved = TRUE)
  )
  .trafficmsd_download(asset_url, binary_path, "TrafficMSD CLI binary")
  actual_sha256 <- digest::digest(
    file = binary_path, algo = "sha256", serialize = FALSE
  )
  if (!identical(tolower(actual_sha256), expected_sha256)) {
    stop("The downloaded TrafficMSD CLI failed SHA-256 verification.",
         call. = FALSE)
  }
  if (!file.copy(binary_path, destination, overwrite = TRUE)) {
    stop("Could not install the TrafficMSD CLI into the user cache.",
         call. = FALSE)
  }
  if (.Platform$OS.type != "windows") {
    Sys.chmod(destination, mode = "0755")
  }
  installed <- normalizePath(destination, winslash = "/", mustWork = TRUE)
  Sys.setenv(TRAFFICMSD_CLI = installed)
  message("TrafficMSD CLI ", manifest$release_version, " installed for ", target)
  invisible(installed)
}
