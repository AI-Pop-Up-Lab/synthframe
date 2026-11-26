# Estimate the footprint of count columns and keys before allocating the frame.
pre_rake_frame_bytes <- function(rows, draws, keys) {
  as.double(rows) * 8 * (1 + draws + keys)
}

pre_rake_memory_available <- function() {
  limits <- numeric()
  if (file.exists("/proc/meminfo")) {
    line <- grep("^MemAvailable:", readLines("/proc/meminfo"), value = TRUE)
    limits <- c(limits, as.numeric(gsub("[^0-9]", "", line)) * 1024)
  }
  for (paths in list(c("/sys/fs/cgroup/memory.max", "/sys/fs/cgroup/memory.current"),
                     c("/sys/fs/cgroup/memory/memory.limit_in_bytes",
                       "/sys/fs/cgroup/memory/memory.usage_in_bytes"))) {
    if (all(file.exists(paths))) {
      values <- vapply(paths, function(path) readLines(path, n = 1L), character(1))
      if (all(grepl("^[0-9]+$", values))) limits <- c(limits, as.numeric(values[1L]) - as.numeric(values[2L]))
    }
  }
  if (any(is.finite(limits))) max(0, min(limits[is.finite(limits)])) else NA_real_
}

pre_rake_disk_fields <- function(directory) {
  result <- tryCatch(suppressWarnings(system2("df", c("-Pk", shQuote(directory)),
                                             stdout = TRUE, stderr = TRUE)),
                     error = function(e) character())
  if (!length(result) || !is.null(attr(result, "status"))) return(rep(NA_character_, 4L))
  strsplit(trimws(tail(result, 1L)), "[[:space:]]+")[[1L]]
}

pre_rake_storage <- function(mode = "auto", directory = NULL, bytes = 0,
                             largest_part_bytes = bytes,
                             final_directory = NULL, final_bytes = 0) {
  mode <- match.arg(mode, c("auto", "memory", "temporary", "keep"))
  if (mode == "keep" && is.null(directory)) stop("Set a directory to keep the pre-rake frame.")
  if (mode == "auto") {
    available <- pre_rake_memory_available()
    # Leave room for the active block, raking workspace and other allocations.
    mode <- if (is.na(available) || bytes + 3 * largest_part_bytes > available / 2) {
      "temporary"
    } else {
      "memory"
    }
  }
  if (mode == "memory") {
    return(list(mode = mode, put = function(frame, name) frame,
                get = identity, cleanup = function() invisible(NULL)))
  }

  parent <- if (is.null(directory)) tempdir() else directory
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(parent)) stop("Could not create pre-rake storage directory: ", parent)
  disk <- pre_rake_disk_fields(parent)
  available <- suppressWarnings(as.numeric(disk[4L]) * 1024)
  if (!is.na(available) && !is.null(final_directory) &&
      identical(disk[1L], pre_rake_disk_fields(final_directory)[1L])) {
    available <- available - final_bytes
  }
  if (!is.na(available) && bytes > available) {
    stop("Insufficient space for estimated pre-rake storage in ", parent,
         ". Choose another storage directory or explicitly select memory.")
  }

  directory <- if (mode == "temporary") tempfile("pre-rake-", tmpdir = parent) else parent
  if (mode == "temporary" && !dir.create(directory)) stop("Could not create temporary pre-rake storage.")
  put <- function(frame, name) {
    path <- file.path(directory, paste0(name, ".rds"))
    save_case_study_rds(frame, path)
    path
  }
  list(mode = mode, put = put, get = readRDS, cleanup = function() {
    if (mode == "temporary" && unlink(directory, recursive = TRUE) != 0L) {
      warning("Could not remove temporary pre-rake storage: ", directory)
    }
  })
}


vignette_pre_rake_storage <- function(out, bytes, ...) {
  mode <- Sys.getenv("VOTEFRAME_PRE_RAKE_STORAGE", "auto")
  default_directory <- if (mode == "keep") file.path(out, "extended_frame") else tempdir()
  pre_rake_storage(mode, Sys.getenv("VOTEFRAME_PRE_RAKE_DIR", default_directory), bytes, ...)
}
