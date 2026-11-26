# Give the three case studies the same locations for generated files.
case_study_outputs <- function(out) {
  folders <- c("models", "diagnostics", "logs", "raked_frame")
  paths <- stats::setNames(as.list(file.path(out, folders)), folders)
  # Create the raked directory only when the run saves calibrated partitions.
  for (path in paths[c("models", "diagnostics", "logs")]) {
    if (!dir.exists(path) && !dir.create(path, recursive = TRUE)) {
      stop("Could not create case-study output directory ", sQuote(path), ".")
    }
  }
  paths
}

# Finish writing the replacement before publishing a model or frame partition.
save_case_study_rds <- function(object, path, compress = TRUE) {
  pending <- tempfile(tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(pending))
  saveRDS(object, pending, compress = compress)
  if (!file.rename(pending, path)) stop("Could not publish ", sQuote(path), ".")
  invisible(path)
}

# Each municipality belongs to one partition. Retain the first-seen group and draw orders.
municipal_partition_counts <- function(parts, read_part, area, outcome) {
  do.call(rbind, lapply(parts, function(part) {
    counts <- as_plain_frame(read_part(part))
    keys <- counts[c(area, outcome)]
    group <- frame_cell_keys(keys, keys)
    columns <- c("N", extended_frame_draw_columns(counts, "N"))
    cbind(keys[!duplicated(group), , drop = FALSE],
          rowsum(counts[columns], group, reorder = FALSE))
  }))
}
