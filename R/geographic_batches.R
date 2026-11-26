# Partition original row indices without splitting any raking target group.
geographic_batches <- function(frame, area_variable, parent_area_variable = NULL,
                               areas_per_batch = NULL, rake_by = list(area_variable)) {
  frame <- as_plain_frame(frame)
  valid_area <- function(column) {
    is.character(column) && length(column) == 1L && !is.na(column) &&
      column %in% names(frame) && !anyNA(frame[[column]]) &&
      all(nzchar(as.character(frame[[column]])))
  }
  if (!nrow(frame) || !valid_area(area_variable)) stop("Supply a complete lower-level area column.")
  if (!is.null(areas_per_batch) &&
      (!is.numeric(areas_per_batch) || length(areas_per_batch) != 1L ||
       !is.finite(areas_per_batch) || areas_per_batch < 1 ||
       areas_per_batch != floor(areas_per_batch))) {
    stop("areas_per_batch must be a positive whole number.")
  }
  if (!is.list(rake_by) || !all(vapply(rake_by, function(by) {
    is.character(by) && all(by %in% names(frame)) && !anyNA(frame[by])
  }, logical(1)))) stop("Supply complete raking grouping columns.")

  area <- as.character(frame[[area_variable]])
  area_rows <- !duplicated(area)
  margin_keys <- lapply(rake_by, function(by) {
    if (length(by)) frame_cell_keys(frame[by], frame[by]) else rep.int("national", nrow(frame))
  })
  intact <- function(groups, by) !anyDuplicated(by[!duplicated(data.frame(by, groups))])
  safe <- function(groups) {
    intact(groups, area) && all(vapply(margin_keys, function(key) intact(groups, key), logical(1)))
  }
  rows <- seq_len(nrow(frame))
  if (is.null(areas_per_batch) && valid_area(parent_area_variable)) {
    parent <- as.character(frame[[parent_area_variable]])
    if (!identical(parent_area_variable, area_variable) && safe(parent)) return(split(rows, parent))
  }

  # A broader margin can force a batch to contain more than one lower-level area.
  units <- area
  if (!safe(units)) {
    candidates <- unique(unlist(rake_by[lengths(rake_by) == 1L], use.names = FALSE))
    candidates <- Filter(function(column) {
      valid_area(column) && safe(as.character(frame[[column]]))
    }, candidates)
    if (!length(candidates)) return(list(national = rows))
    sizes <- vapply(candidates, function(column) length(unique(frame[[column]])), integer(1))
    units <- as.character(frame[[candidates[which.max(sizes)]]])
    if (is.null(areas_per_batch)) return(split(rows, units))
  }

  limit <- if (is.null(areas_per_batch)) 10L else areas_per_batch
  unit_index <- match(units, unique(units))
  weights <- tabulate(unit_index[area_rows])
  batches <- integer(length(weights))
  batch <- 1L
  used <- 0
  for (i in seq_along(weights)) {
    if (used > 0 && used + weights[i] > limit) {
      batch <- batch + 1L
      used <- 0
    }
    batches[i] <- batch
    used <- used + weights[i]
  }
  result <- split(rows, batches[unit_index])
  names(result) <- sprintf("batch-%02d", seq_along(result))
  result
}

vignette_areas_per_batch <- function() {
  value <- Sys.getenv("VOTEFRAME_AREAS_PER_BATCH")
  if (nzchar(value)) suppressWarnings(as.numeric(value))
}

# Clear obsolete numbered outputs only after a replacement run succeeds.
prune_geographic_partitions <- function(directory, current, pattern) {
  old <- setdiff(list.files(directory, pattern = pattern, full.names = TRUE), current)
  if (length(old) && !all(file.remove(old))) stop("Could not remove obsolete geographic partitions.")
}
