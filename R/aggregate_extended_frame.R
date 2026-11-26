# Sum expected counts by group within each draw, keeping the extension category.
aggregate_extended_frame <- function(frame, posterior, by = character()) {
  frame <- as_plain_frame(frame, coerce = TRUE)
  # Locate each requested cell in the saved probability array.
  rows <- match_posterior_cells(frame, posterior)
  if (!is.character(by) || anyNA(by) || anyDuplicated(by) ||
      !all(by %in% names(frame)) ||
      any(by %in% c(posterior$count_variable, posterior$extension_variable,
                    dimnames(posterior$draws)[[3L]],
                    extended_frame_draw_columns(frame, posterior$count_variable)))) {
    stop("Supply distinct grouping columns from the frame.")
  }

  # With no grouping columns, all cells contribute to one total per outcome.
  groups <- frame[, by, drop = FALSE]
  if (anyNA(groups)) stop("Grouping columns must be complete.")
  group_ids <- rep.int(1L, nrow(frame))
  if (length(by)) {
    keys <- frame_cell_keys(groups, groups)
    # Number groups in the order they first appear in the frame.
    group_ids <- match(keys, unique(keys))
  }

  draws <- posterior$draws
  population <- frame[[posterior$count_variable]]
  population_draws <- paired_frame_counts(frame, posterior)
  first <- !duplicated(group_ids)
  # Keep outcomes and draws separate while replacing cells with groups.
  totals <- array(0, dim = c(sum(first), dim(draws)[2:3]),
                  dimnames = c(list(NULL), dimnames(draws)[2:3]))
  for (category in seq_len(dim(draws)[2L])) {
    # Multiply probabilities by cell populations before summing over cells.
    probabilities <- matrix(draws[rows, category, , drop = FALSE],
                            nrow(frame), dim(draws)[3L])
    # Summing draw by draw preserves the variation in each group's totals.
    counts <- probabilities * if (is.null(population_draws)) population else population_draws
    totals[, category, ] <- rowsum(counts, group_ids, reorder = FALSE)
  }
  # Totals are already on the count scale.
  expand_expected_counts(groups[first, , drop = FALSE], totals, 1, posterior)
}

validate_prediction_frame <- function(frame, cell_columns, count_variable) {
  if (!is.character(count_variable) || length(count_variable) != 1L ||
      is.na(count_variable) || !nzchar(count_variable) ||
      !is.data.frame(frame) || !nrow(frame) || anyDuplicated(names(frame)) ||
      !all(c(cell_columns, count_variable) %in% names(frame))) {
    stop("Supply a nonempty frame with unique column names, cell keys, and a count column.")
  }
  cells <- frame[, cell_columns, drop = FALSE]
  if (anyNA(cells) || anyDuplicated(frame_cell_keys(cells, cells))) {
    stop("Frame cells must have complete, unique keys.")
  }
  population <- frame[[count_variable]]
  if (!is.numeric(population) || any(!is.finite(population) | population < 0)) {
    stop("Frame counts must be finite and nonnegative.")
  }
}

# Match on cell keys so a reorder or subset of the frame still uses the right draws.
match_posterior_cells <- function(frame, posterior) {
  if (!inherits(posterior, "synthframe_posterior")) stop("Supply a posterior probability object.")
  # Match using the original cell columns even when the supplied frame has extras.
  reference <- posterior$cells
  columns <- names(reference)
  validate_prediction_frame(frame, columns, posterior$count_variable)
  keys <- frame_cell_keys(frame[, columns, drop = FALSE], reference)
  rows <- match(keys, frame_cell_keys(reference, reference))
  if (anyNA(rows)) stop("Some frame cells have no saved posterior draws.")
  rows
}

frame_cell_keys <- function(cells, reference) {
  # Encode values using the reference column's codes, then combine those codes.
  # This keeps punctuation inside labels from creating ambiguous keys.
  codes <- lapply(names(reference), function(column) {
    match(cells[[column]], unique(reference[[column]]))
  })
  do.call(paste, c(codes, sep = ":"))
}

# Put each cell and outcome on its own row. Each draw column is an expected count.
expand_expected_counts <- function(cells, draws, population, posterior, include_draws = TRUE,
                                   population_draws = NULL, progress = NULL) {
  outcomes <- dimnames(draws)[[2L]]
  # Repeat each cell once per outcome, with outcomes varying fastest.
  cell_rows <- rep(seq_len(nrow(cells)), each = length(outcomes))
  expanded <- cells[cell_rows, , drop = FALSE]
  rownames(expanded) <- NULL
  expanded[[posterior$extension_variable]] <- rep(outcomes, times = nrow(cells))
  # Give every outcome row the population of its source cell.
  population <- rep_len(population, nrow(cells))[cell_rows]

  # Transpose the cell-by-outcome mean to match the expanded row order.
  if (is.null(population_draws)) {
    means <- rowMeans(matrix(draws, nrow(cells) * length(outcomes)))
    expanded[[posterior$count_variable]] <-
      as.vector(t(matrix(means, nrow(cells), length(outcomes)))) * population
  } else {
    expanded[[posterior$count_variable]] <- numeric(nrow(expanded))
  }
  # Apply the same row order and population weights to every draw.
  if (include_draws || !is.null(population_draws)) {
    n_draws <- dim(draws)[3L]
    if (!is.null(progress)) progress(0L, n_draws, "count draws")
    for (i in seq_len(n_draws)) {
      draw <- matrix(draws[, , i], nrow(cells), length(outcomes))
      weight <- if (is.null(population_draws)) population else population_draws[cell_rows, i]
      counts <- as.vector(t(draw)) * weight
      if (!is.null(population_draws)) {
        expanded[[posterior$count_variable]] <-
          expanded[[posterior$count_variable]] + counts / n_draws
      }
      if (include_draws) expanded[[dimnames(draws)[[3L]][i]]] <- counts
      if (!is.null(progress)) progress(i, n_draws)
    }
  }
  expanded
}

# Draw labels carry their simulation number, regardless of CSV column order.
prediction_count_draws <- function(frame, count_variable) {
  if (!is.data.frame(frame) || !is.character(count_variable) ||
      length(count_variable) != 1L || is.na(count_variable) || !nzchar(count_variable)) {
    stop("Supply a prediction frame and a valid population count column name.")
  }
  columns <- extended_frame_draw_columns(frame, count_variable)
  index <- suppressWarnings(as.integer(sub("^.*draw_", "", columns)))
  if (!identical(sort(index, na.last = TRUE), seq_along(columns))) {
    stop("Population draws must be numbered consecutively from 1.")
  }
  columns <- columns[order(index)]
  if (!all(vapply(columns, function(column) {
    x <- frame[[column]]
    is.numeric(x) && all(is.finite(x) & x >= 0)
  }, logical(1)))) stop("Population draws must be finite and nonnegative.")
  columns
}

paired_frame_counts <- function(frame, posterior) {
  if (!isTRUE(posterior$paired_population_draws)) return(NULL)
  columns <- prediction_count_draws(frame, posterior$count_variable)
  if (length(columns) != dim(posterior$draws)[3L]) {
    stop("Supply the original population draw columns to summarize this extension.")
  }
  as.matrix(frame[columns])
}

# Discover the count draws in current and earlier extended-frame exports.
extended_frame_draw_columns <- function(extended_frame, count_variable) {
  prefix <- paste0(count_variable, "_draw_")
  draw_columns <- names(extended_frame)[startsWith(names(extended_frame), prefix)]
  draw_columns <- draw_columns[grepl("^[1-9][0-9]*$",
                                     substring(draw_columns, nchar(prefix) + 1L))]
  # Earlier extended-frame exports used draw_1, draw_2, and so on.
  if (!length(draw_columns)) {
    draw_columns <- grep("^draw_[1-9][0-9]*$", names(extended_frame), value = TRUE)
  }
  draw_columns
}
