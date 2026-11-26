# Draw changes on outcome margins left unconstrained by the rake.
plot_raking_changes <- function(extended, raked, frame, area, outcome, plot_file,
                                 count_variable = "N", constrained_by = area, no_vote = NULL) {
  frame <- as_plain_frame(frame, coerce = TRUE)
  extended <- as_plain_frame(extended, coerce = TRUE)
  raked <- as_plain_frame(raked, coerce = TRUE)
  population_draws <- prediction_count_draws(frame, count_variable)
  cell_columns <- setdiff(names(frame), c(count_variable, population_draws))
  row_keys <- c(cell_columns, outcome)
  stopifnot(identical(extended[row_keys], raked[row_keys]),
            all(c(area, constrained_by) %in% cell_columns))

  groupings <- unlist(lapply(seq_along(cell_columns), function(size) {
    combn(cell_columns, size, simplify = FALSE)
  }), recursive = FALSE)
  groupings <- Filter(function(by) !all(by %in% constrained_by), groupings)
  if (!length(groupings)) return(invisible(NULL))
  local <- vapply(groupings, function(by) any(area %in% by), logical(1))
  groupings <- groupings[order(local)]
  outcomes <- unique(as.character(extended[[outcome]]))
  if (!is.null(no_vote) && (length(no_vote) != 1L || !no_vote %in% outcomes)) {
    stop("no_vote must identify an outcome in the extended frame.")
  }
  if (sum(frame[[count_variable]]) <= 0) stop("The original frame must have positive population.")
  changes <- raking_count_changes(extended, raked, count_variable, population_draws)
  summary <- lapply(groupings, function(by) {
    combine_raking_changes(list(raking_margin_changes(
      changes, extended, frame, by, outcome, outcomes, count_variable, population_draws
    )), outcomes)
  })
  labels <- vapply(groupings, function(by) {
    paste(gsub("_", " ", by, fixed = TRUE), collapse = " × ")
  }, character(1))
  draw_raking_change_panels(summary, labels, outcomes, plot_file)
  invisible(plot_file)
}

# Draw numbers identify simulations even when files store columns in different orders.
raking_count_changes <- function(extended, raked, count_variable, population_draws) {
  draws <- extended_frame_draw_columns(extended, count_variable)
  stopifnot(setequal(draws, extended_frame_draw_columns(raked, count_variable)))
  if (length(population_draws)) {
    draws <- prediction_count_draws(extended, count_variable)
    stopifnot(length(draws) == length(population_draws))
  } else {
    draws <- draws[order(as.numeric(sub("^.*draw_", "", draws)))]
  }
  columns <- c(count_variable, draws)
  raked[columns] - extended[columns]
}

# The same eligible population divides the before and after counts for every outcome.
raking_margin_changes <- function(changes, keys, frame, by, outcome, outcomes,
                                  count_variable, population_draws) {
  frame_groups <- if (length(by)) frame_cell_keys(frame[by], frame[by]) else rep.int(1L, nrow(frame))
  key_groups <- if (length(by)) frame_cell_keys(keys[by], frame[by]) else rep.int(1L, nrow(keys))
  group <- match(key_groups, unique(frame_groups))
  outcome_index <- match(keys[[outcome]], outcomes)
  stopifnot(!anyNA(group), !anyNA(outcome_index))
  populations <- as.matrix(raking_rowsum(frame[c(count_variable, population_draws)], frame_groups,
                                         reorder = FALSE))
  joint <- group + (outcome_index - 1L) * nrow(populations)
  first <- !duplicated(joint)
  delta <- as.matrix(raking_rowsum(changes, joint, reorder = FALSE))
  by_outcome <- lapply(seq_along(outcomes), function(i) {
    selected <- outcome_index[first] == i
    # An absent group/outcome row has zero count in both frames.
    counts <- numeric(nrow(populations))
    counts[group[first][selected]] <- delta[selected, 1L]
    counts
  })
  names(by_outcome) <- outcomes
  eligible <- populations[, 1L] > 0
  points <- lapply(by_outcome, function(counts) unname(100 * counts[eligible] / populations[eligible, 1L]))
  total_population <- sum(populations[, 1L])
  draw_population <- if (length(population_draws)) colSums(populations) else
    rep(total_population, ncol(delta))
  list(points = points, groups = sum(eligible), change = vapply(by_outcome, sum, numeric(1)),
       population = total_population,
       variation = rbind(change = 50 * colSums(abs(delta)), population = draw_population))
}

# Combine numerators and denominators before dividing across disjoint geographic groups.
combine_raking_changes <- function(parts, outcomes) {
  add <- function(name) Reduce(`+`, lapply(parts, `[[`, name))
  variation <- add("variation")
  population <- variation["population", ]
  tv <- unname(variation["change", ] / ifelse(population > 0, population, NA_real_))
  # A category absent from a partition has zero change in every eligible group there.
  points <- lapply(outcomes, function(outcome) {
    unlist(lapply(parts, function(part) {
      if (outcome %in% names(part$points)) part$points[[outcome]] else numeric(part$groups)
    }), use.names = FALSE)
  })
  changes <- vapply(outcomes, function(outcome) {
    sum(vapply(parts, function(part) sum(part$change[names(part$change) == outcome]), numeric(1)))
  }, numeric(1), USE.NAMES = FALSE)
  eligible <- add("population")
  list(points = c(list(tv[-1L]), points),
       squares = c(tv[1L], 100 * changes / if (eligible > 0) eligible else NA_real_))
}

# Use counts for national margins and combine weighted changes for local margins.
plot_partitioned_raking_changes <- function(extended_paths, raked_paths, frame, frame_rows,
                                            area, outcome, no_vote, plot_file, read_extended = readRDS,
                                            read_raked = readRDS) {
  frame <- as_plain_frame(frame, coerce = TRUE)
  summary <- partitioned_raking_change_summary(frame, area, outcome, no_vote)
  for (i in seq_along(frame_rows)) {
    summary$add(read_extended(extended_paths[[i]]), read_raked(raked_paths[[i]]),
                frame[frame_rows[[i]], , drop = FALSE])
  }
  summary$plot(plot_file)
}

# Accumulate one partition at a time while both versions of its counts are available.
partitioned_raking_change_summary <- function(frame, area, outcome, no_vote, groupings = NULL) {
  frame <- as_plain_frame(frame, coerce = TRUE)
  population_draws <- prediction_count_draws(frame, "N")
  cells <- setdiff(names(frame), c("N", population_draws))
  if (is.null(groupings)) {
    groupings <- unlist(lapply(seq_along(cells), function(size) {
      combn(cells, size, simplify = FALSE)
    }), recursive = FALSE)
    groupings <- Filter(function(by) !identical(by, area), groupings)
  }
  if (!length(groupings)) {
    return(list(add = function(...) invisible(NULL), plot = function(...) invisible(NULL)))
  }
  local <- vapply(groupings, function(by) area %in% by, logical(1))
  national_groups <- groupings[!local]
  local_groups <- groupings[local]
  demographic <- unique(unlist(national_groups))
  if (length(national_groups)) {
    keys <- if (length(demographic)) frame_cell_keys(frame[demographic], frame[demographic]) else
      rep.int(1L, nrow(frame))
    national_cells <- frame[!duplicated(keys), demographic, drop = FALSE]
    population_columns <- c("N", population_draws)
    national_cells[population_columns] <- raking_rowsum(frame[population_columns], keys, reorder = FALSE)
  }

  national_changes <- NULL
  local_summaries <- list()
  outcomes <- NULL
  count_columns <- NULL
  add <- function(extended, raked, population_cells) {
    extended <- as_plain_frame(extended, coerce = TRUE)
    raked <- as_plain_frame(raked, coerce = TRUE)
    population_cells <- as_plain_frame(population_cells, coerce = TRUE)
    stopifnot(identical(extended[c(cells, outcome)], raked[c(cells, outcome)]))
    observed <- unique(as.character(extended[[outcome]]))
    new_outcomes <- setdiff(observed, outcomes)
    outcomes <<- c(outcomes, new_outcomes)
    changes <- raking_count_changes(extended, raked, "N", population_draws)
    if (is.null(count_columns)) count_columns <<- names(changes)
    stopifnot(identical(names(changes), count_columns))
    local_summaries[[length(local_summaries) + 1L]] <<- lapply(local_groups, function(by) {
      raking_margin_changes(changes, extended, population_cells, by, outcome, observed, "N", population_draws)
    })
    if (length(national_groups)) {
      group <- if (length(demographic)) {
        match(frame_cell_keys(extended[demographic], national_cells[demographic]),
              frame_cell_keys(national_cells[demographic], national_cells[demographic]))
      } else rep.int(1L, nrow(extended))
      stopifnot(!anyNA(group))
      # Each outcome owns a contiguous block. A newly observed category appends
      # a block without changing the indices of previously accumulated counts.
      ids <- group + (match(extended[[outcome]], outcomes) - 1L) * nrow(national_cells)
      part <- raking_rowsum(changes, ids)
      if (length(new_outcomes)) {
        zeros <- matrix(0, nrow(national_cells) * length(new_outcomes), ncol(changes),
                        dimnames = list(NULL, count_columns))
        national_changes <<- rbind(national_changes, zeros)
      }
      rows <- as.integer(rownames(part))
      national_changes[rows, ] <<- national_changes[rows, , drop = FALSE] + as.matrix(part)
    }
    invisible(NULL)
  }
  plot <- function(plot_file) {
    if (!is.null(no_vote) && (length(no_vote) != 1L || !no_vote %in% outcomes)) {
      stop("no_vote must identify an outcome in the extended frame.")
    }
    national_summaries <- list()
    if (length(national_groups)) {
      national <- national_cells[rep(seq_len(nrow(national_cells)), times = length(outcomes)),
                                 demographic, drop = FALSE]
      national[[outcome]] <- rep(outcomes, each = nrow(national_cells))
      national_summaries <- lapply(national_groups, function(by) {
        combine_raking_changes(list(raking_margin_changes(
          national_changes, national, national_cells, by, outcome, outcomes, "N", population_draws
        )), outcomes)
      })
    }
    local <- lapply(seq_along(local_groups), function(i) {
      combine_raking_changes(lapply(local_summaries, `[[`, i), outcomes)
    })
    labels <- vapply(c(national_groups, local_groups), function(by) {
      paste(gsub("_", " ", by, fixed = TRUE), collapse = " × ")
    }, character(1))
    draw_raking_change_panels(c(national_summaries, local), labels, outcomes, plot_file)
    invisible(plot_file)
  }
  list(add = add, plot = plot)
}

draw_raking_change_panels <- function(summary, labels, outcomes, plot_file) {
  n_columns <- min(4L, length(outcomes) + 1L)
  n_rows <- ceiling((length(outcomes) + 1L) / n_columns)
  grDevices::png(plot_file, width = 1950, height = 505 * n_rows + 120,
                 res = 155, type = "cairo")
  on.exit(grDevices::dev.off())
  graphics::layout(matrix(seq_len(n_rows * (n_columns + 1L)), nrow = n_rows, byrow = TRUE),
                   widths = c(3.35, rep(1.85, n_columns)))
  graphics::par(oma = c(0.65, 0.8, 2.8, 0.8), cex = 1)
  y <- rev(seq_along(labels))
  y_limits <- c(0.5, length(y) + 0.5)
  panel_titles <- c("Total variation (% of eligible population)", outcomes)

  for (panel_row in seq_len(n_rows)) {
    graphics::par(mar = c(1.85, 0.15, 2.2, 0.15))
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = y_limits, xaxs = "i", yaxs = "i")
    graphics::text(1, y, labels, adj = 1, cex = 0.84, col = "#1D3943")
    for (panel_column in seq_len(n_columns)) {
      panel <- (panel_row - 1L) * n_columns + panel_column
      if (panel > length(panel_titles)) {
        graphics::plot.new()
        next
      }
      graphics::par(mar = c(1.85, 0.38, 2.2, 0.38), mgp = c(1.25, 0.36, 0))
      point_values <- lapply(summary, function(x) x$points[[panel]])
      square_values <- vapply(summary, function(x) x$squares[panel], numeric(1))
      limits <- range(c(0, square_values, unlist(point_values, use.names = FALSE)), finite = TRUE)
      if (diff(limits) == 0) limits <- limits + c(-0.1, 0.1)
      graphics::plot(square_values, y, type = "n", xlim = limits, ylim = y_limits,
                     yaxs = "i", axes = FALSE, xlab = "", ylab = "")
      title <- paste(strwrap(panel_titles[panel], width = 23), collapse = "\n")
      graphics::title(title, line = 0.55, cex.main = 0.88)
      graphics::abline(h = y, col = "#E8EFF0")
      graphics::abline(v = 0, col = "#788B91", lty = 2)
      for (i in seq_along(point_values)) {
        values <- point_values[[i]][is.finite(point_values[[i]])]
        n <- length(values)
        if (!n) next
        # Spread coincident points without changing the RNG state.
        jitter <- ((seq_len(n) * (sqrt(5) - 1) / 2) %% 1 - 0.5) * 0.54
        color <- if (panel == 1L) "#246C72" else ifelse(values < 0, "#A44344",
                         ifelse(values > 0, "#23774C", "#788B91"))
        opacity <- min(0.72, 2.5 / sqrt(n))
        graphics::points(values, y[i] + jitter, pch = 16, cex = 0.33,
                         col = grDevices::adjustcolor(color, alpha.f = opacity))
      }
      graphics::points(square_values, y, pch = 22, cex = 0.87, lwd = 1.2,
                       bg = "#FFF7DC", col = "#1D3943")
      ticks <- pretty(limits, n = 4)
      ticks <- ticks[ticks >= limits[1L] & ticks <= limits[2L]]
      graphics::axis(1, at = ticks, cex.axis = 0.82, col = "#9CB1B7", lwd = 1.15)
    }
  }
  graphics::mtext("How raking changed shares of the eligible population", side = 3, outer = TRUE,
                  line = 0.9, cex = 1.55, font = 2, adj = 0)
}
