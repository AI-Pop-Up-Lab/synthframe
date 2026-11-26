# Draw changes on outcome margins left unconstrained by the rake.
plot_raking_changes <- function(extended, raked, frame, area, outcome, plot_file,
                                 count_variable = "N", constrained_by = area, no_vote = NULL) {
  cell_columns <- setdiff(names(frame), count_variable)
  row_keys <- c(cell_columns, outcome)
  stopifnot(identical(extended[row_keys], raked[row_keys]),
            all(c(area, constrained_by) %in% cell_columns))
  draws <- extended_frame_draw_columns(extended, count_variable)
  stopifnot(identical(draws, extended_frame_draw_columns(raked, count_variable)))

  groupings <- unlist(lapply(seq_along(cell_columns), function(size) {
    combn(cell_columns, size, simplify = FALSE)
  }), recursive = FALSE)
  groupings <- Filter(function(by) !all(by %in% constrained_by), groupings)
  if (!length(groupings)) return(invisible(NULL))
  local <- vapply(groupings, function(by) any(area %in% by), logical(1))
  groupings <- groupings[order(local)]
  labels <- vapply(groupings, function(by) {
    paste(gsub("_", " ", by, fixed = TRUE), collapse = " × ")
  }, character(1))
  outcomes <- unique(extended[[outcome]])
  outcome_index <- match(extended[[outcome]], outcomes)
  count_columns <- c(count_variable, draws)
  if (!is.null(no_vote) &&
      (length(no_vote) != 1L || !no_vote %in% outcomes || length(outcomes) < 2L)) {
    stop("Identify the nonvoting outcome when plotting changes conditional on turnout.")
  }
  if (is.null(no_vote)) {
    count_delta <- as.data.frame(Map("-", raked[count_columns], extended[count_columns]))
  }
  full_population <- sum(frame[[count_variable]])
  if (full_population <= 0) stop("The original frame must have positive population.")

  # Use the original joint totals as denominators for both versions of the frame.
  summary <- lapply(groupings, function(by) {
    frame_keys <- frame_cell_keys(frame[by], frame[by])
    population <- as.numeric(rowsum(frame[[count_variable]], frame_keys, reorder = FALSE))
    group <- match(frame_cell_keys(extended[by], frame[by]), unique(frame_keys))
    joint <- group + (outcome_index - 1L) * length(population)
    first <- !duplicated(joint)
    row_group <- group[first]
    if (!is.null(no_vote)) {
      return(combine_conditional_raking_summaries(list(conditional_raking_summary(
        extended[count_columns], raked[count_columns], joint, row_group, population,
        outcome_index[first], outcomes, no_vote
      ))))
    }
    changes <- rowsum(count_delta, joint, reorder = FALSE)
    tv <- 50 * vapply(changes, function(x) sum(abs(x)), numeric(1)) / full_population
    outcome_points <- lapply(seq_along(outcomes), function(i) {
      rows <- outcome_index[first] == i & population[row_group] > 0
      point_changes <- numeric(length(population))
      point_changes[row_group[rows]] <- 100 * changes[[count_variable]][rows] / population[row_group[rows]]
      point_changes[population > 0]
    })
    outcome_means <- vapply(outcome_points, stats::weighted.mean, numeric(1),
                            w = population[population > 0])
    list(points = c(list(unname(tv[draws])), outcome_points),
         squares = c(tv[[count_variable]], outcome_means))
  })

  variation_title <- if (is.null(no_vote)) "Total variation" else "Total variation among voters"
  draw_raking_change_panels(summary, labels, outcomes, plot_file, variation_title)
  invisible(plot_file)
}

# Return weighted sums so groups from disjoint municipality batches can be combined.
conditional_raking_summary <- function(extended, raked, joint, group, population,
                                       outcome_index, outcomes, no_vote, variation_on_cvap = FALSE) {
  voted <- outcomes[outcome_index] != no_vote
  weights <- population[group]
  conditional_shares <- function(totals) {
    by_group <- rowsum(totals[voted, , drop = FALSE], group[voted])
    turnout <- matrix(0, length(population), ncol(totals))
    turnout[as.integer(rownames(by_group)), ] <- by_group
    denominator <- matrix(weights, nrow(totals), ncol(totals))
    denominator[voted, ] <- turnout[group[voted], , drop = FALSE]
    totals[denominator <= 0] <- NA_real_
    totals / denominator
  }
  before <- as.matrix(rowsum(extended, joint, reorder = FALSE))
  after <- as.matrix(rowsum(raked, joint, reorder = FALSE))
  delta <- conditional_shares(after) - conditional_shares(before)
  variation <- if (variation_on_cvap) {
    # CVAP-weighted share changes simplify to count changes divided by total CVAP.
    rbind(change = 50 * colSums(abs(after - before)),
          population = rep(sum(population), ncol(delta)))
  } else vapply(seq_len(ncol(delta)), function(column) {
    valid <- voted & is.finite(delta[, column])
    c(change = 50 * sum(abs(delta[valid, column]) * weights[valid]),
      population = sum(population[unique(group[valid])]))
  }, numeric(2))
  outcome_rows <- lapply(seq_along(outcomes), function(i) {
    which(outcome_index == i & is.finite(delta[, 1L]))
  })
  points <- lapply(outcome_rows, function(rows) unname(100 * delta[rows, 1L]))
  outcome_weights <- lapply(outcome_rows, function(rows) weights[rows])
  weighted_changes <- Map(function(x, w) x * w, points, outcome_weights)
  list(points = points, change = vapply(weighted_changes, sum, numeric(1)),
       population = vapply(outcome_weights, sum, numeric(1)), variation = variation)
}

# Use counts for national margins and combine weighted changes for local margins.
plot_partitioned_raking_changes <- function(extended_paths, raked_paths, frame, frame_rows,
                                            area, outcome, no_vote, plot_file, read_extended = readRDS,
                                            read_raked = readRDS) {
  summary <- partitioned_raking_change_summary(frame, area, outcome, no_vote)
  for (i in seq_along(frame_rows)) {
    summary$add(read_extended(extended_paths[[i]]), read_raked(raked_paths[[i]]),
                frame[frame_rows[[i]], , drop = FALSE])
  }
  summary$plot(plot_file)
}

# Accumulate one partition at a time while both versions of its counts are available.
partitioned_raking_change_summary <- function(frame, area, outcome, no_vote, groupings = NULL,
                                               variation_on_cvap = FALSE) {
  cells <- setdiff(names(frame), "N")
  if (is.null(groupings)) {
    groupings <- unlist(lapply(seq_along(cells), function(size) {
      combn(cells, size, simplify = FALSE)
    }), recursive = FALSE)
    groupings <- Filter(function(by) !identical(by, area), groupings)
  }
  local <- vapply(groupings, function(by) area %in% by, logical(1))
  national_groups <- groupings[!local]
  local_groups <- groupings[local]
  demographic <- unique(unlist(national_groups))
  if (!length(demographic)) demographic <- setdiff(cells, area)

  summarize_margin <- function(extended, raked, population_cells, by) {
    outcomes <- unique(extended[[outcome]])
    keys <- frame_cell_keys(population_cells[by], population_cells[by])
    population <- as.numeric(rowsum(population_cells$N, keys, reorder = FALSE))
    group <- match(frame_cell_keys(extended[by], population_cells[by]), unique(keys))
    outcome_index <- match(extended[[outcome]], outcomes)
    joint <- group + (outcome_index - 1L) * length(population)
    first <- !duplicated(joint)
    columns <- c("N", extended_frame_draw_columns(extended, "N"))
    conditional_raking_summary(extended[columns], raked[columns], joint, group[first],
                               population, outcome_index[first], outcomes, no_vote, variation_on_cvap)
  }
  national_cells <- unique(frame[demographic])
  national_keys <- frame_cell_keys(national_cells, national_cells)
  national_cell <- match(frame_cell_keys(frame[demographic], national_cells), national_keys)
  national_cells$N <- as.numeric(rowsum(frame$N, national_cell))
  national_totals <- NULL
  local_summaries <- list()
  outcomes <- NULL
  add <- function(extended, raked, population_cells) {
    stopifnot(identical(extended[c(cells, outcome)], raked[c(cells, outcome)]))
    local_summaries[[length(local_summaries) + 1L]] <<- lapply(local_groups, function(by) {
      summarize_margin(extended, raked, population_cells, by)
    })
    observed <- unique(extended[[outcome]])
    if (is.null(outcomes)) outcomes <<- observed
    stopifnot(setequal(outcomes, observed))
    columns <- c("N", extended_frame_draw_columns(extended, "N"))
    group <- match(frame_cell_keys(extended[demographic], national_cells[demographic]),
                   national_keys)
    ids <- (group - 1L) * length(outcomes) + match(extended[[outcome]], outcomes)
    aggregate_part <- function(counts) {
      totals <- rowsum(counts[columns], ids)
      padded <- matrix(0, nrow(national_cells) * length(outcomes), length(columns),
                       dimnames = list(NULL, columns))
      padded[as.integer(rownames(totals)), ] <- as.matrix(totals)
      padded
    }
    part <- list(aggregate_part(extended), aggregate_part(raked))
    national_totals <<- if (is.null(national_totals)) part else Map(`+`, national_totals, part)
  }
  plot <- function(plot_file) {
    national <- national_cells[rep(seq_len(nrow(national_cells)), each = length(outcomes)),
                               demographic, drop = FALSE]
    national[[outcome]] <- rep(outcomes, times = nrow(national_cells))
    national <- lapply(national_totals, function(counts) cbind(national, counts))
    national_summaries <- lapply(national_groups, function(by) {
      combine_conditional_raking_summaries(list(summarize_margin(
        national[[1L]], national[[2L]], national_cells, by
      )))
    })
    local <- lapply(seq_along(local_groups), function(i) {
      combine_conditional_raking_summaries(lapply(local_summaries, `[[`, i))
    })
    labels <- vapply(c(national_groups, local_groups), function(by) {
      paste(gsub("_", " ", by, fixed = TRUE), collapse = " × ")
    }, character(1))
    title <- if (variation_on_cvap) "Total variation across all citizen adults" else
      "Total variation among voters"
    draw_raking_change_panels(c(national_summaries, local), labels, outcomes, plot_file, title)
    invisible(plot_file)
  }
  list(add = add, plot = plot)
}

combine_conditional_raking_summaries <- function(parts) {
  add <- function(name) Reduce(`+`, lapply(parts, `[[`, name))
  variation <- add("variation")
  tv <- unname(variation["change", ] / variation["population", ])
  points <- do.call(Map, c(list(c), lapply(parts, `[[`, "points")))
  list(points = c(list(tv[-1L]), points),
       squares = c(tv[1L], add("change") / add("population")))
}

draw_raking_change_panels <- function(summary, labels, outcomes, plot_file, variation_title = "Total variation") {
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
  panel_titles <- c(variation_title, outcomes)

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
  graphics::mtext("How raking changed vote shares", side = 3, outer = TRUE,
                  line = 0.9, cex = 1.55, font = 2, adj = 0)
}
