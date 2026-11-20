# Draw changes on outcome margins left unconstrained by the rake.
plot_raking_changes <- function(extended, raked, frame, area, outcome, plot_file,
                                 count_variable = "N", constrained_by = area) {
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
  count_delta <- as.data.frame(Map("-", raked[count_columns], extended[count_columns]))
  full_population <- sum(frame[[count_variable]])
  if (full_population <= 0) stop("The original frame must have positive population.")

  # Use the original joint totals as denominators for both versions of the frame.
  summary <- lapply(groupings, function(by) {
    frame_keys <- frame_cell_keys(frame[by], frame[by])
    population <- as.numeric(rowsum(frame[[count_variable]], frame_keys, reorder = FALSE))
    group <- match(frame_cell_keys(extended[by], frame[by]), unique(frame_keys))
    joint <- group + (outcome_index - 1L) * length(population)
    first <- !duplicated(joint)
    changes <- rowsum(count_delta, joint, reorder = FALSE)
    row_group <- group[first]
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

  draw_raking_change_panels(summary, labels, outcomes, plot_file)
  invisible(plot_file)
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
  panel_titles <- c("Total variation", outcomes)

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
      limits <- range(c(0, square_values, unlist(point_values, use.names = FALSE)))
      if (diff(limits) == 0) limits <- limits + c(-0.1, 0.1)
      graphics::plot(square_values, y, type = "n", xlim = limits, ylim = y_limits,
                     yaxs = "i", axes = FALSE, xlab = "", ylab = "")
      title <- paste(strwrap(panel_titles[panel], width = 23), collapse = "\n")
      graphics::title(title, line = 0.55, cex.main = 0.88)
      graphics::abline(h = y, col = "#E8EFF0")
      graphics::abline(v = 0, col = "#788B91", lty = 2)
      for (i in seq_along(point_values)) {
        values <- point_values[[i]]
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
