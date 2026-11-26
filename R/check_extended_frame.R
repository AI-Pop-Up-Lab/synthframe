# Compare poststratified outcome shares to targets at the supplied grouping level.
check_extended_frame <- function(extended_frame, targets, by, extension_variable,
                                 target_variable = "target", count_variable = "N",
                                 plot_file = NULL, outcome_order = NULL,
                                 coverage_tolerance = 0, no_vote = NULL, draw_plot = TRUE) {
  extended_frame <- as_plain_frame(extended_frame)
  targets <- as_plain_frame(targets)
  keys <- c(by, extension_variable)
  result_names <- c("estimate", "lower_90", "upper_90")
  if (!is.character(by) || !is.character(extension_variable) ||
      length(extension_variable) != 1L || anyNA(keys) || any(!nzchar(keys)) ||
      anyDuplicated(keys) || any(keys %in% result_names) ||
      !is.character(target_variable) || length(target_variable) != 1L ||
      is.na(target_variable) || !nzchar(target_variable) ||
      target_variable %in% c(keys, result_names) ||
      !is.character(count_variable) || length(count_variable) != 1L ||
      is.na(count_variable) || !nzchar(count_variable) || count_variable %in% keys) {
    stop("Supply distinct grouping, outcome, and value column names.")
  }
  if (!is.data.frame(extended_frame) || !nrow(extended_frame) ||
      anyDuplicated(names(extended_frame)) ||
      !all(c(keys, count_variable) %in% names(extended_frame)) ||
      anyNA(extended_frame[keys])) {
    stop("Supply a nonempty extended frame with complete group and outcome columns and mean counts.")
  }
  if (!is.data.frame(targets) || !nrow(targets) || anyDuplicated(names(targets)) ||
      !all(c(keys, target_variable) %in% names(targets)) ||
      anyNA(targets[keys]) || anyDuplicated(targets[keys])) {
    stop("Supply one target per distinct group and outcome.")
  }
  target_values <- targets[[target_variable]]
  if (!is.numeric(target_values) || any(!is.finite(target_values) |
                                        target_values < 0 | target_values > 1)) {
    stop("Target shares must be finite numbers between 0 and 1.")
  }

  draw_columns <- extended_frame_draw_columns(extended_frame, count_variable)
  draw_columns <- setdiff(draw_columns, keys)
  if (!length(draw_columns)) {
    warning("No posterior draws in the extended frame. Intervals and 90% coverage are unavailable.",
            call. = FALSE)
  }
  count_columns <- c(count_variable, draw_columns)
  counts <- extended_frame[count_columns]
  if (!all(vapply(counts, function(x) {
    is.numeric(x) && all(is.finite(x) & x >= 0)
  }, logical(1)))) stop("Extended frame counts must be finite and nonnegative.")

  # Sum within group and outcome first. The denominator includes every outcome,
  # even when the caller supplies a target for only some of them.
  cell_keys <- extended_frame[keys]
  group_outcome <- frame_cell_keys(cell_keys, cell_keys)
  first <- !duplicated(group_outcome)
  grouped_keys <- cell_keys[first, , drop = FALSE]
  counts_matrix <- as.matrix(raking_rowsum(counts, group_outcome, reorder = FALSE))
  group <- rep.int(1L, nrow(grouped_keys))
  target_group <- rep.int(1L, nrow(targets))
  if (length(by)) {
    group_keys <- grouped_keys[by]
    levels <- frame_cell_keys(unique(group_keys), group_keys)
    group <- match(frame_cell_keys(group_keys, group_keys), levels)
    target_group <- match(frame_cell_keys(targets[by], group_keys), levels)
  }
  outcomes <- unique(as.character(grouped_keys[[extension_variable]]))
  if (anyNA(target_group) || !all(targets[[extension_variable]] %in% outcomes)) {
    stop("Each target must identify a known group and modeled outcome.")
  }
  validate_benchmark_share_totals(target_values, target_group, length(outcomes))

  # A globally modeled category can have no rows in a particular group.
  absent <- !frame_cell_keys(targets[keys], grouped_keys) %in% group_outcome[first]
  if (any(absent)) {
    if (any(target_values[absent] > 0)) stop("A positive target has no extended-frame rows in its group.")
    grouped_keys <- rbind(grouped_keys, targets[absent, keys, drop = FALSE])
    counts_matrix <- rbind(counts_matrix, matrix(0, sum(absent), ncol(counts_matrix)))
    group <- c(group, target_group[absent])
  }
  denominators <- rowsum(counts_matrix, group, reorder = FALSE)
  share_denominator <- denominators[group, , drop = FALSE]
  if (!is.null(no_vote)) {
    if (!is.character(no_vote) || length(no_vote) != 1L ||
        !no_vote %in% grouped_keys[[extension_variable]]) {
      stop("no_vote must identify an outcome in the extended frame.")
    }
    voted <- grouped_keys[[extension_variable]] != no_vote
    vote_counts <- counts_matrix
    vote_counts[!voted, ] <- 0
    turnout <- rowsum(vote_counts, group, reorder = FALSE)
    share_denominator[voted, ] <- turnout[group[voted], , drop = FALSE]

    # Targets arrive on the population scale, even when only some parties are plotted.
    target_voted <- targets[[extension_variable]] != no_vote
    if (any(target_voted)) {
      nonvoter <- match(target_group[target_voted], target_group[!target_voted])
      target_turnout <- 1 - target_values[!target_voted][nonvoter]
      if (anyNA(target_turnout) || any(target_turnout <= 0)) {
        stop("Each party target group needs a No Vote target implying positive turnout.")
      }
      party_shares <- target_values[target_voted] / target_turnout
      validate_benchmark_share_totals(party_shares, target_group[target_voted], length(outcomes) - 1L)
      targets[[target_variable]][target_voted] <- pmin(1, party_shares)
    }
  }
  shares <- counts_matrix / share_denominator

  estimates <- grouped_keys
  estimates$estimate <- shares[, 1L]
  estimates$lower_90 <- estimates$upper_90 <- NA_real_
  if (length(draw_columns)) {
    intervals <- t(apply(shares[, -1L, drop = FALSE], 1L, function(x) {
      if (any(!is.finite(x))) return(c(NA_real_, NA_real_))
      stats::quantile(x, c(0.05, 0.95), names = FALSE)
    }))
    estimates$lower_90 <- intervals[, 1L]
    estimates$upper_90 <- intervals[, 2L]
  }
  comparison <- merge(targets[c(keys, target_variable)], estimates,
                      by = keys, all.x = TRUE, sort = FALSE)
  if (any(!is.finite(comparison$estimate))) {
    stop("Every target must match an extended-frame group and outcome with a positive share denominator.")
  }
  if (length(draw_columns) && anyNA(comparison$lower_90)) {
    stop("Every matched group must have a positive share denominator in every draw.")
  }

  if (is.null(outcome_order)) {
    outcome_order <- outcomes
  } else if (!is.character(outcome_order) || anyNA(outcome_order) ||
             anyDuplicated(outcome_order) ||
             !all(comparison[[extension_variable]] %in% outcome_order)) {
    stop("Supply a distinct outcome order that includes every target outcome.")
  }
  outcomes <- outcome_order
  outcomes <- outcomes[outcomes %in% comparison[[extension_variable]]]
  outcome_panels <- lapply(outcomes, function(outcome) {
    comparison[comparison[[extension_variable]] == outcome, , drop = FALSE]
  })
  overall <- if (is.null(no_vote)) comparison else
    comparison[comparison[[extension_variable]] != no_vote, , drop = FALSE]
  panels <- outcome_panels
  panel_titles <- outcomes
  if (nrow(overall)) {
    panels <- c(list(overall), panels)
    panel_titles <- c("Overall", panel_titles)
  }
  metrics <- frame_target_metrics(panels, panel_titles, target_variable,
                                  scale = 100, coverage_tolerance = coverage_tolerance)
  plot_labels <- panel_titles
  if (!is.null(no_vote)) {
    plot_labels <- ifelse(panel_titles == no_vote,
                          paste0(panel_titles, " (% of population)"),
                          paste0(panel_titles, " (% of votes cast)"))
  }
  if (isTRUE(draw_plot)) {
    plot_frame_target_panels(panels, plot_labels, metrics, target_variable,
                             extension_variable, plot_file)
  }
  names(metrics)[1:2] <- c("bias_pp", "RMSE_pp")
  invisible(list(comparison = comparison, metrics = metrics, plot_file = plot_file))
}

validate_benchmark_share_totals <- function(shares, group, outcome_count) {
  totals <- rowsum(cbind(share = shares, outcomes = 1L), group, reorder = FALSE)
  tolerance <- sqrt(.Machine$double.eps)
  if (any(totals[, "share"] > 1 + tolerance)) stop("Benchmark shares exceed one within a group.")
  complete <- totals[, "outcomes"] == outcome_count
  if (any(abs(totals[complete, "share"] - 1) > tolerance)) {
    stop("Benchmark shares for all modeled outcomes must sum to one within each complete group.")
  }
}

frame_target_metrics <- function(panels, panel_titles, target_variable,
                                 scale, coverage_tolerance) {
  if (!is.numeric(coverage_tolerance) || length(coverage_tolerance) != 1L ||
      !is.finite(coverage_tolerance) || coverage_tolerance < 0) {
    stop("Coverage tolerance must be finite and nonnegative.")
  }
  metrics <- lapply(panels, function(panel) {
    x <- panel[[target_variable]]
    y <- panel$estimate
    error <- scale * (y - x)
    rho <- if (length(unique(x)) > 1L && length(unique(y)) > 1L) {
      stats::cor(x, y, method = "spearman")
    } else NA_real_
    coverage <- if (!all(is.na(panel$lower_90))) {
      100 * mean(x >= panel$lower_90 - coverage_tolerance &
                   x <= panel$upper_90 + coverage_tolerance)
    } else NA_real_
    c(bias = mean(error), RMSE = sqrt(mean(error^2)),
      spearman = rho, coverage_90_pct = coverage)
  })
  as.data.frame(do.call(rbind, metrics), row.names = make.unique(panel_titles))
}

plot_frame_target_panels <- function(panels, panel_titles, metrics, target_variable,
                                     plot_title, plot_file, count_scale = FALSE) {
  scale <- if (count_scale) 1 else 100
  axis_units <- if (count_scale) "population count" else "share (%)"
  metric_units <- if (count_scale) "people" else "pp"
  columns <- ceiling(sqrt(length(panels)))
  rows <- ceiling(length(panels) / columns)
  if (!is.null(plot_file)) {
    grDevices::png(plot_file, width = 4.2 * columns + 0.5,
                   height = 4.1 * rows + 0.5, units = "in", res = 145, type = "cairo")
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  old_par <- graphics::par(mfrow = c(rows, columns), mar = c(2.6, 2.8, 2.5, 0.6),
                           oma = c(2.7, 2.7, 2.5, 0.2), pty = "s",
                           mgp = c(1.4, 0.45, 0))
  on.exit(graphics::par(old_par), add = TRUE, after = FALSE)
  panel_text_size <- if (length(panels) == 1L) 0.95 else 1.30
  for (i in seq_along(panels)) {
    panel <- panels[[i]]
    x <- scale * panel$estimate
    y <- scale * panel[[target_variable]]
    lower <- scale * panel$lower_90
    upper <- scale * panel$upper_90
    upper_limit <- 1.05 * max(c(1, x, y, upper), na.rm = TRUE)
    limits <- c(0, if (count_scale) upper_limit else min(100, upper_limit))
    graphics::plot(x, y, type = "n", xlim = limits, ylim = limits, asp = 1,
                   xaxs = "i", yaxs = "i", xlab = "", ylab = "", las = 1)
    graphics::title(panel_titles[i], cex.main = panel_text_size)
    graphics::abline(0, 1, col = "#8A4A2A", lty = 2)
    if (!all(is.na(lower))) {
      graphics::segments(lower, y, upper, y, col = "#155A764D")
    }
    graphics::points(x, y, pch = 16, cex = 0.65, col = "#155A76B3")
    m <- metrics[i, ]
    rho <- if (is.na(m$spearman)) "NA" else sprintf("%.2f", m$spearman)
    coverage <- if (is.na(m$coverage_90_pct)) "NA" else sprintf("%.1f%%", m$coverage_90_pct)
    graphics::legend("topleft", inset = 0.025, cex = panel_text_size, x.intersp = 0.3,
                     bg = "#FFFFFFCC", box.col = NA, legend = c(
                       sprintf("Bias  %.2f %s", round(m$bias, 2) + 0, metric_units),
                       sprintf("RMSE  %.2f %s", m$RMSE, metric_units),
                       paste("Spearman ", rho),
                       paste("90% coverage ", coverage)
                     ))
  }
  graphics::mtext(paste("Poststratified", axis_units), side = 1, outer = TRUE, line = 0.8)
  graphics::mtext(paste("Target", axis_units), side = 2, outer = TRUE, line = 0.8)
  graphics::mtext(plot_title, side = 3, outer = TRUE, line = 0.6,
                  font = 2, cex = 1.25)

  invisible(NULL)
}
