# Check the outcome margins and the population of every original joint cell.
check_post_rake <- function(raked_frame, frame, targets, by, extension_variable,
                            target_variable = "target", count_variable = "N",
                            outcome_order = NULL, outcome_plot_file = NULL,
                            joint_plot_file = NULL, share_tolerance = 0.000000000002,
                            count_tolerance = 0.00000001, no_vote = NULL) {
  raked_frame <- as_plain_frame(raked_frame)
  margins <- check_extended_frame(
    raked_frame, targets, by, extension_variable, target_variable, count_variable,
    plot_file = outcome_plot_file, outcome_order = outcome_order,
    coverage_tolerance = share_tolerance, no_vote = no_vote
  )
  joint <- post_rake_cell_comparison(raked_frame, frame, extension_variable, count_variable)
  joint_metrics <- plot_post_rake_cells(joint, joint_plot_file, count_tolerance)
  invisible(list(outcome = margins, joint = list(comparison = joint,
                 metrics = joint_metrics, plot_file = joint_plot_file)))
}

post_rake_cell_comparison <- function(raked_frame, frame, extension_variable, count_variable = "N",
                                     include_max_error = FALSE) {
  raked_frame <- as_plain_frame(raked_frame)
  frame <- as_plain_frame(frame)
  population_draws <- prediction_count_draws(frame, count_variable)
  cell_columns <- setdiff(names(frame), c(count_variable, population_draws))
  validate_prediction_frame(frame, cell_columns, count_variable)
  if (!all(cell_columns %in% names(raked_frame))) {
    stop("The raked frame must contain the original joint cell columns.")
  }
  reference <- frame[cell_columns]
  cell <- match(frame_cell_keys(raked_frame[cell_columns], reference),
                frame_cell_keys(reference, reference))
  if (anyNA(cell) || length(unique(cell)) != nrow(frame)) {
    stop("The raked frame must match and cover every original joint cell.")
  }

  draw_columns <- setdiff(extended_frame_draw_columns(raked_frame, count_variable),
                          c(cell_columns, extension_variable))
  if (length(population_draws)) {
    draw_columns <- prediction_count_draws(raked_frame, count_variable)
    if (length(draw_columns) != length(population_draws)) {
      stop("Raked and original population draws must match by draw number.")
    }
  }
  totals <- raking_rowsum(raked_frame[c(count_variable, draw_columns)], cell)
  joint <- data.frame(reference, target_population = frame[[count_variable]],
                      estimate = totals[[count_variable]],
                      lower_90 = NA_real_, upper_90 = NA_real_)
  mean_error <- totals[[count_variable]] - joint$target_population
  if (include_max_error) joint$max_abs_error <- abs(mean_error)
  if (length(draw_columns)) {
    counts <- totals[draw_columns]
    if (include_max_error || length(population_draws)) {
      for (i in seq_along(counts)) {
        reference <- if (length(population_draws)) frame[[population_draws[i]]] else
          joint$target_population
        error <- counts[[i]] - reference
        if (include_max_error) joint$max_abs_error <- pmax(joint$max_abs_error, abs(error))
        # Plot draw errors on the mean-population scale when the reference varies.
        if (length(population_draws)) counts[[i]] <- joint$target_population + error
      }
    }
    intervals <- t(apply(counts, 1L, stats::quantile,
                         probs = c(0.05, 0.95), names = FALSE))
    joint$lower_90 <- intervals[, 1L]
    joint$upper_90 <- intervals[, 2L]
  }
  joint
}

plot_post_rake_cells <- function(joint, joint_plot_file = NULL, count_tolerance = 1e-8) {
  panels <- list(joint)
  titles <- "All original cells"
  joint_metrics <- frame_target_metrics(panels, titles, "target_population",
                                        scale = 1, coverage_tolerance = count_tolerance)
  plot_frame_target_panels(panels, titles, joint_metrics, "target_population",
                           "Original stratification joint", joint_plot_file,
                           count_scale = TRUE)
  names(joint_metrics)[1:2] <- c("bias_count", "RMSE_count")
  invisible(joint_metrics)
}
