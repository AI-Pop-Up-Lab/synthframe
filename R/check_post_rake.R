# Check the outcome margins and the population of every original joint cell.
check_post_rake <- function(raked_frame, frame, targets, by, extension_variable,
                            target_variable = "target", count_variable = "N",
                            outcome_order = NULL, outcome_plot_file = NULL,
                            joint_plot_file = NULL, share_tolerance = 0.000000000002,
                            count_tolerance = 0.00000001) {
  margins <- check_extended_frame(
    raked_frame, targets, by, extension_variable, target_variable, count_variable,
    plot_file = outcome_plot_file, outcome_order = outcome_order,
    coverage_tolerance = share_tolerance
  )

  cell_columns <- setdiff(names(frame), count_variable)
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
  totals <- rowsum(raked_frame[c(count_variable, draw_columns)], cell)
  joint <- data.frame(reference, target_population = frame[[count_variable]],
                      estimate = totals[[count_variable]],
                      lower_90 = NA_real_, upper_90 = NA_real_)
  if (length(draw_columns)) {
    intervals <- t(apply(totals[draw_columns], 1L, stats::quantile,
                         probs = c(0.05, 0.95), names = FALSE))
    joint$lower_90 <- intervals[, 1L]
    joint$upper_90 <- intervals[, 2L]
  }
  panels <- list(joint)
  titles <- "All original cells"
  joint_metrics <- frame_target_metrics(panels, titles, "target_population",
                                        scale = 1, coverage_tolerance = count_tolerance)
  plot_frame_target_panels(panels, titles, joint_metrics, "target_population",
                           "Original stratification joint", joint_plot_file,
                           count_scale = TRUE)
  names(joint_metrics)[1:2] <- c("bias_count", "RMSE_count")
  invisible(list(outcome = margins, joint = list(comparison = joint,
                 metrics = joint_metrics, plot_file = joint_plot_file)))
}
