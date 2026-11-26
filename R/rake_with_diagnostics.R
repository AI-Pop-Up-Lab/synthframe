# Run the rake and generate its target checks and change diagnostic.
rake_with_diagnostics <- function(extended_frame, frame, targets, by, extension_variable,
                                  target_variable = "target", count_variable = "N",
                                  target_scale = c("share", "count"),
                                  output_dir = "data/output", outcome_order = NULL,
                                  area = by, tolerance = 0.000000000001,
                                  max_iterations = 1000L, no_vote = NULL) {
  target_scale <- match.arg(target_scale)
  if (!is.character(area) || anyNA(area) ||
      !all(area %in% setdiff(names(frame), count_variable))) {
    stop("Specify area columns from the original frame.")
  }
  raked <- rake_extended_frame(extended_frame, frame, targets, by, extension_variable,
                                target_variable, count_variable, target_scale,
                                tolerance, max_iterations)

  # The target plots compare shares, even when the rake received count targets.
  if (length(by)) {
    keys <- frame_cell_keys(frame[by], frame[by])
    totals <- raking_rowsum(frame[[count_variable]], keys, reorder = FALSE)
    target_keys <- frame_cell_keys(targets[by], frame[by])
    population <- totals[match(target_keys, rownames(totals)), 1L]
  } else {
    population <- rep(sum(frame[[count_variable]]), nrow(targets))
  }
  shares <- targets[population > 0, , drop = FALSE]
  if (!nrow(shares)) stop("Outcome share diagnostics require positive population.")
  if (target_scale == "count") {
    shares[[target_variable]] <- shares[[target_variable]] / population[population > 0]
  }
  n_groups <- if (length(by)) nrow(unique(frame[by])) else 1L
  n_outcomes <- length(unique(raked[[extension_variable]]))
  constrained_by <- if (nrow(targets) == n_groups * n_outcomes) by else character()

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  group_name <- if (length(by)) paste(by, collapse = "_") else "national"
  group_name <- gsub("[^[:alnum:]_-]", "_", group_name)
  target_check <- check_post_rake(
    raked, frame, shares, by, extension_variable,
    target_variable = target_variable,
    count_variable = count_variable,
    outcome_order = outcome_order,
    outcome_plot_file = file.path(output_dir, paste0("raked_", group_name, "_target_check.png")),
    joint_plot_file = file.path(output_dir, "raked_original_joint_check.png"),
    share_tolerance = 2 * tolerance, no_vote = no_vote
  )
  change_plot <- plot_raking_changes(
    extended_frame, raked, frame, area, extension_variable,
    plot_file = file.path(output_dir, "raking_changes_diagnostic.png"),
    count_variable = count_variable, constrained_by = constrained_by
  )
  invisible(list(frame = raked, target_check = target_check, change_plot = change_plot))
}
