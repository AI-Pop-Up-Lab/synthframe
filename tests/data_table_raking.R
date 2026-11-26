check_data_table_raking <- function(scratch) {
  # Start with keyed data.tables and compare to their base equivalents in the same row order.
  frame <- data.table::data.table(area = rep(c("west", "east"), each = 2L), age = rep(1:2, 2L),
                                  N = c(30, 70, 60, 40), key = c("area", "age"))
  seed <- frame[rep(seq_len(nrow(frame)), each = 3L)]
  seed$vote <- rep(c("No Vote", "A", "B"), nrow(frame))
  seed$N <- seed$N * rep(c(.4, .3, .3), nrow(frame))
  seed$N_draw_1 <- seed$N
  data.table::setkeyv(seed, c("area", "age", "vote"))
  data.table::setattr(seed, "row.names", paste0("outcome-", seq_len(nrow(seed))))
  targets <- data.table::data.table(area = rep(c("west", "east"), each = 3L),
                                    vote = rep(c("A", "B", "No Vote"), 2L),
                                    target = rep(c(.5, .2, .3), 2L), key = c("area", "vote"))
  inputs <- list(frame, seed, targets)
  saved <- lapply(inputs, data.table::copy)
  base_frame <- as.data.frame(frame)
  base_seed <- as.data.frame(seed, row.names = rownames(seed))
  base_targets <- as.data.frame(targets)

  batches <- function(x) geographic_batches(x, "area", areas_per_batch = 1L)
  stopifnot(identical(batches(frame), batches(base_frame)))
  single <- rake_extended_frame(seed, frame, targets, "area", "vote")
  expected <- rake_extended_frame(base_seed, base_frame, base_targets, "area", "vote")
  stopifnot(identical(single, expected))
  joint <- function(x, f, t) rake_joint_extended_frame(
    x, f, list(vote = t), by = "area", area_variable = "area", areas_per_batch = 1L)
  stopifnot(identical(joint(seed, frame, targets), joint(base_seed, base_frame, base_targets)))

  # Count checks aggregate by cell; turnout diagnostics convert a copy of the target shares.
  checks <- function(x, f, t) rake_with_diagnostics(
    x, f, t, "area", "vote", no_vote = "No Vote", output_dir = file.path(scratch, "table-diagnostics"))
  observed <- checks(seed, frame, targets)
  equivalent <- checks(base_seed, base_frame, base_targets)
  stopifnot(identical(observed, equivalent), file.exists(observed$change_plot))
  dt_result <- data.table::as.data.table(single)
  stopifnot(identical(post_rake_cell_comparison(dt_result, frame, "vote"),
                      post_rake_cell_comparison(single, base_frame, "vote")))
  stopifnot(file.exists(plot_raking_changes(seed, single, frame, "area", "vote",
                                             file.path(scratch, "mixed-table-plot.png"))))

  area_rows <- batches(frame)
  split_parts <- function(x) lapply(area_rows, function(i) x[x$area %in% frame$area[i], ])
  partition_plot <- function(x, raked, f, file) {
    plot_partitioned_raking_changes(split_parts(x), split_parts(raked), f, area_rows, "area", "vote",
                                    "No Vote", file, read_extended = identity, read_raked = identity)
  }
  tab_plot <- partition_plot(seed, dt_result, frame, file.path(scratch, "table-parts.png"))
  df_plot <- partition_plot(base_seed, single, base_frame, file.path(scratch, "base-parts.png"))
  stopifnot(identical(unname(tools::md5sum(tab_plot)), unname(tools::md5sum(df_plot))))

  # Exercise the normal MasterP entry point, including survey, rake, and plot targets.
  survey <- as.data.frame(seed)[rep(seq_len(nrow(seed)), each = 12L), c("area", "age", "vote")]
  spec <- function(s, t) list(vote = list(
    survey = s, fit = list(area_variable = "area", predictors = c("area", "age"),
                           depth = 1, splines = FALSE, max_iterations = 5L),
    rake = list(local = list(targets = t, by = "area")),
    plots = list(local = list(targets = t, by = "area", draw_plot = FALSE))))
  run <- function(f, s, t) {
    set.seed(2026)
    MasterP(f, spec(s, t), n_draws = 2L, aggregate_by = list(local = "area"),
            areas_per_batch = 1L, log_file = file.path(scratch, "table-masterp.log"))
  }
  df_result <- run(base_frame, survey, base_targets)
  tab_result <- run(frame, data.table::as.data.table(survey), targets)
  fields <- c("frame", "aggregates", "diagnostics")
  stopifnot(identical(tab_result[fields], df_result[fields]), identical(inputs, saved))
}
