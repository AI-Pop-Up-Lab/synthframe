check_data_table_inputs <- function(scratch) {
  # Include a key, a column named 'columns', reordered draw columns, and explicit area IDs.
  frame <- data.frame(area = c("02", "01", "03"), columns = factor(c("y", "x", "z")),
                      N = c(30, 40, 50), N_draw_2 = c(32, 41, 47), N_draw_1 = c(28, 39, 53))
  frame <- frame[order(frame$area), ]
  rownames(frame) <- NULL
  survey <- frame[rep(seq_len(nrow(frame)), each = 20), c("area", "columns")]
  survey$vote <- rep(c("A", "B"), 30)
  raw <- data.frame(area = rep(frame$area, each = 2L), vote = rep(c("A", "B"), 3),
                    count = c(2, 8, 3, 7, 4, 6))
  custom <- data.frame(score = c(0, -1, 1), row.names = c("03", "02", "01"))
  tables <- lapply(list(frame = frame, survey = survey, raw = raw, custom = custom),
                   data.table::as.data.table)
  data.table::setkeyv(tables$frame, "area")
  rownames(tables$custom) <- rownames(custom)
  snapshots <- lapply(tables, data.table::copy)
  shares <- function(x) process_area_covariates(list(historical = x),
      "area", "vote", "count", outcome_order = c("A", "B"))
  stopifnot(identical(shares(tables$raw), shares(raw)))

  fit <- function(survey, frame, custom) {
    set.seed(732)
    fit_sequential_logit(survey, "vote", "area", predictors = "area", frame = frame,
                         area_covariates = list(A = custom), outcome_order = c("A", "B"),
                         depth = 0.75, splines = FALSE, max_iterations = 5L,
                         log_file = file.path(scratch, "table-fit.csv"))
  }
  plain <- fit(survey, frame, custom)
  tabular <- fit(tables$survey, tables$frame, tables$custom)
  stopifnot(identical(plain$survey_counts, tabular$survey_counts),
            identical(tabular$area_covariates$A, custom),
            identical(plain$stages$A$frame_levels, tabular$stages$A$frame_levels))
  # Prediction also accepts custom tables from a previously saved fit.
  tabular$area_covariates$A <- tables$custom
  predict <- function(model, frame) {
    set.seed(203)
    predict_sequential_logit(model, frame, write_files = FALSE)
  }
  base_posterior <- predict(plain, frame)
  table_posterior <- predict(tabular, tables$frame)
  stopifnot(isTRUE(all.equal(base_posterior, table_posterior)),
            dim(table_posterior$draws)[3L] == 2L)
  stage <- tabular$stages$A
  encoded <- prepare_prediction_stage(tabular, stage, "A", frame)
  encoded_table <- data.table::as.data.table(encoded)
  rownames(encoded_table) <- rownames(encoded)
  stopifnot(identical(register_frame_levels(stage$fit, encoded_table), stage$frame_levels),
            identical(predict_vglmer_levels(stage$fit, encoded_table, 0L),
                      predict_vglmer_levels(stage$fit, encoded, 0L)))

  # Posterior matching must still pair the correct draws when supplied a reordered subset.
  subset <- frame[c(3L, 1L), ]
  tab_subset <- data.table::as.data.table(subset)
  stopifnot(identical(aggregate_extended_frame(subset, base_posterior, "columns"),
                      aggregate_extended_frame(tab_subset, table_posterior, "columns")),
            identical(collapse_extended_frame(subset, base_posterior),
                      collapse_extended_frame(tab_subset, table_posterior)))

  sample_rows <- function(x, sizes = NULL) {
    set.seed(341)
    sample_extended_frame(x, c("columns", "N"), partition_sizes = sizes, size = 3L)
  }
  stopifnot(identical(sample_rows(frame), sample_rows(tables$frame)))
  save_parts <- function(x, prefix) {
    lapply(list(1L, 2:3), function(rows) {
      p <- file.path(scratch, paste0(prefix, "-", rows[1], ".rds"))
      saveRDS(x[rows, , drop = FALSE], p)
      p
    })
  }
  base_parts <- save_parts(frame, "plain")
  table_parts <- save_parts(tables$frame, "table")
  # Sampling a partition reports the selected rows, independent of stored row names.
  plain_sample <- sample_rows(base_parts, c(1L, 2L))
  table_sample <- sample_rows(table_parts, c(1L, 2L))
  rownames(plain_sample) <- rownames(table_sample) <- NULL
  stopifnot(identical(plain_sample, table_sample), identical(tables, snapshots))
}
