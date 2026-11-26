check_prediction_exports <- function(scratch) {
  label <- 'North "A", Centre'
  survey <- data.frame(area = rep(c("01", "02"), each = 100L),
                       vote = rep(c(label, "Other"), 100L))
  fit <- fit_sequential_logit(survey, "vote", "area", depth = 0L,
                              log_file = file.path(scratch, "export-fit.csv"))
  # Include a second CSV block, an embedded quote, and an ID that requires a
  # character column on import.
  frame <- data.frame(area = rep_len(c("01", "02"), 1001L), N = 10L)
  frame[["precinct code"]] <- sprintf("%04d", seq_len(nrow(frame)))
  directory <- file.path(scratch, "prediction-exports")
  run <- function(draws, progress = NULL) {
    predict_sequential_logit(fit, frame, n_draws = draws, output_dir = directory, progress = progress)
  }
  check_pair <- function(posterior) {
    csv <- utils::read.csv(posterior$extended_frame_file, check.names = FALSE,
                           colClasses = c(area = "character", "precinct code" = "character",
                                          vote = "character"))
    expected <- aggregate_extended_frame(frame, posterior, by = c("area", "precinct code"))
    stopifnot(identical(readRDS(posterior$posterior_file), posterior),
              isTRUE(all.equal(csv, expected, tolerance = 1e-12)))
  }
  check_pair(run(2L))
  previous <- run(1L)
  check_pair(previous)
  paths <- c(previous$posterior_file, previous$extended_frame_file)
  before <- tools::md5sum(paths)

  # Remove the completed staged CSV after writing and before publication. This
  # makes the second OS rename fail after the first has replaced the public RDS.
  remove_csv <- function(done, total, detail) {
    if (done == total && startsWith(detail, "exported cells ")) {
      staged <- list.files(directory, pattern = "^\\.voteframe-stage-.*\\.csv$", all.files = TRUE,
                           full.names = TRUE)
      stopifnot(length(staged) == 1L, file.remove(staged))
    }
  }
  failure <- tryCatch(run(2L, remove_csv), error = conditionMessage)
  stopifnot(is.character(failure), grepl("rolled back", failure, fixed = TRUE),
            identical(tools::md5sum(paths), before),
            identical(sort(list.files(directory, all.files = TRUE, no.. = TRUE)), sort(basename(paths))))
  check_pair(previous)

  # On a first export the same failure must leave neither public output behind.
  stopifnot(all(file.remove(paths)))
  failure <- tryCatch(run(2L, remove_csv), error = conditionMessage)
  stopifnot(is.character(failure), grepl("rolled back", failure, fixed = TRUE),
            !length(list.files(directory, all.files = TRUE, no.. = TRUE)))
  check_pair(run(1L))
}
