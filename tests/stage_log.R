check_stage_log <- function() {
  log_file <- tempfile(fileext = ".csv")
  on.exit(unlink(log_file))
  areas <- c("north", "south", "east")
  choices <- c("A", "B", "C")
  survey <- expand.grid(area = areas, vote = choices, respondent = seq_len(12L),
                        stringsAsFactors = FALSE)
  frame <- data.frame(area = areas, N = c(100, 150, 200))
  stage_inputs <- list(A = data.frame(first_x = c(1, 0, 3), row.names = areas),
                       B = data.frame(second_z = c(4, 1, 2), row.names = areas))
  fit <- function(inputs) {
    fit_sequential_logit(survey, "vote", "area", predictors = "area",
                         outcome_order = choices, area_covariates = inputs, frame = frame,
                         depth = 1L, splines = FALSE, max_iterations = 5L, log_file = log_file)
  }
  result <- fit(stage_inputs)
  stopifnot(identical(names(result$stages$A$predictor_names), c("area", "first_x")),
            identical(names(result$stages$B$predictor_names), c("area", "second_z")),
            all(result$fit_log$status == "returned"), all(result$fit_log$fitted_depth == 1),
            all(result$fit_log$estimated_retained_parameters > 0),
            all(result$fit_log$areas == length(areas)))
  prediction <- predict_sequential_logit(result, n_draws = 2L, write_files = FALSE)
  stopifnot(max(abs(apply(prediction$draws, c(1L, 3L), sum) - 1)) < 1e-12)

  # Stage A's completed diagnostics survive the missing area in stage B.
  missing_area <- stage_inputs
  missing_area$B <- missing_area$B[areas != "east", , drop = FALSE]
  error <- tryCatch(fit(missing_area), error = conditionMessage)
  log <- utils::read.csv(log_file)
  stopifnot(is.character(error), grepl("Missing area covariates for east", error, fixed = TRUE),
            identical(log$status, c("returned", "failed")), identical(log$outcome, c("A", "B")),
            log$observations[2L] == 72L, log$cells[2L] == 3L, log$areas[2L] == 3L,
            log$estimated_retained_parameters[1L] > 0, is.na(log$fitted_depth[2L]),
            grepl("Missing area covariates for east", log$error[2L], fixed = TRUE))

  # A first-stage design failure replaces any previous run's rows at the same path.
  invalid <- stage_inputs
  invalid$A$first_x[1L] <- Inf
  error <- tryCatch(fit(invalid), error = conditionMessage)
  log <- utils::read.csv(log_file)
  stopifnot(is.character(error), nrow(log) == 1L, log$stage == 1L, log$outcome == "A",
            log$status == "failed", log$observations == nrow(survey), log$areas == 3L,
            is.na(log$fitted_depth), grepl("first_x", log$error, fixed = TRUE))
}
