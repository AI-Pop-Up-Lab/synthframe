# Frame registration and prediction must derive their inputs from the fitted stage.
check_prediction_design <- function(scratch) {
  survey <- data.frame(area = rep(c("a", "b", "c"), c(40, 50, 4)),
                       vote = rep(c("yes", "no"), 47L), unused = "constant")
  historical <- list(yes = data.frame(past = c(0, 0, 1), row.names = c("a", "b", "c")))
  frame <- data.frame(area = c("a", "c", "d"), N = 50)
  fit <- suppressWarnings(fit_sequential_logit(
    survey, "vote", "area", outcome_order = c("yes", "no"), depth = 2L,
    splines = FALSE, minimum_support = 10L, area_covariates = historical, frame = frame,
    log_file = file.path(scratch, "screened-prediction.csv")))
  stage <- fit$stages$yes
  stopifnot("past" %in% all.vars(stage$candidate_formula),
            !"past" %in% all.vars(stage$formula),
            identical(stage$formula, stats::formula(stage$fit)),
            identical(stage$prediction_columns$predictors, "area"),
            fit$fit_log$frame_only_random_coefficients > 0L)

  # Older saved fits stored the candidate in $formula and no dependency table.
  legacy <- fit
  legacy$stages$yes$prediction_columns <- NULL
  legacy$stages$yes$formula <- stage$candidate_formula
  legacy$stages$yes$candidate_formula <- NULL
  set.seed(10)
  current <- predict_sequential_logit(fit, n_draws = 2L, write_files = FALSE)
  set.seed(10)
  restored <- predict_sequential_logit(legacy, n_draws = 2L, write_files = FALSE)
  stopifnot(all(is.finite(current$draws)), identical(current$draws, restored$draws))

  # A partially screened categorical area predictor retains a generated fixed
  # contrast. It still needs its area lookup even if the frame has a namesake.
  historical$yes$past <- c("A", "B", "C")
  retained <- suppressWarnings(fit_sequential_logit(
    survey, "vote", "area", outcome_order = c("yes", "no"), depth = 0.75,
    splines = FALSE, minimum_support = 10L, area_covariates = historical,
    frame = frame[1:2, ], log_file = file.path(scratch, "retained-prediction.csv")))
  stage <- retained$stages$yes
  stopifnot(length(stage$prediction_columns$contrasts) == 1L,
            setequal(stage$prediction_columns$predictors, c("area", "past")))
  missing_area <- tryCatch(predict_sequential_logit(
    retained, transform(frame, past = "A"), n_draws = 1L, write_files = FALSE),
    error = conditionMessage)
  missing_key <- tryCatch(predict_sequential_logit(
    retained, data.frame(id = "a", N = 50), n_draws = 1L, write_files = FALSE),
    error = conditionMessage)
  stopifnot(is.character(missing_area), grepl("Missing area covariates for d", missing_area),
            is.character(missing_key), grepl("missing predictors.*area", missing_key))

  # A retained crossed group, masked varying slope, and group-specific spline
  # need the original variables underneath their generated solver column names.
  grid <- expand.grid(area = c("a", "b", "c"), segment = c("x", "y"), age = 0:7,
                      stringsAsFactors = FALSE)
  respondents <- grid[rep(seq_len(nrow(grid)), ifelse(grid$area == "c", 2L, 12L)), ]
  respondents$vote <- rep(c("yes", "no"), nrow(respondents) / 2L)
  model <- fit_sequential_logit(respondents, "vote", "area", outcome_order = c("yes", "no"),
                                depth = 2L, minimum_support = 50L,
                                frame = transform(grid, N = 10),
                                log_file = file.path(scratch, "grouped-prediction.csv"))
  stage <- model$stages$yes
  stopifnot(length(stage$prediction_columns$groups) > 0L,
            length(stage$fit$screened_smooth_groups) > 0L,
            setequal(stage$prediction_columns$predictors, names(grid)))
  encoded <- prepare_prediction_stage(model, stage, "yes", grid)
  fitted_rows <- match(frame_cell_keys(grid, grid), frame_cell_keys(stage$data[names(grid)], grid))
  stopifnot(isTRUE(all.equal(
    unname(predict_vglmer_levels(stage$fit, encoded, 0L)),
    unname(predict_vglmer_levels(stage$fit, stage$data[fitted_rows, ], 0L)))))
}
