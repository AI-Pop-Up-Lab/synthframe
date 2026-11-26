# Draw outcome probabilities for each cell and save the draws and extended frame.
predict_sequential_logit <- function(fit, frame = fit$frame, n_draws = 125L, count_variable = "N",
                                     output_dir = "data/output", allow_missing_levels = TRUE,
                                     new_fixed_sd = NULL, write_files = TRUE,
                                     draw_state = NULL, batch_size = nrow(frame), progress = NULL) {
  if (!inherits(fit, "voteframe_sequential_logit")) stop("Supply a sequential logit fit.")
  # Use the fit's population frame unless the caller supplies a different one.
  if (is.null(frame)) stop("Supply a prediction frame or pass frame when fitting the model.")
  frame <- as_plain_frame(frame)
  count_draws <- prediction_count_draws(frame, count_variable)
  if (missing(n_draws) && length(count_draws)) n_draws <- length(count_draws)
  if (!is.numeric(n_draws) || length(n_draws) != 1L || !is.finite(n_draws) ||
      n_draws < 1 || n_draws != floor(n_draws) || n_draws > .Machine$integer.max) {
    stop("n_draws must be a positive integer.")
  }
  if (!is.logical(allow_missing_levels) || length(allow_missing_levels) != 1L ||
      is.na(allow_missing_levels)) stop("allow_missing_levels must be TRUE or FALSE.")
  if (!is.logical(write_files) || length(write_files) != 1L || is.na(write_files)) {
    stop("write_files must be TRUE or FALSE.")
  }
  if (!is.numeric(batch_size) || length(batch_size) != 1L || !is.finite(batch_size) ||
      batch_size < 1 || batch_size != floor(batch_size)) {
    stop("batch_size must be a positive integer.")
  }
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) ||
      !nzchar(output_dir)) stop("Supply an output directory.")

  # Counts and count draws are payload, so only other columns identify a cell.
  cell_columns <- setdiff(names(frame), c(count_variable, count_draws))
  validate_prediction_frame(frame, cell_columns, count_variable)
  cells <- frame[, cell_columns, drop = FALSE]
  outcomes <- fit$outcome_order
  if (is.null(draw_state) && batch_size < nrow(frame)) draw_state <- new.env(parent = emptyenv())
  if (!is.null(draw_state)) {
    bind_prediction_draw_state(draw_state, fit, n_draws, new_fixed_sd)
    if (is.null(draw_state$stages)) {
      draw_state$stages <- lapply(fit$stages, function(stage) new.env(parent = emptyenv()))
    }
  }
  draw_names <- paste0(count_variable, "_draw_", seq_len(n_draws))
  if (length(count_draws) && length(count_draws) != n_draws) {
    stop("n_draws must match the number of incoming population draws (",
         length(count_draws), ").")
  }
  if (fit$extension_variable %in% c(names(frame), draw_names)) {
    stop("The new extension column must have a distinct name.")
  }

  # The axes are frame cell, outcome, and posterior draw.
  draws <- array(0, dim = c(nrow(frame), length(outcomes), n_draws),
                 dimnames = list(NULL, outcomes, draw_names))
  # Every cell starts with all its probability available.
  remaining <- matrix(1, nrow(frame), n_draws)
  starts <- seq.int(1L, nrow(frame), by = batch_size)
  total_batches <- length(starts) * length(fit$stages)
  if (!is.null(progress)) progress(0L, total_batches, "prediction batches across outcome stages")
  for (i in seq_along(fit$stages)) {
    outcome <- outcomes[i]
    stage <- fit$stages[[outcome]]
    newdata <- prepare_prediction_stage(fit, stage, outcome, cells)
    # Each draw combines fitted coefficients with any frame-only effects.
    for (batch in seq_along(starts)) {
      start <- starts[batch]
      rows <- seq.int(start, min(start + batch_size - 1L, nrow(frame)))
      logits <- tryCatch(
        predict_vglmer_levels(stage$fit, newdata[rows, , drop = FALSE], n_draws,
                              allow_missing_levels = allow_missing_levels,
                              new_fixed_sd = new_fixed_sd,
                              draw_state = draw_state$stages[[outcome]]),
        error = function(error) stop("Prediction failed for ", sQuote(outcome), ": ",
                                      conditionMessage(error), call. = FALSE)
      )
      if (!identical(dim(logits), c(as.integer(n_draws), length(rows))) ||
          any(!is.finite(logits))) stop("Invalid posterior draws for ", sQuote(outcome), ".")
      logits <- t(as.matrix(logits))
      conditional <- stats::plogis(logits)
      draws[rows, i, ] <- remaining[rows, , drop = FALSE] * conditional
      # The opposite tail stays positive when the conditional rounds to one.
      remaining[rows, ] <- remaining[rows, , drop = FALSE] * stats::plogis(-logits)
      if (!is.null(progress)) {
        progress((i - 1L) * length(starts) + batch, total_batches,
                 sprintf("%s, cells %d–%d", outcome, start, tail(rows, 1L)))
      }
    }
  }
  # The last outcome has no fitted stage. It receives the remaining probability.
  draws[, length(outcomes), ] <- remaining

  paths <- NULL
  if (write_files) {
    if (!dir.exists(output_dir) &&
        !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE) && !dir.exists(output_dir)) {
      stop("Could not create prediction output directory ", sQuote(output_dir), ".")
    }
    paths <- file.path(normalizePath(output_dir, mustWork = TRUE),
                       c("posterior_probabilities.rds", "extended_frame.csv"))
  }
  # Save the cell keys alongside the draws so later calls can match by value.
  posterior <- structure(list(cells = cells,
                              draws = draws, extension_variable = fit$extension_variable,
                              count_variable = count_variable,
                              paired_population_draws = length(count_draws) > 0L,
                              allow_missing_levels = allow_missing_levels,
                              new_fixed_sd = new_fixed_sd,
                              posterior_file = if (write_files) paths[1L],
                              extended_frame_file = if (write_files) paths[2L]),
                         class = "voteframe_posterior")
  if (!write_files) return(posterior)
  # Finish both files before replacing the previous exports.
  temporary <- tempfile(".voteframe-stage-", tmpdir = dirname(paths), fileext = c(".rds", ".csv"))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(posterior, temporary[1L])
  # Write the full frame in blocks so a second copy of all draws stays off the heap.
  starts <- seq.int(1L, nrow(frame), by = 1000L)
  if (!is.null(progress)) progress(0L, length(starts), "frame export batches")
  for (batch in seq_along(starts)) {
    start <- starts[batch]
    rows <- seq.int(start, min(start + 999L, nrow(frame)))
    block <- expand_expected_counts(frame[rows, cell_columns, drop = FALSE],
                                    draws[rows, , , drop = FALSE],
                                    frame[[count_variable]][rows], posterior,
                                    population_draws = if (length(count_draws)) {
                                      as.matrix(frame[rows, count_draws, drop = FALSE])
                                    })
    utils::write.table(block, temporary[2L], sep = ",", qmethod = "double", row.names = FALSE,
                       col.names = start == 1L, append = start != 1L)
    if (!is.null(progress)) {
      progress(batch, length(starts), sprintf("exported cells %d–%d", start, tail(rows, 1L)))
    }
  }
  publish_prediction_exports(temporary, paths)
  posterior
}

# Serialize the two renames. Only the RDS needs a backup: the CSV stays at its
# old path until the last rename succeeds. A hard link avoids copying a large RDS.
publish_prediction_exports <- function(staged, paths) {
  lock <- file.path(dirname(paths[1L]), ".voteframe-publish")
  if (!dir.create(lock, showWarnings = FALSE)) {
    stop("Cannot acquire the prediction export lock at ", sQuote(lock),
         ". Check for an active or interrupted export before retrying.")
  }
  cleanup <- TRUE
  on.exit(if (cleanup) unlink(lock, recursive = TRUE), add = TRUE)
  if (any(file.exists(paths) & !utils::file_test("-f", paths))) {
    stop("Prediction export destinations must be regular files: ", paste(paths, collapse = ", "), ".")
  }
  previous <- file.exists(paths[1L])
  backup <- file.path(lock, basename(paths[1L]))
  if (previous && !suppressWarnings(file.link(paths[1L], backup)) &&
      !suppressWarnings(file.copy(paths[1L], backup))) {
    stop("Could not back up the previous posterior at ", sQuote(backup), ".")
  }
  suspendInterrupts({
    if (!suppressWarnings(file.rename(staged[1L], paths[1L]))) {
      stop("Could not publish the posterior file at ", sQuote(paths[1L]), ".")
    }
    if (!suppressWarnings(file.rename(staged[2L], paths[2L]))) {
      restored <- if (previous) suppressWarnings(file.rename(backup, paths[1L])) else
        suppressWarnings(file.remove(paths[1L]))
      if (!restored) {
        cleanup <- FALSE
        stop("Could not roll back an incomplete prediction export at ", sQuote(paths[1L]),
             ". Recovery directory: ", sQuote(lock), ".")
      }
      stop("Could not publish the frame CSV at ", sQuote(paths[2L]),
           ". The posterior change was rolled back.")
    }
  })
}

# Trace the solver's input columns back through VoteFrame's generated terms.
stage_prediction_columns <- function(stage) {
  model <- stage$fit
  required <- c(model$formula$interpret_gam$pred.names, names(model$reduced_fixed_levels),
                unname(model$screened_smooth_groups))
  for (term in model$screened_random) {
    required <- c(required, term$group, all.vars(stats::reformulate(term$slopes)))
  }
  # Older saved fits can contain random terms that are drawn only at prediction.
  for (group in names(model$prior_random)) {
    required <- c(required, group, all.vars(stats::reformulate(model$prior_random[[group]])))
  }
  groups <- intersect(names(stage$grouping_columns), required)
  contrasts <- intersect(names(stage$fixed_contrasts), required)
  required <- c(required, unlist(stage$grouping_columns[groups], use.names = FALSE),
                vapply(stage$fixed_contrasts[contrasts], function(x) x$predictor, character(1)))
  list(predictors = names(stage$predictor_names)[stage$predictor_names %in% required],
       groups = groups, contrasts = contrasts)
}

prepare_prediction_stage <- function(fit, stage, outcome, frame, unique_rows = FALSE) {
  columns <- stage$prediction_columns
  if (is.null(columns)) columns <- stage_prediction_columns(stage)
  survey_columns <- intersect(columns$predictors, fit$predictors)
  area_columns <- setdiff(columns$predictors, survey_columns)
  if (length(area_columns)) survey_columns <- union(survey_columns, fit$area_variable)
  missing <- setdiff(survey_columns, names(frame))
  if (length(missing)) {
    stop("Frame is missing predictors for ", sQuote(outcome), ": ", paste(missing, collapse = ", "), ".")
  }
  newdata <- frame[, survey_columns, drop = FALSE]
  if (anyNA(newdata)) stop("Frame predictors for ", sQuote(outcome), " must be complete.")
  if (unique_rows) newdata <- unique(newdata)
  if (length(area_columns)) {
    covariates <- as_plain_frame(fit$area_covariates[[outcome]])
    missing <- setdiff(area_columns, colnames(covariates))
    if (length(missing)) {
      stop("Missing area covariates for ", sQuote(outcome), ": ", paste(missing, collapse = ", "), ".")
    }
    newdata <- join_stage_covariates(newdata, fit$area_variable,
                                     covariates[, area_columns, drop = FALSE])
  }
  # Restore the formula-safe column names used in the fitted model.
  names(newdata) <- unname(stage$predictor_names[names(newdata)])
  for (column in names(newdata)) {
    if (is.factor(stage$data[[column]])) {
      # Keep new levels visible so vglmer can apply its missing-level policy.
      newdata[[column]] <- factor(newdata[[column]])
    } else if (!is.numeric(newdata[[column]]) || any(!is.finite(newdata[[column]]))) {
      stop("Frame predictor ", sQuote(column), " must contain finite numeric values.")
    }
  }
  # Rebuild crossed group labels in the same way as during fitting.
  for (group in columns$groups) {
    newdata[[group]] <- sequential_group(newdata, stage$grouping_columns[[group]])
  }
  for (column in columns$contrasts) {
    contrast <- stage$fixed_contrasts[[column]]
    values <- unname(contrast$values[as.character(newdata[[contrast$predictor]])])
    if (anyNA(values) && is.null(contrast$baseline)) {
      stop("A rank-reduced fixed factor has unseen levels in ", sQuote(contrast$predictor), ".")
    }
    if (anyNA(values)) values[is.na(values)] <- contrast$baseline
    newdata[[column]] <- values
  }
  newdata
}
