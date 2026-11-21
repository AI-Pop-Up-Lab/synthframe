# Fit each outcome except the last using respondents still eligible at that stage.
fit_sequential_logit <- function(auxiliary_survey, extension_variable, area_variable,
                                 area_covariates = NULL, no_vote = NULL,
                                 depth = 1L, splines = TRUE, predictors = NULL,
                                 control = NULL, log_file = tempfile(
                                   "voteframe-fit-", tmpdir = ".", fileext = ".csv"),
                                 frame = NULL, outcome_order = NULL) {
  # The outcome and area ID are separate survey columns.
  if (!is.character(extension_variable) || length(extension_variable) != 1L ||
      !is.character(area_variable) || length(area_variable) != 1L ||
      anyNA(c(extension_variable, area_variable)) ||
      any(!nzchar(c(extension_variable, area_variable))) ||
      identical(extension_variable, area_variable)) {
    stop("Supply distinct extension and area variable names.")
  }
  if (!is.numeric(depth) || length(depth) != 1L || !is.finite(depth) ||
      !(depth %in% c(0, 0.5, 0.75) || (depth >= 1 && depth == floor(depth)))) {
    stop("depth must be 0, 0.5, 0.75, or a positive integer.")
  }
  if (!is.logical(splines) || length(splines) != 1L || is.na(splines)) {
    stop("splines must be TRUE or FALSE.")
  }
  # Use all survey predictors by default. The area ID is always needed.
  if (is.null(predictors)) predictors <- setdiff(names(auxiliary_survey), extension_variable)
  predictors <- unique(c(area_variable, predictors))
  # The optional frame defines population support for the fitted terms.
  if (!is.null(frame)) {
    if (!is.data.frame(frame) || !nrow(frame) || anyDuplicated(names(frame)) ||
        !all(predictors %in% names(frame)) || anyNA(frame[, predictors, drop = FALSE])) {
      stop("Supply a nonempty frame with unique column names and complete survey predictors.")
    }
    frame <- as.data.frame(frame)
  }
  survey <- aggregate_extension_survey(auxiliary_survey, extension_variable,
                                       predictors, no_vote, outcome_order = outcome_order)
  outcomes <- survey$outcome_order
  # The last outcome gets whatever probability remains.
  stages <- utils::head(outcomes, -1L)
  # Convert the area data now that the survey has set the order.
  if (depth >= 0.75 &&
      inherits(area_covariates, c("voteframe_area_covariate", "voteframe_area_covariate_list"))) {
    area_covariates <- conditional_area_shares(area_covariates, outcomes)
  }
  # At depths that use area predictors, every fitted outcome needs its own matrix.
  if (depth >= 0.75 && !is.null(area_covariates) &&
      (!is.list(area_covariates) || anyDuplicated(names(area_covariates)) ||
       !all(stages %in% names(area_covariates)) ||
       any(vapply(area_covariates[stages], is.null, logical(1))))) {
    stop("Supply an area covariate matrix named for each fitted outcome.")
  }
  if (!is.character(log_file) || length(log_file) != 1L ||
      is.na(log_file) || !nzchar(log_file)) stop("Supply a path for the fit log.")

  # Reuse the prediction encoder so the frame uses exactly the fitted term labels.
  prediction_inputs <- list(predictors = predictors, area_variable = area_variable,
                            area_covariates = area_covariates)
  # Population counts and extra frame columns play no role in registering levels.
  frame_predictors <- if (!is.null(frame)) unique(frame[, predictors, drop = FALSE])
  fits <- stats::setNames(vector("list", length(stages)), stages)
  fit_log <- data.frame()
  for (i in seq_along(stages)) {
    outcome <- stages[i]
    stage_label <- paste0("Stage ", i, "/", length(stages), " ", sQuote(outcome))
    message(stage_label, " — preparing data and selecting depth.")
    # The current outcome is a success. Every later outcome is a failure.
    successes <- as.numeric(survey$counts[, i])
    failures <- rowSums(survey$counts[, seq.int(i + 1L, length(outcomes)), drop = FALSE])
    # Drop cells where everyone chose an earlier outcome.
    at_risk <- successes + failures > 0
    stage <- survey$cells[at_risk, , drop = FALSE]
    # Join only after filtering cells. Earlier outcomes have left the risk set.
    if (depth >= 0.75) {
      stage <- join_stage_covariates(stage, area_variable, area_covariates[[outcome]])
    }
    area_predictors <- setdiff(names(stage), predictors)
    # Count respondents, including those who share a cell.
    observations <- sum(successes + failures)
    selection <- select_sequential_stage_design(
      stage, area_variable, depth, splines, area_predictors, observations
    )
    design <- selection$design
    candidate <- design$depth
    fit_log <- rbind(fit_log, data.frame(
      stage = i, outcome = outcome, observations = observations,
      cells = sum(at_risk), omitted_survey_rows = survey$omitted, requested_depth = depth,
      fitted_depth = candidate, downgraded = candidate < depth,
      estimated_parameters = design$parameter_count,
      fixed_coefficients = design$fixed_coefficients,
      random_coefficients = design$random_coefficients,
      spline_coefficients = design$spline_coefficients,
      observations_per_parameter = observations / design$parameter_count,
      candidate_depths_and_counts = paste(selection$attempted, collapse = "; "),
      dropped_constant_predictors = paste(design$constant_predictors, collapse = "; "),
      dropped_fixed_columns = paste(design$aliased_fixed, collapse = "; "),
      frame_only_random_coefficients = NA_integer_,
      frame_only_fixed_coefficients = NA_integer_,
      engine = "vglmer", status = "started", error = ""
    ))
    # Save the depth decision before calling the fitting engine.
    utils::write.csv(fit_log, log_file, row.names = FALSE)
    # Add the two counts only after building the predictor formula.
    # Both count vectors follow the same row order as the filtered stage data.
    design$data$.success <- successes[at_risk]
    design$data$.failure <- failures[at_risk]
    design <- tryCatch(
      {
        if (length(design$constant_predictors)) {
          warning(stage_label, " omitted constant predictors: ",
                  paste(design$constant_predictors, collapse = ", "), call. = FALSE)
        }
        if (length(design$aliased_fixed)) {
          warning(stage_label, " omitted linearly dependent fixed columns: ",
                  paste(design$aliased_fixed, collapse = ", "), call. = FALSE)
        }
        if (observations < design$parameter_count) {
          warning("Outcome ", sQuote(outcome), " exceeds the coefficient limit at depth 0 (",
                  design$parameter_count, " estimated coefficients for ", observations,
                  " eligible respondents). Attempting the depth 0 fit.",
                  call. = FALSE, immediate. = TRUE)
        }
        if (!design$hierarchical) {
          stop("The selected depth has no random effect or spline. An area random ",
               "intercept requires at least two observed areas.")
        }
        if (!requireNamespace("vglmer", quietly = TRUE)) stop("Install vglmer to fit the model.")
        if (design$joint_smooth && !requireNamespace("gKRLS", quietly = TRUE)) {
          stop("Install gKRLS to fit joint smooths of multiple continuous predictors.")
        }
        if (!is.null(frame_predictors)) {
          # Prepare the population design for this stage's selected formula.
          frame_stage <- prepare_prediction_stage(prediction_inputs, design, outcome,
                                                  frame_predictors)
        }
        message(stage_label, " — fitting at depth ", candidate,
                if (candidate < depth) paste0(" (requested ", depth, ")"),
                " with ", observations, " eligible respondents and ", design$parameter_count,
                " estimated coefficients.")
        # Resolve v_s() inside vglmer's namespace without attaching the package.
        environment(design$formula) <- asNamespace("vglmer")
        if (is.null(control)) control <- vglmer::vglmer_control()
        design$fit <- withCallingHandlers(
          vglmer::vglmer(design$formula, data = design$data, family = "binomial",
                         control = control),
          warning = function(w) message(stage_label, " — warning from vglmer.")
        )
        if (!is.null(frame_predictors)) {
          message(stage_label, " — registering frame levels.")
          # With zero survey trials, frame-only effects have no likelihood contribution.
          # Integrate them out during fitting and draw them from their term at prediction.
          design$frame_levels <- register_frame_levels(design$fit, frame_stage)
        }
        design
      },
      error = function(error) {
        # Keep the failed stage and its error in the same log.
        fit_log$status[i] <- "failed"
        fit_log$error[i] <- conditionMessage(error)
        utils::write.csv(fit_log, log_file, row.names = FALSE)
        stop("Stage for outcome ", sQuote(outcome), " failed: ",
             conditionMessage(error), ". Fit log at ", log_file, call. = FALSE)
      }
    )
    # Count extra coefficient columns, including multiple slopes for one new level.
    if (!is.null(design$frame_levels)) {
      fit_log$frame_only_random_coefficients[i] <-
        sum(lengths(lapply(design$frame_levels$random, function(term) term$frame_only)))
      fit_log$frame_only_fixed_coefficients[i] <- length(design$frame_levels$fixed$frame_only)
    }
    # A returned model may still carry convergence diagnostics from vglmer.
    fit_log$status[i] <- "returned"
    utils::write.csv(fit_log, log_file, row.names = FALSE)
    fits[[outcome]] <- design
    message(stage_label, " — model returned (", i, " of ", length(stages), " completed).")
  }

  # Save the stage designs and area covariates needed to rebuild prediction data.
  no_vote <- if (length(no_vote) && !is.na(no_vote)) as.character(no_vote)
  structure(list(stages = fits, outcome_order = outcomes, no_vote = no_vote,
                 survey_counts = survey$counts,
                 survey_cells = survey$cells, omitted_rows = survey$omitted,
                 predictors = predictors, area_variable = area_variable,
                 extension_variable = extension_variable, area_covariates = area_covariates,
                 frame = frame, fit_log = fit_log, log_file = normalizePath(log_file)),
            class = "voteframe_sequential_logit")
}

# Select the deepest candidate supported by the stage's eligible respondents.
select_sequential_stage_design <- function(stage, area_variable, depth, splines,
                                           area_predictors, observations) {
  # Start from the requested depth at every stage.
  # Interaction orders beyond the number of predictors give the same formula.
  # Include the requested depth once, then try smaller depth values.
  next_integer <- min(depth - 1, ncol(stage) - 1)
  lower_integers <- if (next_integer >= 1) seq.int(next_integer, 1)
  candidate_depths <- unique(c(depth, lower_integers, 0.75, 0.5, 0))
  candidate_depths <- candidate_depths[candidate_depths <= depth]
  attempted <- character()
  for (candidate in candidate_depths) {
    design <- sequential_stage_design(stage, area_variable, candidate, splines,
                                      area_predictors)
    attempted <- c(attempted, paste0(candidate, "=", design$parameter_count))
    # Accept equality. One respondent per estimated coefficient is enough.
    if (observations >= design$parameter_count) break
  }
  design$depth <- candidate
  list(design = design, attempted = attempted)
}
