# Fit each outcome except the last using respondents still eligible at that stage.
fit_sequential_logit <- function(auxiliary_survey, extension_variable, area_variable,
                                 area_covariates = NULL, no_vote = NULL,
                                 depth = 1L, splines = TRUE, predictors = NULL,
                                 control = NULL, log_file = tempfile(
                                   "voteframe-fit-", tmpdir = ".", fileext = ".csv"),
                                 frame = NULL, outcome_order = NULL,
                                 no_interactions = character(),
                                 tolerance_parameters = NULL, tolerance_elbo = NULL,
                                 max_iterations = NULL, interaction_support_log = NULL,
                                 depth_support_dir = NULL, minimum_support = -1L,
                                 progress = NULL, solver_trace = FALSE) {
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
  if (is.null(minimum_support)) minimum_support <- -1L
  if (!is.numeric(minimum_support) || length(minimum_support) != 1L ||
      !is.finite(minimum_support) || minimum_support < -1 ||
      minimum_support != floor(minimum_support)) {
    stop("minimum_support must be -1 or a nonnegative integer.")
  }
  if (!is.character(no_interactions) || anyNA(no_interactions) ||
      any(!nzchar(no_interactions))) {
    stop("Supply no_interactions as a character vector of predictor names.")
  }
  tolerances <- list(tolerance_parameters = tolerance_parameters, tolerance_elbo = tolerance_elbo)
  for (name in names(tolerances)) {
    value <- tolerances[[name]]
    if (!is.null(value) &&
        (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0)) {
      stop(name, " must be a finite nonnegative number.")
    }
  }
  if (!is.null(max_iterations) &&
      (!is.numeric(max_iterations) || length(max_iterations) != 1L ||
       !is.finite(max_iterations) || max_iterations < 1 ||
       max_iterations > .Machine$integer.max || max_iterations != floor(max_iterations))) {
    stop("max_iterations must be a positive integer.")
  }
  solver_overrides <- Filter(Negate(is.null), tolerances)
  if (!is.null(max_iterations)) solver_overrides$iterations <- as.integer(max_iterations)
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
  stage_covariates <- if (is.list(area_covariates)) {
    unlist(lapply(area_covariates[intersect(stages, names(area_covariates))], colnames),
           use.names = FALSE)
  }
  unknown <- setdiff(no_interactions, c(predictors, stage_covariates))
  if (length(unknown)) {
    stop("Unknown no_interactions predictors: ", paste(unknown, collapse = ", "), ".")
  }
  if (!is.character(log_file) || length(log_file) != 1L ||
      is.na(log_file) || !nzchar(log_file)) stop("Supply a path for the fit log.")

  if (!is.null(interaction_support_log) &&
      (!is.character(interaction_support_log) || length(interaction_support_log) != 1L ||
       is.na(interaction_support_log) || !nzchar(interaction_support_log))) {
    stop("Supply a path for the interaction support log.")
  }
  if (!is.null(depth_support_dir)) {
    if (!is.character(depth_support_dir) || length(depth_support_dir) != 1L ||
        is.na(depth_support_dir) || !nzchar(depth_support_dir)) {
      stop("Supply a directory for the depth support plots.")
    }
    if (!dir.exists(depth_support_dir) &&
        !dir.create(depth_support_dir, recursive = TRUE, showWarnings = FALSE)) {
      stop("Cannot create depth support directory: ", depth_support_dir)
    }
  }

  # Reuse the prediction encoder so the frame uses exactly the fitted term labels.
  prediction_inputs <- list(predictors = predictors, area_variable = area_variable,
                            area_covariates = area_covariates)
  # Population counts and extra frame columns play no role in registering levels.
  frame_predictors <- if (!is.null(frame)) unique(frame[, predictors, drop = FALSE])
  fits <- stats::setNames(vector("list", length(stages)), stages)
  fit_log <- data.frame()
  support_log <- data.frame()
  if (!is.null(progress)) progress(0L, length(stages), paste("starting", stages[1L]))
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
      stage, area_variable, depth, splines, area_predictors, observations, no_interactions,
      successes[at_risk] + failures[at_risk], minimum_support, successes[at_risk]
    )
    design <- selection$design
    prepared <- selection$prepared
    candidate <- design$depth
    fit_log <- rbind(fit_log, data.frame(
      stage = i, outcome = outcome, observations = observations,
      cells = sum(at_risk), omitted_survey_rows = survey$omitted, requested_depth = depth,
      fitted_depth = candidate, retained_order = prepared$retained_order,
      downgraded = candidate < depth,
      estimated_parameters = design$parameter_count,
      fixed_coefficients = design$fixed_coefficients,
      retained_fixed_coefficients = design$retained_fixed_coefficients,
      estimated_retained_parameters = prepared$estimated_parameters,
      random_coefficients = design$random_coefficients,
      spline_coefficients = design$spline_coefficients,
      observations_per_parameter = observations / prepared$estimated_parameters,
      candidate_depths_and_counts = paste(selection$attempted, collapse = "; "),
      dropped_constant_predictors = paste(design$constant_predictors, collapse = "; "),
      dropped_fixed_columns = paste(design$aliased_fixed, collapse = "; "),
      hierarchy_dropped_fixed_columns = paste(design$orphaned_fixed, collapse = "; "),
      redundant_random_groups = paste(prepared$redundant_random_groups, collapse = "; "),
      hierarchy_dropped_random_terms = paste(prepared$orphaned_random_terms, collapse = "; "),
      unsupported_random_main_effects = paste(prepared$unsupported_random_main_effects, collapse = "; "),
      unsupported_random_interactions = paste(prepared$unsupported_random_interactions, collapse = "; "),
      sparse_random_slopes = paste(names(prepared$sparse_random_slopes), collapse = "; "),
      sparse_random_slope_mean_support = paste(
        as.character(prepared$sparse_random_slopes), collapse = "; "),
      unsupported_fixed_columns = paste(design$unsupported_fixed, collapse = "; "),
      fixed_condition_before = design$fixed_screen$condition_before,
      fixed_condition_after = design$fixed_screen$condition_after,
      fixed_dependency_columns = paste(vapply(names(design$fixed_screen$related_columns),
        function(column) paste0(column, " -> ", paste(design$fixed_screen$related_columns[[column]],
                                                     collapse = ", ")), character(1)), collapse = "; "),
      frame_only_random_coefficients = NA_integer_,
      frame_only_fixed_coefficients = NA_integer_,
      minimum_support = minimum_support,
      dropped_fixed = NA_integer_, dropped_scalar_random = NA_integer_,
      dropped_correlated_random = NA_integer_, dropped_smooth = NA_integer_,
      dropped_smooth_terms = "", dropped_smooth_level_columns = NA_real_,
      max_iterations = if (!is.null(max_iterations)) max_iterations else NA_integer_,
      iterations_used = NA_integer_, max_parameter_change = NA_real_,
      engine = "vglmer", status = "started", error = ""
    ))
    # Save the depth decision before calling the fitting engine.
    utils::write.csv(fit_log, log_file, row.names = FALSE)
    # Add the two counts only after building the predictor formula.
    # Both count vectors follow the same row order as the filtered stage data.
    design$data$.success <- successes[at_risk]
    design$data$.failure <- failures[at_risk]
    if (!is.null(interaction_support_log)) {
      support_log <- rbind(support_log, sequential_interaction_support(design, outcome))
      utils::write.csv(support_log, interaction_support_log, row.names = FALSE)
    }
    design <- tryCatch(
      {
        if (!is.null(depth_support_dir)) {
          plot_path <- file.path(depth_support_dir, paste0(
            utils::URLencode(extension_variable, reserved = TRUE, repeated = TRUE),
            "-stage-", i, ".png"))
          plot_sequential_depth_support(
            stage, area_variable, depth, splines, area_predictors, no_interactions,
            successes[at_risk] + failures[at_risk], design, outcome, plot_path
          )
        }
        if (length(design$constant_predictors)) {
          warning(stage_label, " omitted constant predictors: ",
                  paste(design$constant_predictors, collapse = ", "), call. = FALSE)
        }
        if (length(design$aliased_fixed)) {
          warning(stage_label, " omitted exactly or nearly dependent fixed columns: ",
                  paste(design$aliased_fixed, collapse = ", "), call. = FALSE)
        }
        if (observations < prepared$estimated_parameters) {
          warning("Outcome ", sQuote(outcome), " exceeds the coefficient limit at depth 0 (",
                  prepared$estimated_parameters, " estimated coefficients for ", observations,
                  " eligible respondents). Attempting the depth 0 fit.",
                  call. = FALSE, immediate. = TRUE)
        }
        if (!design$hierarchical) {
          stop("The selected depth has no random effect or spline. An area random ",
               "intercept requires at least two observed areas.")
        }
        if (!requireNamespace("vglmer", quietly = TRUE)) stop("Install vglmer to fit the model.")
        if (prepared$joint_smooth && !requireNamespace("gKRLS", quietly = TRUE)) {
          stop("Install gKRLS to fit joint smooths of multiple continuous predictors.")
        }
        if (!is.null(frame_predictors)) {
          # Prepare the population design for this stage's selected formula.
          frame_stage <- prepare_prediction_stage(prediction_inputs, design, outcome,
                                                  frame_predictors)
        }
        message(stage_label, " — fitting at depth ", candidate,
                if (candidate < depth) paste0(" (requested ", depth, ")"),
                " with ", observations, " eligible respondents and ", prepared$estimated_parameters,
                " estimated coefficients.")
        # Resolve v_s() inside vglmer's namespace without attaching the package.
        environment(design$formula) <- asNamespace("vglmer")
        if (is.null(control)) {
          control <- vglmer::vglmer_control(tolerance_parameters = 0.01, tolerance_elbo = 1e-5)
        }
        for (name in names(solver_overrides)) control[[name]] <- solver_overrides[[name]]
        if (isTRUE(solver_trace)) {
          control$quiet <- FALSE
          control$quiet_rho <- FALSE
          control$print_prog <- 1L
        }
        fit_log[i, paste0("dropped_", names(prepared$dropped))] <- as.list(prepared$dropped)
        fit_log$dropped_smooth_terms[i] <- paste(prepared$dropped_smooth_terms, collapse = "; ")
        fit_log$dropped_smooth_level_columns[i] <- prepared$dropped_smooth_level_columns
        if (!prepared$hierarchical) fit_log$engine[i] <- "glm"
        utils::write.csv(fit_log, log_file, row.names = FALSE)
        design$fit <- if (prepared$hierarchical) {
          vglmer::vglmer(prepared$formula, data = prepared$data, family = "binomial",
                         control = control)
        } else {
          fit_sequential_fixed_only(prepared$formula, prepared$data, control$iterations)
        }
        design$fit$screened_random <- prepared$random_prediction
        design$fit$screened_smooth_groups <- prepared$screened_smooth_groups
        for (contrast in design$fixed_contrasts) {
          design$fit$reduced_fixed_levels[[contrast$predictor]] <- names(contrast$values)
        }
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
    diagnostics <- design$fit$internal_parameters
    if (!is.null(diagnostics$it_max)) fit_log$max_iterations[i] <- diagnostics$it_max
    if (!is.null(diagnostics$it_used)) fit_log$iterations_used[i] <- diagnostics$it_used
    if (length(diagnostics$parameter.change)) {
      fit_log$max_parameter_change[i] <- max(unlist(diagnostics$parameter.change))
    }
    fit_log$status[i] <- "returned"
    utils::write.csv(fit_log, log_file, row.names = FALSE)
    fits[[outcome]] <- design
    message(stage_label, " — model returned (", i, " of ", length(stages), " completed).")
    if (!is.null(progress)) {
      detail <- sprintf("%s, %s iterations, max parameter change %s",
                        outcome, fit_log$iterations_used[i],
                        format(fit_log$max_parameter_change[i], digits = 4))
      if (i < length(stages)) detail <- paste0(detail, "; next ", stages[i + 1L])
      progress(i, length(stages), detail)
    }
  }

  # Save the stage designs and area covariates needed to rebuild prediction data.
  no_vote <- if (length(no_vote) && !is.na(no_vote)) as.character(no_vote)
  structure(list(stages = fits, outcome_order = outcomes, no_vote = no_vote,
                 survey_counts = survey$counts,
                 survey_cells = survey$cells, omitted_rows = survey$omitted,
                 predictors = predictors, area_variable = area_variable,
                 extension_variable = extension_variable, area_covariates = area_covariates,
                 frame = frame, no_interactions = no_interactions,
                 fit_log = fit_log, log_file = normalizePath(log_file)),
            class = "voteframe_sequential_logit")
}

# Select the deepest candidate supported by the stage's eligible respondents.
select_sequential_stage_design <- function(stage, area_variable, depth, splines,
                                           area_predictors, observations,
                                           no_interactions = character(),
                                           trials = rep(1, nrow(stage)), minimum_support = -1L,
                                           successes = numeric(length(trials))) {
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
                                      area_predictors, no_interactions, trials, minimum_support)
    design$data$.success <- successes
    design$data$.failure <- trials - successes
    prepared <- prepare_sequential_fit(design)
    attempted <- c(attempted, paste0(candidate, "=", design$parameter_count, " -> ",
                                     prepared$estimated_parameters))
    # Accept equality. Count only coefficients that can enter the fit.
    if (observations >= prepared$estimated_parameters) break
  }
  design$depth <- candidate
  list(design = design, prepared = prepared, attempted = attempted)
}

# Count respondents rather than collapsed survey rows. Conditional variation
# is measured within each grouping level for varying slopes.
sequential_interaction_support <- function(design, outcome) {
  data <- design$data
  counts <- cbind(successes = data$.success, failures = data$.failure)
  trials <- rowSums(counts)
  labels <- stats::setNames(names(design$predictor_names), unname(design$predictor_names))
  rows <- list()
  for (group in names(design$group_slopes)) {
    group_predictors <- design$grouping_columns[[group]]
    if (is.null(group_predictors)) group_predictors <- group
    group_rows <- split(seq_len(nrow(data)), data[[group]], drop = TRUE)
    respondents <- rowsum(trials, data[[group]], reorder = FALSE)
    for (term in design$group_slopes[[group]]) {
      predictors <- if (term == "1") character() else strsplit(term, ":", fixed = TRUE)[[1L]]
      if (length(group_predictors) + length(predictors) < 2L) next
      values <- if (length(predictors)) Reduce(`*`, data[predictors]) else rep(1, nrow(data))
      support <- rowsum(counts * (values != 0), data[[group]], reorder = FALSE)
      support <- support[names(group_rows), , drop = FALSE]
      distinct <- nonmodal <- rep(NA_integer_, length(group_rows))
      if (length(predictors)) {
        frequencies <- lapply(group_rows, function(index) {
          rowsum(trials[index], values[index], reorder = FALSE)
        })
        distinct <- lengths(frequencies)
        nonmodal <- vapply(frequencies, function(x) sum(x) - max(x), numeric(1))
      }
      rows[[length(rows) + 1L]] <- data.frame(
        outcome = outcome,
        interaction = paste(labels[c(group_predictors, predictors)], collapse = " × "),
        level = names(group_rows),
        successes = support[, "successes"],
        failures = support[, "failures"],
        group_respondents = respondents[names(group_rows), 1L],
        nonzero_respondents = rowSums(support),
        distinct_slope_values = distinct,
        respondents_outside_modal_slope_value = nonmodal,
        row.names = NULL
      )
    }
  }
  do.call(rbind, rows)
}
