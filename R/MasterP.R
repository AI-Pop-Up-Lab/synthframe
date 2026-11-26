# Fit ordered extensions, split population draws, and calibrate all margins together.
MasterP <- function(mother_frame, extensions, n_draws = NULL, count_variable = "N",
                    aggregate_by = list(), tolerance = 1e-12, max_iterations = 1000L,
                    keep_fits = FALSE, pre_rake_mode = "auto", pre_rake_dir = NULL,
                    area_variable = NULL, parent_area_variable = NULL, areas_per_batch = NULL,
                    verbose = FALSE, log_file = NULL) {
  log <- masterp_logger(log_file, verbose)
  on.exit(log$close(), add = TRUE)
  log$run(masterp_execute(mother_frame, extensions, n_draws, count_variable, aggregate_by,
                          tolerance, max_iterations, keep_fits, pre_rake_mode, pre_rake_dir,
                          area_variable, parent_area_variable, areas_per_batch, log))
}

masterp_execute <- function(mother_frame, extensions, n_draws, count_variable, aggregate_by,
                            tolerance, max_iterations, keep_fits, pre_rake_mode, pre_rake_dir,
                            area_variable, parent_area_variable, areas_per_batch, log) {
  log$write("Validating population frame, extension specifications, and calibration targets")
  pre_rake_mode <- match.arg(pre_rake_mode, c("auto", "memory", "temporary", "keep"))
  if (pre_rake_mode == "keep" && is.null(pre_rake_dir)) {
    stop("Set pre_rake_dir to keep the pre-rake frame.")
  }
  population_draws <- prediction_count_draws(mother_frame, count_variable)
  keys <- setdiff(names(mother_frame), c(count_variable, population_draws))
  validate_prediction_frame(mother_frame, keys, count_variable)
  mother_frame <- as.data.frame(mother_frame)
  masterp_named_list(extensions, "extensions", allow_empty = FALSE)
  masterp_named_list(aggregate_by, "aggregate_by")
  outcomes <- names(extensions)
  if (is.null(n_draws)) n_draws <- if (length(population_draws)) length(population_draws) else 125L
  if (!is.numeric(n_draws) || length(n_draws) != 1L || !is.finite(n_draws) ||
      n_draws < 1L || n_draws != floor(n_draws) || n_draws > .Machine$integer.max) {
    stop("n_draws must be a positive integer.")
  }
  if (length(population_draws) && n_draws != length(population_draws)) {
    stop("n_draws must match the number of incoming population draws.")
  }
  draw_columns <- paste0(count_variable, "_draw_", seq_len(n_draws))
  prospective_columns <- stats::setNames(as.list(outcomes), outcomes)
  if (any(outcomes %in% names(mother_frame)) ||
      length(extended_frame_draw_columns(prospective_columns, count_variable)) ||
      any(grepl("^draw_[1-9][0-9]*$", outcomes))) {
    stop("Extension names must be new columns and distinct from population draws.")
  }
  if (!is.logical(keep_fits) || length(keep_fits) != 1L || is.na(keep_fits)) {
    stop("keep_fits must be TRUE or FALSE.")
  }
  for (by in aggregate_by) masterp_grouping(by, c(keys, outcomes))

  # Validate the configuration before starting a potentially expensive fit.
  margins <- list()
  for (outcome in outcomes) {
    spec <- extensions[[outcome]]
    masterp_options(spec, c("survey", "fit", "predict", "rake", "plots", "register_frame"),
                    paste("Extension", sQuote(outcome)))
    masterp_options(spec$fit, setdiff(names(formals(fit_sequential_logit)),
                                      c("auxiliary_survey", "extension_variable", "frame",
                                        "progress", "solver_trace")), "fit")
    masterp_options(spec$predict, setdiff(names(formals(predict_sequential_logit)),
                                         c("fit", "frame", "n_draws", "count_variable", "progress")), "predict")
    if (is.null(spec$fit$area_variable)) stop("Set fit$area_variable for ", sQuote(outcome), ".")
    if (!is.null(spec$register_frame) && !identical(spec$register_frame, TRUE) &&
        !identical(spec$register_frame, FALSE)) stop("register_frame must be TRUE or FALSE.")
    masterp_named_list(spec$rake, paste("Raking margins for", sQuote(outcome)), allow_empty = FALSE)
    for (margin in names(spec$rake)) {
      config <- spec$rake[[margin]]
      masterp_options(config, c("targets", "by", "target_variable", "target_scale"), "rake")
      masterp_grouping(config$by, keys)
      if (!is.data.frame(config$targets) ||
          !all(c(config$by, outcome, masterp_default(config$target_variable, "target")) %in%
               names(config$targets))) stop("Missing target columns for ", sQuote(outcome), ".")
      margins[[length(margins) + 1L]] <- c(list(outcome = outcome, name = margin), config)
    }
    masterp_named_list(masterp_default(spec$plots, list()), "plots")
    for (plot in spec$plots) {
      masterp_options(plot, c("targets", "by", "target_variable", "outcome_order",
                              "coverage_tolerance", "no_vote", "draw_plot",
                              "before_file", "after_file"), "plots")
      masterp_grouping(plot$by, setdiff(c(keys, outcomes), outcome))
      if (!is.data.frame(plot$targets)) stop("Plot comparisons require a target table.")
    }
  }
  log$write(sprintf("Validated %d population cells, %d extensions, %d calibration margins, and %d draws",
                    nrow(mother_frame), length(outcomes), length(margins), n_draws))

  extended <- mother_frame
  available <- keys
  fit_info <- fits <- diagnostics <- list()
  extension_progress <- log$progress("Extensions")
  extension_progress(0L, length(outcomes), paste(outcomes, collapse = ", "))
  for (i in seq_along(outcomes)) {
    outcome <- outcomes[i]
    spec <- extensions[[outcome]]
    fit_args <- spec$fit
    log$write(paste("Preparing auxiliary survey for", sQuote(outcome)))
    surveys <- if (is.data.frame(spec$survey)) list(spec$survey) else spec$survey
    if (!is.list(surveys) || !length(surveys) ||
        !all(vapply(surveys, is.data.frame, logical(1)))) {
      stop("Supply a survey data frame or a list of survey data frames for ", sQuote(outcome), ".")
    }
    shared <- Reduce(intersect, lapply(surveys, names))
    predictors <- masterp_default(fit_args$predictors, intersect(available, shared))
    predictors <- unique(c(fit_args$area_variable, predictors))
    columns <- c(predictors, outcome)
    if (!all(predictors %in% available) || !all(columns %in% shared) ||
        any(vapply(surveys, function(x) anyDuplicated(names(x)) > 0L, logical(1)))) {
      stop("Every survey must contain the outcome and the selected predictors available in the frame for ",
           sQuote(outcome), ".")
    }
    survey <- do.call(rbind, lapply(surveys, function(x) as.data.frame(x)[columns]))
    fit_args$predictors <- predictors
    if (is.null(fit_args$log_file)) {
      fit_args$log_file <- paste0(log$path, "-fit-", i, ".csv")
    }
    if (identical(normalizePath(fit_args$log_file, mustWork = FALSE), log$path)) {
      stop("The fit CSV and MasterP run log must use different paths.")
    }
    fit_label <- paste("fitting", sQuote(outcome))
    log$write(sprintf("Prepared %d survey rows for %s. Stage CSV %s",
                      nrow(survey), sQuote(outcome), fit_args$log_file))
    fit <- log$step(fit_label, log$capture_output(
      do.call(fit_sequential_logit, c(list(auxiliary_survey = survey, extension_variable = outcome,
                                          frame = if (isTRUE(spec$register_frame)) extended,
                                          progress = log$progress(fit_label), solver_trace = TRUE),
                                     fit_args))
    ))
    fit_info[[outcome]] <- fit[c("outcome_order", "no_vote", "fit_log", "log_file")]
    if (keep_fits) fits[[outcome]] <- fit

    predict_args <- masterp_default(spec$predict, list())
    # Keep intermediate predictions in memory unless the caller requests exports.
    if (is.null(predict_args$write_files)) predict_args$write_files <- FALSE
    predict_label <- paste("predicting", sQuote(outcome))
    posterior <- log$step(predict_label, do.call(predict_sequential_logit, c(
      list(fit = fit, frame = extended, n_draws = n_draws, count_variable = count_variable,
           progress = log$progress(predict_label)), predict_args
    )))
    if (!is.null(posterior$posterior_file)) {
      log$write(paste("Wrote prediction exports", posterior$posterior_file,
                      "and", posterior$extended_frame_file))
    }
    extend_label <- paste("extending the frame with", sQuote(outcome))
    extended <- log$step(extend_label, expand_expected_counts(
      posterior$cells, posterior$draws, extended[[count_variable]], posterior,
      population_draws = paired_frame_counts(extended, posterior),
      progress = log$progress(extend_label)
    ))
    available <- c(available, outcome)
    rm(posterior, fit)
    extension_progress(i, length(outcomes), sprintf("%s, %d extended rows", outcome, nrow(extended)))
  }

  if (is.null(area_variable)) {
    fitted_areas <- vapply(extensions, function(spec) spec$fit$area_variable, character(1))
    area_columns <- intersect(fitted_areas, keys)
    if (length(area_columns)) {
      counts <- vapply(mother_frame[area_columns], function(x) length(unique(x)), integer(1))
      area_variable <- area_columns[which.max(counts)]
    }
  }
  margin_ids <- make.unique(vapply(margins, function(x) paste(x$outcome, x$name, sep = "/"),
                                    character(1)))
  margin_option <- function(field, default = NULL) {
    stats::setNames(lapply(margins, function(x) masterp_default(x[[field]], default)), margin_ids)
  }
  rake_label <- "joint raking"
  raked <- log$step(rake_label, log$capture_output(rake_joint_extended_frame(
    extended, mother_frame, targets = margin_option("targets"), by = margin_option("by"),
    extension_variables = outcomes, margin_variables = unlist(margin_option("outcome")),
    count_variable = count_variable, target_variable = unlist(margin_option("target_variable", "target")),
    target_scale = unlist(margin_option("target_scale", "share")), tolerance = tolerance,
    max_iterations = max_iterations, area_variable = area_variable,
    parent_area_variable = parent_area_variable, areas_per_batch = areas_per_batch,
    progress = log$progress(rake_label), solver_trace = TRUE
  )))

  storage <- log$step("choosing pre-rake storage",
                      pre_rake_storage(pre_rake_mode, pre_rake_dir, bytes = as.numeric(object.size(extended))))
  on.exit(storage$cleanup(), add = TRUE)
  pre_rake_part <- log$step(paste("pre-rake storage using", storage$mode),
                           storage$put(extended, "extended_frame"))
  if (storage$mode == "keep") log$write(paste("Wrote the pre-rake frame", pre_rake_part))
  rm(extended)
  n_checks <- 2L * sum(vapply(extensions, function(spec) length(spec$plots), integer(1)))
  checks_done <- 0L
  check_progress <- log$progress("Benchmark checks")
  if (n_checks) check_progress(0L, n_checks)
  for (outcome in outcomes) {
    for (comparison in names(extensions[[outcome]]$plots)) {
      plot <- extensions[[outcome]]$plots[[comparison]]
      files <- list(before = plot$before_file, after = plot$after_file)
      plot[c("before_file", "after_file")] <- NULL
      if (!"no_vote" %in% names(plot)) plot$no_vote <- fit_info[[outcome]]$no_vote
      if (is.null(plot$outcome_order)) plot$outcome_order <- fit_info[[outcome]]$outcome_order
      diagnostics[[outcome]][[comparison]] <- lapply(names(files), function(phase) {
        counts <- if (phase == "before") storage$get(pre_rake_part) else raked
        if (is.null(plot$draw_plot)) plot$draw_plot <- !is.null(files[[phase]])
        result <- do.call(check_extended_frame, c(list(
          extended_frame = counts, extension_variable = outcome, count_variable = count_variable,
          plot_file = files[[phase]]
        ), plot))
        checks_done <<- checks_done + 1L
        check_progress(checks_done, n_checks, paste(outcome, comparison, phase, "raking",
                                                    files[[phase]]))
        result
      })
      names(diagnostics[[outcome]][[comparison]]) <- names(files)
    }
  }
  rm(pre_rake_part)

  aggregate_progress <- log$progress("Aggregating raked counts")
  if (length(aggregate_by)) aggregate_progress(0L, length(aggregate_by))
  aggregates <- lapply(seq_along(aggregate_by), function(i) {
    by <- aggregate_by[[i]]
    counts <- raked[c(count_variable, draw_columns)]
    result <- if (!length(by)) {
      as.data.frame(lapply(counts, sum))
    } else {
      groups <- raked[by]
      ids <- frame_cell_keys(groups, groups)
      cbind(groups[!duplicated(ids), , drop = FALSE], rowsum(counts, ids, reorder = FALSE))
    }
    rownames(result) <- NULL
    aggregate_progress(i, length(aggregate_by), names(aggregate_by)[i])
    result
  })
  names(aggregates) <- names(aggregate_by)
  list(frame = raked, aggregates = aggregates, diagnostics = diagnostics,
       fit_info = fit_info, fits = if (keep_fits) fits, log_file = log$path)
}

masterp_default <- function(value, default) {
  if (is.null(value)) default else value
}

masterp_named_list <- function(value, label, allow_empty = TRUE) {
  keys <- names(value)
  if (!is.list(value) || (!allow_empty && !length(value)) || length(keys) != length(value) ||
      anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys)) {
    stop(label, " must be a named list with distinct names.")
  }
}

masterp_options <- function(value, allowed, label) {
  if (is.null(value)) return(invisible(NULL))
  masterp_named_list(value, label)
  unknown <- setdiff(names(value), allowed)
  if (length(unknown)) stop("Unknown ", label, " options: ", paste(unknown, collapse = ", "), ".")
}

masterp_grouping <- function(by, columns) {
  if (!is.character(by) || anyNA(by) || anyDuplicated(by) || !all(by %in% columns)) {
    stop("Supply distinct grouping columns available in the frame. Use character() for a national total.")
  }
}
