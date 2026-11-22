# Fit ordered extensions, split population draws, and calibrate all margins together.
MasterP <- function(mother_frame, extensions, n_draws = NULL, count_variable = "N",
                    aggregate_by = list(), tolerance = 1e-12, max_iterations = 1000L,
                    keep_fits = FALSE) {
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
                                      c("auxiliary_survey", "extension_variable", "frame")), "fit")
    masterp_options(spec$predict, setdiff(names(formals(predict_sequential_logit)),
                                         c("fit", "frame", "n_draws", "count_variable")), "predict")
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

  extended <- mother_frame
  available <- keys
  fit_info <- fits <- diagnostics <- list()
  for (outcome in outcomes) {
    spec <- extensions[[outcome]]
    fit_args <- spec$fit
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
    if (is.null(fit_args$log_file)) fit_args$log_file <- tempfile("masterp-fit-", fileext = ".csv")
    fit <- do.call(fit_sequential_logit, c(list(auxiliary_survey = survey,
                                               extension_variable = outcome,
                                               frame = if (isTRUE(spec$register_frame)) extended),
                                          fit_args))
    fit_info[[outcome]] <- fit[c("outcome_order", "no_vote", "fit_log", "log_file")]
    if (keep_fits) fits[[outcome]] <- fit

    predict_args <- masterp_default(spec$predict, list())
    if (is.null(predict_args$write_files)) predict_args$write_files <- FALSE
    posterior <- do.call(predict_sequential_logit, c(list(fit = fit, frame = extended,
                                                          n_draws = n_draws,
                                                          count_variable = count_variable),
                                                     predict_args))
    extended <- expand_expected_counts(posterior$cells, posterior$draws, extended[[count_variable]],
                                       posterior, population_draws = paired_frame_counts(extended, posterior))
    available <- c(available, outcome)
    rm(posterior, fit)
  }

  margin_ids <- make.unique(vapply(margins, function(x) paste(x$outcome, x$name, sep = "/"),
                                    character(1)))
  margin_option <- function(field, default = NULL) {
    stats::setNames(lapply(margins, function(x) masterp_default(x[[field]], default)), margin_ids)
  }
  raked <- rake_joint_extended_frame(
    extended, mother_frame, targets = margin_option("targets"), by = margin_option("by"),
    extension_variables = outcomes, margin_variables = unlist(margin_option("outcome")),
    count_variable = count_variable, target_variable = unlist(margin_option("target_variable", "target")),
    target_scale = unlist(margin_option("target_scale", "share")), tolerance = tolerance,
    max_iterations = max_iterations
  )

  for (outcome in outcomes) {
    for (comparison in names(extensions[[outcome]]$plots)) {
      plot <- extensions[[outcome]]$plots[[comparison]]
      files <- list(before = plot$before_file, after = plot$after_file)
      plot[c("before_file", "after_file")] <- NULL
      if (!"no_vote" %in% names(plot)) plot$no_vote <- fit_info[[outcome]]$no_vote
      if (is.null(plot$outcome_order)) plot$outcome_order <- fit_info[[outcome]]$outcome_order
      diagnostics[[outcome]][[comparison]] <- lapply(names(files), function(phase) {
        counts <- if (phase == "before") extended else raked
        if (is.null(plot$draw_plot)) plot$draw_plot <- !is.null(files[[phase]])
        do.call(check_extended_frame, c(list(extended_frame = counts, extension_variable = outcome,
                                            count_variable = count_variable, plot_file = files[[phase]]),
                                        plot))
      })
      names(diagnostics[[outcome]][[comparison]]) <- names(files)
    }
  }
  rm(extended)

  aggregates <- lapply(aggregate_by, function(by) {
    counts <- raked[c(count_variable, draw_columns)]
    if (!length(by)) return(as.data.frame(lapply(counts, sum)))
    groups <- raked[by]
    ids <- frame_cell_keys(groups, groups)
    result <- cbind(groups[!duplicated(ids), , drop = FALSE], rowsum(counts, ids, reorder = FALSE))
    rownames(result) <- NULL
    result
  })
  list(frame = raked, aggregates = aggregates, diagnostics = diagnostics,
       fit_info = fit_info, fits = if (keep_fits) fits)
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
