check_us_fit_cache <- function() {
  directory <- tempfile("us-fit-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  path <- file.path(directory, "pres_fit.rds")
  survey <- expand.grid(district = c("east", "west", "north"), vote = c("A", "B"),
                        respondent = seq_len(12L), stringsAsFactors = FALSE)
  history <- list(A = data.frame(last_vote = c(.2, .4, .8), row.names = unique(survey$district)))
  args <- list(auxiliary_survey = survey, extension_variable = "vote", area_variable = "district",
               area_covariates = history, predictors = "district", outcome_order = c("A", "B"),
               depth = 1L, splines = FALSE, max_iterations = 3L,
               log_file = file.path(directory, "pres_fit.csv"))
  cache <- function() load_or_fit_us_model(path, args, 2026L, reuse = TRUE)
  fresh <- cache()
  # A marker on the disk copy distinguishes an actual hit from a deterministic refit.
  fresh$cache_marker <- TRUE
  stopifnot(all(c("R", "platform", "packages") %in% names(fresh$us_fit_runtime)))
  fresh$us_fit_runtime$R <- "R version 4.3.3"
  fresh$us_fit_runtime$packages["Matrix"] <- "1.6-5"
  saveRDS(fresh, path)
  args$auxiliary_survey$respondent <- 0L
  stopifnot(isTRUE(cache()$cache_marker))

  # Changing a respondent's outcome leaves the extension and stage names identical.
  args$auxiliary_survey$vote[1L] <- "B"
  survey_fit <- cache()
  stopifnot(is.null(survey_fit$cache_marker), sum(survey_fit$survey_counts[, "A"]) == 35L,
            !identical(fresh$us_fit_fingerprint, survey_fit$us_fit_fingerprint))
  args$area_covariates$A$last_vote[1L] <- .7
  history_fit <- cache()
  stopifnot(history_fit$area_covariates$A$last_vote[1L] == .7,
            !identical(history_fit$us_fit_fingerprint, survey_fit$us_fit_fingerprint))
  args$max_iterations <- 2L
  new_control <- cache()
  stopifnot(all(new_control$fit_log$max_iterations == 2L),
            !identical(new_control$us_fit_fingerprint, history_fit$us_fit_fingerprint))

  new_control$cache_marker <- TRUE
  new_control$us_fit_fingerprint <- NULL
  saveRDS(new_control, path)
  migrated <- cache()
  stopifnot(is.null(migrated$cache_marker), identical(migrated$us_fit_fingerprint,
                                                      readRDS(path)$us_fit_fingerprint))
  writeLines("incomplete", path)
  repaired <- cache()
  stopifnot(inherits(repaired, "voteframe_sequential_logit"),
            identical(repaired$us_fit_fingerprint, migrated$us_fit_fingerprint))

  # An unsuccessful refit leaves the previous complete model available.
  args$area_covariates$A <- args$area_covariates$A[-1L, , drop = FALSE]
  error <- tryCatch(cache(), error = conditionMessage)
  stopifnot(is.character(error), grepl("east", error, fixed = TRUE),
            identical(readRDS(path)$us_fit_fingerprint, repaired$us_fit_fingerprint))
}
