# Hash model inputs and parsed fitting code, independent of the run directory and R build.
us_fit_fingerprint <- function(fit_args, seed) {
  # Destinations do not affect coefficients. Retain the selected survey
  # columns, including their types/levels, and the full fitting options.
  spec <- fit_args[setdiff(names(fit_args), c("log_file", "interaction_support_log",
                                             "depth_support_dir", "progress", "solver_trace"))]
  survey <- as.data.frame(spec$auxiliary_survey)
  predictors <- if (is.null(spec$predictors)) names(survey) else spec$predictors
  spec$auxiliary_survey <- survey[unique(c(spec$area_variable, predictors, spec$extension_variable))]
  fit_sources <- file.path("R", c("as_plain_frame.R", "prepare_sequential_data.R", "process_area_covariates.R",
                                  "build_sequential_formula.R", "fit_screened_vglmer.R",
                                  "fit_sequential_logit.R"))
  if (!is.null(spec$frame)) {
    fit_sources <- c(fit_sources, file.path("R", c("register_frame_levels.R",
                         "predict_sequential_logit.R", "predict_vglmer_levels.R")))
  }
  # No source references: comments, formatting and absolute paths can differ across checkouts.
  code <- lapply(fit_sources, parse, keep.source = FALSE, encoding = "UTF-8")
  key <- list(inputs = spec[sort(names(spec))], seed = seed, rng = RNGkind(),
              contrasts = getOption("contrasts"), source = code)
  # digest skips the serializer's R version header when hashing a version-2 object.
  digest::digest(key, algo = "sha256", serializeVersion = 2L)
}

# The US case study can resume each national fit independently.
load_or_fit_us_model <- function(path, fit_args, seed,
                                 reuse = identical(Sys.getenv("SYNTHFRAME_US_REUSE_FITS"), "true")) {
  fingerprint <- us_fit_fingerprint(fit_args, seed)
  if (reuse && file.exists(path)) {
    loadNamespace("gKRLS")
    saved <- tryCatch(readRDS(path), error = function(e) NULL)
    if (inherits(saved, "synthframe_sequential_logit") &&
        identical(saved$extension_variable, fit_args$extension_variable) &&
        identical(names(saved$stages), utils::head(saved$outcome_order, -1L)) &&
        identical(saved$us_fit_fingerprint, fingerprint)) {
      message("Reusing verified fit ", path)
      return(saved)
    }
    rm(saved)
    message("Refitting ", path, " because its saved inputs and settings could not be verified.")
  }
  set.seed(seed)
  fit <- if (isTRUE(getOption("knitr.in.progress"))) {
    withCallingHandlers(
      do.call(fit_sequential_logit, fit_args),
      message = function(m) cat(conditionMessage(m), file = stderr()),
      warning = function(w) cat("Warning: ", conditionMessage(w), "\n", sep = "", file = stderr())
    )
  } else do.call(fit_sequential_logit, fit_args)
  fit$us_fit_fingerprint <- fingerprint
  packages <- c("vglmer", "gKRLS", "lme4", "mgcv", "Matrix")
  fit$us_fit_runtime <- list(R = R.version.string, platform = R.version$platform,
                             packages = vapply(packages, function(p) as.character(utils::packageVersion(p)),
                                               character(1)))
  save_case_study_rds(fit, path, compress = FALSE)
  fit
}
