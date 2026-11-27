# Run from the repository root or against the installed package in R CMD check.
main <- function() {
  if (checkout) for (file in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(file)
  test_file <- function(name) if (checkout) file.path("tests", name) else name
  scratch <- tempfile("synthframe-integration-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  set.seed(2026)

  frame <- expand.grid(area = c("02", "01"), age = c("young", "older"),
                       stringsAsFactors = FALSE)
  frame$N <- c(100, 200, 150, 250)
  frame$N_draw_1 <- frame$N + c(20, -20, 20, -20)
  frame$N_draw_2 <- 2 * frame$N - frame$N_draw_1
  population <- c("N", "N_draw_1", "N_draw_2")
  choices <- c("No Vote", "A", "B")
  targets <- expand.grid(area = unique(frame$area), vote = choices,
                         stringsAsFactors = FALSE)
  targets$target <- rep(c(0.3, 0.4, 0.3), each = 2L)
  historical <- process_area_covariates(targets, "area", "vote", "target",
                                        outcome_order = choices)
  stopifnot(identical(names(historical), choices[-3L]),
            isTRUE(all.equal(unname(historical[["A"]][, 1L]), rep(4 / 7, 2L))))

  survey <- frame[rep(seq_len(nrow(frame)), each = 90L), c("area", "age")]
  survey$vote <- factor(rep(rep(choices, each = 30L), 4L), levels = choices)
  survey$second_vote <- rep(rep(choices, each = 10L), 12L)
  fit_options <- list(area_variable = "area", no_vote = "No Vote", outcome_order = choices,
                      depth = 1L, splines = FALSE, max_iterations = 5L)
  fit <- do.call(fit_sequential_logit, c(list(auxiliary_survey = survey,
    extension_variable = "vote", predictors = c("area", "age"), frame = frame,
    log_file = file.path(scratch, "standalone-fit.csv")), fit_options))
  # The same stage cache can span batches, but cannot supply another fit or draw count.
  stage <- fit$stages[[1L]]
  stage_data <- prepare_prediction_stage(fit, stage, choices[1L], frame)
  state <- new.env(parent = emptyenv())
  sample_stage <- function(model, n) predict_vglmer_levels(model, stage_data, n, draw_state = state)
  first_draw <- sample_stage(stage$fit, 1L)
  stopifnot(identical(sample_stage(stage$fit, 1), first_draw))
  for (request in list(list(stage$fit, 2L), list(fit$stages[[2L]]$fit, 1L))) {
    error <- tryCatch(do.call(sample_stage, request), error = conditionMessage)
    stopifnot(is.character(error), grepl("same fit, draw count", error, fixed = TRUE))
  }
  posterior <- predict_sequential_logit(fit, frame, write_files = FALSE, batch_size = 2L)
  extended <- aggregate_extended_frame(frame, posterior, by = c("area", "age"))
  collapsed <- collapse_extended_frame(frame, posterior)
  stopifnot(isTRUE(all.equal(extended[c("area", "age", "vote", "N")], collapsed)))
  for (column in population) {
    stopifnot(isTRUE(all.equal(unname(rowsum(extended[[column]], rep(1:4, each = 3L))[, 1L]),
                                frame[[column]])))
  }

  # Both solvers and the cell check must use each draw's own population.
  single <- rake_extended_frame(extended, frame, targets, "area", "vote")
  joint <- rake_joint_extended_frame(extended, frame, list(vote = targets), by = "area")
  checked <- post_rake_cell_comparison(single, frame, "vote", include_max_error = TRUE)
  stopifnot(isTRUE(all.equal(single, joint)), max(checked$max_abs_error) < 1e-8,
            max(abs(checked$lower_90 - frame$N)) < 1e-8)
  wrong <- single
  wrong$N_draw_2[1L] <- wrong$N_draw_2[1L] + 1
  stopifnot(abs(max(post_rake_cell_comparison(wrong, frame, "vote", include_max_error = TRUE)$max_abs_error) - 1) < 1e-8)
  result <- check_extended_frame(single, targets, "area", "vote", draw_plot = FALSE)
  stopifnot(max(abs(result$comparison$estimate - result$comparison$target)) < 1e-8)
  legacy <- c("draw_1", "draw_2")
  names(extended)[match(population[-1L], names(extended))] <- legacy
  stopifnot(max(post_rake_cell_comparison(rake_extended_frame(extended, frame, targets,
    "area", "vote"), frame, "vote", include_max_error = TRUE)$max_abs_error) < 1e-8)
  names(extended)[match(legacy, names(extended))] <- population[-1L]

  source(test_file("raking_change_shares.R"), local = TRUE)
  check_raking_change_shares()
  batches <- geographic_batches(frame, "area", areas_per_batch = 1L)
  parts <- function(x) lapply(batches, function(i) x[x$area %in% frame$area[i], ])
  direct <- file.path(scratch, "direct-changes.png")
  shuffled <- single[c(setdiff(names(single), population[-1L]), rev(population[-1L]))]
  stopifnot(file.exists(plot_raking_changes(extended, shuffled, frame, "area", "vote",
                                          direct, no_vote = "No Vote")))
  file <- file.path(scratch, "changes.png")
  stopifnot(file.exists(plot_partitioned_raking_changes(parts(extended), parts(single), frame,
    batches, "area", "vote", "No Vote", file, read_extended = identity, read_raked = identity)))
  stopifnot(nrow(sample_extended_frame(parts(single) |> lapply(function(x) {
    p <- tempfile(tmpdir = scratch)
    saveRDS(x, p)
    p
  }), c("area", "vote"), lengths(batches) * 3L, size = 5L)) == 5L)
  stopifnot(length(geographic_batches(frame, "area", areas_per_batch = 1L,
                                      rake_by = list(character()))) == 1L)

  second_targets <- targets
  names(second_targets)[names(second_targets) == "vote"] <- "second_vote"
  options <- function(outcome_targets) list(survey = survey, fit = fit_options,
    predict = list(batch_size = 2L), rake = list(area = list(targets = outcome_targets, by = "area")))
  specs <- list(vote = options(targets), second_vote = options(second_targets))
  specs$vote$fit$predictors <- c("area", "age")
  specs$second_vote$fit$predictors <- c("area", "age", "vote")
  duplicate_exports <- lapply(specs, function(spec) {
    spec$predict$write_files <- TRUE
    spec
  })
  # Both spellings refer to the same directory, which has not been created yet.
  duplicate_exports$vote$predict$output_dir <- file.path(scratch, "predictions")
  duplicate_exports$second_vote$predict$output_dir <- file.path(scratch, ".", "predictions")
  message <- tryCatch(MasterP(frame, duplicate_exports,
                              log_file = file.path(scratch, "duplicate-exports.log")),
                      error = conditionMessage)
  stopifnot(is.character(message), grepl("separate predict$output_dir", message, fixed = TRUE))
  work <- file.path(scratch, "unused-prefit")
  full <- MasterP(frame, specs, aggregate_by = list(total = character()), areas_per_batch = 1L,
                  pre_rake_mode = "temporary", pre_rake_dir = work,
                  log_file = file.path(scratch, "masterp.log"))
  total <- full$aggregates$total
  stopifnot(nrow(full$frame) == nrow(frame) * length(choices)^2L, !dir.exists(work),
            max(abs(unlist(total[population]) - colSums(frame[population]))) < 1e-8,
            any(grepl("MasterP finished", readLines(full$log_file), fixed = TRUE)))
  for (outcome in names(specs)) {
    table <- specs[[outcome]]$rake$area$targets
    check <- check_extended_frame(full$frame, table, "area", outcome, draw_plot = FALSE)
    stopifnot(max(abs(check$comparison$estimate - check$comparison$target)) < 1e-8)
  }
  source(test_file("area_covariates.R"), local = TRUE)
  source(test_file("stage_screening.R"), local = TRUE)
  source(test_file("prediction_design.R"), local = TRUE)
  source(test_file("stage_log.R"), local = TRUE)
  source(test_file("structural_zeros.R"), local = TRUE)
  source(test_file("raking_paths.R"), local = TRUE)
  source(test_file("benchmark_validation.R"), local = TRUE)
  source(test_file("data_table_inputs.R"), local = TRUE)
  source(test_file("data_table_raking.R"), local = TRUE)
  if (checkout) source(test_file("us_fit_cache.R"), local = TRUE)
  source(test_file("prediction_exports.R"), local = TRUE)
  if (requireNamespace("data.table", quietly = TRUE)) {
    check_data_table_inputs(scratch)
    check_data_table_raking(scratch)
  } else message("Skipping data.table input checks. Install data.table to run them.")
  if (checkout) check_us_fit_cache()
  check_prediction_exports(scratch)
  check_structural_zeros()
  check_raking_paths()
  check_benchmark_validation()
  check_area_covariates()
  check_stage_screening()
  check_prediction_design(scratch)
  check_stage_log()

  cat("SynthFrame integration checks passed.\n")
}
checkout <- file.exists("R/MasterP.R")
if (!checkout) {
  library(synthframe)
  # Keep test expressions in a regular R environment for data.table dispatch.
  bindings <- as.list(asNamespace("synthframe"), all.names = TRUE)
  test_env <- list2env(Filter(is.function, bindings), parent = globalenv())
  test_env$checkout <- FALSE
  environment(main) <- test_env
}
main()
