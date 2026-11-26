check_area_covariates <- function() {
  reference <- data.frame(area = rep(c("01", "02"), each = 3L),
                          vote = rep(c("A", "No Vote", "C"), 2L),
                          total = c(20, 30, 50, 30, 10, 60))
  frame <- expand.grid(area = c("01", "02"), age = c("young", "older"),
                       stringsAsFactors = FALSE)
  frame$N <- 100
  survey <- frame[rep(seq_len(nrow(frame)), each = 60L), c("area", "age")]
  survey$vote <- rep(c("A", "No Vote", "C"), times = nrow(survey) / 3)
  fit_log <- tempfile(fileext = ".csv")
  on.exit(unlink(fit_log))
  fit_shares <- function(input, order) {
    fit_sequential_logit(survey, "vote", "area", area_covariates = input, no_vote = "No Vote",
                         outcome_order = order, predictors = c("area", "age"), frame = frame,
                         depth = 1L, splines = FALSE, max_iterations = 5L,
                         log_file = fit_log)$area_covariates
  }

  # With No Vote fitted first, A's denominator excludes the historical nonvoters.
  order <- c("A", "No Vote", "C")
  deferred <- process_area_covariates(reference, "area", "vote", "total")
  expected <- fit_shares(deferred, order)
  stopifnot(identical(names(expected), c("No Vote", "A")),
            isTRUE(all.equal(unname(expected[["No Vote"]][, 1L]), c(30 / 100, 10 / 100))),
            isTRUE(all.equal(unname(expected[["A"]][, 1L]), c(20 / 70, 30 / 90))))

  # Cover both stale denominators with the same stage names and a changed residual outcome.
  for (initial in list(order, c("A", "C", "No Vote"))) {
    eager <- process_area_covariates(reference, "area", "vote", "total", initial)
    stopifnot(identical(names(eager), head(initial, -1L)),
              is.matrix(eager[["A"]]),
              identical(fit_shares(eager, initial), expected))
  }

  # A caller's stage-specific predictors may use units other than historical shares.
  custom <- lapply(expected, function(stage) stage * 100)
  stopifnot(identical(fit_shares(custom, order), custom))

  # In-place edits retain the helper's source metadata and belong to the supplied order.
  modified <- process_area_covariates(reference, "area", "vote", "total", order)
  modified[["A"]] <- cbind(modified[["A"]], local_score = c(4, 5))
  problem <- tryCatch(fit_shares(modified, order), error = conditionMessage)
  stopifnot(is.character(problem), grepl("Reordering would discard edits", problem, fixed = TRUE))
  aligned <- process_area_covariates(reference, "area", "vote", "total", c("No Vote", "A", "C"))
  aligned[["A"]][, 1L] <- c(0.25, 0.4)
  stopifnot(identical(fit_shares(aligned, order)[["A"]], aligned[["A"]]))
}
