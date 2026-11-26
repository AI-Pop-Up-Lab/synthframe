check_stage_screening <- function() {
  log_file <- tempfile(fileext = ".csv")
  on.exit(unlink(log_file))
  fit <- function(survey, depth = 0, ...) {
    fit_sequential_logit(survey, "vote", "area", depth = depth, log_file = log_file, ...)
  }
  check_failure <- function(survey, expected, ...) {
    error <- tryCatch({fit(survey, ...); NULL}, error = conditionMessage)
    log <- read.csv(log_file)
    failed <- tail(log, 1)
    stopifnot(is.character(error), failed$status == "failed", failed$engine == "vglmer")
    for (fragment in expected) {
      stopifnot(grepl(fragment, error, fixed = TRUE), grepl(fragment, failed$error, fixed = TRUE))
    }
    log
  }

  # With 40 and 4 eligible people, the cutoff can leave two, one or zero levels.
  survey <- data.frame(area = rep(c("a", "b"), c(40, 4)), vote = rep(c("A", "B"), 22))
  baseline <- fit(survey, minimum_support = 0)
  stopifnot(inherits(baseline$stages$A$fit, "vglmer"))
  for (cutoff in c(10, 100)) {
    qualified <- if (cutoff == 10) 1 else 0
    log <- check_failure(survey, minimum_support = cutoff, expected = c(
      "No estimable random effect or spline", "44 eligible respondents",
      paste("minimum_support =", cutoff), paste("area:", qualified, "of 2 observed levels")))
    stopifnot(log$unsupported_random_main_effects == "area", log$dropped_scalar_random == 2)
  }

  # Stage B has both classes, but all its eligible respondents are from the same area.
  two_stages <- data.frame(area = rep(c("a", "b"), c(30, 10)),
                           vote = c(rep(c("A", "B", "C"), 10), rep("A", 10)))
  log <- check_failure(two_stages, outcome_order = c("A", "B", "C"), expected = c(
    "20 eligible respondents (10 successes, 10 failures)", "area: 1 of 1 observed levels"))
  stopifnot(identical(log$outcome, c("A", "B")), identical(log$status, c("returned", "failed")))

  # A singleton area cannot remove a separately estimable group or global spline.
  with_group <- transform(survey, stratum = rep(c("young", "young", "old", "old"), 11))
  grouped <- fit(with_group, depth = 1, splines = FALSE, minimum_support = 10)
  stopifnot(inherits(grouped$stages$A$fit, "vglmer"),
            identical(names(grouped$stages$A$fit$internal_parameters$names_of_RE), "stratum"))
  with_spline <- transform(survey, x = rep(rep(c(-2, -1, 1, 2), each = 2), length.out = 44))
  smoothed <- fit(with_spline, depth = 2, minimum_support = 10)
  model <- smoothed$stages$A$fit
  smooths <- model$formula$interpret_gam$smooth.spec
  stopifnot(inherits(model, "vglmer"), !length(model$formula$re),
            length(smooths) == 1L, smooths[[1]]$term == "x", smooths[[1]]$by == "NA")

  # At depth 2 the condition screen drops x and its spline. Depth 1 retains both.
  cells <- data.frame(area = "a", x = c(24, 23, 14, 26, 19, 20),
                      z = c(0, 0, 1, 0, 1, 0), w = c(5, 5, 3, 5, 4, 4))
  smaller <- cells[rep(seq_len(nrow(cells)), each = 8), ]
  smaller$vote <- rep(c("A", "B"), 24)
  selected <- fit(smaller, depth = 2)
  stopifnot(inherits(selected$stages$A$fit, "vglmer"), selected$fit_log$fitted_depth == 1,
            selected$stages$A$fit$formula$interpret_gam$smooth.spec[[1]]$term == "x")
}
