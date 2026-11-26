check_benchmark_validation <- function() {
  counts <- data.frame(area = c("b", "a", "b", "a", "a"),
                       vote = c("No Vote", "A", "A", "B", "No Vote"),
                       N = c(30, 20, 70, 50, 30))
  counts$N_draw_1 <- counts$N
  bench <- function(area, vote, share) data.frame(area = area, vote = vote, target = share)
  compare <- function(targets, no_vote = NULL, by = "area", frame = counts) {
    check_extended_frame(frame, targets, by, "vote", no_vote = no_vote, draw_plot = FALSE)$comparison
  }
  fails <- function(targets, no_vote = NULL, by = "area", frame = counts) {
    inherits(try(compare(targets, no_vote, by, frame), silent = TRUE), "try-error")
  }
  full <- bench("a", c("B", "No Vote", "A"), c(.5, .3, .2))
  partial <- full[-1L, ]
  for (target_table in list(full, partial)) {
    result <- compare(target_table)
    stopifnot(isTRUE(all.equal(result$estimate, result$target)))
  }
  conditional <- compare(partial, "No Vote")
  stopifnot(isTRUE(all.equal(conditional$target[conditional$vote == "A"], 2 / 7)),
            fails(transform(full, target = c(.4, .3, .2))),
            fails(transform(partial, target = c(.8, .3)), by = character()),
            fails(transform(partial, target = c(.8, .3)), "No Vote"),
            fails(bench("a", "A", .2), "No Vote"))

  # A near-zero turnout can hide a material conditional excess inside roundoff on the population scale.
  stopifnot(fails(bench("a", c("No Vote", "A"), c(1 - 1e-10, 2e-10)), "No Vote"),
            fails(bench("a", c("No Vote", "A"), c(1, 0)), "No Vote"),
            nrow(compare(bench("a", "No Vote", 1), "No Vote")) == 1L)

  # Absent rows count as zero only for known outcomes in a group with a defined denominator.
  sparse <- bench("b", c("B", "No Vote", "A"), c(0, .3, .7))
  for (no_vote in list(NULL, "No Vote")) {
    result <- compare(rbind(sparse, partial), no_vote)
    zero <- subset(result, area == "b" & vote == "B")
    stopifnot(nrow(zero) == 1L, all(zero[c("estimate", "lower_90", "upper_90")] == 0))
  }
  zero_draw <- counts
  zero_draw$N_draw_1[zero_draw$area == "b"] <- 0
  stopifnot(fails(bench("b", "B", .1)), fails(bench("unknown", "B", 0)),
            fails(bench("b", "unknown", 0)), fails(bench("b", "B", 0), frame = zero_draw))
}
