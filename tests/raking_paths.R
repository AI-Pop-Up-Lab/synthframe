# A mean-only mother frame exercises the single solver even when the extended
# frame has simulation columns. Compare each solver with counts derived by hand.
check_raking_paths <- function() {
  frame <- data.frame(region = c("north", "north", "south", "south"),
                      cell = c("x", "y", "x", "y"), N = c(80, 120, 60, 40))
  # North/x has an explicit zero for C. South/y has no row for B.
  extended <- data.frame(
    region = rep(c("north", "south"), c(6L, 5L)), cell = rep(c("x", "y", "x", "y"), c(3, 3, 3, 2)),
    vote = c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A", "C"),
    N = c(20, 20, 0, 12, 3, 9, 6, 18, 6, 4, 16),
    N_draw_1 = c(40, 10, 0, 12, 2, 1, 4, 4, 12, 4, 16))
  # The A share in each cell initially equals that of the other cell in its
  # region, in each simulation. Targeting only A thus gives every northern cell
  # 25% A and every southern cell 60% A. Split the rest in its own B:C ratio.
  expected <- data.frame(
    N = c(20, 60, 0, 30, 22.5, 67.5, 36, 18, 6, 24, 16),
    N_draw_1 = c(20, 60, 0, 30, 60, 30, 36, 6, 18, 24, 16))
  order <- c(11, 5, 3, 8, 2, 9, 6, 10, 1, 7, 4)
  extended <- extended[order, ]
  expected <- expected[order, ]
  frame <- frame[c(3, 2, 4, 1), ]
  targets <- data.frame(region = c("south", "north"), vote = "A", target = c(60, 50))
  single <- rake_extended_frame(extended, frame, targets, by = "region",
                                extension_variable = "vote", target_scale = "count")
  count_columns <- names(expected)
  keys <- c("region", "cell", "vote")
  stopifnot(identical(single[keys], extended[keys]),
            max(abs(as.matrix(single[count_columns]) - as.matrix(expected))) < 1e-8)

  # Cross a second outcome with these cells. Its seed split is the same in
  # every row, so its independent national 80:20 target multiplies each of the
  # known vote counts by 0.8 or 0.2.
  joint <- extended[rep(seq_len(nrow(extended)), each = 2L), ]
  joint$second <- rep(c("L", "R"), nrow(extended))
  joint[count_columns] <- joint[count_columns] * ifelse(joint$second == "L", 0.4, 0.6)
  expected_joint <- expected[rep(seq_len(nrow(expected)), each = 2L), ] *
    ifelse(joint$second == "L", 0.8, 0.2)
  targets$target <- c(0.6, 0.25)
  both <- rake_joint_extended_frame(
    joint, frame, targets = list(vote = targets, second = data.frame(second = "R", target = 60)),
    by = list(vote = "region", second = character()),
    target_scale = c(vote = "share", second = "count"))
  stopifnot(identical(both[c(keys, "second")], joint[c(keys, "second")]),
            max(abs(as.matrix(both[count_columns]) - as.matrix(expected_joint))) < 1e-8)

  # Every input count fits in a 32-bit integer, but the group total does not.
  large_frame <- data.frame(cell = c("x", "y"), N = rep(1500000000L, 2))
  large_seed <- data.frame(cell = rep(large_frame$cell, each = 2L), vote = c("A", "B"),
                           N = rep(750000000L, 4))
  large_targets <- data.frame(vote = c("A", "B"), target = c(1300000000L, 1700000000L))
  single <- rake_extended_frame(large_seed, large_frame, large_targets, character(), "vote",
                                target_scale = "count")
  joint <- rake_joint_extended_frame(large_seed, large_frame, list(vote = large_targets),
                                     character(), target_scale = "count")
  expected <- rep(c(650000000, 850000000), 2)
  stopifnot(max(abs(single$N - expected)) < 1e-5, max(abs(joint$N - expected)) < 1e-5)
  large_seed$N <- as.integer(expected)
  large_seed$N_draw_1 <- large_seed$N
  large_targets$target <- large_targets$target / sum(large_frame$N)
  check <- check_extended_frame(large_seed, large_targets, character(), "vote", draw_plot = FALSE)
  stopifnot(max(abs(check$comparison$estimate - check$comparison$target)) < 1e-12)
}
