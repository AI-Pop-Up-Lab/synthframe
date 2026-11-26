check_structural_zeros <- function() {
  frame <- data.frame(area = c("B", "A", "B", "A"), age = c(1L, 2L, 2L, 1L),
                      N = c(35, 40, 65, 60), N_draw_1 = c(30, 55, 60, 75))
  # Y is a known outcome. Neither of the B cells has a row for it.
  seed <- frame[c(4L, 3L, 2L, 1L, 2L, 4L), ]
  seed$vote <- c("Y", "X", "X", "X", "Y", "X")
  for (col in c("N", "N_draw_1")) seed[seed$area == "A", col] <- seed[seed$area == "A", col] / 2
  target <- data.frame(area = c("B", "A", "B", "A"), vote = c("X", "X", "Y", "Y"),
                       target = c(1, .6, 0, .4))
  bake <- function(target, batch = FALSE, scale = "share") {
    rake_joint_extended_frame(seed, frame, list(local = target), by = "area",
                              extension_variables = "vote", margin_variables = c(local = "vote"),
                              target_scale = scale, area_variable = if (batch) "area",
                              areas_per_batch = if (batch) 1L)
  }

  # Full targets and a lone structural-zero target both preserve the B cells.
  for (tab in list(target, target[-1L, ])) {
    expected <- bake(tab)
    got <- bake(tab, batch = TRUE)
    stopifnot(isTRUE(all.equal(got, expected)), identical(got[c("area", "age", "vote")],
                                                        seed[c("area", "age", "vote")]))
    for (col in c("N", "N_draw_1")) {
      b <- got$area == "B"
      a <- got$area == "A"
      stopifnot(isTRUE(all.equal(got[[col]][b], seed[[col]][b])),
                abs(sum(got[[col]][a & got$vote == "X"]) / sum(got[[col]][a]) - .6) < 1e-10)
    }
  }

  # Counts use a different residual in each draw when the population varies.
  counts <- target[c(2L, 3L), ]
  counts$target <- c(60, 0)
  calibrated <- bake(counts, batch = TRUE, scale = "count")
  stopifnot(isTRUE(all.equal(calibrated, bake(counts, scale = "count"))))
  for (col in c("N", "N_draw_1")) {
    stopifnot(abs(sum(calibrated[[col]][calibrated$area == "A" & calibrated$vote == "X"]) - 60) < 1e-8)
  }

  # A positive target needs support. A misspelled outcome is unknown everywhere.
  unsupported <- transform(target, target = c(.9, .6, .1, .4))
  typo <- target
  typo$vote[3L] <- "misspelled"
  for (batch in c(FALSE, TRUE)) {
    for (bad in list(unsupported, typo)) {
      stopifnot(inherits(try(bake(bad, batch), silent = TRUE), "try-error"))
    }
  }
}
