check_raking_change_shares <- function() {
  # Capture the quantities passed to the renderer from each plotting entry point.
  captured <- NULL
  plot_env <- new.env(parent = environment(plot_raking_changes))
  plot_env$draw_raking_change_panels <- function(summary, labels, outcomes, plot_file) {
    captured <<- list(margins = setNames(summary, labels), outcomes = outcomes)
  }
  direct <- plot_raking_changes
  environment(direct) <- plot_env
  plot_env$partitioned_raking_change_summary <- partitioned_raking_change_summary
  environment(plot_env$partitioned_raking_change_summary) <- plot_env
  partitioned <- plot_partitioned_raking_changes
  environment(partitioned) <- plot_env

  frame <- data.frame(area = c("east", "west"), age = "adult", N = c(100, 200),
                      N_draw_1 = c(120, 180), N_draw_2 = c(80, 240))
  outcomes <- c("No Vote", "A", "B")
  extended <- data.frame(area = rep(frame$area, each = 3L), age = "adult",
                         vote = rep(outcomes, 2L), N = c(40, 30, 30, 80, 60, 60),
                         N_draw_1 = c(48, 36, 36, 72, 54, 54),
                         N_draw_2 = c(80, 0, 0, 240, 0, 0))
  raked <- extended
  raked$N <- c(60, 20, 20, 60, 70, 70)
  raked$N_draw_1 <- c(72, 24, 24, 90, 45, 45)
  raked$N_draw_2 <- c(72, 4, 4, 216, 12, 12)

  direct(extended, raked, frame, "area", "vote", "unused", no_vote = "No Vote")
  result <- captured
  same <- function(x, y) isTRUE(all.equal(unname(x), y))
  local <- result$margins[["area × age"]]
  national <- result$margins[["age"]]
  # Party A always has half the votes cast. Its eligible-population share falls
  # ten points in East and rises five points in West. These cancel nationally.
  stopifnot(identical(result$outcomes, outcomes),
            same(local$points[[2L]], c(20, -10)),
            same(local$points[[3L]], c(-10, 5)), same(local$points[[4L]], c(-10, 5)),
            same(local$squares, c(100 * 40 / 300, 0, 0, 0)),
            same(national$squares, c(0, 0, 0, 0)),
            same(local$points[[1L]], c(14, 10)), same(national$points[[1L]], c(14, 10)))
  direct(extended, raked, frame, "area", "vote", "unused")
  stopifnot(identical(result, captured))

  # A separate file may reorder both its outcome rows and its draw columns.
  as_parts <- function(x) list(x[1:3, ], x[6:4, c("area", "age", "vote", "N", "N_draw_2", "N_draw_1")])
  partitioned(as_parts(extended), as_parts(raked), frame, list(1L, 2L), "area", "vote",
              "No Vote", "unused", read_extended = identity, read_raked = identity)
  stopifnot(isTRUE(all.equal(result, captured)))

  # Disjoint ballot choices include a nonvoting label first observed in the
  # second partition. Absent choices contribute zero in every group there.
  extended$vote <- raked$vote <- c("A", "B", "C", "No Vote", "D", "E")
  direct(extended, raked, frame, "area", "vote", "unused", no_vote = "No Vote")
  result <- captured
  first <- list(extended[1:3, ], extended[4:6, ])
  second <- list(raked[1:3, ], raked[4:6, ])
  partitioned(first, second, frame, list(1L, 2L), "area", "vote",
              "No Vote", "unused", read_extended = identity, read_raked = identity)
  stopifnot(isTRUE(all.equal(result, captured)),
            same(captured$margins[["area × age"]]$points[[2L]], c(20, 0)),
            same(captured$margins[["area × age"]]$points[[5L]], c(0, -10)),
            same(captured$margins[["age"]]$points[[1L]], c(14, 10)))
  figure <- tempfile(fileext = ".png")
  on.exit(unlink(figure))
  plot_partitioned_raking_changes(first, second, frame, list(1L, 2L), "area", "vote", "No Vote",
                                  figure, read_extended = identity, read_raked = identity)
  stopifnot(file.info(figure)$size > 0)
}
