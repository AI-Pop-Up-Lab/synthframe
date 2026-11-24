# Compare support for candidate designs before running the fitting engine.
plot_sequential_depth_support <- function(stage, area_variable, depth, splines,
                                          area_predictors, no_interactions,
                                          trials, selected, outcome, path) {
  depths <- sort(unique(c(if (depth >= 1) 1:3, depth, selected$depth)))
  # Building joint spline bases uses random sketches. Restore the fit's RNG
  # state and give every candidate the same starting state when a seed is set.
  seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (!is.null(seed)) {
      assign(".Random.seed", seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  results <- lapply(depths, function(candidate) {
    if (!is.null(seed)) assign(".Random.seed", seed, envir = .GlobalEnv)
    design <- if (candidate == selected$depth) selected else {
      sequential_stage_design(stage, area_variable, candidate, splines,
                              area_predictors, no_interactions, trials)
    }
    sequential_coefficient_support(design, trials)
  })
  title <- paste("Coefficient support in the", outcome, "stage")
  write_sequential_depth_support(results, depths, sum(trials), title, path)
}

# Count exact nonzeros in each coefficient column. For group-specific columns,
# sum the basis within each group without materializing the crossed matrix.
sequential_coefficient_support <- function(design, trials) {
  data <- design$data
  support <- function(basis, group = NULL, include_global = TRUE) {
    nonzero <- as.matrix(basis != 0)
    weighted <- nonzero * trials
    counts <- if (include_global) rbind(colSums(nonzero), colSums(weighted))
    if (is.null(group)) return(counts)
    cbind(counts, rbind(as.vector(rowsum(nonzero * 1L, group)),
                        as.vector(rowsum(weighted, group))))
  }

  fixed <- design$candidate_fixed_terms
  spline_blocks <- list()
  if (length(design$smooth_terms)) {
    if (!requireNamespace("vglmer", quietly = TRUE)) {
      stop("Install vglmer to build the spline depth support diagnostic.")
    }
    if (design$joint_smooth && !requireNamespace("gKRLS", quietly = TRUE)) {
      stop("Install gKRLS to build the joint spline depth support diagnostic.")
    }
    build_spline <- utils::getFromNamespace("vglmer_build_spline", "vglmer")
    for (term in design$smooth_terms) {
      spec <- eval(str2lang(term), envir = asNamespace("vglmer"))
      joint <- spec$type %in% c("gKRLS", "randwalk")
      values <- if (joint) data[spec$term] else data[[spec$term]]
      if (!joint) fixed <- c(fixed, spec$term)
      basis <- build_spline(x = values, by = NULL, type = spec$type,
                            knots = spec$knots, xt = spec$xt,
                            force_vector = spec$force_vector,
                            outer_okay = spec$outer_okay, by_re = spec$by_re)
      spline_blocks[[length(spline_blocks) + 1L]] <- support(basis[[1L]]$x, data[[spec$by]])
    }
  }

  fixed_block <- support(stats::model.matrix(stats::reformulate(c("1", unique(fixed))), data))
  random_blocks <- lapply(names(design$group_slopes), function(group) {
    basis <- stats::model.matrix(stats::reformulate(design$group_slopes[[group]]), data)
    support(basis, data[[group]], include_global = FALSE)
  })
  scalar <- lengths(design$group_slopes) == 1L
  blocks <- list("Scalar random effects" = random_blocks[scalar],
                 "Correlated random effects" = random_blocks[!scalar],
                 "Smooths" = spline_blocks, "Fixed effects" = list(fixed_block))
  coefficients <- sum(vapply(c(list(fixed_block), random_blocks, spline_blocks),
                             ncol, integer(1L)))
  nbins <- sum(trials) + 1L
  cumulative_share <- function(blocks, row) {
    counts <- unlist(lapply(blocks, function(block) block[row, ]), use.names = FALSE)
    100 * cumsum(tabulate(counts + 1L, nbins)) / coefficients
  }
  by_effect <- vapply(blocks, cumulative_share, numeric(nbins), row = 2L)
  list(coefficients = coefficients,
       rows = cumulative_share(unlist(blocks, recursive = FALSE), 1L),
       respondents = rowSums(by_effect),
       respondents_by_effect = by_effect)
}

write_sequential_depth_support <- function(results, depths, observations, title, path) {
  thresholds <- 0:observations
  colors <- if (length(depths) <= 3L) c("#176A61", "#155A9C", "#A34718")[seq_along(depths)] else {
    grDevices::hcl.colors(length(depths), "Dark 3")
  }
  counts <- format(vapply(results, "[[", integer(1), "coefficients"), big.mark = ",", trim = TRUE)

  n_breakdown_rows <- ceiling(length(depths) / 3L)
  grDevices::png(path, width = 2400, height = 1150 + 590 * n_breakdown_rows, res = 170)
  on.exit(grDevices::dev.off(), add = TRUE)
  breakdown_panels <- c(seq_along(depths) + 3L, rep(0L, n_breakdown_rows * 3L - length(depths)))
  graphics::layout(rbind(rep(c(1L, 2L), each = 3L), rep(3L, 6L),
                         matrix(rep(breakdown_panels, each = 2L), ncol = 6L, byrow = TRUE),
                         rep(length(depths) + 4L, 6L)),
                   heights = c(5, 0.65, rep(3.7, n_breakdown_rows), 0.65))
  graphics::par(oma = c(0, 0, 3.2, 0), las = 1, bty = "l", family = "sans")
  draw_panel <- function(heading, transform, ticks, y_label = "") {
    x <- transform(thresholds)
    graphics::plot(x, results[[1L]]$rows, type = "n", ylim = c(0, 100),
                   xaxs = "i", yaxs = "i", axes = FALSE,
                   xlab = "Nonzero observations per coefficient", ylab = y_label)
    graphics::abline(h = seq(0, 100, 20), col = "#DCE4ED")
    graphics::axis(1, at = transform(ticks), col = "#627186",
                   labels = format(ticks, big.mark = ",", trim = TRUE))
    graphics::axis(2, at = seq(0, 100, 20), col = "#627186")
    graphics::box(bty = "l", col = "#627186")
    for (i in seq_along(results)) {
      graphics::lines(x, results[[i]]$rows, type = "s", col = colors[i], lwd = 2.7)
      graphics::lines(x, results[[i]]$respondents, type = "s", col = colors[i], lwd = 2.7, lty = 2)
    }
    graphics::title(main = heading, line = 0.8)
  }

  linear_ticks <- pretty(thresholds, n = 6L)
  powers <- 10^(0:floor(log10(observations)))
  log_ticks <- sort(c(0, powers, 3 * powers))
  graphics::par(mar = c(4.8, 5, 2, 1.3))
  draw_panel("Linear axis", identity, linear_ticks[linear_ticks <= observations],
             "Cumulative share of coefficients (%)")
  graphics::par(mar = c(4.8, 3.3, 2, 1.6))
  draw_panel("Logarithmic axis", function(x) log10(1 + x), log_ticks[log_ticks <= observations])

  graphics::par(mar = rep(0, 4))
  graphics::plot.new()
  graphics::legend("center", legend = c(paste0("Depth ", depths, " (", counts, " coefficients)"),
                                        "Aggregated survey rows", "Individual respondents"),
                   col = c(colors, "#4B5563", "#4B5563"), lty = c(rep(1, length(depths)), 1, 2),
                   lwd = 2.7, bty = "n", ncol = min(5L, length(depths) + 2L),
                   cex = 0.92, x.intersp = 0.7)

  effect_colors <- c("#267D83", "#835799", "#D58B45", "#68778D")
  x <- log10(1 + thresholds)
  step_x <- c(x[1L], rep(x[-1L], each = 2L))
  step_y <- function(y) c(rep(y[-length(y)], each = 2L), tail(y, 1L))
  breakdown_ticks <- log_ticks[log_ticks <= observations]
  for (i in seq_along(results)) {
    graphics::par(mar = c(4.2, 5, 2.3, 1.3))
    by_effect <- results[[i]]$respondents_by_effect
    graphics::plot(x, results[[i]]$respondents, type = "n", ylim = c(0, 100),
                   xaxs = "i", yaxs = "i", axes = FALSE, ylab = "",
                   xlab = "Nonzero respondents per coefficient")
    graphics::abline(h = seq(0, 100, 20), col = "#DCE4ED")
    # Draw stacked step areas so the top edge matches the respondent curve.
    lower <- rep(0, length(x))
    for (j in seq_len(ncol(by_effect))) {
      upper <- lower + by_effect[, j]
      graphics::polygon(c(step_x, rev(step_x)), c(step_y(upper), rev(step_y(lower))),
                        col = effect_colors[j], border = NA)
      lower <- upper
    }
    graphics::axis(1, at = log10(1 + breakdown_ticks), col = "#627186",
                   labels = format(breakdown_ticks, big.mark = ",", trim = TRUE))
    graphics::axis(2, at = seq(0, 100, 20), col = "#627186", las = 1)
    graphics::box(bty = "l", col = "#627186")
    if ((i - 1L) %% 3L == 0L) {
      graphics::mtext("Share of all coefficients (%)", side = 2, line = 3.3, las = 0)
    }
    graphics::title(main = paste("Depth", depths[i], "by effect type"), line = 0.6)
  }
  graphics::par(mar = rep(0, 4))
  graphics::plot.new()
  graphics::legend("center", legend = colnames(results[[1L]]$respondents_by_effect),
                   fill = effect_colors, border = NA, bty = "n", horiz = TRUE, cex = 0.92)
  graphics::mtext(title, outer = TRUE, side = 3, line = 1, font = 2, cex = 1.3)
}
