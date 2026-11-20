# Use posterior mean counts so each cell's outcome counts add to its population.
collapse_extended_frame <- function(frame, posterior) {
  frame <- as.data.frame(frame)
  rows <- match_posterior_cells(frame, posterior)
  cell_columns <- setdiff(names(frame), c(posterior$count_variable,
                                        extended_frame_draw_columns(frame, posterior$count_variable)))
  if (posterior$extension_variable %in% cell_columns) {
    stop("The frame already contains the extension variable.")
  }
  # Average before selecting rows so collapse needs only one count per outcome.
  draws <- posterior$draws
  population_draws <- paired_frame_counts(frame, posterior)
  if (!is.null(population_draws)) {
    return(expand_expected_counts(frame[, cell_columns, drop = FALSE],
                                  draws[rows, , , drop = FALSE],
                                  frame[[posterior$count_variable]], posterior,
                                  include_draws = FALSE, population_draws = population_draws))
  }
  # Combine the draws for each cell and outcome into one mean probability.
  means <- rowMeans(matrix(draws, dim(draws)[1L] * dim(draws)[2L]))
  # Restore the cell and outcome axes so the shared frame formatter can use them.
  means <- array(means, c(dim(draws)[1:2], 1L),
                 dimnames = c(dimnames(draws)[1:2], list(NULL)))
  # Multiply the matched mean probabilities by the supplied cell populations.
  expand_expected_counts(frame[, cell_columns, drop = FALSE],
                         means[rows, , , drop = FALSE],
                         frame[[posterior$count_variable]], posterior,
                         include_draws = FALSE)
}
