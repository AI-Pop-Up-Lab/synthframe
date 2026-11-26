# Define the scope of each fitted effect on the population frame.
register_frame_levels <- function(fit, frame_data) {
  frame_data <- as_plain_frame(frame_data)
  # Build the frame design using the fitted contrasts, slopes, and spline bases.
  fixed <- build_prediction_fixed_matrix(fit, frame_data, allow_missing_levels = TRUE)
  blocks <- build_prediction_random_blocks(fit, restore_screened_random(fit, frame_data))
  fitted_random <- rownames(fit$alpha$mean)

  # Keep coefficients within their term. Separate splines can reuse column labels.
  random <- lapply(seq_along(blocks), function(i) {
    fitted <- if (is.null(fit$prior_random[[names(blocks)[i]]])) {
      fitted_random[fit$internal_parameters$cyclical_pos[[i]]]
    }
    # These columns come from combinations present in the frame.
    available <- colnames(blocks[[i]])
    # A frame-only coefficient has a prediction column but no survey estimate.
    list(fitted = fitted, frame = available, frame_only = setdiff(available, fitted))
  })
  names(random) <- names(blocks)
  # Record fixed contrasts separately because they lack a shared variance.
  fitted_fixed <- rownames(fit$beta$mean)
  list(fixed = list(fitted = fitted_fixed, frame = colnames(fixed),
                    frame_only = setdiff(colnames(fixed), fitted_fixed)),
       random = random)
}
