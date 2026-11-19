# Build a candidate formula and count the coefficients it would estimate.
sequential_stage_design <- function(data, area_variable, depth, splines,
                                    area_predictors = character()) {
  if (anyDuplicated(names(data)) || anyNA(names(data)) || any(!nzchar(names(data)))) {
    stop("Stage predictors must have unique, nonempty names.")
  }
  # Area IDs are categories, even when stored as numbers.
  for (name in names(data)) {
    x <- data[[name]]
    if (name == area_variable || is.factor(x) || is.character(x) || is.logical(x)) {
      if (anyNA(x) || any(!nzchar(as.character(x)))) {
        stop("Missing categorical values in ", name, ".")
      }
      data[[name]] <- factor(x)
    } else if (!is.numeric(x) || any(!is.finite(x))) {
      stop("Expected finite numeric or categorical values in ", name, ".")
    }
  }

  # Make names safe for vglmer formulas and reserve the two count columns.
  original_names <- names(data)
  names(data) <- make.names(c(".success", ".failure", original_names), unique = TRUE)[
    seq_along(original_names) + 2L
  ]
  predictor_names <- stats::setNames(names(data), original_names)
  # Stage data has already been filtered. Levels and ranges come from this stage.
  unique_values <- vapply(data, function(x) length(unique(x)), integer(1))
  factors <- names(data)[vapply(data, is.factor, logical(1))]
  # Constant predictors have no variation to explain within this stage.
  factors <- factors[unique_values[factors] > 1L]
  continuous <- names(data)[!vapply(data, is.factor, logical(1)) & unique_values > 1L]
  # A univariate spline needs at least four distinct values.
  smoothable <- continuous[splines & unique_values[continuous] >= 4L]

  fixed <- character()
  smooth <- character()
  smooth_width <- numeric()
  grouping_columns <- list()
  group_slopes <- list()
  predictors <- c(factors, continuous)
  # Below depth one, only depth 0.75 adds area covariates, as fixed effects.
  area_group <- unname(predictor_names[area_variable])
  # Depth zero keeps the global intercept and an area random intercept.
  if (depth == 0 && area_group %in% factors) group_slopes[[area_group]] <- "1"
  if (depth %in% c(0.5, 0.75)) {
    area_columns <- unname(predictor_names[area_predictors])
    # At depth 0.5, categorical survey predictors get random intercepts.
    for (group in setdiff(factors, area_columns)) group_slopes[[group]] <- "1"
    if (depth == 0.75) fixed <- intersect(area_columns, predictors)
  }
  # A term uses one unit of depth for each predictor it contains.
  for (size in seq_len(min(depth, length(predictors)))) {
    for (terms in utils::combn(predictors, size, simplify = FALSE)) {
      # Factors determine the grouping. Numeric predictors determine the slopes.
      groups <- intersect(terms, factors)
      numeric <- intersect(terms, continuous)
      group <- NULL
      if (length(groups)) {
        if (length(groups) == 1L) {
          group <- groups
        } else {
          # Group jointly on the observed tuples of factor levels.
          group <- paste0(".group_", paste(match(groups, factors), collapse = "_"))
          if (group %in% predictor_names) {
            stop("Predictor name collides with generated grouping column ", group, ".")
          }
          if (is.null(grouping_columns[[group]])) {
            grouping_columns[[group]] <- groups
            # Quoting labels keeps tuples distinct even when levels contain dots.
            data[[group]] <- sequential_group(data, groups)
          }
        }
        # Each group gets one intercept, shared by all its slope terms.
        if (!group %in% names(group_slopes)) group_slopes[[group]] <- "1"
      }

      # A numeric term is fixed unless it varies by a categorical group.
      has_spline <- any(numeric %in% smoothable)
      if (length(numeric)) {
        # Several numeric predictors form a product term.
        linear <- paste(numeric, collapse = ":")
        if (is.null(group)) {
          fixed <- c(fixed, linear)
        } else {
          group_slopes[[group]] <- c(group_slopes[[group]], linear)
        }
      }
      if (has_spline) {
        # Use a truncated linear spline for one predictor and gKRLS for several.
        arguments <- numeric
        if (length(numeric) > 1L) arguments <- c(arguments, 'type = "gKRLS"')
        if (!is.null(group)) arguments <- c(arguments, paste("by =", group))
        smooth <- c(smooth, paste0("v_s(", paste(arguments, collapse = ", "), ")"))
        # Estimate basis size from the default knot or sketch rule.
        width <- if (length(numeric) == 1L) {
          floor(min(unique_values[numeric] / 4, 35))
        } else {
          min(nrow(data), 5 * ceiling(nrow(data)^(1 / 3)))
        }
        # A by-smooth includes the global basis plus one basis per observed level.
        if (!is.null(group)) width <- width * (1 + nlevels(data[[group]]))
        smooth_width <- c(smooth_width, width)
      }
    }
  }

  random <- vapply(names(group_slopes), function(group) {
    paste0("(", paste(group_slopes[[group]], collapse = " + "), " | ", group, ")")
  }, character(1))
  # Every by-smooth already supplies a global smooth in vglmer.
  # Drop the standalone global term when one or more by-smooths supply it.
  global <- !grepl(", by = ", smooth, fixed = TRUE)
  supplied_by_group <- sub(", by = [^)]*\\)$", ")", smooth[!global])
  keep_smooth <- !(global & smooth %in% supplied_by_group)
  smooth <- smooth[keep_smooth]

  # Count fixed columns, random coefficients per group level, and spline columns.
  # Include the global intercept. Count columns as built, before shrinkage.
  fixed_width <- ncol(stats::model.matrix(stats::reformulate(c("1", fixed)), data))
  # Each observed level gets its own coefficient for every term in its group.
  random_width <- sum(vapply(names(group_slopes), function(group) {
    nlevels(data[[group]]) * length(group_slopes[[group]])
  }, numeric(1)))
  spline_width <- sum(smooth_width[keep_smooth])
  parameter_count <- fixed_width + random_width + spline_width

  # vglmer treats the two response columns as binomial successes and failures.
  formula <- stats::reformulate(c("1", fixed, random, smooth),
                                response = "cbind(.success, .failure)", env = baseenv())
  list(data = data, formula = formula, predictor_names = predictor_names,
       grouping_columns = grouping_columns, factors = factors,
       parameter_count = parameter_count,
       fixed_coefficients = fixed_width, random_coefficients = random_width,
       spline_coefficients = spline_width,
       hierarchical = length(random) + length(smooth) > 0L,
       joint_smooth = any(grepl('type = "gKRLS"', smooth, fixed = TRUE)))
}

# Use the same group labels in the survey and the prediction frame.
sequential_group <- function(data, columns) {
  labels <- lapply(data[columns], function(x) encodeString(as.character(x), quote = '"'))
  factor(do.call(paste, c(unname(labels), sep = ".")))
}
