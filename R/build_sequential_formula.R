# Build a candidate formula and count the coefficients it would estimate.
sequential_stage_design <- function(data, area_variable, depth, splines,
                                    area_predictors = character(),
                                    no_interactions = character(), trials = rep(1, nrow(data)),
                                    minimum_support = -1L) {
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
  smooth_predictors <- list()
  grouping_columns <- list()
  group_slopes <- list()
  predictors <- c(factors, continuous)
  interaction_predictors <- setdiff(predictors, unname(predictor_names[no_interactions]))
  # Below depth one, only depth 0.75 adds area covariates, as fixed effects.
  area_group <- unname(predictor_names[area_variable])
  area_columns <- unname(predictor_names[area_predictors])
  # Depth zero keeps the global intercept and an area random intercept.
  if (depth == 0 && area_group %in% factors) group_slopes[[area_group]] <- "1"
  if (depth %in% c(0.5, 0.75)) {
    # At depth 0.5, categorical survey predictors get random intercepts.
    for (group in setdiff(factors, area_columns)) group_slopes[[group]] <- "1"
    if (depth == 0.75) fixed <- intersect(area_columns, predictors)
  }
  # A term uses one unit of depth for each predictor it contains.
  max_size <- min(depth, length(predictors), max(1L, length(interaction_predictors)))
  for (size in seq_len(max_size)) {
    eligible <- if (size == 1L) predictors else interaction_predictors
    for (terms in utils::combn(eligible, size, simplify = FALSE)) {
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
        smooth_predictors[[length(smooth_width)]] <- terms
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
  nominal_smooth <- smooth[keep_smooth]

  # Count fixed columns, random coefficients per group level, and spline columns.
  # Include the global intercept. Count columns as built, before shrinkage.
  supported_random_levels <- lapply(names(group_slopes), function(group) {
    counts <- rowsum(trials, data[[group]])
    rownames(counts)[counts[, 1L] >= minimum_support]
  })
  fixed_design <- independent_sequential_fixed_effects(fixed, data, trials, minimum_support)
  fixed <- fixed_design$terms
  data <- fixed_design$data
  fixed_width <- fixed_design$rank
  # Each observed level gets its own coefficient for every term in its group.
  random_counts <- lapply(names(group_slopes), function(group) {
    group_predictors <- grouping_columns[[group]]
    if (is.null(group_predictors)) group_predictors <- group
    slopes <- strsplit(group_slopes[[group]], ":", fixed = TRUE)
    list(predictors = lapply(slopes, function(slope) c(group_predictors, setdiff(slope, "1"))),
         counts = rep(nlevels(data[[group]]), length(slopes)))
  })
  random_counts <- list(
    predictors = unlist(lapply(random_counts, "[[", "predictors"), recursive = FALSE),
    counts = unlist(lapply(random_counts, "[[", "counts"), use.names = FALSE)
  )
  random_width <- sum(random_counts$counts)
  spline_width <- sum(smooth_width[keep_smooth])
  parameter_count <- fixed_design$nominal_rank + random_width + spline_width
  combination_predictors <- c(fixed_design$coefficient_predictors, random_counts$predictors,
                              smooth_predictors[keep_smooth])
  combinations <- vapply(combination_predictors, function(variables) {
    if (!length(variables)) return("(Intercept)")
    paste(names(predictor_names)[predictor_names %in% variables], collapse = " × ")
  }, character(1))
  combination_counts <- tapply(c(fixed_design$coefficient_counts, random_counts$counts,
                                  smooth_width[keep_smooth]), combinations, sum)

  # vglmer treats the two response columns as binomial successes and failures.
  formula <- stats::reformulate(c("1", fixed, random, nominal_smooth),
                                response = "cbind(.success, .failure)", env = baseenv())
  list(data = data, formula = formula, predictor_names = predictor_names,
       grouping_columns = grouping_columns, group_slopes = group_slopes, factors = factors,
       supported_random_levels = supported_random_levels,
       fixed_terms = fixed, candidate_fixed_terms = fixed_design$candidate_terms,
       smooth_terms = nominal_smooth, candidate_smooth_terms = smooth,
       candidate_smooth_width = smooth_width,
       fixed_contrasts = fixed_design$contrasts,
       constant_predictors = names(predictor_names)[unique_values[unname(predictor_names)] == 1L],
       aliased_fixed = fixed_design$aliased,
       unsupported_fixed = fixed_design$unsupported,
       orphaned_fixed = fixed_design$orphaned, retained_keys = fixed_design$retained_keys,
       smooth_predictors = smooth_predictors,
       fixed_screen = fixed_design$diagnostics,
       parameter_count = parameter_count,
       combination_counts = combination_counts,
       fixed_coefficients = fixed_design$nominal_rank, retained_fixed_coefficients = fixed_width,
       random_coefficients = random_width,
       spline_coefficients = spline_width,
       hierarchical = length(random) + length(smooth) > 0L,
       joint_smooth = any(grepl('type = "gKRLS"', smooth, fixed = TRUE)))
}

# Keep an independent basis for the unpenalized design. Hierarchical terms
# have their own priors and do not enter this rank check.
independent_sequential_fixed_effects <- function(fixed, data, trials = rep(1, nrow(data)),
                                                 minimum_support = -1L) {
  terms <- stats::terms(stats::reformulate(c("1", fixed)), keep.order = TRUE)
  columns <- stats::model.matrix(terms, data)
  term_labels <- attr(terms, "term.labels")
  assignment <- attr(columns, "assign")
  supported <- assignment == 0L | colSums((columns != 0) * trials) >= minimum_support
  eligible <- which(supported)
  weighted <- columns * sqrt(trials)
  norms <- sqrt(colSums(weighted^2))
  scaled <- sweep(weighted, 2L, ifelse(norms > 0, norms, 1), "/")
  condition <- function(x) {
    if (nrow(x) < ncol(x)) return(Inf)
    values <- svd(x, nu = 0L, nv = 0L)$d
    max(values) / min(values)
  }
  maximum_condition <- 30
  before <- condition(scaled[, eligible, drop = FALSE])
  term_predictors <- strsplit(term_labels, ":", fixed = TRUE)
  term_keys <- vapply(term_predictors, sequential_term_key, character(1))
  keys <- c("", term_keys)[assignment + 1L]
  parents <- lapply(term_predictors, sequential_term_parents)
  degree <- c(0L, attr(terms, "order"))[assignment + 1L]
  prune_hierarchy <- function(columns) {
    present <- unique(keys[columns])
    for (i in order(lengths(term_predictors))) {
      if (!all(parents[[i]] %in% present)) present <- setdiff(present, term_keys[i])
    }
    columns[keys[columns] %in% present]
  }
  keep <- prune_hierarchy(eligible)
  orphaned <- setdiff(eligible, keep)
  removed <- list()
  while (condition(scaled[, keep, drop = FALSE]) >= maximum_condition) {
    x <- scaled[, keep, drop = FALSE]
    weakest <- svd(x, nu = 0L, nv = ncol(x))
    direction <- abs(weakest$v[, ncol(x)])
    involved <- keep[direction >= 0.01 * max(direction)]
    candidates <- involved[assignment[involved] != 0L]
    if (!length(candidates)) candidates <- keep[assignment[keep] != 0L]
    # Prefer removing a higher-order term in the weak direction. Within an
    # order, choose the deletion that gives the best-conditioned design.
    candidates <- candidates[degree[candidates] == max(degree[candidates])]
    alternatives <- lapply(candidates, function(column) prune_hierarchy(setdiff(keep, column)))
    scores <- vapply(alternatives, function(columns) condition(scaled[, columns, drop = FALSE]),
                     numeric(1))
    # On a tie, remove the later formula column and retain the earlier one.
    best_score <- min(scores)
    tied <- if (is.finite(best_score)) scores <= best_score * (1 + 1e-12) else scores == best_score
    best <- tail(which(tied), 1L)
    column <- candidates[best]
    removed[[colnames(columns)[column]]] <- colnames(columns)[setdiff(involved, column)]
    orphaned <- c(orphaned, setdiff(keep, c(alternatives[[best]], column)))
    keep <- alternatives[[best]]
  }
  aliases <- as.character(names(removed))
  retained <- character()
  contrasts <- list()
  for (i in seq_along(term_labels)) {
    term_columns <- which(assignment == i)
    retained_columns <- intersect(term_columns, keep)
    if (!length(retained_columns)) next
    label <- term_labels[[i]]
    if (length(retained_columns) == length(term_columns)) {
      retained <- c(retained, label)
      next
    }
    # A partially aliased term can only be a categorical fixed main effect
    # in these designs. Represent its surviving contrasts explicitly.
    predictor <- data[[label]]
    levels <- levels(predictor)
    level_rows <- match(levels, predictor)
    coding <- columns[level_rows, term_columns, drop = FALSE]
    reference <- which(rowSums(abs(coding)) == 0)
    baseline <- if (length(reference)) reference[1L] else 1L
    for (column in retained_columns) {
      name <- tail(make.unique(c(names(data), paste0(".fixed_", make.names(colnames(columns)[column])))), 1L)
      data[[name]] <- columns[, column]
      contrasts[[name]] <- list(predictor = label,
                               values = setNames(columns[level_rows, column], levels),
                               baseline = columns[level_rows[baseline], column])
      retained <- c(retained, name)
    }
  }
  counts <- tabulate(assignment[keep] + 1L, nbins = length(term_labels) + 1L)
  nominal_rank <- if (length(keep) == ncol(scaled)) length(keep) else qr(scaled, tol = 1e-10)$rank
  list(terms = retained, candidate_terms = fixed, data = data, rank = length(keep),
       nominal_rank = nominal_rank, aliased = aliases, contrasts = contrasts,
       unsupported = colnames(columns)[!supported],
       orphaned = colnames(columns)[orphaned],
       retained_keys = unique(keys[keep]),
       diagnostics = list(condition_before = before,
                          condition_after = if (identical(eligible, keep)) before else {
                            condition(scaled[, keep, drop = FALSE])
                          },
                          related_columns = removed),
       coefficient_predictors = c(list(character()), strsplit(term_labels, ":", fixed = TRUE))[counts > 0L],
       coefficient_counts = counts[counts > 0L])
}

# Use the same group labels in the survey and the prediction frame.
sequential_group <- function(data, columns) {
  labels <- lapply(data[columns], function(x) encodeString(as.character(x), quote = '"'))
  factor(do.call(paste, c(unname(labels), sep = ".")))
}

# Canonical keys for checking immediate parent terms across effect types.
sequential_term_key <- function(predictors) {
  paste(sort(predictors), collapse = ":")
}

sequential_term_parents <- function(predictors) {
  if (length(predictors) < 2L) return(character())
  vapply(seq_along(predictors), function(i) sequential_term_key(predictors[-i]), character(1))
}
