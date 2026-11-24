# Encode retained random levels as ordinary vglmer terms. The original
# grouping column stays available for any spline that uses it.
prepare_sequential_fit <- function(design) {
  data <- design$data
  random <- character()
  prediction <- list()
  dropped <- c(fixed = length(design$unsupported_fixed), scalar_random = 0L,
               correlated_random = 0L, smooth = 0L)
  retained_keys <- design$retained_keys
  groups <- names(design$group_slopes)
  group_predictors <- lapply(groups, function(group) {
    if (is.null(design$grouping_columns[[group]])) group else design$grouping_columns[[group]]
  })
  group_keys <- vapply(group_predictors, sequential_term_key, character(1))
  level_counts <- vapply(groups, function(group) nlevels(data[[group]]), integer(1))
  redundant_groups <- vapply(seq_along(groups), function(i) {
    parents <- match(sequential_term_parents(group_predictors[[i]]), group_keys)
    any(level_counts[parents[!is.na(parents)]] == level_counts[i])
  }, logical(1))
  support <- design$supported_random_levels
  trials <- data$.success + data$.failure
  supported_rows <- lapply(seq_along(groups), function(i) {
    if (length(design$group_slopes[[groups[i]]]) == 1L) return(NULL)
    index <- match(data[[groups[i]]], support[[i]])
    rows <- which(!is.na(index))
    list(rows = rows, levels = index[rows], trials = trials[rows])
  })
  retained_slopes <- vector("list", length(groups))
  orphaned <- character()
  unsupported_interactions <- character()
  unsupported_main <- character()
  sparse_slopes <- numeric()
  redundant_intercepts <- character()
  candidates <- do.call(rbind, lapply(seq_along(groups), function(i) {
    slopes <- design$group_slopes[[groups[i]]]
    predictors <- lapply(strsplit(slopes, ":", fixed = TRUE),
                         function(x) c(group_predictors[[i]], setdiff(x, "1")))
    data.frame(group = i, slope = slopes, predictors = I(predictors))
  }))
  for (row in order(lengths(candidates$predictors))) {
    i <- candidates$group[row]
    predictors <- candidates$predictors[[row]]
    key <- sequential_term_key(predictors)
    if (!all(sequential_term_parents(predictors) %in% retained_keys)) {
      orphaned <- c(orphaned, key)
      next
    }
    if (!length(support[[i]])) {
      if (length(predictors) == 1L) {
        unsupported_main <- c(unsupported_main, key)
      } else {
        unsupported_interactions <- c(unsupported_interactions, key)
      }
      next
    }
    slope <- candidates$slope[row]
    if (slope == "1" && redundant_groups[i]) {
      redundant_intercepts <- c(redundant_intercepts, key)
      next
    }
    if (slope != "1") {
      columns <- strsplit(slope, ":", fixed = TRUE)[[1L]]
      group_rows <- supported_rows[[i]]
      values <- Reduce(`*`, lapply(data[columns], `[`, group_rows$rows))
      average <- mean_varying_slope_support(values, group_rows$levels, group_rows$trials)
      if (average < 10) {
        sparse_slopes[key] <- average
        next
      }
    }
    retained_slopes[[i]] <- c(retained_slopes[[i]], slope)
    retained_keys <- c(retained_keys, key)
  }
  for (i in seq_along(groups)) {
    group <- groups[i]
    slopes <- retained_slopes[[i]]
    retained <- support[[i]]
    original_width <- length(design$group_slopes[[group]])
    width <- length(slopes)
    kind <- if (original_width == 1L) "scalar_random" else "correlated_random"
    dropped[kind] <- dropped[kind] + original_width * nlevels(data[[group]]) -
      width * length(retained)
    if (!width) next
    if (length(retained) == nlevels(data[[group]])) {
      random <- c(random, paste0("(", paste(slopes, collapse = " + "), " | ", group, ")"))
      next
    }
    basis <- stats::model.matrix(stats::reformulate(slopes), data)
    generated <- tail(make.unique(c(names(data), paste0(".vf_", group, "_",
                                                        c("level", seq_len(width))))), width + 1L)
    fit_group <- generated[1L]
    fit_columns <- generated[-1L]
    labels <- as.character(data[[group]])
    active <- labels %in% retained
    labels[!active] <- retained[1L]
    data[[fit_group]] <- factor(labels, levels = retained)
    data[fit_columns] <- as.data.frame(basis * active)
    random <- c(random, paste0("(0 + ", paste(fit_columns, collapse = " + "),
                               " | ", fit_group, ")"))
    prediction[[fit_group]] <- list(group = group, slopes = slopes, columns = fit_columns)
  }

  screened_groups <- vapply(prediction, "[[", character(1), "group")
  smooth <- design$candidate_smooth_terms
  smooth_keys <- vapply(design$smooth_predictors, sequential_term_key, character(1))
  eligible <- smooth_keys %in% retained_keys
  grouped <- grepl(", by = ", smooth, fixed = TRUE)
  supplied <- sub(", by = [^)]*\\)$", ")", smooth[eligible & grouped])
  keep_smooth <- eligible & (grouped | !smooth %in% supplied)
  removed_smooth <- smooth %in% design$smooth_terms & !eligible
  formula_env <- asNamespace("vglmer")
  screened_smooth_groups <- character()
  removed_smooth_columns <- 0
  for (i in which(keep_smooth)) {
    spec <- as.list(str2lang(smooth[i]))[-1L]
    by <- as.character(spec$by)
    if (!length(by) || !by %in% screened_groups) next
    group <- match(by, groups)
    retained <- support[[group]]
    existing <- match(by, screened_smooth_groups)
    if (is.na(existing)) {
      masked_by <- tail(make.unique(c(names(data), paste0(".vf_smooth_", by))), 1L)
      data[[masked_by]] <- factor(data[[by]], levels = retained)
      screened_smooth_groups[masked_by] <- by
    } else {
      masked_by <- names(screened_smooth_groups)[existing]
    }
    total_levels <- nlevels(data[[by]])
    removed_smooth_columns <- removed_smooth_columns +
      (total_levels - length(retained)) * design$candidate_smooth_width[i] / (1 + total_levels)
    if (is.null(spec$type)) {
      term <- as.character(spec[[1L]])
      # Use the native penalized-basis path without the automatic linear terms.
      # The callback evaluates the original truncated-power basis at prediction.
      register_sequential_spline_basis()
      if (identical(formula_env, asNamespace("vglmer"))) formula_env <- new.env(parent = formula_env)
      values <- unique(data[[term]])
      count <- floor(min(length(values) / 4, 35))
      probs <- seq(0, 1, length.out = count + 2L)[seq_len(count) + 1L]
      name <- paste0(".vf_knots_", i)
      formula_env[[name]] <- structure(
        list(term = term, knots = stats::quantile(values, probs, names = FALSE)),
        class = "voteframe_tpf")
      smooth[i] <- paste0("v_s(", term, ', type = "randwalk", knots = ', name,
                          ", by = ", masked_by, ", by_re = FALSE, outer_okay = TRUE)")
    } else {
      spec$by <- as.name(masked_by)
      spec$by_re <- FALSE
      spec$outer_okay <- TRUE
      smooth[i] <- paste(deparse(as.call(c(list(as.name("v_s")), spec))), collapse = "")
    }
  }
  if (length(screened_smooth_groups)) {
    # Missing labels remove only the group-specific basis. Keep these survey
    # rows in the model frame for their response and all other terms.
    attr(data, "na.action") <- stats::na.pass
  }
  dropped["smooth"] <- sum(design$candidate_smooth_width[removed_smooth]) + removed_smooth_columns
  formula <- stats::reformulate(c("1", design$fixed_terms, random, smooth[keep_smooth]),
                                "cbind(.success, .failure)", env = formula_env)
  list(formula = formula, data = data, random_prediction = prediction,
       screened_smooth_groups = screened_smooth_groups,
       dropped_smooth_level_columns = removed_smooth_columns,
       hierarchical = length(random) + sum(keep_smooth) > 0L,
       joint_smooth = any(grepl('type = "gKRLS"', smooth[keep_smooth], fixed = TRUE)),
       estimated_parameters = design$retained_fixed_coefficients + design$random_coefficients -
         sum(dropped[c("scalar_random", "correlated_random")]) +
         sum(design$candidate_smooth_width[keep_smooth]) - removed_smooth_columns,
       orphaned_random_terms = orphaned, redundant_random_groups = redundant_intercepts,
       unsupported_random_interactions = unsupported_interactions,
       unsupported_random_main_effects = unsupported_main, sparse_random_slopes = sparse_slopes,
       dropped = dropped, dropped_smooth_terms = design$candidate_smooth_terms[removed_smooth])
}

# Count trials outside each group's modal slope value. A constant group
# contributes zero, including when its common slope value is nonzero.
mean_varying_slope_support <- function(values, groups, trials) {
  ordered <- order(groups, values)
  groups <- groups[ordered]
  values <- values[ordered]
  start <- c(TRUE, diff(groups) != 0L | values[-1L] != values[-length(values)])
  counts <- rowsum(trials[ordered], cumsum(start), reorder = FALSE)[, 1L]
  modal_counts <- tapply(counts, groups[start], max)
  (sum(counts) - sum(modal_counts)) / length(modal_counts)
}

register_sequential_spline_basis <- function() {
  registerS3method("Predict.matrix", "voteframe_tpf", predict_sequential_tpf,
                   envir = asNamespace("mgcv"))
}

predict_sequential_tpf <- function(object, data, ...) {
  basis <- outer(data[[object$term]], object$knots, "-")
  Matrix::drop0(basis * (basis > 0))
}

# vglmer requires a hierarchical term. If screening removes them all,
# fit the remaining aggregated binomial likelihood with glm().
fit_sequential_fixed_only <- function(formula, data, iterations) {
  model <- stats::glm(formula, data, family = stats::binomial(),
                      control = stats::glm.control(maxit = iterations))
  if (!model$converged || any(!is.finite(stats::coef(model)))) {
    stop("The fixed-only binomial model did not converge with finite coefficients.")
  }
  structure(
    list(beta = list(mean = as.matrix(stats::coef(model)), covariance = stats::vcov(model)),
         alpha = list(mean = matrix(numeric(), 0L, 1L)),
         formula = list(fe_terms = stats::terms(model), fe_Xlevels = model$xlevels,
                        fe_contrasts = model$contrasts,
                        interpret_gam = list(pred.names = all.vars(formula[[3L]]))),
         internal_parameters = list(it_used = model$iter)),
    class = "voteframe_fixed_logit")
}
