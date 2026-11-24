# Return log-odds with draws in rows and prediction cells in columns.
# With n_draws = 0, return one row using fitted means and zero for new effects.
predict_vglmer_levels <- function(fit, newdata, n_draws = 125L,
                                  allow_missing_levels = TRUE, new_fixed_sd = NULL,
                                  draw_state = NULL) {
  if (!inherits(fit, c("vglmer", "voteframe_fixed_logit"))) stop("Supply a fitted stage.")
  if (!is.numeric(n_draws) || length(n_draws) != 1L || !is.finite(n_draws) ||
      n_draws < 0 || n_draws != floor(n_draws) || n_draws > .Machine$integer.max) {
    stop("n_draws must be a nonnegative integer.")
  }
  if (!is.logical(allow_missing_levels) || length(allow_missing_levels) != 1L ||
      is.na(allow_missing_levels)) stop("allow_missing_levels must be TRUE or FALSE.")
  if (!is.null(new_fixed_sd) &&
      (!is.numeric(new_fixed_sd) || length(new_fixed_sd) != 1L ||
       !is.finite(new_fixed_sd) || new_fixed_sd <= 0)) {
    stop("new_fixed_sd must be a positive log-odds standard deviation.")
  }

  newdata <- restore_screened_random(fit, as.data.frame(newdata))
  missing <- setdiff(fit$formula$interpret_gam$pred.names, names(newdata))
  if (length(missing)) stop("Prediction data is missing: ", paste(missing, collapse = ", "), ".")
  if (!nrow(newdata) || anyNA(newdata[fit$formula$interpret_gam$pred.names])) {
    stop("Supply complete prediction rows.")
  }

  # Use fitted coefficient names to line up the design and the draws.
  fixed_names <- rownames(fit$beta$mean)
  random_names <- rownames(fit$alpha$mean)
  # Use vglmer's own variational draws for every coefficient learned in the fit.
  coefficients <- if (!is.null(draw_state$coefficients)) {
    draw_state$coefficients
  } else if (n_draws == 0) {
    t(as.matrix(rbind(fit$beta$mean, fit$alpha$mean)))
  } else if (inherits(fit, "voteframe_fixed_logit")) {
    draws <- mvtnorm::rmvnorm(n_draws, as.vector(fit$beta$mean), fit$beta$covariance)
    colnames(draws) <- fixed_names
    draws
  } else {
    vglmer::posterior_samples.vglmer(fit, samples = n_draws)
  }
  if (!is.null(draw_state)) draw_state$coefficients <- coefficients
  if (!identical(colnames(coefficients), c(fixed_names, random_names))) {
    stop("The vglmer coefficient order does not match the fitted design.")
  }

  # Start the log odds with the fitted fixed effects.
  fixed <- build_prediction_fixed_matrix(fit, newdata, allow_missing_levels)
  fitted_columns <- match(fixed_names, colnames(fixed))
  if (anyNA(fitted_columns)) stop("The prediction design is missing fitted fixed-effect columns.")
  logits <- fixed[, fitted_columns, drop = FALSE] %*%
    t(coefficients[, seq_along(fixed_names), drop = FALSE])
  new_fixed <- setdiff(seq_len(ncol(fixed)), fitted_columns)
  if (length(new_fixed) && n_draws > 0) {
    # Fixed effects have no shared variance in this fit. Their new terms need an explicit prior.
    if (is.null(new_fixed_sd)) {
      fixed_levels <- c(fit$formula$fe_Xlevels, fit$reduced_fixed_levels)
      unseen <- unlist(lapply(names(fixed_levels), function(predictor) {
        values <- setdiff(unique(as.character(newdata[[predictor]])), fixed_levels[[predictor]])
        if (length(values)) paste0(sQuote(predictor), " = ", dQuote(values))
      }), use.names = FALSE)
      if (!length(unseen)) unseen <- colnames(fixed)[new_fixed]
      stop("Fixed-effect values in the frame are absent from the fitted survey stage: ",
           paste(unseen, collapse = ", "), ". Supply a positive new_fixed_sd or harmonise ",
           "the frame and survey categories.", call. = FALSE)
    }
    # Draw each new contrast once per simulation. All matching rows reuse it.
    prior <- if (is.null(draw_state)) {
      matrix(stats::rnorm(length(new_fixed) * n_draws, sd = new_fixed_sd),
             nrow = length(new_fixed))
    } else {
      cached_prediction_effects(draw_state$fixed, colnames(fixed)[new_fixed],
                                function(n) matrix(stats::rnorm(n * n_draws, sd = new_fixed_sd),
                                                   nrow = n))
    }
    logits <- logits + fixed[, new_fixed, drop = FALSE] %*% prior
  }

  # Split off the random coefficients. Each fitted term gets its own design block.
  random <- coefficients[, length(fixed_names) + seq_along(random_names), drop = FALSE]
  blocks <- build_prediction_random_blocks(fit, newdata)
  for (i in seq_along(blocks)) {
    block <- blocks[[i]]
    # Separate spline blocks can reuse a column name. Match only within this block.
    term <- names(blocks)[i]
    prior_only <- !is.null(fit$prior_random[[term]])
    fitted <- if (!prior_only) fit$internal_parameters$cyclical_pos[[i]]
    index <- match(colnames(block), random_names[fitted])
    known <- !is.na(index)
    if (!all(known)) {
      # A level must match a whole coefficient vector or none of it.
      width <- if (prior_only) length(fit$prior_random[[term]]) else fit$internal_parameters$d_j[[i]]
      if (ncol(block) %% width != 0) {
        stop("Incomplete random-effect prediction design for ", sQuote(term), ".", call. = FALSE)
      }
      level_columns <- matrix(seq_len(ncol(block)), nrow = width)
      missing_per_level <- colSums(matrix(!known, nrow = width))
      partial <- which(missing_per_level > 0L & missing_per_level < width)
      if (length(partial)) {
        columns <- level_columns[, partial[1L]]
        group <- fit$screened_random[[term]]$group
        if (is.null(group)) group <- term
        stop("Partial random-effect match for grouping term ", sQuote(group), ". Matched: ",
             paste(sQuote(colnames(block)[columns[known[columns]]]), collapse = ", "),
             ". No fitted match for: ",
             paste(sQuote(colnames(block)[columns[!known[columns]]]), collapse = ", "),
             ". Each group level must match all or none of its coefficients. ",
             "Check that the saved fit and prediction design use the same coefficient labels.",
             call. = FALSE)
      }
    }
    if (any(known)) {
      logits <- logits + as.matrix(block[, known, drop = FALSE] %*%
        t(random[, fitted[index[known]], drop = FALSE]))
    }
    if (all(known)) next
    if (!allow_missing_levels) stop("New levels in ", sQuote(term), " are disabled.")
    # At the point estimate, only the fitted coefficients contribute.
    if (n_draws == 0) next

    new_levels <- level_columns[, missing_per_level == width, drop = FALSE]
    new_coefficients <- if (is.null(draw_state)) {
      roots <- prediction_variance_roots(fit, i, term, width, n_draws)
      sample_new_prediction_effects(roots, width, ncol(new_levels), n_draws)
    } else {
      if (is.null(draw_state$random[[term]])) {
        draw_state$random[[term]] <- list(
          effects = new.env(parent = emptyenv()),
          roots = prediction_variance_roots(fit, i, term, width, n_draws)
        )
      }
      cached <- draw_state$random[[term]]
      cached_prediction_effects(cached$effects, colnames(block)[new_levels[1L, ]],
                                function(n) sample_new_prediction_effects(cached$roots, width, n, n_draws),
                                width)
    }
    # Multiply by this term's intercept, slope, or spline basis values.
    logits <- logits + as.matrix(block[, as.vector(new_levels), drop = FALSE] %*% new_coefficients)
  }
  t(logits)
}

# Reuse an effect whenever its level appears in another prediction batch.
cached_prediction_effects <- function(cache, labels, sample, width = 1L) {
  new <- unique(labels[!vapply(labels, exists, logical(1), envir = cache, inherits = FALSE)])
  if (length(new)) {
    draws <- sample(length(new))
    for (i in seq_along(new)) {
      rows <- (i - 1L) * width + seq_len(width)
      cache[[new[i]]] <- draws[rows, , drop = FALSE]
    }
  }
  do.call(rbind, mget(labels, envir = cache, inherits = FALSE))
}

draw_vglmer_variance_roots <- function(fit, block, n_draws) {
  scale <- as.matrix(fit$sigma$cov[[block]])
  df <- fit$sigma$df[[block]]
  width <- fit$internal_parameters$d_j[[block]]
  if (!identical(dim(scale), rep(as.integer(width), 2L)) ||
      any(!is.finite(scale)) || !is.finite(df) || df <= width - 1L) {
    stop("Missing or invalid variance posterior for ", names(fit$sigma$df)[block], ".")
  }
  if (width == 1L) {
    if (scale[1L, 1L] <= 0) stop("The fitted variance scale must be positive.")
    return(sqrt(scale[1L, 1L] / stats::rchisq(n_draws, df)))
  }
  covariance <- CholWishart::rInvWishart(n_draws, df, scale)
  lapply(seq_len(n_draws), function(i) t(chol(covariance[, , i])))
}

sample_new_prediction_effects <- function(roots, width, n_levels, n_draws) {
  z <- matrix(stats::rnorm(width * n_levels * n_draws), width * n_levels, n_draws)
  apply_prediction_variance_roots(z, roots, width)
}

apply_prediction_variance_roots <- function(z, roots, width) {
  if (width == 1L) return(sweep(z, 2L, roots, "*"))
  for (i in seq_len(ncol(z))) z[, i] <- as.vector(roots[[i]] %*% matrix(z[, i], width))
  z
}

# Extend factor contrasts while keeping the original columns and coding intact.
build_prediction_fixed_matrix <- function(fit, data, allow_missing_levels) {
  terms <- stats::delete.response(fit$formula$fe_terms)
  levels <- fit$formula$fe_Xlevels
  contrasts <- fit$formula$fe_contrasts
  for (column in names(levels)) {
    original <- levels[[column]]
    unseen <- setdiff(unique(as.character(data[[column]])), original)
    if (!length(unseen)) next
    if (!allow_missing_levels) stop("New fixed-effect levels in ", sQuote(column), " are disabled.")

    # Preserve fitted coding. Use the reference level when the coding has one.
    original_factor <- factor(original, levels = original)
    if (!is.null(contrasts[[column]])) {
      original_factor <- stats::`contrasts<-`(original_factor, value = contrasts[[column]])
    }
    coding <- stats::contrasts(original_factor)
    reference <- which(rowSums(abs(coding)) == 0)
    baseline <- if (length(reference)) reference[1L] else 1L
    if (is.null(colnames(coding))) colnames(coding) <- seq_len(ncol(coding))
    new_names <- make.unique(c(colnames(coding), paste0(".new_", unseen)))
    # Keep old contrasts intact for fitted levels. Give each new level its own indicator.
    extended <- cbind(rbind(coding, coding[rep.int(baseline, length(unseen)), , drop = FALSE]),
                      rbind(matrix(0, length(original), length(unseen)), diag(length(unseen))))
    levels[[column]] <- c(original, unseen)
    dimnames(extended) <- list(levels[[column]], new_names)
    contrasts[[column]] <- extended
  }
  frame <- stats::model.frame(terms, data = data, xlev = levels, na.action = stats::na.fail)
  matrix <- stats::model.matrix(terms, frame, contrasts.arg = contrasts)
  matrix <- matrix[, !colnames(matrix) %in% fit$screened_fixed_columns, drop = FALSE]
  for (column in names(fit$reduced_fixed_levels)) {
    values <- as.character(data[[column]])
    unseen <- setdiff(unique(values), fit$reduced_fixed_levels[[column]])
    if (!length(unseen)) next
    if (!allow_missing_levels) stop("New fixed-effect levels in ", sQuote(column), " are disabled.")
    indicators <- outer(values, unseen, "==") * 1
    colnames(indicators) <- tail(make.unique(c(colnames(matrix), paste0(column, ".new_", unseen))),
                                 length(unseen))
    matrix <- cbind(matrix, indicators)
  }
  matrix
}

# Recreate the sparse design in the same block order used by vglmer.
build_prediction_random_blocks <- function(fit, newdata) {
  rownames(newdata) <- as.character(seq_len(nrow(newdata)))
  formula <- fit$formula
  fitted_terms <- fit$internal_parameters$names_of_RE
  blocks <- list()
  if (length(formula$re)) {
    terms <- stats::delete.response(stats::terms(formula$interpret_gam$fake.formula))
    data <- stats::model.frame(terms, data = newdata, drop.unused.levels = TRUE,
                               na.action = stats::na.fail)
    # Use the same random-effects matrix builder and ordering as vglmer.
    random <- reformulas::mkReTrms(stats::formula(fit, form = "re"), data,
                                   reorder.terms = FALSE, reorder.vars = FALSE)
    if (anyDuplicated(names(random$cnms))) stop("Prediction requires distinct random-effect terms.")
    for (i in seq_along(random$cnms)) {
      group <- names(random$cnms)[i]
      # Gp marks this term's span in the sparse matrix.
      columns <- seq.int(random$Gp[i] + 1L, random$Gp[i + 1L])
      terms <- random$cnms[[i]]
      if (!identical(terms, fitted_terms[[group]])) stop("Misaligned random effects for ", group, ".")
      block <- Matrix::t(random$Zt[columns, , drop = FALSE])
      # Recreate vglmer's labels from the group, coefficient, and level.
      colnames(block) <- paste(group, rep_len(terms, length(columns)),
                               rownames(random$Zt)[columns], sep = " @ ")
      blocks[[group]] <- block
    }
  }

  # Reuse fitted knots and basis transforms, including joint gKRLS smooths.
  smooths <- formula$interpret_gam$smooth.spec
  if (length(smooths)) {
    # The fitted basis builder lives inside vglmer's namespace.
    build_spline <- utils::getFromNamespace("vglmer_build_spline", "vglmer")
    if (any(vapply(fit$internal_parameters$spline$attr,
                   function(x) inherits(x$knots, "voteframe_tpf"), logical(1)))) {
      register_sequential_spline_basis()
    }
    for (i in seq_along(smooths)) {
      spec <- smooths[[i]]
      fitted <- fit$internal_parameters$spline$attr[[i]]
      joint <- spec$type %in% c("gKRLS", "randwalk")
      values <- if (joint) newdata[spec$term] else newdata[[spec$term]]
      label <- if (joint) paste0("(", paste(spec$term, collapse = ","), ")") else spec$term
      basis <- build_spline(x = values, knots = fitted$knots, xt = fitted$xt,
                            Boundary.knots = fitted$Boundary.knots, by = newdata[[fitted$by]],
                            outer_okay = TRUE, type = fitted$type, override_warn = TRUE,
                            force_vector = TRUE)
      for (j in seq_along(basis)) {
        # A by-smooth has a global basis and a separate basis for group deviations.
        suffix <- c("base", "int")[j]
        term <- paste("spline", label, i, suffix, sep = "-")
        block <- basis[[j]]$x
        colnames(block) <- paste("spline", label, colnames(block), sep = " @ ")
        blocks[[term]] <- block
      }
    }
  }
  if (!identical(as.character(names(blocks)), as.character(names(fitted_terms)))) {
    stop("The prediction blocks do not match the fitted vglmer design.")
  }
  for (group in names(fit$prior_random)) {
    basis <- stats::model.matrix(stats::reformulate(fit$prior_random[[group]]), newdata)
    labels <- factor(newdata[[group]])
    width <- ncol(basis)
    columns <- outer(width * (as.integer(labels) - 1L), seq_len(width), "+")
    names <- as.vector(outer(colnames(basis), levels(labels),
                            function(coefficient, level) paste(group, coefficient, level, sep = " @ ")))
    blocks[[group]] <- Matrix::sparseMatrix(i = rep(seq_len(nrow(basis)), width),
                                          j = as.vector(columns), x = as.vector(basis),
                                          dims = c(nrow(basis), length(names)),
                                          dimnames = list(NULL, names))
  }
  blocks
}

# With zero supported levels, use a proper inverse-Wishart prior with
# mean identity on the original intercept/slope coefficient scale.
prediction_variance_roots <- function(fit, block, term, width, n_draws) {
  if (is.null(fit$prior_random[[term]])) {
    return(draw_vglmer_variance_roots(fit, block, n_draws))
  }
  if (width == 1L) return(1 / sqrt(stats::rchisq(n_draws, 3L)))
  covariance <- CholWishart::rInvWishart(n_draws, width + 2L, diag(width))
  lapply(seq_len(n_draws), function(i) t(chol(covariance[, , i])))
}

restore_screened_random <- function(fit, data) {
  for (name in names(fit$screened_smooth_groups)) {
    data[[name]] <- factor(data[[fit$screened_smooth_groups[[name]]]])
  }
  for (fit_group in names(fit$screened_random)) {
    term <- fit$screened_random[[fit_group]]
    data[[fit_group]] <- factor(data[[term$group]])
    data[term$columns] <- as.data.frame(stats::model.matrix(stats::reformulate(term$slopes), data))
  }
  data
}
