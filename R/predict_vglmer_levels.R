# Return log-odds with draws in rows and prediction cells in columns.
# With n_draws = 0, return one row using fitted means and zero for new effects.
predict_vglmer_levels <- function(fit, newdata, n_draws = 125L,
                                  allow_missing_levels = TRUE, new_fixed_sd = NULL,
                                  draw_state = NULL) {
  if (!inherits(fit, "vglmer")) stop("Supply a vglmer fit.")
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

  newdata <- as.data.frame(newdata)
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
      stop("Unseen fixed-effect contrasts require an explicit new_fixed_sd: ",
           paste(colnames(fixed)[new_fixed], collapse = ", "), ".")
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
    fitted <- fit$internal_parameters$cyclical_pos[[i]]
    index <- match(colnames(block), random_names[fitted])
    known <- !is.na(index)
    if (any(known)) {
      logits <- logits + as.matrix(block[, known, drop = FALSE] %*%
        t(random[, fitted[index[known]], drop = FALSE]))
    }
    if (all(known)) next
    term <- names(blocks)[i]
    if (!allow_missing_levels) stop("New levels in ", sQuote(term), " are disabled.")
    # At the point estimate, only the fitted coefficients contribute.
    if (n_draws == 0) next

    # Columns for a level are adjacent. Keep intercepts and slopes together.
    width <- fit$internal_parameters$d_j[[i]]
    if (ncol(block) %% width != 0) stop("Incomplete prediction design for ", sQuote(term), ".")
    level_columns <- matrix(seq_len(ncol(block)), nrow = width)
    # A new level needs a whole coefficient vector. A partial match signals a design error.
    missing_per_level <- colSums(matrix(!known, nrow = width))
    if (any(!missing_per_level %in% c(0, width))) {
      stop("Partially matched coefficient block for ", sQuote(term), ".")
    }
    new_levels <- level_columns[, missing_per_level == width, drop = FALSE]
    new_coefficients <- if (is.null(draw_state)) {
      draw_new_vglmer_effects(fit, i, ncol(new_levels), n_draws)
    } else {
      if (is.null(draw_state$random[[term]])) {
        draw_state$random[[term]] <- list(
          effects = new.env(parent = emptyenv()),
          roots = draw_vglmer_variance_roots(fit, i, n_draws)
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
  stats::model.matrix(terms, frame, contrasts.arg = contrasts)
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
  if (!identical(names(blocks), names(fitted_terms))) {
    stop("The prediction blocks do not match the fitted vglmer design.")
  }
  blocks
}

# Draw one covariance per term and draw. All new levels share it in that draw.
draw_new_vglmer_effects <- function(fit, block, n_levels, n_draws) {
  width <- fit$internal_parameters$d_j[[block]]
  draws <- matrix(stats::rnorm(width * n_levels * n_draws), width * n_levels, n_draws)
  apply_prediction_variance_roots(draws, draw_vglmer_variance_roots(fit, block, n_draws), width)
}
