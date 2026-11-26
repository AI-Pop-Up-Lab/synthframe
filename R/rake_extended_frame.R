# Balance each extended count column to the original cells and target shares.
rake_extended_frame <- function(extended_frame, frame, targets, by, extension_variable,
                                 target_variable = "target", count_variable = "N",
                                 target_scale = c("share", "count"),
                                 tolerance = 0.000000000001, max_iterations = 1000L) {
  target_scale <- match.arg(target_scale)
  if (!is.data.frame(extended_frame) || !nrow(extended_frame) ||
      anyDuplicated(names(extended_frame)) || !is.data.frame(frame) ||
      !is.data.frame(targets) || !nrow(targets) || anyDuplicated(names(targets))) {
    stop("Supply a stratification frame, a nonempty extended frame, and a target table.")
  }
  if (!all(vapply(list(extension_variable, target_variable, count_variable), function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }, logical(1)))) stop("Supply valid outcome, target, and count column names.")
  extended_frame <- as_plain_frame(extended_frame)
  frame <- as_plain_frame(frame)
  targets <- as_plain_frame(targets)
  population_draws <- prediction_count_draws(frame, count_variable)
  # The joint solver already pairs each count draw with its original population.
  if (length(population_draws)) {
    return(rake_joint_extended_frame(
      extended_frame, frame, targets = stats::setNames(list(targets), extension_variable),
      by = by, target_variable = target_variable, count_variable = count_variable,
      target_scale = target_scale, tolerance = tolerance, max_iterations = max_iterations
    ))
  }
  cell_columns <- setdiff(names(frame), count_variable)
  validate_prediction_frame(frame, cell_columns, count_variable)
  if (extension_variable %in% names(frame) || !is.character(by) || anyNA(by) ||
      anyDuplicated(by) || !all(by %in% cell_columns) ||
      !all(c(cell_columns, extension_variable, count_variable) %in% names(extended_frame))) {
    stop("Grouping columns must identify original frame levels, and the extended frame must contain the cell keys, outcome, and mean count.")
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L || !is.finite(tolerance) ||
      tolerance <= 0 || tolerance >= 1 || !is.numeric(max_iterations) ||
      length(max_iterations) != 1L || !is.finite(max_iterations) ||
      max_iterations < 1 || max_iterations != floor(max_iterations)) {
    stop("Supply a tolerance between 0 and 1 and a positive integer iteration limit.")
  }

  if (!requireNamespace("SQUAREM", quietly = TRUE)) {
    stop("Install SQUAREM to rake the extended frame.")
  }

  outcome <- as.character(extended_frame[[extension_variable]])
  if (anyNA(outcome) || any(!nzchar(outcome))) stop("Extended outcomes must be complete.")
  outcomes <- unique(outcome)
  outcome_index <- match(outcome, outcomes)
  reference <- frame[cell_columns]
  cell <- match(frame_cell_keys(extended_frame[cell_columns], reference),
                frame_cell_keys(reference, reference))
  if (anyNA(cell) || length(unique(cell)) != nrow(frame)) {
    stop("Extended-frame cell keys must match the original frame and cover every original cell.")
  }
  if (anyDuplicated(data.frame(cell, outcome_index))) {
    stop("The extended frame has duplicate cell–outcome rows.")
  }

  target_keys <- c(by, extension_variable)
  if (target_variable %in% target_keys ||
      !all(c(target_keys, target_variable) %in% names(targets)) ||
      anyNA(targets[target_keys]) || anyDuplicated(targets[target_keys])) {
    stop("Supply at most one target per grouping combination and outcome.")
  }
  values <- targets[[target_variable]]
  if (!is.numeric(values) || any(!is.finite(values) | values < 0) ||
      (target_scale == "share" && any(values > 1))) {
    stop("Targets must be finite and nonnegative, with shares between 0 and 1.")
  }

  # Integer codes match labels by value and retain the original frame row order.
  if (length(by)) {
    group_keys <- frame[by]
    levels <- unique(group_keys)
    level_keys <- frame_cell_keys(levels, group_keys)
    group <- match(frame_cell_keys(group_keys, group_keys), level_keys)
    target_group <- match(frame_cell_keys(targets[by], group_keys), level_keys)
    group_labels <- do.call(paste, c(lapply(by, function(name) {
      paste0(name, "=", levels[[name]])
    }), sep = ", "))
  } else {
    group <- rep.int(1L, nrow(frame))
    target_group <- rep.int(1L, nrow(targets))
    group_labels <- "all cells"
  }
  target_outcome <- match(as.character(targets[[extension_variable]]), outcomes)
  if (anyNA(target_group)) {
    stop("Targets contain a grouping combination absent from the original frame: ",
         paste(unlist(targets[which(is.na(target_group))[1L], by]), collapse = ", "), ".")
  }
  if (anyNA(target_outcome)) {
    stop("Target outcome absent from the extended frame: ",
         sQuote(as.character(targets[[extension_variable]][which(is.na(target_outcome))[1L]])), ".")
  }
  targets_by_group <- matrix(NA_real_, length(group_labels), length(outcomes))
  targets_by_group[cbind(target_group, target_outcome)] <- values

  count_columns <- c(count_variable, extended_frame_draw_columns(extended_frame, count_variable))
  if (any(count_columns %in% c(cell_columns, extension_variable)) ||
      !all(vapply(extended_frame[count_columns], function(x) {
        is.numeric(x) && all(is.finite(x) & x >= 0)
      }, logical(1)))) stop("Extended count columns must be distinct, finite, and nonnegative.")

  raked_counts <- matrix(0, nrow(extended_frame), length(count_columns),
                         dimnames = list(NULL, count_columns))
  cell_groups <- split(seq_len(nrow(frame)), group)
  extended_groups <- split(seq_len(nrow(extended_frame)), group[cell])
  population <- frame[[count_variable]]
  for (i in seq_along(cell_groups)) {
    cells <- cell_groups[[i]]
    rows <- extended_groups[[i]]
    total <- sum(population[cells])
    supplied <- targets_by_group[i, ]
    constrained <- which(!is.na(supplied))
    if (!length(constrained)) stop("Missing outcome targets for ", group_labels[i], ".")
    if (total == 0) {
      if (target_scale == "count" && any(supplied[constrained] > 0)) {
        stop("Positive count target for zero-population group ", group_labels[i], ".")
      }
      next
    }
    target <- supplied[constrained]
    if (target_scale == "count") target <- target / total
    residual <- 1 - sum(target)
    complete <- length(constrained) == length(outcomes)
    if (residual < -tolerance || (complete && abs(residual) > tolerance)) {
      stop("Outcome targets are incompatible with the population total for ", group_labels[i], ".")
    }
    # Untargeted outcomes share the remaining mass and retain their separate rows.
    if (!complete) target <- c(target, max(0, residual))
    target <- target / sum(target)
    margin <- match(seq_along(outcomes), constrained, nomatch = length(constrained) + 1L)
    index <- cbind(match(cell[rows], cells), outcome_index[rows])

    for (j in seq_along(count_columns)) {
      column <- count_columns[j]
      weights <- matrix(0, length(cells), length(outcomes))
      weights[index] <- extended_frame[[column]][rows]
      fitted <- rake_group_shares(weights, population[cells] / total, target, margin,
                                  tolerance, max_iterations, group_labels[i], column)
      raked_counts[rows, j] <- fitted[index] * total
    }
  }
  result <- extended_frame
  result[count_columns] <- as.data.frame(raked_counts)
  result
}

# Base rowsum() preserves integer storage. Group totals can exceed that range
# even when every individual count fits in an integer.
raking_rowsum <- function(counts, group, reorder = TRUE) {
  if (is.data.frame(counts)) {
    integers <- vapply(counts, is.integer, logical(1))
    counts[integers] <- lapply(counts[integers], as.double)
  } else if (is.integer(counts)) {
    storage.mode(counts) <- "double"
  }
  rowsum(counts, group, reorder = reorder)
}

rake_group_shares <- function(weights, cell_share, target, margin, tolerance,
                              max_iterations, group_label, column) {
  active_rows <- cell_share > 0
  active_columns <- target > 0
  weights[!active_rows, ] <- 0
  weights[, !active_columns[margin]] <- 0
  separate_outcomes <- identical(margin, seq_len(ncol(weights)))
  pooled <- if (separate_outcomes) weights else t(rowsum(t(weights), margin))
  seed <- pooled[active_rows, active_columns, drop = FALSE]
  seed_row_totals <- rowSums(seed)
  if (any(seed_row_totals == 0) || any(colSums(seed) == 0)) {
    stop("Raking has no positive support for a required margin in ", group_label,
         " (", column, "). Check zero counts and missing cell–outcome combinations.",
         call. = FALSE)
  }
  row_target <- cell_share[active_rows]
  column_target <- target[active_columns]
  if (length(column_target) == 1L) {
    fitted <- matrix(row_target, ncol = 1L)
  } else {
    seed <- seed / seed_row_totals
    reference <- ncol(seed)
    # Profile out cell scaling and fix one outcome offset at zero.
    factors <- function(offsets) {
      offsets <- c(offsets, 0)
      exp(offsets - max(offsets))
    }
    update <- function(offsets) {
      cell_scale <- row_target / drop(seed %*% factors(offsets))
      log_scale <- log(column_target / drop(crossprod(seed, cell_scale)))
      (log_scale - log_scale[reference])[-reference]
    }
    offsets <- tryCatch(
      SQUAREM::squarem(update(numeric(reference - 1L)), update,
                      control = list(tol = tolerance / 2, maxiter = max_iterations))$par,
      error = function(e) {
        stop("Raking failed for ", group_label, " (", column, "): ",
             conditionMessage(e), call. = FALSE)
      }
    )
    fitted <- sweep(seed, 2L, factors(offsets), "*")
    fitted <- fitted * (row_target / rowSums(fitted))
  }
  if (any(!is.finite(fitted)) ||
      max(abs(rowSums(fitted) - row_target),
          abs(colSums(fitted) - column_target)) > tolerance) {
    stop("Raking did not match the margins for ", group_label, " (", column,
         ") within tolerance. Check the supported cells and iteration limit.",
         call. = FALSE)
  }
  fitted_pool <- matrix(0, nrow(pooled), ncol(pooled))
  fitted_pool[active_rows, active_columns] <- fitted
  if (separate_outcomes) return(fitted_pool)

  # Split the fitted residual margin in the seed proportions within each cell.
  scale <- fitted_pool / ifelse(pooled > 0, pooled, 1)
  weights * scale[, margin, drop = FALSE]
}
