# Calibrate multiple extension margins without undoing an earlier constraint.
rake_joint_extended_frame <- function(extended_frame, frame, targets, by,
                                       extension_variables = names(targets),
                                       target_variable = "target", count_variable = "N",
                                       target_scale = "share",
                                       tolerance = 0.000000000001, max_iterations = 1000L,
                                       margin_variables = stats::setNames(names(targets), names(targets)),
                                       area_variable = NULL, parent_area_variable = NULL,
                                       areas_per_batch = NULL, progress = NULL,
                                       solver_trace = FALSE) {
  margin_names <- names(targets)
  if (!is.character(extension_variables) || !length(extension_variables) ||
      anyNA(extension_variables) || any(!nzchar(extension_variables)) ||
      anyDuplicated(extension_variables) || !is.list(targets) || !length(targets) ||
      length(margin_names) != length(targets) || anyNA(margin_names) ||
      any(!nzchar(margin_names)) || anyDuplicated(margin_names) ||
      !is.character(margin_variables) || !setequal(names(margin_variables), margin_names) ||
      anyDuplicated(names(margin_variables)) || !all(margin_variables %in% extension_variables)) {
    stop("Supply named target tables and match each margin to an extension variable.")
  }
  if (is.character(by)) by <- stats::setNames(rep(list(by), length(targets)), margin_names)
  if (!is.list(by) || !setequal(names(by), margin_names) || anyDuplicated(names(by))) {
    stop("Supply shared grouping columns or a named list of grouping columns by margin.")
  }
  target_variable <- joint_margin_options(target_variable, margin_names)
  target_scale <- joint_margin_options(target_scale, margin_names)
  if (!all(target_scale %in% c("share", "count")) || !is.numeric(tolerance) ||
      length(tolerance) != 1L || !is.finite(tolerance) || tolerance <= 0 || tolerance >= 1 ||
      !is.numeric(max_iterations) || length(max_iterations) != 1L ||
      !is.finite(max_iterations) || max_iterations < 1 || max_iterations != floor(max_iterations) ||
      max_iterations > .Machine$integer.max) {
    stop("Supply a target column, a tolerance between zero and one, and a positive iteration limit.")
  }
  extended_frame <- as_plain_frame(extended_frame)
  frame <- as_plain_frame(frame)
  targets <- lapply(targets, as_plain_frame)
  population_draws <- prediction_count_draws(frame, count_variable)
  keys <- setdiff(names(frame), c(count_variable, population_draws))
  validate_prediction_frame(frame, keys, count_variable)
  row_keys <- c(keys, extension_variables)
  if (!is.data.frame(extended_frame) || !nrow(extended_frame) ||
      anyDuplicated(names(extended_frame)) || any(extension_variables %in% names(frame)) ||
      !all(c(row_keys, count_variable) %in% names(extended_frame)) ||
      anyNA(extended_frame[row_keys])) {
    stop("Supply unique complete rows keyed by the original frame and all extension variables.")
  }
  row_ids <- extended_frame[row_keys]
  if (anyDuplicated(frame_cell_keys(row_ids, row_ids))) {
    stop("Supply unique complete rows keyed by the original frame and all extension variables.")
  }
  if (!requireNamespace("SQUAREM", quietly = TRUE)) {
    stop("Install SQUAREM to rake the extended frame.")
  }

  reference <- frame[keys]
  cell <- match(frame_cell_keys(extended_frame[keys], reference),
                frame_cell_keys(reference, reference))
  if (anyNA(cell) || length(unique(cell)) != nrow(frame)) {
    stop("The extended frame must match and cover every original cell.")
  }
  count_columns <- c(count_variable, extended_frame_draw_columns(extended_frame, count_variable))
  if (any(count_columns %in% row_keys) ||
      !all(vapply(extended_frame[count_columns], function(x) {
        is.numeric(x) && all(is.finite(x) & x >= 0)
      }, logical(1)))) stop("Extended count columns must be distinct, finite, and nonnegative.")
  if (length(population_draws)) {
    count_columns <- c(count_variable, prediction_count_draws(extended_frame, count_variable))
    if (length(population_draws) != length(count_columns) - 1L) {
      stop("Extended and original population draws must match by draw number.")
    }
  }

  # A zero target may name an outcome present only in another geographic block.
  outcome_levels <- lapply(extended_frame[unique(unname(margin_variables))], function(x) {
    unique(as.character(x))
  })
  rake_block <- function(extended_frame, frame, targets, cell, progress) {
    raked <- extended_frame
    if (!is.null(progress)) progress(0L, length(count_columns), "mean and posterior draw columns")
    for (i in seq_along(count_columns)) {
      column <- count_columns[i]
      if (i == 1L || length(population_draws)) {
        population <- frame[[c(count_variable, population_draws)[i]]]
        total <- sum(population)
        if (!is.finite(total)) stop("Original population totals must be finite.")
        cell_share <- if (total > 0) population / total else population
        margins <- lapply(margin_names, function(margin) {
          extension <- margin_variables[[margin]]
          prepare_joint_rake_margin(frame, extended_frame, cell, cell_share,
                                    targets[[margin]], by[[margin]], extension,
                                    population, target_variable[[margin]], target_scale[[margin]],
                                    tolerance, keys, outcome_levels[[extension]])
        })
        names(margins) <- margin_names
      }
      raked[[column]] <- if (total == 0) numeric(nrow(raked)) else {
        fit_joint_rake(extended_frame[[column]], cell, cell_share, margins,
                       tolerance, max_iterations, column, solver_trace) * total
      }
      if (!is.null(progress)) progress(i, length(count_columns), column)
    }
    raked
  }

  if (is.null(area_variable) && (!is.null(parent_area_variable) || !is.null(areas_per_batch))) {
    stop("Set area_variable when supplying geographic batch options.")
  }
  if (!is.null(area_variable)) {
    batches <- geographic_batches(frame, area_variable, parent_area_variable, areas_per_batch, by)
    if (length(batches) > 1L) {
      work <- length(batches) * length(count_columns)
      if (!is.null(progress)) progress(0L, work, sprintf("%d geographic batches × %d count columns",
                                                        length(batches), length(count_columns)))
      target_keys <- lapply(margin_names, function(margin) {
        columns <- by[[margin]]
        if (!all(columns %in% names(targets[[margin]]))) stop("Missing raking grouping columns.")
        target <- frame_cell_keys(targets[[margin]][columns], frame[columns])
        if (!all(target %in% frame_cell_keys(frame[columns], frame[columns]))) {
          stop("A grouping target level does not match the frame for ", sQuote(margin), ".")
        }
        target
      })
      batch_index <- integer(nrow(frame))
      batch_index[unlist(batches, use.names = FALSE)] <- rep(seq_along(batches), lengths(batches))
      extended_rows <- split(seq_len(nrow(extended_frame)), batch_index[cell])
      raked <- extended_frame
      for (i in seq_along(batches)) {
        if (isTRUE(solver_trace)) {
          cat("Geographic raking batch ", i, "/", length(batches), " — ", names(batches)[i],
              "\n", sep = "")
        }
        original <- frame[batches[[i]], , drop = FALSE]
        rows <- extended_rows[[i]]
        batch_targets <- stats::setNames(lapply(seq_along(margin_names), function(j) {
          margin <- margin_names[j]
          columns <- by[[margin]]
          keep <- target_keys[[j]] %in% frame_cell_keys(original[columns], frame[columns])
          targets[[margin]][keep, , drop = FALSE]
        }), margin_names)
        batch_progress <- if (!is.null(progress)) function(done, total, detail) {
          if (done > 0L) progress((i - 1L) * length(count_columns) + done, work,
                                  paste0("batch ", names(batches)[i], " — ", detail))
        }
        block <- rake_block(extended_frame[rows, , drop = FALSE], original, batch_targets,
                            match(cell[rows], batches[[i]]), batch_progress)
        raked[rows, count_columns] <- block[count_columns]
      }
      return(raked)
    }
  }

  rake_block(extended_frame, frame, targets, cell, progress)
}

joint_margin_options <- function(value, margins) {
  if (is.character(value) && length(value) == 1L && is.null(names(value))) {
    value <- stats::setNames(rep(value, length(margins)), margins)
  }
  if (!is.character(value) || anyNA(value) || any(!nzchar(value)) ||
      !setequal(names(value), margins) || anyDuplicated(names(value))) {
    stop("Supply a shared target option or one named option per margin.")
  }
  value
}

prepare_joint_rake_margin <- function(frame, extended, cell, cell_share, targets, by,
                                       extension, population, target_variable,
                                       target_scale, tolerance, keys, outcomes) {
  if (!is.character(by) || anyNA(by) || anyDuplicated(by) || !all(by %in% keys)) {
    stop("Invalid grouping columns for ", sQuote(extension), ".")
  }
  target_keys <- c(by, extension)
  if (!is.data.frame(targets) || !nrow(targets) || anyDuplicated(names(targets)) ||
      target_variable %in% target_keys || !all(c(target_keys, target_variable) %in% names(targets)) ||
      anyNA(targets[target_keys]) || anyDuplicated(targets[target_keys])) {
    stop("Supply one target per grouping and outcome for ", sQuote(extension), ".")
  }
  values <- targets[[target_variable]]
  if (!is.numeric(values) || any(!is.finite(values) | values < 0) ||
      (target_scale == "share" && any(values > 1))) {
    stop("Invalid target values for ", sQuote(extension), ".")
  }

  if (length(by)) {
    group_keys <- frame_cell_keys(frame[by], frame[by])
    levels <- unique(group_keys)
    group <- match(group_keys, levels)
    target_group <- match(frame_cell_keys(targets[by], frame[by]), levels)
  } else {
    group <- rep.int(1L, nrow(frame))
    target_group <- rep.int(1L, nrow(targets))
  }
  if (any(!nzchar(outcomes))) stop("Extended outcomes must be nonempty for ", sQuote(extension), ".")
  target_outcome <- match(as.character(targets[[extension]]), outcomes)
  if (anyNA(target_group) || anyNA(target_outcome)) {
    stop("A grouping or outcome target level does not match the frame for ", sQuote(extension), ".")
  }

  group_population <- as.numeric(raking_rowsum(population, group))
  group_share <- as.numeric(rowsum(cell_share, group))
  supplied <- matrix(NA_real_, length(group_population), length(outcomes))
  supplied[cbind(target_group, target_outcome)] <- values
  # Each group has its own outcome partition. Missing targets share a residual.
  margin_index <- matrix(0L, nrow(supplied), ncol(supplied))
  target <- reference <- mass <- vector("list", nrow(supplied))
  offset <- 0L
  for (i in seq_len(nrow(supplied))) {
    constrained <- which(!is.na(supplied[i, ]))
    if (!length(constrained)) stop("Missing targets for group ", i, " of ", sQuote(extension), ".")
    shares <- supplied[i, constrained]
    if (target_scale == "count") {
      if (group_population[i] == 0 && any(shares > 0)) {
        stop("Positive count target in a zero-population group of ", sQuote(extension), ".")
      }
      if (group_population[i] == 0) next
      shares <- shares / group_population[i]
    }
    complete <- length(constrained) == length(outcomes)
    remainder <- 1 - sum(shares)
    if (remainder < -tolerance || (complete && abs(remainder) > tolerance)) {
      stop("Targets do not match the population total for group ", i, " of ", sQuote(extension), ".")
    }
    if (!complete) shares <- c(shares, max(0, remainder))
    if (group_share[i] == 0) next
    shares <- shares / sum(shares)
    margin_index[i, ] <- offset + match(seq_along(outcomes), constrained,
                                         nomatch = length(constrained) + 1L)
    target[[i]] <- shares * group_share[i]
    mass[[i]] <- rep(group_share[i], length(shares))
    reference[[i]] <- rep(offset + which.max(shares), length(shares))
    offset <- offset + length(shares)
  }
  outcome <- match(as.character(extended[[extension]]), outcomes)
  list(index = margin_index[cbind(group[cell], outcome)], target = unlist(target),
       mass = unlist(mass), reference = unlist(reference))
}

fit_joint_rake <- function(seed, cell, cell_share, margins, tolerance, max_iterations, column,
                           solver_trace = FALSE) {
  active <- seed > 0 & cell_share[cell] > 0
  for (margin in margins) {
    active <- active & c(0, margin$target)[margin$index + 1L] > 0
  }
  cell <- match(cell[active], which(cell_share > 0))
  cell_share <- cell_share[cell_share > 0]
  if (length(unique(cell)) != length(cell_share)) {
    stop("No positive seed support for an original cell after imposing zero targets (", column, ").")
  }

  seed <- as.double(seed[active])
  # Scale each seed row once to preserve association and avoid large count values.
  seed <- seed / as.numeric(rowsum(seed, cell))[cell]
  offset <- 0L
  for (i in seq_along(margins)) {
    margin <- margins[[i]]
    positive <- which(margin$target > 0)
    index <- match(margin$index[active], positive)
    if (length(unique(index)) != length(positive)) {
      stop("No positive seed support for a target of ", sQuote(names(margins)[i]),
           " (", column, ").")
    }
    margins[[i]] <- list(index = index, target = margin$target[positive],
                          mass = margin$mass[positive],
                          reference = match(margin$reference[positive], positive),
                          offsets = offset + seq_along(positive))
    offset <- offset + length(positive)
  }

  normalize_cells <- function(weights) {
    weights * (cell_share / as.numeric(rowsum(weights, cell)))[cell]
  }
  fitted_counts <- function(offsets) {
    log_scale <- numeric(length(seed))
    for (margin in margins) log_scale <- log_scale + offsets[margin$offsets][margin$index]
    normalize_cells(seed * exp(log_scale - max(log_scale)))
  }
  update <- function(offsets) {
    weights <- fitted_counts(offsets)
    for (margin in margins) {
      current <- as.numeric(rowsum(weights, margin$index))
      ratio <- margin$target / current
      log_ratio <- log(ratio)
      offsets[margin$offsets] <- offsets[margin$offsets] +
        log_ratio - log_ratio[margin$reference]
      weights <- normalize_cells(weights * ratio[margin$index])
    }
    offsets
  }
  # A complete cycle updates all extension margins in one fixed-point solve.
  initial <- numeric(offset)
  if (isTRUE(solver_trace)) cat("SQUAREM — calibrating ", column, "\n", sep = "")
  solution <- tryCatch(
    SQUAREM::squarem(initial, update, control = list(tol = tolerance / 2,
                                                   maxiter = max_iterations,
                                                   trace = solver_trace))$par,
    error = function(error) {
      stop("Joint raking failed for ", sQuote(column), ": ", conditionMessage(error),
           call. = FALSE)
    }
  )
  fitted <- fitted_counts(solution)
  if (any(!is.finite(fitted)) ||
      max(abs(as.numeric(rowsum(fitted, cell)) - cell_share)) > tolerance) {
    stop("Joint raking did not preserve the original cells for ", sQuote(column), ".")
  }
  for (i in seq_along(margins)) {
    margin <- margins[[i]]
    error <- abs(as.numeric(rowsum(fitted, margin$index)) - margin$target) / margin$mass
    if (max(error) > tolerance) {
      stop("Joint raking did not meet the ", sQuote(names(margins)[i]),
           " targets for ", sQuote(column), ". Check margin compatibility and joint support.")
    }
  }
  result <- numeric(length(active))
  result[active] <- fitted
  result
}
