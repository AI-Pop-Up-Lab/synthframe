# Calibrate multiple extension margins without undoing an earlier constraint.
rake_joint_extended_frame <- function(extended_frame, frame, targets, by,
                                       extension_variables = names(targets),
                                       target_variable = "target", count_variable = "N",
                                       target_scale = c("share", "count"),
                                       tolerance = 0.000000000001, max_iterations = 1000L) {
  target_scale <- match.arg(target_scale)
  if (!is.character(extension_variables) || length(extension_variables) < 2L ||
      anyNA(extension_variables) || any(!nzchar(extension_variables)) ||
      anyDuplicated(extension_variables) || !is.list(targets) ||
      !setequal(names(targets), extension_variables) || anyDuplicated(names(targets))) {
    stop("Supply target tables named for each of at least two distinct extension variables.")
  }
  if (is.character(by)) by <- stats::setNames(rep(list(by), length(targets)), names(targets))
  if (!is.list(by) || !setequal(names(by), extension_variables) || anyDuplicated(names(by))) {
    stop("Supply shared grouping columns or a named list of grouping columns by extension.")
  }
  if (!is.character(target_variable) || length(target_variable) != 1L ||
      is.na(target_variable) || !nzchar(target_variable) || !is.numeric(tolerance) ||
      length(tolerance) != 1L || !is.finite(tolerance) || tolerance <= 0 || tolerance >= 1 ||
      !is.numeric(max_iterations) || length(max_iterations) != 1L ||
      !is.finite(max_iterations) || max_iterations < 1 || max_iterations != floor(max_iterations) ||
      max_iterations > .Machine$integer.max) {
    stop("Supply a target column, a tolerance between zero and one, and a positive iteration limit.")
  }
  keys <- setdiff(names(frame), count_variable)
  validate_prediction_frame(frame, keys, count_variable)
  row_keys <- c(keys, extension_variables)
  if (!is.data.frame(extended_frame) || !nrow(extended_frame) ||
      anyDuplicated(names(extended_frame)) || any(extension_variables %in% names(frame)) ||
      !all(c(row_keys, count_variable) %in% names(extended_frame)) ||
      anyNA(extended_frame[row_keys]) || anyDuplicated(extended_frame[row_keys])) {
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
  population <- frame[[count_variable]]
  total <- sum(population)
  if (!is.finite(total) || total <= 0) stop("The original frame must have finite positive population.")
  cell_share <- population / total
  margins <- lapply(extension_variables, function(extension) {
    prepare_joint_rake_margin(frame, extended_frame, cell, cell_share,
                              targets[[extension]], by[[extension]], extension,
                              population, target_variable, target_scale, tolerance, keys)
  })
  names(margins) <- extension_variables

  count_columns <- c(count_variable, extended_frame_draw_columns(extended_frame, count_variable))
  if (any(count_columns %in% row_keys) ||
      !all(vapply(extended_frame[count_columns], function(x) {
        is.numeric(x) && all(is.finite(x) & x >= 0)
      }, logical(1)))) stop("Extended count columns must be distinct, finite, and nonnegative.")

  raked <- extended_frame
  for (column in count_columns) {
    raked[[column]] <- fit_joint_rake(extended_frame[[column]], cell, cell_share, margins,
                                      tolerance, max_iterations, column) * total
  }
  raked
}

prepare_joint_rake_margin <- function(frame, extended, cell, cell_share, targets, by,
                                       extension, population, target_variable,
                                       target_scale, tolerance, keys) {
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
  outcomes <- unique(as.character(extended[[extension]]))
  if (any(!nzchar(outcomes))) stop("Extended outcomes must be nonempty for ", sQuote(extension), ".")
  target_outcome <- match(as.character(targets[[extension]]), outcomes)
  if (anyNA(target_group) || anyNA(target_outcome)) {
    stop("A grouping or outcome target level does not match the frame for ", sQuote(extension), ".")
  }

  group_population <- as.numeric(rowsum(population, group))
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

fit_joint_rake <- function(seed, cell, cell_share, margins, tolerance, max_iterations, column) {
  active <- seed > 0 & cell_share[cell] > 0
  for (margin in margins) {
    active <- active & c(0, margin$target)[margin$index + 1L] > 0
  }
  cell <- match(cell[active], which(cell_share > 0))
  cell_share <- cell_share[cell_share > 0]
  if (length(unique(cell)) != length(cell_share)) {
    stop("No positive seed support for an original cell after imposing zero targets (", column, ").")
  }

  seed <- seed[active]
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
  solution <- tryCatch(
    SQUAREM::squarem(initial, update, control = list(tol = tolerance / 2,
                                                   maxiter = max_iterations))$par,
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
