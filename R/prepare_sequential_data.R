# Count outcomes within cells defined by identical survey predictors.
aggregate_extension_survey <- function(survey, extension_variable, predictors, no_vote,
                                       outcome_order = NULL) {
  columns <- c(predictors, extension_variable)
  if (!is.data.frame(survey) || anyDuplicated(columns) ||
      anyDuplicated(names(survey)) ||
      !all(columns %in% names(survey))) {
    stop("Supply distinct predictor and extension columns in auxiliary_survey.")
  }
  survey <- as.data.frame(survey)[, columns, drop = FALSE]
  # Treat empty strings in the selected columns as missing.
  for (name in columns) {
    if (is.factor(survey[[name]])) survey[[name]] <- as.character(survey[[name]])
    if (is.character(survey[[name]])) survey[[name]][survey[[name]] == ""] <- NA
  }

  # Rank outcomes using all recorded choices. Fit using complete rows.
  frequency <- table(as.character(survey[[extension_variable]]))
  keep <- stats::complete.cases(survey)
  omitted <- sum(!keep)
  if (omitted) {
    warning("Removed ", omitted, " incomplete survey rows before aggregation.",
            call. = FALSE)
  }
  survey <- survey[keep, , drop = FALSE]
  outcome <- as.character(survey[[extension_variable]])
  # Use label order to break frequency ties consistently.
  positions <- order(-as.integer(frequency), names(frequency), method = "radix")
  order <- names(frequency)[positions]
  if (length(order) < 2L) stop("At least two survey outcomes are required.")
  # Each recorded outcome needs at least one row that can enter the model.
  missing_outcomes <- setdiff(order, outcome)
  if (length(missing_outcomes)) {
    stop("No complete survey rows for: ", paste(missing_outcomes, collapse = ", "))
  }
  if (!is.null(no_vote) && length(no_vote) != 1L) {
    stop("no_vote must identify one observed extension outcome.")
  }
  if (!is.null(no_vote) && !is.na(no_vote) && !as.character(no_vote) %in% order) {
    stop("no_vote was not observed in the survey.")
  }
  # Move non-voting to the front and keep the relative order of the rest.
  if (!is.null(outcome_order)) {
    if (!is.character(outcome_order) || anyDuplicated(outcome_order) ||
        !setequal(outcome_order, order)) {
      stop("outcome_order must list each observed outcome exactly once.")
    }
    order <- outcome_order
  } else if (!is.null(no_vote) && !is.na(no_vote)) {
    order <- c(as.character(no_vote), setdiff(order, as.character(no_vote)))
  }

  # Integer codes give each cell a unique key even when labels contain punctuation.
  cells <- survey[, predictors, drop = FALSE]
  codes <- lapply(cells, function(x) match(x, unique(x)))
  key <- do.call(paste, c(unname(codes), sep = ":"))
  # Keep the first row for each key as the cell's predictor values.
  first <- !duplicated(key)
  # Keep count rows in cell order and count columns in outcome order.
  counts <- table(factor(key, levels = key[first]), factor(outcome, levels = order))
  # Cell labels live in cells. Outcome labels stay on the count matrix.
  dimnames(counts) <- list(NULL, order)
  list(cells = cells[first, , drop = FALSE], counts = counts,
       outcome_order = order, omitted = omitted)
}

# Attach area predictors to each survey cell using its area ID.
join_stage_covariates <- function(cells, area_variable, covariates) {
  if (is.null(covariates)) return(cells)
  # Row names identify areas. Covariate column names must be new to the frame.
  if (!(is.matrix(covariates) || is.data.frame(covariates)) ||
      is.null(rownames(covariates)) || anyNA(rownames(covariates)) ||
      any(!nzchar(rownames(covariates))) || anyDuplicated(rownames(covariates)) ||
      is.null(colnames(covariates)) || anyNA(colnames(covariates)) ||
      any(!nzchar(colnames(covariates))) || anyDuplicated(colnames(covariates)) ||
      any(colnames(covariates) %in% names(cells))) {
    stop("Area covariates need unique area row names and distinct predictor column names.")
  }
  # Several cells can share the same area covariate row.
  index <- match(as.character(cells[[area_variable]]), rownames(covariates))
  # Missing area rows are an error because their covariates are unknown.
  if (anyNA(index)) {
    missing <- unique(cells[[area_variable]][is.na(index)])
    stop("Missing area covariates for ", paste(missing, collapse = ", "), ".")
  }
  cbind(cells, as.data.frame(covariates[index, , drop = FALSE], optional = TRUE))
}
