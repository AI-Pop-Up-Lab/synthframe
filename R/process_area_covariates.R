# Build one area-by-predictor matrix per stage. Row names are area IDs.
process_area_covariates <- function(area_data, area_variable,
                                    outcome_variable, value_variable,
                                    outcome_order = NULL) {
  # A data frame is one dataset. A named list supplies several predictors.
  multiple <- is.list(area_data) && !is.data.frame(area_data)
  datasets <- if (multiple) area_data else list(past_share = area_data)
  # Each dataset name becomes a predictor column.
  predictor_names <- names(datasets)
  if (!length(datasets) || is.null(predictor_names) || anyNA(predictor_names) ||
      any(!nzchar(predictor_names)) || anyDuplicated(predictor_names)) {
    stop("Name each area covariate dataset with its distinct predictor column name.")
  }
  totals <- lapply(datasets, area_outcome_values, area_variable, outcome_variable, value_variable)
  # Check coverage here. Match row positions by area ID when computing shares.
  area_ids <- rownames(totals[[1L]])
  for (predictor in predictor_names) {
    if (!setequal(area_ids, rownames(totals[[predictor]]))) {
      stop("Area covariate dataset ", sQuote(predictor), " has a different set of areas.")
    }
  }
  # Tag the tables so the fitter can distinguish them from ready-to-use matrices.
  if (multiple) {
    totals <- structure(totals, class = "voteframe_area_covariate_list")
  } else {
    totals <- structure(totals[[1L]], class = c("voteframe_area_covariate", "matrix", "array"))
  }
  # The fitter can supply the outcome order later.
  if (is.null(outcome_order)) return(totals)
  conditional_area_shares(totals, outcome_order)
}

# Put one dataset into a table with areas as rows and outcomes as columns.
area_outcome_values <- function(area_data, area_variable, outcome_variable, value_variable) {
  columns <- list(area_variable, outcome_variable, value_variable)
  valid_name <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!all(vapply(columns, valid_name, logical(1)))) {
    stop("Supply the area, outcome, and value column names.")
  }
  columns <- unlist(columns, use.names = FALSE)
  from_csv <- is.character(area_data) && length(area_data) == 1L
  # Read IDs as text to preserve leading zeros.
  if (from_csv) {
    area_data <- utils::read.csv(area_data, check.names = FALSE,
                                        colClasses = "character")
  }
  if (!is.data.frame(area_data) || anyDuplicated(columns) ||
      anyDuplicated(names(area_data)) ||
      !all(columns %in% names(area_data))) {
    stop("Supply distinct area, outcome, and value columns in the area covariate data.")
  }

  area <- as.character(area_data[[area_variable]])
  outcome <- as.character(area_data[[outcome_variable]])
  value <- area_data[[value_variable]]
  # Parse only the value column as a number when reading a CSV.
  if (from_csv) value <- suppressWarnings(as.numeric(value))
  if (!length(area) || anyNA(area) || anyNA(outcome) ||
      any(!nzchar(area)) || any(!nzchar(outcome)) ||
      !is.numeric(value) || any(!is.finite(value) | value < 0)) {
    stop("Area covariate data require area IDs, outcome labels, and nonnegative values.")
  }
  # A pair can supply only one value. Otherwise assignment would overwrite it.
  if (anyDuplicated(data.frame(area, outcome))) {
    stop("Area covariate data must contain at most one row per area and outcome.")
  }

  # An omitted area/outcome pair represents a zero value.
  totals <- matrix(0, nrow = length(unique(area)), ncol = length(unique(outcome)),
                   dimnames = list(unique(area), unique(outcome)))
  # The two columns of this index locate each value by area and outcome.
  totals[cbind(match(area, rownames(totals)), match(outcome, colnames(totals)))] <- value
  totals
}

conditional_area_shares <- function(totals, outcome_order) {
  datasets <- if (inherits(totals, "voteframe_area_covariate_list")) {
    totals
  } else {
    list(past_share = totals)
  }
  outcome_order <- as.character(outcome_order)
  if (length(outcome_order) < 2L || anyNA(outcome_order) || anyDuplicated(outcome_order)) {
    stop("Supply at least two distinct outcomes in model order.")
  }
  # Each dataset must contain the modeled outcome labels somewhere.
  # Individual areas can omit a pair, which was filled with zero above.
  for (predictor in names(datasets)) {
    missing <- setdiff(outcome_order, colnames(datasets[[predictor]]))
    if (length(missing)) {
      stop("Area covariate dataset ", sQuote(predictor), " is missing outcomes: ",
           paste(missing, collapse = ", "), ".")
    }
  }
  area_ids <- rownames(datasets[[1L]])
  stages <- stats::setNames(vector("list", length(outcome_order) - 1L),
                           utils::head(outcome_order, -1L))
  for (i in seq_along(stages)) {
    # The denominator includes only outcomes still in play at this stage.
    remaining <- outcome_order[seq.int(i, length(outcome_order))]
    # Keep one row per area and one column per input dataset.
    stage <- matrix(0, length(area_ids), length(datasets),
                    dimnames = list(area_ids, names(datasets)))
    for (predictor in names(datasets)) {
      # Match areas by ID and use zero when the remaining values sum to zero.
      values <- datasets[[predictor]][area_ids, remaining, drop = FALSE]
      denominator <- rowSums(values)
      # The first remaining column is the current outcome's numerator.
      stage[, predictor] <- values[, 1L] / ifelse(denominator > 0, denominator, 1)
    }
    stages[[i]] <- stage
  }
  stages
}
