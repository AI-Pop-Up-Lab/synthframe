\name{predict_sequential_logit}
\alias{predict_sequential_logit}
\alias{aggregate_extended_frame}
\alias{collapse_extended_frame}
\title{Predict and Extend Population Counts}
\description{Predict the outcome probabilities in population cells and convert them to expected counts.}
\usage{
predict_sequential_logit(fit, frame = fit$frame, n_draws = 125L, count_variable = "N", output_dir = "data/output",
    allow_missing_levels = TRUE, new_fixed_sd = NULL, write_files = TRUE, draw_state = NULL,
    batch_size = nrow(frame), progress = NULL)
aggregate_extended_frame(frame, posterior, by = character())
collapse_extended_frame(frame, posterior)
}
\arguments{
  \item{fit}{Sequential model returned by \code{fit_sequential_logit}.}
  \item{frame}{Original population frame. For prediction, defaults to the frame stored in the fit.}
  \item{n_draws}{Number of posterior draws. In \code{MasterP}, \code{NULL} uses the incoming draw count when present and 125 otherwise.}
  \item{count_variable}{Name of the population count column.}
  \item{output_dir}{Directory for generated files.}
  \item{allow_missing_levels}{Whether to allow frame levels absent from the fit.}
  \item{new_fixed_sd}{Optional positive prior standard deviation for unseen fixed effects.}
  \item{write_files}{Whether to save posterior probabilities and expanded counts to disk.}
  \item{draw_state}{Optional environment for sharing sampled coefficients and new-level effects across batches.}
  \item{batch_size}{Maximum number of prediction rows to process at once.}
  \item{progress}{Optional callback accepting completed work, total work, and a message.}
  \item{posterior}{Posterior probability object returned by \code{predict_sequential_logit}.}
  \item{by}{Grouping column names. Joint calibration also accepts a named list of grouping columns by margin.}
}
\details{
When the frame already contains population draws, prediction and aggregation
pair counts by draw number. Use shared draw state for separate prediction
batches that should use the same sampled effects.
}
\value{Prediction returns a posterior probability object. Aggregation returns mean counts and draw counts by the requested keys. Collapsing returns the mean counts for each original cell and outcome.}
