\name{MasterP}
\alias{MasterP}
\title{Fit, Extend, and Calibrate a Population Frame}
\description{Run the full SynthFrame workflow for one or more categorical outcomes.}
\usage{
MasterP(mother_frame, extensions, n_draws = NULL, count_variable = "N", aggregate_by = list(),
    tolerance = 1e-12, max_iterations = 1000L, keep_fits = FALSE, pre_rake_mode = "auto",
    pre_rake_dir = NULL, area_variable = NULL, parent_area_variable = NULL, areas_per_batch = NULL,
    verbose = FALSE, log_file = NULL)
}
\arguments{
  \item{mother_frame}{Original population cells and their counts.}
  \item{extensions}{Named list in fitting order. Each entry specifies a survey or list of surveys, fitting options in \code{fit}, prediction options in \code{predict}, and optional named \code{rake} and \code{plots} specifications.}
  \item{n_draws}{Number of posterior draws. In \code{MasterP}, \code{NULL} uses the incoming draw count when present and 125 otherwise.}
  \item{count_variable}{Name of the population count column.}
  \item{aggregate_by}{Named list of grouping columns for summaries. Use \code{character()} for a population total.}
  \item{tolerance}{Numerical tolerance for calibration.}
  \item{max_iterations}{Iteration limit for the relevant solver.}
  \item{keep_fits}{Whether to include the fitted models in the return value.}
  \item{pre_rake_mode}{Storage mode for the extended frame used by diagnostics. One of \code{auto}, \code{memory}, \code{temporary}, or \code{keep}.}
  \item{pre_rake_dir}{Directory for pre-rake storage. Required in \code{keep} mode.}
  \item{area_variable}{Name of the column identifying areas.}
  \item{parent_area_variable}{Optional column naming the preferred geographic batch for each area.}
  \item{areas_per_batch}{Optional number of areas per batch. Calibration margins that span areas keep their areas together.}
  \item{verbose}{Whether to print pipeline progress.}
  \item{log_file}{Path for the run log or, when fitting directly, the stage log CSV.}
}
\details{
Each extension name becomes an outcome column. Later stages can use earlier outcomes
as predictors when the auxiliary survey records them. Calibration is joint across
all configured margins. An example margin is
\code{rake = list(area = list(targets = target_table, by = "area"))}.
}
\value{A list with the calibrated frame, requested aggregates, diagnostics, fit summaries, and run log path. If requested, the list also contains the fitted models.}
