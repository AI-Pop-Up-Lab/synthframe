\name{fit_sequential_logit}
\alias{fit_sequential_logit}
\alias{aggregate_extension_survey}
\alias{process_area_covariates}
\title{Prepare and Fit Sequential Outcome Models}
\description{Aggregate the auxiliary survey, prepare optional area predictors, and fit a sequence of binary logistic models for one categorical outcome.}
\usage{
fit_sequential_logit(auxiliary_survey, extension_variable, area_variable, area_covariates = NULL,
    no_vote = NULL, depth = 1L, splines = TRUE, predictors = NULL, control = NULL,
    log_file = tempfile("synthframe-fit-", tmpdir = ".", fileext = ".csv"), frame = NULL,
    outcome_order = NULL, no_interactions = character(), tolerance_parameters = NULL,
    tolerance_elbo = NULL, max_iterations = NULL, interaction_support_log = NULL,
    depth_support_dir = NULL, minimum_support = -1L, progress = NULL, solver_trace = FALSE)
aggregate_extension_survey(survey, extension_variable, predictors, no_vote, outcome_order = NULL)
process_area_covariates(area_data, area_variable, outcome_variable, value_variable, outcome_order = NULL)
}
\arguments{
  \item{auxiliary_survey}{Respondent-level data frame containing the predictors and outcome.}
  \item{extension_variable}{Name of the categorical outcome column. For original-cell checks, pass all outcome columns added to the frame.}
  \item{area_variable}{Name of the column identifying areas.}
  \item{area_covariates}{Optional area predictor tables, one per fitted outcome stage.}
  \item{no_vote}{Outcome label for nonvoting, if present.}
  \item{depth}{Maximum interaction order considered during stage design screening.}
  \item{splines}{Whether to consider spline terms.}
  \item{predictors}{Names of the survey predictor columns. The fitter can infer these when omitted.}
  \item{control}{Optional control list from \code{vglmer::vglmer_control}.}
  \item{log_file}{Path for the run log or, when fitting directly, the stage log CSV.}
  \item{frame}{Original population frame. For prediction, defaults to the frame stored in the fit.}
  \item{outcome_order}{Optional vector of outcome levels in the desired order.}
  \item{no_interactions}{Predictor columns to exclude from interactions.}
  \item{tolerance_parameters}{Optional convergence tolerance on parameter change.}
  \item{tolerance_elbo}{Optional convergence tolerance on the evidence lower bound.}
  \item{max_iterations}{Iteration limit for the relevant solver.}
  \item{interaction_support_log}{Optional path for the interaction support CSV.}
  \item{depth_support_dir}{Optional directory for candidate-depth diagnostic plots.}
  \item{minimum_support}{Minimum survey support for interaction levels. A negative value selects the fitter defaults.}
  \item{progress}{Optional callback accepting completed work, total work, and a message.}
  \item{solver_trace}{Whether to emit solver diagnostic output.}
  \item{survey}{Respondent-level data frame.}
  \item{area_data}{Data frame or CSV path containing area-level outcome values.}
  \item{outcome_variable}{Name of the column identifying outcome levels.}
  \item{value_variable}{Name of the outcome value column, or a vector of columns.}
}
\details{
Each stage models its outcome against the remaining categories. The final outcome
receives the remaining probability. Stage predictors are screened for support
and collinearity before fitting.
}
\value{The fitter returns a sequential fit with one model per fitted stage and a stage log. Survey aggregation returns cell counts and outcome order. Area covariate preparation returns area-indexed values or stage-specific conditional shares.}
