\name{rake_extended_frame}
\alias{rake_extended_frame}
\alias{rake_joint_extended_frame}
\alias{rake_with_diagnostics}
\title{Calibrate Extended Population Counts}
\description{Adjust predicted counts to known outcome margins while preserving original cell totals.}
\usage{
rake_extended_frame(extended_frame, frame, targets, by, extension_variable, target_variable = "target",
    count_variable = "N", target_scale = c("share", "count"), tolerance = 1e-12,
    max_iterations = 1000L)
rake_joint_extended_frame(extended_frame, frame, targets, by, extension_variables = names(targets),
    target_variable = "target", count_variable = "N", target_scale = "share",
    tolerance = 1e-12, max_iterations = 1000L, margin_variables = stats::setNames(names(targets),
        names(targets)), area_variable = NULL, parent_area_variable = NULL, areas_per_batch = NULL,
    progress = NULL, solver_trace = FALSE)
rake_with_diagnostics(extended_frame, frame, targets, by, extension_variable, target_variable = "target",
    count_variable = "N", target_scale = c("share", "count"), output_dir = "data/output",
    outcome_order = NULL, area = by, tolerance = 1e-12, max_iterations = 1000L,
    no_vote = NULL)
}
\arguments{
  \item{extended_frame}{Frame with one row per population cell and outcome combination.}
  \item{frame}{Original population frame. For prediction, defaults to the frame stored in the fit.}
  \item{targets}{Table of calibration targets or benchmarks. Joint calibration takes a named list of margin tables.}
  \item{by}{Grouping column names. Joint calibration also accepts a named list of grouping columns by margin.}
  \item{extension_variable}{Name of the categorical outcome column. For original-cell checks, pass all outcome columns added to the frame.}
  \item{target_variable}{Name of the target column. Joint calibration also accepts a named mapping by margin.}
  \item{count_variable}{Name of the population count column.}
  \item{target_scale}{Whether calibration targets are shares or counts.}
  \item{tolerance}{Numerical tolerance for calibration.}
  \item{max_iterations}{Iteration limit for the relevant solver.}
  \item{extension_variables}{Names of the outcome columns jointly calibrated.}
  \item{margin_variables}{Named mapping from margin names to outcome columns.}
  \item{area_variable}{Name of the column identifying areas.}
  \item{parent_area_variable}{Optional column naming the preferred geographic batch for each area.}
  \item{areas_per_batch}{Optional number of areas per batch. Calibration margins that span areas keep their areas together.}
  \item{progress}{Optional callback accepting completed work, total work, and a message.}
  \item{solver_trace}{Whether to emit solver diagnostic output.}
  \item{output_dir}{Directory for generated files.}
  \item{outcome_order}{Optional vector of outcome levels in the desired order.}
  \item{area}{Name of the area grouping column. The combined diagnostic wrapper also accepts a vector.}
  \item{no_vote}{Outcome label for nonvoting, if present.}
}
\details{
Targets identify outcomes and optionally grouping keys. Joint calibration
solves several margins together, including margins at different geographic
levels. Provide feasible targets on the selected scale.
}
\value{The solvers return the calibrated frame, including draw columns when supplied. The diagnostic wrapper generates plots and returns the raked frame and diagnostic results.}
\examples{
frame <- data.frame(area = c("east", "west"), N = c(100, 200))
extended <- data.frame(area = rep(frame$area, each = 2),
                       vote = rep(c("A", "B"), 2), N = c(40, 60, 100, 100))
targets <- data.frame(vote = c("A", "B"), target = c(0.6, 0.4))
raked <- rake_extended_frame(extended, frame, targets, character(), "vote")
aggregate(N ~ vote, raked, sum)
}
