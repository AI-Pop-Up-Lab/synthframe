\name{check_extended_frame}
\alias{check_extended_frame}
\alias{check_post_rake}
\alias{post_rake_cell_comparison}
\alias{plot_raking_changes}
\title{Check Population Frame Predictions and Calibration}
\description{Compare outcome shares with benchmarks, check original cell totals after raking, and visualize changes from calibration.}
\usage{
check_extended_frame(extended_frame, targets, by, extension_variable, target_variable = "target",
    count_variable = "N", plot_file = NULL, outcome_order = NULL, coverage_tolerance = 0,
    no_vote = NULL, draw_plot = TRUE)
check_post_rake(raked_frame, frame, targets, by, extension_variable, target_variable = "target",
    count_variable = "N", outcome_order = NULL, outcome_plot_file = NULL, joint_plot_file = NULL,
    share_tolerance = 2e-12, count_tolerance = 1e-08, no_vote = NULL)
post_rake_cell_comparison(raked_frame, frame, extension_variable, count_variable = "N", include_max_error = FALSE)
plot_raking_changes(extended, raked, frame, area, outcome, plot_file, count_variable = "N",
    constrained_by = area, no_vote = NULL)
}
\arguments{
  \item{extended_frame}{Frame with one row per population cell and outcome combination.}
  \item{targets}{Table of calibration targets or benchmarks. Joint calibration takes a named list of margin tables.}
  \item{by}{Grouping column names. Joint calibration also accepts a named list of grouping columns by margin.}
  \item{extension_variable}{Name of the categorical outcome column. For original-cell checks, pass all outcome columns added to the frame.}
  \item{target_variable}{Name of the target column. Joint calibration also accepts a named mapping by margin.}
  \item{count_variable}{Name of the population count column.}
  \item{plot_file}{Path for a PNG plot, when requested.}
  \item{outcome_order}{Optional vector of outcome levels in the desired order.}
  \item{coverage_tolerance}{Numerical tolerance used when checking interval coverage.}
  \item{no_vote}{Outcome label for nonvoting, if present.}
  \item{draw_plot}{Whether to draw the benchmark plot.}
  \item{raked_frame}{Calibrated extended frame.}
  \item{frame}{Original population frame. For prediction, defaults to the frame stored in the fit.}
  \item{outcome_plot_file}{Optional PNG path for the outcome comparison.}
  \item{joint_plot_file}{Optional PNG path for the original-cell comparison.}
  \item{share_tolerance}{Tolerance used for outcome share comparisons.}
  \item{count_tolerance}{Tolerance used for original-cell count comparisons.}
  \item{include_max_error}{Whether to report maximum count error across the mean and draw columns.}
  \item{extended}{Extended frame before calibration.}
  \item{raked}{Extended frame after calibration, in the same row order.}
  \item{area}{Name of the area grouping column. The combined diagnostic wrapper also accepts a vector.}
  \item{outcome}{Name of the outcome column.}
  \item{constrained_by}{Grouping columns whose margins were constrained by calibration.}
}
\details{
Benchmark plots can express party outcomes as shares of voters when a
nonvoting outcome is identified. Raking-change plots use the original
population as the denominator for all outcomes.
}
\value{Benchmark checks return comparisons and summary metrics. The original-cell function returns a data frame. The change plot returns its file path invisibly.}
