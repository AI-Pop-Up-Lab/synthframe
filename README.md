# VoteFrame

VoteFrame aims to extend a population stratification frame with a categorical
variable observed in survey data. The planned workflow fits a multilevel model
using shared covariates and area-level predictors, then uses posterior draws
to estimate the population count in each cell for each outcome category.

## Country input layout

Each country and election cycle has its own folder under [data/input/](data/input/).
The folder contains three modeling inputs and two subfolders.

| Entry | Contents |
| --- | --- |
| `election_results.csv` | Area-level results from the last general election before the study election |
| `frame.csv` | Deepest available population stratification frame before extension |
| `auxiliary_survey.csv` | Processed national election study |
| `raw/` | Downloaded source extracts and metadata |
| `processing/` | Recoding code, intermediate files, and candidate rebuilds |

## Turnout and past vote

For each area, turnout is the sum of votes for the modeled vote choices
divided by that area's total population in the frame. The `Did not vote` share
is the remaining frame population divided by the same total. Include valid
votes and any separately recorded blank ballots when the auxiliary survey
offers a blank-vote choice. Other invalid ballots contribute to `Did not vote`.

## Denmark 2026

The [Denmark 2026 README](data/input/denmark%202026/README.md) documents the
population frame, the 2022 election results, and the auxiliary survey. It
includes the source selections, recoding rules, and rebuild instructions.

[data/output/](data/output/) is the destination for generated modeling files.

The [Denmark 2026 vignette](vignette_denmark_2026.md) shows the R code,
captured results, diagnostic plots, and a preview of the final raked frame.
Edit the [R Markdown source](vignette_denmark_2026.Rmd) and regenerate the
GitHub view from the repository root with `knitr`. This uses the captured
results and existing figures.

```r
knitr::opts_chunk$set(eval = FALSE)
knitr::knit("vignette_denmark_2026.Rmd", output = "vignette_denmark_2026.md")
```

## Fitting the extension variable

Load the R functions from the repository root with `vglmer` installed.

```r
for (file in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(file)

country <- "data/input/denmark 2026"
survey <- read.csv(file.path(country, "auxiliary_survey.csv"))
frame <- read.csv(file.path(country, "frame.csv"))
area_covariates <- process_area_covariates(
  file.path(country, "election_results.csv"),
  area_variable = "municipality",
  outcome_variable = "vote_choice_2022",
  value_variable = "pop_pct"
)

fit <- fit_sequential_logit(
  auxiliary_survey = survey,
  extension_variable = "vote_choice_2022",
  area_variable = "municipality",
  area_covariates = area_covariates,
  no_vote = "Did not vote",
  depth = 2,
  frame = frame
)

fit$outcome_order
fit$stages[["Did not vote"]]$formula
```

By default the fitter uses every survey column except the outcome. Supply
`predictors` to select columns. The area is always a categorical predictor.
Use factors for other categorical predictors stored as numbers. Frequency
uses all recorded outcomes, with ties broken by label. The fit uses complete
survey rows. Missing outcomes or selected predictors, including empty strings,
trigger a warning with the number of rows removed before aggregation. Each
stage's failures are the counts for later outcomes in the same cell. To recover a category probability,
multiply its conditional probability by the probability of passing every
earlier stage. The final category receives the remaining probability, so there
is one fewer fit than outcome categories.

Use a named list of matrices for custom area covariates. Name each matrix after
its stage outcome, each row after its area, and each column after its predictor.
A data frame with area IDs as row names also supports categorical area
predictors. The area covariate helper accepts a CSV path, a data frame, or a named
list of either. Each dataset uses the same area, outcome, and value column
names, contains one row per area and outcome, and covers the same set of
areas. The list names become predictor column names. A single unnamed input
uses `area_covariate`. Areas are matched by ID, so rows can be in any order.

For each dataset, the helper divides the stage outcome's
value by the sum over the remaining outcomes. An absent area/outcome pair is
zero, and a zero denominator gives a zero share. Counts and shares with a
common denominator within area give the same conditional shares.
Pass `outcome_order` to the area covariate helper to get the stage matrices directly.
Otherwise the fitter supplies the order it computes from the survey.
For variables that should enter on their original scale, supply them in the
custom stage matrices.

Depth 0 fits an intercept plus an area random intercept. Depth 0.5 adds
random intercepts for the other categorical survey predictors. Depth 0.75 also adds linear fixed effects for
the area covariates. Depth 1 gives main effects with splines enabled by
default. Higher integer depths count distinct predictors in a term.
Categorical terms use random
intercepts, and crossed terms use random slopes for the continuous effects
within the group. Truncated linear splines add global curves and group-level
deviations as specified in Goplerud's Appendix A.4. Each `vglmer`
factor-by-curve term includes its own global spline. Joint smooths of multiple
continuous predictors use `gKRLS`, which must be installed for those models.
The `gKRLS` sketch uses R's random seed.
A numeric predictor with fewer than four distinct values uses a linear main
effect, and constant predictors are omitted. Set `splines = FALSE` for linear
effects and their interactions.
All stages use `vglmer`. It requires a random effect or spline, and an area
random intercept requires at least two observed areas. A stage that cannot
meet those engine requirements stops the fit and records the reason in the log.
The fitted object retains each stage's formula, data, and model fit for
inspection. [Supplementary material](https://static.cambridge.org/content/id/urn%3Acambridge.org%3Aid%3Aarticle%3AS0003055423000035/resource/name/S0003055423000035sup001.pdf)

An area-level predictor has one value per area within a stage. The formula
builder excludes terms that combine an area-level predictor with a grouping
that contains the area itself. Within such a group, the predictor is constant,
so its varying slope or smooth is confounded with the group intercept. This
rule applies before counting coefficients. The area random intercept and its
interactions with demographic factors are eligible, as are each area
predictor's overall effect and its varying slopes and smooths by demographic
groups, subject to depth and spline settings. See [Goplerud, supplement
§A.4](https://mgoplerud.com/papers/Goplerud_MMM_Full.pdf#page=19) for the
factor-by-curve specification.

Each stage starts from the requested depth and selects the highest allowed
depth at which the number of eligible respondents is at least the
estimated number of parameters. If every candidate exceeds that limit,
the fitter warns and attempts depth 0 anyway. Each respondent contributes one trial,
including respondents who share an aggregated cell. A later stage may
select a higher depth than an earlier stage if its design has shrunk enough.
The requested depth is the ceiling for every stage. The estimate counts
fixed-effect design columns, one
coefficient per random intercept or slope per observed grouping level, and
one coefficient per spline basis column. Truncated linear splines use
`floor(min(distinct values / 4, 35))` basis columns. Joint kernel smooths
use the default sketch width, capped at the number of aggregated cells.
Each by-smooth counts its global basis and one basis per observed group.
This is a pre-fit coefficient count that gives no discount for shrinkage
or rank deficiency and excludes variance and covariance hyperparameters.

The fitter warns with the selected depth for each stage. It writes a CSV
in the working directory by default. Supply `log_file` to choose its path.
The CSV records each candidate depth and count, the selected depth,
respondent and cell totals, and fit status. The returned object exposes the
log as `fit$fit_log` and its path as `fit$log_file`.

## Predicting and extending the frame

```r
set.seed(123)
posterior <- predict_sequential_logit(fit, n_draws = 125)
by_municipality_age <- aggregate_extended_frame(
  frame, posterior, by = c("municipality", "age_group")
)
collapsed <- collapse_extended_frame(frame, posterior)
```

Pass `frame` to the fitter to register the population levels of each fitted
effect at every stage. Crossed effects use the combinations present in the
frame. Inspect a stage's `frame_levels` for its fitted, frame, and frame-only
coefficient names. The fit log records the counts of frame-only coefficients.
The predictor uses the saved frame by default. It also accepts a frame
explicitly, including for fits created without one.

Frame-only levels have zero survey trials. Their random coefficients are
integrated out during estimation and sampled from their fitted term during
prediction. The depth rule counts coefficients estimated from the survey.
An effect absent from the fitted formula has no variance block to sample.

The predictor converts each stage's sampled log odds into conditional
probabilities. Within each draw, it multiplies those probabilities by the
chance of reaching the stage. The last outcome gets the remaining probability,
so each cell's outcome probabilities sum to one in every draw.

Unseen random-effect levels are centered at zero on the log-odds scale.
For each posterior draw, the predictor draws a covariance from the fitted
term's inverse-Wishart distribution, then draws a zero-centered coefficient
vector for each new level. Cells sharing that level also share its draw.
This covers random intercepts, slopes, crossed groups, and group-specific
splines. Spline predictions reuse the fitted basis. New numeric values
evaluate the same fitted effects and basis functions.

Unseen fixed categorical contrasts have no fitted population variance.
To simulate those contrasts, supply `new_fixed_sd` for an explicit
zero-centered normal prior on the log-odds scale. The new contrast is
relative to the fitted reference level, or the first fitted level for
coding with no reference level. Pass `allow_missing_levels = FALSE` to
reject new levels. For stage-level point predictions on the log-odds scale,
`predict_vglmer_levels(..., n_draws = 0)` uses the fitted coefficient means
and zero for new coefficients.

`predict_sequential_logit()` writes `posterior_probabilities.rds` and
`extended_frame.csv` to `data/output/` by default. Set `output_dir` to choose
another directory. A rerun replaces those files. The saved R object holds a
cell-by-outcome-by-draw probability array and the frame keys. The CSV has one
row per frame cell and extension category. Its `N_draw_1`, `N_draw_2`, and subsequent
columns are draw-specific expected counts. `N` is their posterior mean.
The draw names use the supplied count column name as a prefix.

Aggregation multiplies the probabilities by the frame's population counts
and sums the result within each draw. Supply any combination of frame columns
in `by`, or use `by = character()` for totals by outcome. The original frame
can be reordered or subsetted because cell keys match it to the saved draws.
The collapsed frame keeps just the posterior mean expected count for each
cell and category. Those means add back to each original cell population.

Use `check_extended_frame(extended_frame, targets, by, extension_variable)`
to compare expected counts aggregated as shares against target shares on
the 0–1 scale. Target rows are keyed by the grouping columns in `by`
and the extension category. Set `target_variable` to the target share
column and `plot_file` to save the identity-line panels as a PNG.
Pass `outcome_order = fit$outcome_order` to use the fitted sequence.
The function returns the matched estimates and pooled and outcome-level
bias, RMSE, Spearman correlation, and 90% interval coverage. A mean-only extended
frame can be checked with a warning that intervals and coverage are unavailable.

Use `rake_extended_frame(extended_frame, frame, targets, by, extension_variable)`
to balance each expected count column to the original joint frame and the
outcome targets within each group defined by `by`. The solver uses
`SQUAREM::squarem()` to accelerate the outcome-scaling update, with the
original cell totals enforced algebraically. On the full Denmark rake,
the benchmark took 13.4 seconds versus 25.1 seconds for the original
loop, excluding file I/O. Install `SQUAREM` to run the rake. The
[mathematical note](raking_method.Rmd) derives the update. Supply target shares
or use `target_scale = "count"` for count targets. Supply at least one
outcome target per group. Untargeted outcomes share the remaining
population in their group. The function preserves each original cell
population and matches group and outcome keys by value. Missing
cell–outcome combinations and zero counts remain zero. If those zeros
make a margin unreachable, the error names the group and count column.
When draws are present, each draw and the mean-count column are raked
independently.

Use `check_post_rake()` to compare the raked outcome shares with their
targets and the raked counts with the original joint cell populations.
It returns both comparisons and can save a separate identity-line plot
for each.

Use `plot_raking_changes()` with the aligned extended and raked frames
to generate one diagnostic figure for the outcome margins left
unconstrained by a municipality rake. The
[Denmark vignette](vignette_denmark_2026.md#step-10-visualize-changes-on-margins-left-unconstrained)
defines total variation and explains how to read the figure.

Use `rake_with_diagnostics()` to run the same rake and generate the
target, original-joint, and unconstrained-margin plots automatically.
It returns the raked frame and the diagnostics. The Denmark vignette
uses this entry point.

### Extending a frame twice

Use the first extended frame as the prediction frame for the second fit.
When it contains `N_draw_1`, `N_draw_2`, and so on,
`predict_sequential_logit()` defaults to that number of draws. An explicit
`n_draws` must match. It multiplies the second extension's probability for
each outcome in draw `i` by the incoming `N_draw_i`. The new `N` is the
mean of those products. With only an incoming `N`, the second extension
uses it as the population in every draw. Save each prediction in its own
output directory.

After the second extension, rake to both outcome margins in one call with
the **original** stratification frame. Supply target tables named for the
extension variables. Use a shared `by` vector or name a grouping vector for
each target table.

```r
second <- predict_sequential_logit(
  second_fit, frame = first_extended, output_dir = "data/output/second-extension"
)
twice_extended <- read.csv(second$extended_frame_file)
final <- rake_joint_extended_frame(
  twice_extended, original_frame,
  targets = list(first_outcome = first_targets, second_outcome = second_targets),
  by = list(first_outcome = "municipality", second_outcome = "education")
)
```

The joint rake enforces the original cell populations and both sets of
outcome targets on `N` and on every draw. Its solver verifies all margins
together before returning. As in the single-extension rake, the mean-count
column is raked independently of the draws.
