# VoteFrame

VoteFrame extends population stratification frames with categorical
outcomes observed in surveys. It fits sequential multilevel models,
predicts outcome counts for each population cell, and calibrates the
extended frame to known margins.

## Reproduce a case study

The case studies are on the `package` branch. Each example runs from
the prepared inputs tracked under
[`data/input/`](data/input/). Its input guide covers sources, recodes,
and any upstream steps needed to rebuild those inputs.

| Case study | Read the results | Run the source | Input guide |
| --- | --- | --- | --- |
| Denmark 2026 | [Vignette](vignettes/vignette_denmark_2026.md) | [R Markdown](vignettes/vignette_denmark_2026.Rmd) | [Denmark inputs](data/input/denmark%202026/README.md) |
| Sweden 2026 | [Vignette](vignettes/vignette_sweden_2026.md) | [R Markdown](vignettes/vignette_sweden_2026.Rmd) | [Sweden inputs](data/input/sweden%202026/README.md) |
| US 2026 | [Vignette](vignettes/vignette_us_2026.md) | [R Markdown](vignettes/vignette_us_2026.Rmd) | [US inputs](data/input/us%202026/README.md) |

Install the R packages in your own R library. The figures use Cairo PNG
support, which you can check with `capabilities("cairo")`.

```r
install.packages(c("knitr", "vglmer", "gKRLS", "SQUAREM"), repos = "https://cloud.r-project.org")
```

The saved US results used R 4.3.3, vglmer 1.0.8, gKRLS 1.0.5, SQUAREM 2026.1,
knitr 1.52, and Matrix 1.6-5. Use a compatible release of Matrix if
recreating that R 4.3 environment. [CRAN Matrix archive](https://cran.r-project.org/src/contrib/Archive/Matrix/)

From the repository root, set `country` to one of `"denmark"`, `"sweden"`, or
`"us"` and run the block below. The chunks execute in order and write the
rendered Markdown beside the R Markdown source.

```r
country <- "denmark"
vignette <- file.path("vignettes", paste0("vignette_", country, "_2026"))
if (country == "us") Sys.setenv(VOTEFRAME_US_FIT = "true")
knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(error = FALSE)
knitr::knit(paste0(vignette, ".Rmd"), output = paste0(vignette, ".md"))
```

### Generated outputs

Each run writes to `data/output/<country> 2026/` using the same layout.

| Directory | Contents |
| --- | --- |
| `models/` | Fitted models as RDS files |
| `diagnostics/` | Fit and screening CSVs, benchmark metrics, and original-cell checks |
| `logs/` | Captured render or run logs and any recovery notes |
| `raked_frame/` | Calibrated counts for the mean and all 125 draws |

Denmark writes one `raked_frame/part-01.rds` file. Sweden writes one
partition per geographic batch, and the US writes one per state and DC.
The report figures are in each case's matching directory under
`vignettes/assets/`. Set `VOTEFRAME_PRE_RAKE_STORAGE=keep` to retain the
optional pre-rake partitions in `extended_frame/`.
The US example calibrates state presidential and district House margins
jointly. Its saved national frame occupies about 6.5 GiB. Use
`VOTEFRAME_US_RAKED_DIR` for a larger disk when needed.

Denmark and Sweden use 2022 municipal shares of the electoral roll as
predictors and calibration targets. `No Vote` counts eligible people who
cast no ballot. `Invalid` includes blank and other rejected ballots in
official returns. The comparisons use shares among voters for party
choices and shares of the full population for No Vote.

### Reuse completed US fits

For a vignette render that reuses completed fits, set
`VOTEFRAME_US_REUSE_FITS=true` before knitting. The vignette loads each
available fit from `models/` and starts a fresh fit for any missing model.
It saves each completed model before continuing. The default run
directory is `data/output/us 2026`. Set `VOTEFRAME_US_OUTPUT_DIR` to use
another run directory. Set `VOTEFRAME_US_RAKE=false` to render only the
pre-rake benchmark comparison.

## Fitting the extension variable

### Run the full pipeline with `MasterP()`

Source the files in `R/` and pass a named list of extension specifications in
the order they should be added to the mother frame. Each name becomes a new
outcome column. An extension can use one auxiliary survey or pool a list of
surveys by stacking respondents. The selected columns must have harmonized
names and coding across surveys. Later fits can use previous outcomes as
predictors when their surveys contain those outcomes.

```r
result <- MasterP(
  mother_frame,
  extensions = list(
    presidential = list(
      survey = presidential_survey,
      fit = list(area_variable = "district_id", depth = 2,
                 no_interactions = c("district_id", "state_fips"),
                 area_covariates = presidential_covariates),
      predict = list(batch_size = 4000L),
      rake = list(state = list(targets = presidential_state_targets, by = "state_fips")),
      plots = list(district = list(targets = presidential_district_benchmarks,
                                   by = "district_id", after_file = "presidential_district.png"))
    ),
    house = list(
      survey = list(house_survey_1, house_survey_2),
      fit = list(area_variable = "district_id", depth = 2,
                 no_interactions = c("district_id", "state_fips"),
                 area_covariates = house_covariates),
      rake = list(district = list(targets = house_district_targets, by = "district_id"))
    )
  ),
  aggregate_by = list(state_presidential = c("state_fips", "presidential"),
                      national_joint = c("presidential", "house"),
                      national_total = character())
)

result$frame
result$aggregates$state_presidential
```

`MasterP()` reports the current step and tracks completed fit stages,
prediction batches, and raking batches and draws. Time estimates appear
after the first unit finishes and update from the observed pace. Set
`verbose = TRUE` to see solver iterations and every progress update in
the console. The run log includes the detailed trace, warnings, and any
failure. Each run gets a timestamped log under `data/output/masterp_logs/`
by default. Set `log_file` to write it to a different path.
See `result$log_file` for the path and `result$fit_info` for the stage CSVs.

Use `fit` for options such as `predictors`, `outcome_order`, `no_vote`,
splines, solver control, and stage-specific area covariates. By default,
predictors are columns shared by the extension's surveys and its input frame.
Set `register_frame = TRUE` in an extension to register its population
levels during fitting. Its `predict` list accepts row batching and
unseen-level options. Predictions stay in memory by default.

Each extension supplies one or more named `rake` margins. Use `by` for
grouping columns from the mother frame, or `character()` for a national
margin. Target tables use the extension name for the outcome column and
`target` for the value column by default. Set `target_variable` or
`target_scale = "count"` within a margin to override those defaults.
`MasterP()` balances all margins simultaneously. Count targets must be
compatible with the corresponding population total in each draw.

Optional `plots` can use independent benchmark shares and grouping
columns. Each named comparison returns before- and after-rake diagnostics.
Set `before_file` or `after_file` to write the corresponding plot.

If the mother frame already has numbered population draws such as
`N_draw_1`, `MasterP()` infers the draw count and matches prediction draw
`i` with population draw `i`. The rake preserves each incoming cell's
count separately for `N` and each draw. With a mean-only mother frame,
`n_draws` defaults to 125 and each draw starts from `N`.
`result$frame` contains the full calibrated frame. MasterP keeps the
pre-rake frame in memory or temporary storage according to available
memory. Use
`pre_rake_mode = "keep", pre_rake_dir = "path/to/output"` to save the full
joint pre-rake frame as `extended_frame.rds`. The `"temporary"` mode uses
disk after the joint rake and removes the temporary file when the call
ends. Joint raking itself requires the full frame in memory. Set an
extension's `predict$write_files = TRUE` to export that extension's own
predictions as well.

The vignettes use `VOTEFRAME_PRE_RAKE_STORAGE` with values `auto` (the
default), `memory`, `temporary`, or `keep`. Automatic mode estimates the
pre-rake footprint against available memory, including Linux container
limits where visible, and uses temporary disk when memory looks tight or
cannot be measured. The Sweden and Denmark vignettes batch by
constituency, and the US vignette batches by state or DC. Set
`VOTEFRAME_PARENT_AREA` to choose a parent column and
`VOTEFRAME_AREAS_PER_BATCH` to request a fixed number of lower-level areas
per batch. Without a usable parent, the default is ten lower-level areas.

MasterP and `rake_joint_extended_frame()` accept `area_variable`,
`parent_area_variable`, and `areas_per_batch`. MasterP infers the
lower-level area from the fitted extensions when omitted. Every raking
margin must remain whole within a batch. A statewide margin can therefore
require an entire state even if the requested batch size is smaller.
Crossing margins without a safe geographic grouping use one batch.
MasterP batches joint calibration after fitting and extending the full
frame. In the vignettes the active prediction or raking block must fit
in memory. Disk modes check estimated space where `df` is available.
Set `VOTEFRAME_PRE_RAKE_DIR` to choose another volume or output directory.
Temporary files default to R's temporary directory. Kept files default
to `extended_frame/` under the vignette's output directory.
`result$aggregates` sums its mean and draw columns at each requested level.
Fit details are in `result$fit_info`. Pass `keep_fits = TRUE` to retain
the model objects as well.

### Fit one extension directly

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
  value_variable = "share_eligible"
)

fit <- fit_sequential_logit(
  auxiliary_survey = survey,
  extension_variable = "vote_choice_2022",
  area_variable = "municipality",
  area_covariates = area_covariates,
  no_vote = "No Vote",
  depth = 4,
  predictors = c("municipality", "gender", "age_group", "education"),
  frame = frame
)

fit$outcome_order
fit$stages[["No Vote"]]$formula
```

By default the fitter uses every survey column except the outcome. Supply
`predictors` to select columns. The area is always a categorical predictor.
Use factors for other categorical predictors stored as numbers. Frequency
uses all recorded outcomes, with ties broken by label. Supply
`outcome_order` to set the sequence explicitly, including the final
category that receives the remaining probability. For vote extensions,
supply the nonvoter label as `no_vote`. The fitter always puts that label
first and preserves the relative order of the other outcomes.
The fit uses complete survey rows. Missing outcomes or selected predictors, including empty strings,
trigger a warning with the number of rows removed before aggregation. Each
stage's failures are the counts for later outcomes in the same cell. To recover a category probability,
multiply its conditional probability by the probability of passing every
earlier stage. The final category receives the remaining probability, so there
is one fewer fit than outcome categories.

Use a named list of matrices or data frames for custom area covariates.
Name each entry after its stage outcome, each row after its area, and each
column after its predictor. Each stage can have its own predictor columns
and values. For shared predictors, reuse the same table with
`setNames(rep(list(area_predictors), length(stages)), stages)`, where
`stages` contains the fitted outcomes. A data frame supports categorical
area predictors. The area covariate helper accepts a CSV path, a data frame, or a named
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
Use `no_interactions` to name selected survey or stage covariate predictors
that should enter only as main effects, including their own splines when
eligible. The exclusion applies at every candidate depth before counting
coefficients. Categorical terms use random intercepts, and crossed terms
use random slopes for the continuous effects within the group. Truncated linear splines add global curves and group-level
deviations as specified in Goplerud's Appendix A.4. Each `vglmer`
factor-by-curve term includes its own global spline. Joint smooths of multiple
continuous predictors use `gKRLS`, which must be installed for those models.
The `gKRLS` sketch uses R's random seed.
A numeric predictor with fewer than four distinct values uses a linear main
effect. Predictions at a stage use the design estimable from its eligible
survey rows. Constant predictors are omitted before constructing terms.
The fitter checks the unpenalized fixed-effects columns for linear dependence
and keeps an independent set, including when a product duplicates a main
effect. The selected stage warns and records omitted predictors and columns in
its fit log. The rank check leaves the hierarchical interaction and spline
bases intact. Set `splines = FALSE` for linear
effects and their interactions.
All stages use `vglmer`. It requires a random effect or spline, and an area
random intercept requires at least two observed areas. A stage that cannot
meet those engine requirements stops the fit and records the reason in the log.
By default, VoteFrame accepts convergence when the largest tracked
variational parameter change is below 0.01 or the positive ELBO gain is
below 10⁻⁵. A supplied `vglmer_control()` provides its own defaults. Use
`tolerance_parameters` and `tolerance_elbo` to override either threshold,
with zero disabling the corresponding stopping criterion. Set these options
in the extension's `fit` list when using `MasterP()`. The solver can
also return at its iteration cap, with a non-convergence message. Set
`max_iterations` to override the cap for each stage. To inspect support for
categorical interactions and varying slopes, set `interaction_support_log`
to a CSV path. The audit counts stage respondents with nonzero entries by
outcome and measures within-group slope variation, including how many
respondents have a slope value other than the group’s most common value.
Set `depth_support_dir` to write a PNG for each stage before fitting. The
two panels use linear and logarithmic axes to show the cumulative share of
coefficient columns by their number of nonzero observations. Solid lines
count aggregated survey rows and dashed lines count individual respondents.
The lower panels break down the respondent curve into scalar random effects,
correlated random effects, smooths, and fixed effects. Each shaded area uses
all coefficients at that depth as its denominator.
For an integer depth request, the plots compare depths 1–3 as well as the
requested and selected depths if different. Rejected candidates can be
expensive to inspect, especially with splines. With `MasterP()`, set
`depth_support_dir` in the extension's `fit` list.
Set `minimum_support = 10L` to screen fixed columns and random-effect levels
with fewer than 10 eligible respondents. The default `-1L` and a cutoff of
zero disable the support screen. With `MasterP()`, set the cutoff in the
extension's `fit` list. VoteFrame counts respondents using each aggregated
row's binomial trial count. The fixed intercept stays in the model.
Screened fixed columns contribute zero in prediction. Intercept support
determines which group levels enter each random-effect block. A separate
within-group support check below determines which slopes enter the block.
Screened levels use the fitted term covariance for prediction. If every level of a random term is screened, VoteFrame drops the term from
fitting and prediction. If screening leaves only fixed effects in the
likelihood, VoteFrame uses a binomial GLM and draws fixed coefficients from
its normal approximation. Eligible survey rows and penalized spline bases
are preserved throughout, except that a grouped spline is dropped when
all levels of its companion random term are screened. VoteFrame also drops
any spline that uses a predictor whose fixed main effect is screened. The log names each
dropped smooth and records its estimated coefficient count.

For each candidate depth, VoteFrame screens low support first, then
checks the supported unpenalized fixed design for collinearity. It weights
each row by the square root of its binomial trial count and scales each
column to unit Euclidean length. For each singular value, the condition
index is the largest singular value divided by that singular value. The
largest index is the condition number. Belsley, Kuh, and Welsch use about
30 as a flag to inspect collinearity in linear regression, along with the
variance-decomposition proportions for the same component. VoteFrame
adopts 30 as a pre-fit pruning cutoff for the weighted fixed design. This
is a modeling choice for this logistic fit. It does not compute Belsley's
variance-decomposition diagnostic. [SAS/STAT guide to Belsley's diagnostics](https://support.sas.com/documentation/cdl/en/sgug/59902/HTML/default/fit_sect29.htm)

The screen starts with the full supported fixed design. While its
condition number is at or above 30, it finds the weakest singular
direction. Columns with absolute loadings at least 1% of that
direction's largest absolute loading are candidates for removal. The
intercept is protected. Among candidates of the highest interaction
order, VoteFrame removes the column whose deletion and hierarchy cascade
produce the lowest condition number. Ties remove the later formula
column. This rule uses predictor variation alone.

Every interaction requires all its immediate lower-order terms, which
recursively requires its main effects. For example, a three-way
interaction requires all three two-way interactions. This applies to fixed
products, crossed random intercepts, varying slopes, and the spline for
each term. An interaction whose parent term is fully screened is removed. If a dropped grouped spline had supplied an
otherwise eligible global spline, the global spline is fitted on its own.
The log records which fixed columns failed support or collinearity, which
terms were removed by hierarchy, and the condition number before and after
the fixed-design pruning. All selection runs before the solver is called.
The fitted object retains each stage's formula, data, and model fit for
inspection. [Supplementary material](https://static.cambridge.org/content/id/urn%3Acambridge.org%3Aid%3Aarticle%3AS0003055423000035/resource/name/S0003055423000035sup001.pdf)

VoteFrame leaves constant predictors out when constructing each stage
design. A one-level factor has no estimable contrast, and a constant
numeric predictor supplies no main-effect variation. A crossed random
intercept is removed if its observed grouping is identical to one of its
immediate parents. For example, if district already determines state,
district by state gives the same grouping as district. The fit log lists
these removals under `redundant_random_groups`.

VoteFrame checks the actual slope column, including products for interaction
slopes. For each retained group level it counts eligible respondents whose
slope value differs from the most frequent value in that level, using the
aggregated binomial trial counts. A constant level contributes zero.
VoteFrame averages those counts equally across all retained levels. It
drops the slope and its associated grouped smooth when the average is
below 10. An average of exactly 10 qualifies. This checks the number of
respondents providing within-group comparisons and uses exact values to
count distinct slope values. The fit log records dropped terms under
`sparse_random_slopes` and the corresponding averages under
`sparse_random_slope_mean_support`. The hierarchy rule removes any
dependent higher-order terms. Within a surviving grouped spline, only
levels whose linear random block survived get fitted spline deviations.
Screened respondents still inform the global curve and other applicable
terms. At prediction, each screened or unseen level gets a deviation
curve drawn from the fitted spline term's variance, shared by all its
cells within a draw. The area random intercept and its
interactions with demographic factors are eligible, as are each area
predictor's overall effect and its varying slopes and smooths by demographic
groups, subject to depth and spline settings. See [Goplerud, supplement
§A.4](https://mgoplerud.com/papers/Goplerud_MMM_Full.pdf#page=19) for the
factor-by-curve specification.

Each stage starts from the requested depth and selects the highest allowed
depth at which the number of eligible respondents is at least the
estimated number of retained coefficients after screening. If every candidate exceeds that limit,
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
This is a pre-fit coefficient count after support, collinearity, and hierarchy screening. It
gives no discount for shrinkage and excludes variance and covariance
hyperparameters.

The fitter warns with the selected depth for each stage. It writes a CSV
in the working directory by default. Supply `log_file` to choose its path.
The CSV records nominal and retained estimates for each attempted depth, the selected depth,
respondent and cell totals, and fit status. The returned object exposes the
log as `fit$fit_log` and its path as `fit$log_file`.

`fitted_depth` records the accepted candidate depth, including fractional
values. `retained_order` counts the predictors in the largest term that
survives screening. An overall intercept has order 0, a main effect or
area random intercept has order 1, and a two-predictor interaction has
order 2.

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
Without `new_fixed_sd`, prediction stops and lists the exact predictor names
and values that are missing from the fitted survey stage. Harmonise the frame
and survey categories or supply `new_fixed_sd` for an explicit
zero-centered normal prior on the log-odds scale. The new contrast is
relative to the fitted reference level, or the first fitted level for
coding with no reference level. Pass `allow_missing_levels = FALSE` to
reject new levels. For stage-level point predictions on the log-odds scale,
`predict_vglmer_levels(..., n_draws = 0)` uses the fitted coefficient means
and zero for new coefficients.

`predict_sequential_logit()` writes `posterior_probabilities.rds` and
`extended_frame.csv` to `data/output/` by default. Set `output_dir` to choose
another directory. A rerun replaces those files. The saved R object holds a
cell-by-outcome-by-draw probability array and the frame keys.
Use `write_files = FALSE` to return the same posterior in memory without
writing the files.
Use `batch_size` to limit how many rows enter each stage design at a time.
For multiple prediction calls with the same fit, pass the same initially
empty environment as `draw_state` to reuse fitted coefficient draws and
frame-only effects across batches. Use a separate environment per fit.
The CSV has one
row per frame cell and extension category. Its `N_draw_1`, `N_draw_2`, and subsequent
columns are draw-specific expected counts. `N` is their posterior mean.
The draw names use the supplied count column name as a prefix.

Aggregation multiplies the probabilities by the frame's population counts
and sums the result within each draw. Supply any combination of frame columns
in `by`, or use `by = character()` for totals by outcome. The original frame
can be reordered or subsetted because cell keys match it to the saved draws.
The collapsed frame keeps just the posterior mean expected count for each
cell and category. Those means add back to each original cell population.

## Checking and calibrating the extended frame

Use `check_extended_frame(extended_frame, targets, by, extension_variable)`
to compare expected counts aggregated as shares against target shares on
the 0–1 scale. Target rows are keyed by the grouping columns in `by`
and the extension category. Set `target_variable` to the target share
column and `plot_file` to save the identity-line panels as a PNG.
Pass `outcome_order = fit$outcome_order` to use the fitted sequence.
Pass `no_vote = fit$no_vote` for vote diagnostics. With this option, each
group's targets must include its population share for No Vote. The party
panels and Overall compare shares among voters, and the No Vote panel
uses shares of the population. Draw intervals use turnout in the same draw.
The function returns the matched estimates and pooled and outcome-level
bias, RMSE, Spearman correlation, and 90% interval coverage. A mean-only extended
frame can be checked with a warning that intervals and coverage are unavailable.

Use `rake_extended_frame(extended_frame, frame, targets, by, extension_variable)`
to balance each expected count column to the original joint frame and the
outcome targets within each group defined by `by`. The solver uses
`SQUAREM::squarem()` to accelerate the outcome-scaling update. Each
update scales the outcome weights toward their targets and normalizes
the weights within each original cell to its population. Supply target shares
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
[Denmark vignette](vignettes/vignette_denmark_2026.md#measure-how-raking-changes-the-estimates)
explains how to read this figure.

Use `rake_with_diagnostics()` to run the same rake and generate the
target, original-joint, and unconstrained-margin plots automatically.
It returns the raked frame and the diagnostics.

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
