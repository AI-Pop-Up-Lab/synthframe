# SynthFrame

SynthFrame extends population stratification frames with categorical survey outcomes.

$$
\text{sequential multilevel fits} \longrightarrow
\text{outcome counts by population cell} \longrightarrow
\text{counts calibrated to known margins}
$$

After installing dependencies or editing code, run `Rscript --vanilla tests/integration.R`
from the repository root before a long run. It fits a small model, extends a
synthetic frame, and checks both raking targets and original population totals.
Install `data.table` to include the keyed-table input checks. SynthFrame accepts
both base data frames and `data.table` inputs for fitting, prediction and raking.
The Markdown reports in Table 1 can be read directly. Run their R Markdown
sources to generate results with the current code.

## Reproduce a case study

Run each case study from the repository root on `main`. The prepared inputs
are tracked under [`data/input/`](data/input/). Each input guide covers
sources, recodes, and any upstream steps needed to rebuild those inputs.

*Table 1. Case-study reports, runnable vignettes, and input guides.*

| Case study | Read the results | Run the source | Input guide |
| --- | --- | --- | --- |
| Denmark 2026 | [Vignette](vignettes/vignette_denmark_2026.md) | [R Markdown](vignettes/vignette_denmark_2026.Rmd) | [Denmark inputs](data/input/denmark%202026/README.md) |
| Sweden 2026 | [Vignette](vignettes/vignette_sweden_2026.md) | [R Markdown](vignettes/vignette_sweden_2026.Rmd) | [Sweden inputs](data/input/sweden%202026/README.md) |
| US 2026 | [Vignette](vignettes/vignette_us_2026.md) | [R Markdown](vignettes/vignette_us_2026.Rmd) | [US inputs](data/input/us%202026/README.md) |

The Denmark and Sweden reports use fresh fits. The US report uses the saved
fits and refreshed predictions and calibration. The raking-change figures
express each outcome, including nonvoting, as a share of the eligible population.

Install the R packages in your own R library. The figures use Cairo PNG
support, which you can check with `capabilities("cairo")`.

*Snippet 1. Install the R packages used by the vignettes.*

```r
install.packages(c("knitr", "vglmer", "gKRLS", "SQUAREM", "digest"), repos = "https://cloud.r-project.org")
```

The original published US run used R 4.3.3, vglmer 1.0.8, gKRLS 1.0.5, SQUAREM 2026.1,
knitr 1.52, and Matrix 1.6-5. Use a compatible release of Matrix if
recreating that R 4.3 environment. [CRAN Matrix archive](https://cran.r-project.org/src/contrib/Archive/Matrix/)

From the repository root, set `country` to one of `"denmark"`, `"sweden"`, or
`"us"` and run the block below. The chunks execute in order and write the
rendered Markdown beside the R Markdown source. For the US, the default is
a pre-fit report covering the inputs and model designs. On a
memory-limited Codespace, read the completed national report and import the
saved outputs. To run the full US analysis on a machine with sufficient
memory and disk space, set `SYNTHFRAME_US_FIT=true` before running Snippet 2.

*Snippet 2. Render a case study from the repository root.*

```r
country <- "denmark"
vignette <- file.path("vignettes", paste0("vignette_", country, "_2026"))
prefit <- country == "us" && !identical(Sys.getenv("SYNTHFRAME_US_FIT"), "true")
report <- paste0(vignette, if (prefit) "_prefit.md" else ".md")
knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(error = FALSE)
knitr::knit(paste0(vignette, ".Rmd"), output = report)
```

### Generated outputs

Full runs use `data/output/<country> 2026/` for the files below. The US
pre-fit run writes the report and design figures before fitting begins.

*Table 2. Files written to each case study’s output directory.*

| Directory | Contents |
| --- | --- |
| `models/` | Fitted models as RDS files |
| `diagnostics/` | Fit and screening CSVs, benchmark metrics, and original-cell checks |
| `logs/` | Directory for render logs or recovery notes you choose to save |
| `raked_frame/` | Calibrated counts for the mean and all 125 draws |

*Table 3. Raked-frame partitions and calibration margins by case study.*

| Case | Raked partitions | Calibration |
| --- | --- | --- |
| Denmark | One file, `raked_frame/part-01.rds` | 2022 municipal shares of the electoral roll |
| Sweden | One per geographic batch | 2022 municipal shares of the electoral roll |
| US | By default, one per state and DC, about 6.5 GiB nationwide. A batch-size override can group whole states. | Joint calibration to 2024 state-level presidential and district-level House margins, accounting for redistricting. [Redistricting methods](https://github.com/robertocerinaprojects/polling-without-people) |

Denmark and Sweden also use these shares as predictors. `No Vote` is eligible
people who cast no ballot. `Invalid` includes blank and other rejected ballots
in official returns. In benchmark plots with `no_vote` set,
$s_{\mathrm{No\ Vote}}=N_{\mathrm{No\ Vote}}/N$
and $s_{\mathrm{party}}=N_{\mathrm{party}}/(N-N_{\mathrm{No\ Vote}})$.
The raking-change plots use the whole eligible population for every choice.

Each render updates the figures it produces under the matching
`vignettes/assets/` directory, including when you change the data output directory.
Set `SYNTHFRAME_PRE_RAKE_STORAGE=keep` to retain `extended_frame/` partitions and
`SYNTHFRAME_US_RAKED_DIR` to put the US raked frame on another disk.

The [2026 case study output release](https://github.com/AI-Pop-Up-Lab/synthframe/releases/tag/case-study-outputs-2026-11-27)
provides precomputed model and frame RDS files. Extract the country archives
from the repository root to restore their `data/output/` paths. The
[file manifest](data/output/case-study-outputs-2026-11-27.csv) lists sizes and
SHA-256 checksums. Fresh runs generate their own output files from the tracked inputs.

### Reuse completed US fits

The US run saves each completed model under `data/output/us 2026/models/` as
`pres_fit.rds` or `house_fit.rds`. To reuse a fit on a later full run, set
both `SYNTHFRAME_US_FIT=true` and `SYNTHFRAME_US_REUSE_FITS=true` before
knitting. Each model is checked against
its inputs, settings and fitting code. A missing, unreadable, unverified or
mismatched fit triggers a fresh fit for that model. Set `SYNTHFRAME_US_OUTPUT_DIR`
to use another run directory or `SYNTHFRAME_US_RAKE=false` to render only the
pre-rake benchmark comparison. When transferring a completed run, copy the
diagnostics alongside the models. Each saved fit contains its stage log in
`fit$fit_log` and its producer versions in `fit$us_fit_runtime`. The stored
`fit$log_file` path points to the machine where the model was fitted.

## Fitting the extension variable

### Run the full pipeline with `MasterP()`

Source the files in `R/` and pass a named list of extension specifications in
the order they should be added to the mother frame. Each name becomes a new
outcome column. An extension can use one auxiliary survey or pool a list of
surveys by stacking respondents. The selected columns must have harmonized
names and coding across surveys. Later fits can use previous outcomes as
predictors when their surveys contain those outcomes.

*Snippet 3. Fit two extensions with MasterP, jointly rake their margins, and return population summaries.*

```r
result <- MasterP(                                        # Fit, extend, and jointly rake the frame.
  mother_frame,                                           # Original population cells and their counts.
  extensions = list(                                      # Add outcome columns in the order listed.
    presidential = list(                                  # Choose the first outcome column name.
      survey = presidential_survey,                       # Survey with a matching presidential column.
      fit = list(                                         # Specify the presidential model.
        area_variable = "district_id",                     # Match area covariates by district.
        no_vote = "No Vote",                               # Model nonvoting before party choice.
        depth = 2,                                        # Request interactions up to order two.
        no_interactions = c("district_id", "state_fips"),   # Keep these predictors out of interactions.
        area_covariates = presidential_covariates           # Supply district covariates for the vote stages.
      ),                                                  # End the presidential fit options.
      predict = list(batch_size = 4000L),                   # Predict in batches of up to 4,000 cells.
      rake = list(state = list(                           # Name the presidential calibration margin.
        targets = presidential_state_targets,              # Supply the presidential targets by state.
        by = "state_fips"                                  # Calibrate separately in each state.
      )),                                                 # End the presidential raking options.
      plots = list(district = list(                        # Compare predictions with district benchmarks.
        targets = presidential_district_benchmarks,        # Population shares, including No Vote.
        by = "district_id",                                # Compare within each district.
        after_file = "presidential_district.png"             # Save the after-rake comparison plot.
      ))                                                  # End the presidential benchmark options.
    ),                                                    # End the first extension.
    house = list(                                         # Choose the second outcome column name.
      survey = list(house_survey_1, house_survey_2),         # Pool two surveys with matching House columns.
      fit = list(                                         # Specify the House model.
        area_variable = "district_id",                     # Match area covariates by district.
        no_vote = "No Vote",                               # Use the same nonvoter label for House.
        depth = 2,                                        # Request interactions up to order two.
        no_interactions = c("district_id", "state_fips"),   # Keep these predictors out of interactions.
        area_covariates = house_covariates                  # Supply district covariates for the vote stages.
      ),                                                  # End the House fit options.
      rake = list(district = list(                         # Name the House calibration margin.
        targets = house_district_targets,                  # Supply the House targets by district.
        by = "district_id"                                 # Calibrate separately in each district.
      ))                                                  # End the House raking options.
    )                                                     # End the second extension.
  ),                                                      # End the ordered list of extensions.
  aggregate_by = list(                                    # Name and define the summaries to return.
    state_presidential = c("state_fips", "presidential"),   # Presidential counts by state and choice.
    national_joint = c("presidential", "house"),            # National counts by both vote choices.
    national_total = character()                          # Sum all cells into one national total.
  )                                                       # End the summary definitions.
)                                                         # Run MasterP and store the result.

result$frame                                              # Inspect the full calibrated frame.
result$aggregates$state_presidential                      # Inspect the presidential summary by state.
```

*Table 4. MasterP settings, progress reporting, and returned objects.*

| Setting or output | Meaning |
| --- | --- |
| Progress | Current step and completed fit stages, prediction batches, and raking batches/draws. ETA starts after the first unit and updates with observed pace. |
| `verbose = TRUE` | Show solver iterations and every progress update in the console. |
| `log_file` / `result$log_file` | Choose / inspect the log path. Default is timestamped in `data/output/masterp_logs/`. Includes detailed trace, warnings, and failures. |
| `fit` | Predictors, outcome order, nonvoter label, splines, solver controls, and stage area covariates. Default predictors are the survey–input-frame column intersection. |
| `register_frame = TRUE` | Register the extension's population levels during fitting. |
| `predict` | Row batching and unseen-level options. Predictions stay in memory by default. For `write_files = TRUE`, give each exporting extension its own `output_dir`. |
| `rake` | One or more named margins per extension, calibrated jointly. `by` groups original frame columns, with `character()` for national. Use the extension name for the outcome column, `target_variable` to override `target`, and `target_scale = "count"` for counts compatible with population totals in every draw. |
| `plots` | Optional named independent benchmark comparisons at the supplied grouping. Returns before/after diagnostics. Set `before_file` / `after_file` to save plots. |
| `result$frame` / `result$aggregates` | Full calibrated frame / sums of mean and draw counts at requested levels. |
| `result$fit_info` / `keep_fits = TRUE` | Fit details and stage CSVs / retain model objects too. |

Numbered incoming population draws determine `n_draws` and pair with prediction
draws by index. With a mean-only mother frame, $D=125$ by default and every
draw starts from $N$. Calibration preserves each original cell total separately
for $N$ and every draw.

**Storage.** Benchmark checks retain the pre-rake frame in RAM or on disk.
MasterP's `pre_rake_mode = "keep"` and `pre_rake_dir` save the joint pre-rake frame
as `extended_frame.rds`. Mode `"temporary"` stores it on disk after joint raking
and removes it at exit. MasterP requires the full frame in RAM for joint raking.
Each vignette holds the active prediction or raking block in RAM. Denmark
also combines all of its raked blocks in memory to write a single file.

*Table 5. Vignette settings for pre-rake storage and geographic batching.*

| Vignette setting | Behavior |
| --- | --- |
| `SYNTHFRAME_PRE_RAKE_STORAGE` | `memory`, `temporary`, `keep`, or `auto` (default). Auto compares estimated size to available RAM, including visible Linux container limits. It uses temporary disk when RAM is tight or unknown. |
| `SYNTHFRAME_PRE_RAKE_DIR` | Override storage location. Defaults are R's temporary directory or `extended_frame/` in the output directory for kept files. Disk modes check estimated free space where `df` is available. |
| `SYNTHFRAME_PARENT_AREA` / `SYNTHFRAME_AREAS_PER_BATCH` | Choose a parent column / a fixed number of lower-level areas per batch. Default without a usable parent is 10. Denmark and Sweden batch by constituency, the US by state or DC. |

**Batching.** MasterP and `rake_joint_extended_frame()` accept `area_variable`,
`parent_area_variable`, and `areas_per_batch`. MasterP infers the lower-level area
from the fitted extensions and batches calibration after extending the full frame.
Each margin must fit wholly inside one batch. A statewide margin can require a
whole state, and crossing margins with no safe geographic grouping use one batch.

### Fit one extension directly

Load the R functions from the repository root with `vglmer` installed.
This example uses conditional historical shares and the default support and
stopping settings. The Denmark vignette uses each party's share of the
electoral roll, sets `minimum_support = 10L`, disables ELBO stopping, and caps
each stage at 100 iterations.

*Snippet 4. Fit a single sequential-logit extension using the Denmark inputs.*

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

**Stage order.** The standalone fitter defaults to all survey columns except the
outcome. Select columns with `predictors`. Area is always categorical. Use factors
for other numeric-coded categories. The fitter warns with the number of rows
removed for missing or empty outcomes or selected predictors before aggregation.

Order outcomes by frequency across recorded outcomes (ties by label), or supply
the full `outcome_order`, including the last outcome. If specified, `no_vote`
moves first and preserves the relative order of the others. For cell $`x`$, outcome
$`k`$, and $`K`$ ordered outcomes, fit $`K-1`$ stages. The successes are $`n_{xk}`$
and the failures are $`\sum_{j>k} n_{xj}`$:

```math
q_k(x)=P(Y=k\mid Y\ge k,x),\qquad q_K(x)=1.
```

```math
p_k(x)=q_k(x)\prod_{j=1}^{k-1}(1-q_j(x)),\qquad
\sum_{k=1}^{K}p_k(x)=1.
```

**Area covariates.** How you supply the data determines whether the pipeline
rescales it. The fitter does not infer this from column names or values.

**Conditional shares.** Use `process_area_covariates()` for counts or shares
that describe a distribution over the modeled outcomes, such as past vote
shares. For example, with `No Vote` first, the first stage uses the share of
eligible people who did not vote. The next stage uses the first party's share
among voters. Each subsequent stage excludes the earlier outcomes.

The helper accepts a CSV path, data frame, or named list of either, with common
area, outcome, and value column names and the same areas in every dataset. Supply
at most one row per area and outcome. Rows match by ID and can be reordered.
List names become predictor names. A single unnamed input uses `area_covariate`.
Each dataset must contain every modeled outcome somewhere.
For value $`v_{ak}`$ in area $`a`$ and stage $`k`$, the covariate is

```math
x_{ak}=\frac{v_{ak}}{\sum_{j\ge k}v_{aj}},
\qquad v_{ak}=0\text{ for missing pairs},\qquad 0/0\mapsto0.
```

Counts and shares with a common within-area denominator give the same result.
Leave `outcome_order` unset to let the fitter compute these shares in its final
stage order. Supply it to inspect the stage matrices immediately. When passed
directly to the fitter, the helper's result retains the original totals so the
fitter can recompute the denominators if the order changes, including when
`no_vote` moves first.

**Original scales.** For predictors like median income or population density,
pass a plain named list of matrices or data frames to `area_covariates`
(`fit$area_covariates` in MasterP). Name each entry for a fitted outcome, each row
for its area ID, and each column for its predictor. The fitter joins by area ID
and uses the values as supplied. Columns and values can vary by stage. Use data
frames for categorical predictors. To reuse one table at every fitted stage,
use `setNames(rep(list(area_predictors), length(stages)), stages)`. To combine
both types, compute the conditional shares in the final stage order, including
`no_vote` first when specified. Build a new plain list of stage tables by joining
those shares to the original-scale predictors by area ID.

*Table 6. Model terms available at each candidate depth.*

| Candidate depth | Design |
| --- | --- |
| $`0`$ | Intercept and area random intercept |
| $`0.5`$ | Add other categorical random intercepts |
| $`0.75`$ | Add linear fixed area covariates |
| $`1`$ | Main effects, with splines enabled by default |
| Integer $`d>1`$ | Terms with at most $`d`$ distinct predictors |

`no_interactions` limits named survey or area predictors to main effects,
including eligible splines, at every candidate depth before counting coefficients.
Categorical terms use random intercepts. Crosses with continuous effects use
group-specific random slopes. Area intercepts can interact with demographic factors.
Area covariates can have overall effects and group-specific slopes or smooths,
subject to depth and spline settings.

Truncated linear splines add global curves and grouped deviations. Each `vglmer`
factor-by-curve term includes a global spline. Joint continuous smooths require
`gKRLS`, whose sketch uses R's random seed. Numeric predictors with fewer than
four distinct values use linear main effects. `splines = FALSE` uses linear
effects and interactions. [Goplerud, supplement §A.4](https://mgoplerud.com/papers/Goplerud_MMM_Full.pdf#page=19)

#### Ensuring a well-behaved fitting process: handling design flaws

1. **Estimability and support.** Build each stage from its eligible respondents.
   Drop constants and crossed random groupings identical to an immediate parent
   (e.g. district × state when district determines state). The fit log lists
   these groups under `redundant_random_groups`. Keep independent unpenalized
   fixed columns, including when a product duplicates a main effect, and warn
   and log omitted predictors and columns at the selected stage. The rank
   check uses the unpenalized fixed design. Removing a parent term can also
   remove dependent interactions and splines under the hierarchy rule below.
   For `minimum_support = 10L`, require $`s_j=\sum_i t_i\mathbf{1}(X_{ij}\ne0)\ge10`$
   for fixed columns and random-intercept levels, where $`t_i`$ is the aggregated
   row's binomial trial count. The intercept is protected. `-1L` (default) and
   `0L` disable this screen. Screened fixed columns contribute zero at prediction.
   A random-effect group needs at least two supported levels. When fewer
   remain, drop its intercept, slopes and grouped spline deviations. In a retained
   group, screened levels use the fitted covariance at prediction. Drop smooths involving a
   screened fixed main effect. Keep eligible survey rows and surviving penalized
   spline bases. The log names dropped smooths and estimated coefficient counts.
2. **Within-group slopes.** For each retained level $`g`$, let $`s_g`$ count eligible
   respondents whose actual slope value (including products) differs from the
   level's most frequent exact value, weighted by trial counts. Constant levels
   give $`s_g=0`$. Drop the slope and grouped smooth if
   $`\sum_g s_g/|G|<10`$. Logs record `sparse_random_slopes` and
   `sparse_random_slope_mean_support`. Only retained linear-block levels get fitted
   spline deviations. Screened respondents still inform the global curve and
   other applicable terms. At prediction a screened or unseen level shares a
   new deviation drawn from the fitted spline variance across all its cells in a draw.
3. **Collinearity after support.** Let $`W`$ be diagonal with $`W_{ii}=\sqrt{t_i}`$. Scale the
   supported fixed design to $`Z_j=WX_j/\lVert WX_j\rVert_2`$. For descending singular
   values, $`\kappa_j=\sigma_1/\sigma_j`$ and $`\kappa=\max_j\kappa_j`$.
   While $`\kappa\ge30`$, take the weakest right singular vector $`v`$ and consider
   non-intercept columns with $`|v_j|\ge0.01\max_l|v_l|`$. Among candidates of the
   highest interaction order, remove the one giving the lowest $`\kappa`$ after
   hierarchy pruning. Ties remove the later formula column. The rule uses
   predictor variation alone. Belsley, Kuh, and Welsch use about 30 alongside
   variance-decomposition proportions for linear regression. SynthFrame uses 30
   as a logistic pre-fit pruning choice and computes condition indices alone.
   [SAS/STAT guide](https://support.sas.com/documentation/cdl/en/sgug/59902/HTML/default/fit_sect29.htm)
4. **Hierarchy throughout.** For an interaction with predictor set $`S`$, retain
   every immediate parent $`S\setminus\{j\}`$, $`j\in S`$, recursively. This applies
   to fixed products, crossed intercepts, varying slopes, and their splines.
   If a parent is fully screened, drop its dependent terms. Fit an otherwise
   eligible global spline on its own if its grouped term is dropped.

Before solving, log fixed support and collinearity failures, hierarchy removals,
and condition numbers before and after pruning. In each newly fitted stage,
`stage$formula` is the formula supplied to the solver. Use `stage$candidate_formula`
to inspect the design at the selected depth before random-effect and spline
screening. [Supplementary material](https://static.cambridge.org/content/id/urn%3Acambridge.org%3Aid%3Aarticle%3AS0003055423000035/resource/name/S0003055423000035sup001.pdf)

**Depth selection.** For each stage with $`n`$ eligible respondents, select
$`d^*=\max\{d\in H:C_{\mathrm{retained}}(d)\le n\}`$.
Here $`H`$ contains candidate depths at or below the requested depth that
retain a random effect or spline. Each respondent supplies one trial, including
respondents in shared cells. If every eligible candidate exceeds the budget,
warn and attempt depth 0 when its area intercept is estimable. Otherwise the
stage reports a failure. Later stages can select higher depths if their design
shrinks enough.

```math
C_{\mathrm{retained}}=
\#\text{fixed columns}+\#\text{random intercepts and slopes across levels}
+\#\text{spline basis columns}.
```

Count after support, collinearity, and hierarchy screening, without shrinkage
discounts or variance/covariance hyperparameters. A truncated linear basis uses
$`\lfloor\min(\#\text{distinct values}/4,35)\rfloor`$ columns. Joint kernels cap the
default sketch width at the aggregated cell count. Each by-smooth counts a global
basis and one per observed group.

**Solver.** SynthFrame fits every stage with `vglmer`. An area random intercept
requires at least two observed areas. If a stage has no eligible random effect
or spline after screening, fitting stops and records the reason in the stage
log. SynthFrame defaults accept
$`\max|\Delta\theta|<0.01`$ for tracked variational parameters or
$`0<\Delta\mathrm{ELBO}<10^{-5}`$. A supplied `vglmer_control()` has its own defaults.
The fitter's `tolerance_parameters` and `tolerance_elbo` override the corresponding
control values. Zero disables that criterion. A positive `tolerance_rel_elbo`
in `vglmer_control()` also enables relative ELBO stopping. Set `max_iterations`
to cap each stage. At the cap the solver can return with a non-convergence message.

*Table 7. Fit audit options and the files they produce.*

| Audit option | Output |
| --- | --- |
| `interaction_support_log` | CSV of nonzero stage respondents by outcome for categorical interactions and varying slopes, including within-group slope variation and counts differing from the modal value. |
| `depth_support_dir` | Pre-fit PNG per stage. Linear/log axes show cumulative shares of columns by nonzero observations. Solid lines count aggregated rows, dashed lines count respondents. Lower panels break the respondent curve into scalar/correlated random effects, smooths, and fixed effects. Each shaded area uses all coefficients at that depth. Integer requests show depths 1–3 and distinct requested/selected depths. Rejected candidates can be expensive, especially with splines. |
| `log_file` | Fit CSV path (working directory by default), also in `fit$log_file`. Each stage records its outcome, status and any error. Completed stages include the selected depth, candidate and retained coefficient counts, respondents and cells. A stage failure leaves earlier stages in the CSV. |

Set these fit and screening options in the extension's `fit` list with MasterP.
`fitted_depth` is the accepted candidate, including fractional depths.
`retained_order` is the largest surviving term's predictor count
(intercept $`0`$, main effect or area intercept $`1`$, two-way interaction $`2`$).

## Predicting and extending the frame

*Snippet 5. Draw posterior probabilities and summarize expected counts at two levels of detail.*

```r
set.seed(123)
posterior <- predict_sequential_logit(fit, n_draws = 125)
by_municipality_age <- aggregate_extended_frame(
  frame, posterior, by = c("municipality", "age_group")
)
collapsed <- collapse_extended_frame(frame, posterior)
```

**Population levels.** Pass `frame` to the fitter to register population levels,
including observed combinations for crossed effects. Each stage's `frame_levels`
lists fitted, frame, and frame-only coefficient names. The log counts frame-only
coefficients. Prediction defaults to the saved frame and accepts a replacement
or a frame supplied for the first time. Supply the survey predictors used by
the retained stages and the area ID when a retained predictor needs an area join.

Frame-only levels have zero survey trials. The fit integrates their coefficients
out and the depth rule counts only survey-estimated coefficients. For stage
log odds $\eta_k^{(d)}$, use $q_k^{(d)}=1/(1+\exp(-\eta_k^{(d)}))$
in the stage formula above. For each draw and random term, sample covariance
$\Sigma^{(d)}$ from its fitted inverse-Wishart distribution and share one
$\mathcal N(0,\Sigma^{(d)})$ coefficient vector across cells in each new level.
This covers intercepts, slopes, crossed groups, and group-specific splines.
A variance block is available only for effects in the fitted formula.
Spline prediction reuses fitted bases, including for new numeric values.

**New fixed levels.** Unseen fixed contrasts lack fitted population variances.
Prediction stops and lists missing predictor names and values unless you
harmonize categories or supply `new_fixed_sd`. If you set it to $`s`$,
the log-odds prior is $`\mathcal{N}(0,s^2)`$. Contrasts use the
fitted reference or the first fitted level when coding has no reference.
`allow_missing_levels = FALSE` rejects new levels.
`predict_vglmer_levels(..., n_draws = 0)` gives stage log odds using fitted
coefficient means and zero for new coefficients.

**Exports and batching.** `predict_sequential_logit()` replaces
`posterior_probabilities.rds` (cell × outcome × draw probabilities and frame keys)
and `extended_frame.csv` under `data/output/`. Override with `output_dir` or use
`write_files = FALSE` for the same posterior in memory. An export that fails
while publishing the CSV rolls back the posterior file. When importing the
CSV, declare identifier columns as character to preserve leading zeros and set
`check.names = FALSE` to retain the original column names.
`batch_size` limits how many cells enter each design-matrix calculation. The
posterior array still holds all cells supplied in one call. To limit its size,
predict on partitions of the frame. Pass the same initially empty `draw_state`
environment to each call with the same fit, draw count, and `new_fixed_sd` so
fitted coefficients and frame-only effects reuse their draws across partitions.

**Counts.** For cell $x$, outcome $k$, and draw $d$, the CSV stores one row
per $(x,k)$ with

$$
N_{xk}^{(d)}=N_x^{(d)}p_k^{(d)}(x),
\qquad \bar N_{xk}=D^{-1}\sum_{d=1}^D N_{xk}^{(d)},
\qquad \sum_k \bar N_{xk}=D^{-1}\sum_d N_x^{(d)}.
$$

Columns use `N_draw_1`, `N_draw_2`, … with posterior mean `N` (or the supplied
count prefix). Aggregate by summing counts within each draw over any frame
columns in `by`, or use `character()` for totals by outcome. Keys allow frame
subsets and reordering. The collapsed frame keeps only mean counts per cell
and category. They sum to the original count for a mean-only frame and the mean
of the incoming draw counts for a paired frame.

## Checking and calibrating the extended frame

*Table 8. Functions for checking targets, original cell totals, and changes from raking.*

| Function | Purpose |
| --- | --- |
| `check_extended_frame(extended_frame, targets, by, extension_variable)` | Compare aggregate shares with targets in $[0,1]$, keyed by `by` and outcome. Set `target_variable`, `plot_file` for identity-line PNGs, and `outcome_order = fit$outcome_order` for the fitted order. Returns matched estimates, pooled and outcome-level bias, RMSE, Spearman correlation, and 90% coverage. Mean-only inputs warn that intervals and coverage are unavailable. |
| `check_post_rake()` | Check outcome targets and original joint-cell populations. Returns both comparisons and can save separate identity-line plots. |
| `plot_raking_changes()` | Compare aligned extended and raked frames for margins left unconstrained by a municipality rake. [Denmark vignette](vignettes/vignette_denmark_2026.md#measure-how-raking-changes-the-estimates) |
| `rake_with_diagnostics()` | Run the same rake and return the raked frame plus target, original-joint, and unconstrained-margin diagnostics and plots. |

For benchmark comparisons with `check_extended_frame()` or `check_post_rake()`,
set `no_vote = fit$no_vote` to show party and Overall panels as shares among voters.
Supply benchmark shares on the eligible-population scale, including the No Vote
share for each group with a party target. Draw intervals use turnout from the same
draw. Each group's supplied benchmark shares may sum to at most one. A complete
set of outcomes must sum to one. The numerical tolerance for these checks is
`sqrt(.Machine$double.eps)` (about $`1.5\times10^{-8}`$).

The “what changes with raking” plots use the whole eligible population for
every outcome. For example, take 100 eligible people. If nonvoters rise from
40 to 60 and two parties each fall from 30 to 20, the three panels show +20,
−10 and −10 percentage points. Total variation is half the sum of the absolute
changes, or 20 percentage points in this example.

**Raking.** Use `rake_extended_frame(extended_frame, frame, targets, by, extension_variable)`.
For every mean or draw column, let $N_x$ be the corresponding original cell
count and $T_{gk}$ the target expressed as a count. The raked counts $R_{xk}$ satisfy

$$
\sum_k R_{xk}=N_x,\qquad \sum_{x\in g}R_{xk}=T_{gk}
\quad\text{for each supplied target in group }g.
$$

`SQUAREM::squarem()` accelerates outcome scaling toward targets and normalization
within each cell. Supply at least one target per group as shares, or counts with
`target_scale = "count"`. Untargeted outcomes divide the remainder. Keys match
by value. Missing combinations and zero counts stay zero. Geographic batching
uses the outcomes observed across the whole frame. A zero target for a known
outcome can be met in a group with no rows for that outcome. A positive target
requires positive support.

Some feasible sparse targets force an initially positive count to exactly zero.
For example, consider ten cells numbered 1 to 10, with population $`10i`$ in
cell $`i`$. Cell 1 supports only outcome 1. Each later cell splits its seed
equally between outcomes $`i-1`$ and $`i`$. Set the count target for outcome
$`i`$ to $`10i`$. The unique solution assigns every cell entirely to its own
outcome. At the default settings both solvers report a failure to match the targets.

Mean and draw columns are raked independently. With incoming population draws,
both solvers pair columns by draw number. For the original-cell plot, let
$N_x^{\mathrm{input}}$ be the value in the original frame's count column
(`N` by default). The 90% interval uses the 5th and 95th percentiles of
the following values across draws.

$$
\text{plotted count}^{(d)}_x=N_x^{\mathrm{input}}+\left(\sum_k R^{(d)}_{xk}-N_x^{(d)}\right).
$$

For example, if the original `N` is 100 and two population draws are 80
and 160, a rake that preserves both draw totals places both interval limits at 100.

### Extending a frame twice

Use the first extended frame as the second fit's prediction frame. Incoming
`N_draw_1`, `N_draw_2`, … set the default draw count and any explicit `n_draws`
must match. Apply the same count formula above using the incoming count for draw
$d$ or $N_x^{(d)}=N_x$ when only `N` is supplied. Save each prediction in its own
output directory.

After the second extension, rake to both outcome margins in one call with
the **original** stratification frame. Supply target tables named for the
extension variables. Use a shared `by` vector or name a grouping vector for
each target table.

*Snippet 6. Predict a second outcome and jointly calibrate both outcomes against the original frame.*

```r
second <- predict_sequential_logit(
  second_fit, frame = first_extended, write_files = FALSE
)
twice_extended <- aggregate_extended_frame(first_extended, second, by = names(second$cells))
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
