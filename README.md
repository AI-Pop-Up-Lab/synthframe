# VoteFrame

VoteFrame extends population stratification frames with categorical survey outcomes.

$$
\text{sequential multilevel fits} \longrightarrow
\text{outcome counts by population cell} \longrightarrow
\text{counts calibrated to known margins}
$$

After installing dependencies or editing code, run `Rscript --vanilla tests/integration.R`
from the repository root before a long run. It fits a small model, extends a
synthetic frame, and checks both raking targets and original population totals.
Skip it if you're reading the vignettes. Run the relevant vignette to reproduce published results.

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
rendered Markdown beside the R Markdown source. For the US, obtain the saved
fits and place them in the run directory as described below before running
this block.

```r
country <- "denmark"
vignette <- file.path("vignettes", paste0("vignette_", country, "_2026"))
if (country == "us") {
  run_dir <- Sys.getenv("VOTEFRAME_US_OUTPUT_DIR", unset = "data/output/us 2026")
  fits <- file.path(run_dir, "models", c("pres_fit.rds", "house_fit.rds"))
  if (!all(file.exists(fits))) {
    stop("Place both saved US fits in ", file.path(run_dir, "models"), " before knitting.")
  }
  Sys.setenv(VOTEFRAME_US_FIT = "true", VOTEFRAME_US_REUSE_FITS = "true")
}
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

| Case | Raked partitions | Calibration |
| --- | --- | --- |
| Denmark | One file, `raked_frame/part-01.rds` | 2022 municipal shares of the electoral roll |
| Sweden | One per geographic batch | 2022 municipal shares of the electoral roll |
| US | One per state and DC, about 6.5 GiB nationwide | State presidential and district House margins, jointly |

Denmark and Sweden also use these shares as predictors. `No Vote` is eligible
people who cast no ballot. `Invalid` includes blank and other rejected ballots
in official returns. In comparisons, $s_{\mathrm{No\ Vote}}=N_{\mathrm{No\ Vote}}/N$
and $s_{\mathrm{party}}=N_{\mathrm{party}}/(N-N_{\mathrm{No\ Vote}})$.

Figures are under the matching `vignettes/assets/` directory.
Set `VOTEFRAME_PRE_RAKE_STORAGE=keep` to retain `extended_frame/` partitions and
`VOTEFRAME_US_RAKED_DIR` to put the US raked frame on another disk.

### Reuse completed US fits

Reuse the saved presidential and House fits for the published inputs and
model settings. Budget roughly 12 hours or more just for model fitting on
a GitHub Codespace with 16 GB RAM and 32 GB of storage. Prediction and
raking take additional time.

The fit files are stored outside Git. Obtain copies of `pres_fit.rds` and
`house_fit.rds` and place them in `data/output/us 2026/models/`. If you set
`VOTEFRAME_US_OUTPUT_DIR`, use the `models/` folder in that run directory.
The example above checks for both files and enables reuse. When knitting
directly with `VOTEFRAME_US_REUSE_FITS=true`, a missing file triggers a fresh
fit for that model. Set `VOTEFRAME_US_REUSE_FITS=false` for an intentional
refit of both models. Set `VOTEFRAME_US_RAKE=false` to render only the
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
result <- MasterP(                                        # Fit, extend, and jointly rake the frame.
  mother_frame,                                           # Original population cells and their counts.
  extensions = list(                                      # Add outcome columns in the order listed.
    presidential = list(                                  # Choose the first outcome column name.
      survey = presidential_survey,                       # Survey with a matching presidential column.
      fit = list(                                         # Specify the presidential model.
        area_variable = "district_id",                     # Match area covariates by district.
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
        targets = presidential_district_benchmarks,        # Supply the benchmark shares.
        by = "district_id",                                # Compare within each district.
        after_file = "presidential_district.png"             # Save the after-rake comparison plot.
      ))                                                  # End the presidential benchmark options.
    ),                                                    # End the first extension.
    house = list(                                         # Choose the second outcome column name.
      survey = list(house_survey_1, house_survey_2),         # Pool two surveys with matching House columns.
      fit = list(                                         # Specify the House model.
        area_variable = "district_id",                     # Match area covariates by district.
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
Vignettes require the active prediction or raking block in RAM.

| Vignette setting | Behavior |
| --- | --- |
| `VOTEFRAME_PRE_RAKE_STORAGE` | `memory`, `temporary`, `keep`, or `auto` (default). Auto compares estimated size to available RAM, including visible Linux container limits. It uses temporary disk when RAM is tight or unknown. |
| `VOTEFRAME_PRE_RAKE_DIR` | Override storage location. Defaults are R's temporary directory or `extended_frame/` in the output directory for kept files. Disk modes check estimated free space where `df` is available. |
| `VOTEFRAME_PARENT_AREA` / `VOTEFRAME_AREAS_PER_BATCH` | Choose a parent column / a fixed number of lower-level areas per batch. Default without a usable parent is 10. Denmark and Sweden batch by constituency, the US by state or DC. |

**Batching.** MasterP and `rake_joint_extended_frame()` accept `area_variable`,
`parent_area_variable`, and `areas_per_batch`. MasterP infers the lower-level area
from the fitted extensions and batches calibration after extending the full frame.
Each margin must fit wholly inside one batch. A statewide margin can require a
whole state, and crossing margins with no safe geographic grouping use one batch.

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

**Stage order.** The standalone fitter defaults to all survey columns except the
outcome. Select columns with `predictors`. Area is always categorical. Use factors
for other numeric-coded categories. The fitter warns with the number of rows
removed for missing or empty outcomes or selected predictors before aggregation.

Order outcomes by frequency across recorded outcomes (ties by label), or supply
the full `outcome_order`, including the last outcome. If specified, `no_vote`
moves first and preserves the relative order of the others. For cell $x$, outcome
$k$, and $K$ ordered outcomes, fit $K-1$ stages. The successes are $n_{xk}$
and the failures are $\sum_{j>k} n_{xj}$:

$$
q_k(x)=P(Y=k\mid Y\ge k,x),\qquad
p_k(x)=q_k(x)\prod_{j<k}(1-q_j(x)),\qquad
q_K(x)=1,\qquad \sum_{k=1}^{K}p_k(x)=1.
$$

**Area covariates.** Custom inputs are named lists of matrices or data frames
indexed by stage outcome, area row name, and predictor column name. Columns and
values may vary by stage. Use data frames for categorical predictors. For the
same table at every fitted stage, use
`setNames(rep(list(area_predictors), length(stages)), stages)`.

The helper accepts a CSV path, data frame, or named list of either, with common
area, outcome, and value column names and the same areas in every dataset. Supply
at most one row per area and outcome. Rows match by ID and can be reordered.
List names become predictor names. A single unnamed input uses `area_covariate`.
For value $v_{ak}$ in area $a$ and stage $k$, the covariate is

$$
x_{ak}=\frac{v_{ak}}{\sum_{j\ge k}v_{aj}},
\qquad v_{ak}=0\text{ for missing pairs},\qquad 0/0\mapsto0.
$$

Counts and shares with a common within-area denominator give the same result.
Pass `outcome_order` to return the stage matrices directly or let the fitter supply
its order. Use custom stage matrices to preserve original scales.

| Candidate depth | Design |
| --- | --- |
| $0$ | Intercept and area random intercept |
| $0.5$ | Add other categorical random intercepts |
| $0.75$ | Add linear fixed area covariates |
| $1$ | Main effects, with splines enabled by default |
| Integer $d>1$ | Terms with at most $d$ distinct predictors |

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

**Design screening.**

1. **Estimability and support.** Build each stage from its eligible respondents.
   Drop constants and crossed random groupings identical to an immediate parent
   (e.g. district × state when district determines state). The fit log lists
   these groups under `redundant_random_groups`. Keep independent unpenalized
   fixed columns, including when a product duplicates a main effect, and warn
   and log omitted predictors and columns at the selected stage. This rank
   check leaves hierarchical interaction and spline bases intact.
   For `minimum_support = 10L`, require $s_j=\sum_i t_i\mathbf{1}(X_{ij}\ne0)\ge10$
   for fixed columns and random-intercept levels, where $t_i$ is the aggregated
   row's binomial trial count. The intercept is protected. `-1L` (default) and
   `0L` disable this screen. Screened fixed columns contribute zero at prediction.
   Screened levels use the fitted term covariance. If all levels are screened,
   drop the random term and its grouped spline. Drop smooths involving a
   screened fixed main effect. Keep eligible survey rows and surviving penalized
   spline bases. The log names dropped smooths and estimated coefficient counts.
2. **Within-group slopes.** For each retained level $g$, let $s_g$ count eligible
   respondents whose actual slope value (including products) differs from the
   level's most frequent exact value, weighted by trial counts. Constant levels
   give $s_g=0$. Drop the slope and grouped smooth if
   $\sum_g s_g/|G|<10$. Logs record `sparse_random_slopes` and
   `sparse_random_slope_mean_support`. Only retained linear-block levels get fitted
   spline deviations. Screened respondents still inform the global curve and
   other applicable terms. At prediction a screened or unseen level shares a
   new deviation drawn from the fitted spline variance across all its cells in a draw.
3. **Collinearity after support.** With $W=\operatorname{diag}(\sqrt{t_i})$, scale the
   supported fixed design to $Z_j=WX_j/\lVert WX_j\rVert_2$. For descending singular
   values, $\kappa_j=\sigma_1/\sigma_j$ and $\kappa=\max_j\kappa_j$.
   While $\kappa\ge30$, take the weakest right singular vector $v$ and consider
   non-intercept columns with $|v_j|\ge0.01\max_l|v_l|$. Among candidates of the
   highest interaction order, remove the one giving the lowest $\kappa$ after
   hierarchy pruning. Ties remove the later formula column. The rule uses
   predictor variation alone. Belsley, Kuh, and Welsch use about 30 alongside
   variance-decomposition proportions for linear regression. VoteFrame uses 30
   as a logistic pre-fit pruning choice and computes condition indices alone.
   [SAS/STAT guide](https://support.sas.com/documentation/cdl/en/sgug/59902/HTML/default/fit_sect29.htm)
4. **Hierarchy throughout.** For an interaction with predictor set $S$, retain
   every immediate parent $S\setminus\{j\}$, $j\in S$, recursively. This applies
   to fixed products, crossed intercepts, varying slopes, and their splines.
   If a parent is fully screened, drop its dependent terms. Fit an otherwise
   eligible global spline on its own if its grouped term is dropped.

Before solving, log fixed support and collinearity failures, hierarchy removals,
and condition numbers before and after pruning. Each fitted stage retains formula,
data, and model fit. [Supplementary material](https://static.cambridge.org/content/id/urn%3Acambridge.org%3Aid%3Aarticle%3AS0003055423000035/resource/name/S0003055423000035sup001.pdf)

**Depth selection.** For each stage with $n$ eligible respondents, select
$d^*=\max\{d\le d_{\mathrm{requested}}:C_{\mathrm{retained}}(d)\le n\}$.
Each respondent supplies one trial, including shared cells. If the set is empty,
warn and attempt depth 0. Later stages can select higher depths if their design
shrinks enough.

$$
C_{\mathrm{retained}}=
\#\text{fixed columns}+\#\text{random intercepts and slopes across levels}
+\#\text{spline basis columns}.
$$

Count after support, collinearity, and hierarchy screening, without shrinkage
discounts or variance/covariance hyperparameters. A truncated linear basis uses
$\lfloor\min(\#\text{distinct values}/4,35)\rfloor$ columns. Joint kernels cap the
default sketch width at the aggregated cell count. Each by-smooth counts a global
basis and one per observed group.

**Solver.** `vglmer` requires a random effect or spline and at least two observed
areas for an area random intercept. Ineligible stages stop with a logged reason.
If screening leaves only fixed effects, fit a binomial GLM and sample fixed
coefficients from its normal approximation. VoteFrame defaults accept
$\max|\Delta\theta|<0.01$ for tracked variational parameters or
$0<\Delta\mathrm{ELBO}<10^{-5}$. A supplied `vglmer_control()` has its own defaults.
Override with `tolerance_parameters` or `tolerance_elbo` (zero disables that
criterion) and `max_iterations` per stage. At the cap the solver can return with
a non-convergence message.

| Audit option | Output |
| --- | --- |
| `interaction_support_log` | CSV of nonzero stage respondents by outcome for categorical interactions and varying slopes, including within-group slope variation and counts differing from the modal value. |
| `depth_support_dir` | Pre-fit PNG per stage. Linear/log axes show cumulative shares of columns by nonzero observations. Solid lines count aggregated rows, dashed lines count respondents. Lower panels break the respondent curve into scalar/correlated random effects, smooths, and fixed effects. Each shaded area uses all coefficients at that depth. Integer requests show depths 1–3 and distinct requested/selected depths. Rejected candidates can be expensive, especially with splines. |
| `log_file` | Fit CSV path (working directory by default), also in `fit$log_file`. `fit$fit_log` records nominal and retained counts for each attempted depth, selected depth, respondents, cells, and fit status. The fitter warns with the selected depth for each stage. |

Set these fit and screening options in the extension's `fit` list with MasterP.
`fitted_depth` is the accepted candidate, including fractional depths.
`retained_order` is the largest surviving term's predictor count
(intercept $0$, main effect or area intercept $1$, two-way interaction $2$).

## Predicting and extending the frame

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
or a frame supplied for the first time.

Frame-only levels have zero survey trials. The fit integrates their coefficients
out and the depth rule counts only survey-estimated coefficients. For stage
log odds $\eta_k^{(d)}$, use $q_k^{(d)}=\operatorname{logit}^{-1}(\eta_k^{(d)})$
in the stage formula above. For each draw and random term, sample covariance
$\Sigma^{(d)}$ from its fitted inverse-Wishart distribution and share one
$\mathcal N(0,\Sigma^{(d)})$ coefficient vector across cells in each new level.
This covers intercepts, slopes, crossed groups, and group-specific splines.
A variance block is available only for effects in the fitted formula.
Spline prediction reuses fitted bases, including for new numeric values.

**New fixed levels.** Unseen fixed contrasts lack fitted population variances.
Prediction stops and lists missing predictor names and values unless you
harmonize categories or supply `new_fixed_sd`, giving a
$\mathcal N(0,\texttt{new\_fixed\_sd}^{\,2})$ log-odds prior. Contrasts use the
fitted reference or the first fitted level when coding has no reference.
`allow_missing_levels = FALSE` rejects new levels.
`predict_vglmer_levels(..., n_draws = 0)` gives stage log odds using fitted
coefficient means and zero for new coefficients.

**Exports and batching.** `predict_sequential_logit()` replaces
`posterior_probabilities.rds` (cell × outcome × draw probabilities and frame keys)
and `extended_frame.csv` under `data/output/`. Override with `output_dir` or use
`write_files = FALSE` for the same posterior in memory.
`batch_size` limits rows per stage design. Share one initially empty
`draw_state` environment across calls with the same fit to reuse fitted draws
and frame-only effects. Use a separate environment per fit.

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

| Function | Purpose |
| --- | --- |
| `check_extended_frame(extended_frame, targets, by, extension_variable)` | Compare aggregate shares with targets in $[0,1]$, keyed by `by` and outcome. Set `target_variable`, `plot_file` for identity-line PNGs, and `outcome_order = fit$outcome_order` for the fitted order. Returns matched estimates, pooled and outcome-level bias, RMSE, Spearman correlation, and 90% coverage. Mean-only inputs warn that intervals and coverage are unavailable. |
| `check_post_rake()` | Check outcome targets and original joint-cell populations. Returns both comparisons and can save separate identity-line plots. |
| `plot_raking_changes()` | Compare aligned extended and raked frames for margins left unconstrained by a municipality rake. [Denmark vignette](vignettes/vignette_denmark_2026.md#measure-how-raking-changes-the-estimates) |
| `rake_with_diagnostics()` | Run the same rake and return the raked frame plus target, original-joint, and unconstrained-margin diagnostics and plots. |

For vote diagnostics, set `no_vote = fit$no_vote` and supply the No Vote population
share in every group. Party and Overall panels use voter shares, while the No Vote
panel uses population shares. Draw intervals use turnout from the same draw.

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
by value. Missing combinations and zero counts stay zero. Unreachable margins
report the group and count column.

Mean and draw columns are raked independently. With incoming population draws,
both solvers pair columns by draw number. The original-cell plot centers draw
errors on the original mean so the plot measures agreement with the paired
population even when that population varies across draws:

$$
\text{plotted count}^{(d)}_x=\bar N_x+\left(\sum_k R^{(d)}_{xk}-N_x^{(d)}\right).
$$

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
