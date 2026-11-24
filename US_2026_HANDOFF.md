# US 2026 frame extension handoff

## Resume the current national fit

Start with [US_2026_RESUME.md](US_2026_RESUME.md). It shows which models
have saved checkpoints and gives the commands to resume fitting and run
the 125-draw pre-rake comparisons. The [run log](data/output/us-2026-national-b0bb47d/run.log)
and fit checkpoints are in `data/output/us-2026-national-b0bb47d/`
on this Codespace's workspace disk. The fit started from commit
`b0bb47d` using joint updates and a parameter-change tolerance of 0.01.

All three presidential stages converged. No Vote used 27 iterations
(maximum parameter change 0.00694), Democratic 47 (0.00691), and
Republican 80 (0.00461). The complete model is saved as
`data/output/us-2026-national-b0bb47d/pres_fit.rds`. House No Vote then
started at depth 2 with 55,413 respondents and 14,943 estimated retained
coefficients. At 15:56 UTC on November 24, 2026, House No Vote had
completed two iterations and had no saved House stage yet. The largest
parameter change at iteration 2 was 353.30, with an ELBO increase of
1,172.86. The process was left running until the user stops the
Codespace. Check the saved log for any later progress. Presidential
scoring is pending. The presidential log
contains a solver warning that it corrected a negative determinant in a
parameter-expansion matrix at iteration 1. All three saved stages still
met the tolerance. Use the live checkpoint table and log for subsequent
progress.

1. Reopen the same Codespace and check the log before starting a process.
   If the fit is still advancing, keep monitoring it. The user requested
   an update after every completed iteration.
2. If the fit stopped, follow the restart command in
   [US_2026_RESUME.md](US_2026_RESUME.md). Once a full model is marked
   **Ready**, the script loads that model and skips its fit. It restarts
   an incomplete model from its first stage.
3. Once both full models are present, run the 125-draw pre-rake
   comparison from the same note. Verify that all 51 state and DC
   partitions are represented and that the stage logs meet the 0.01
   parameter-change tolerance before reporting convergence. Raking is
   disabled in this run.

The sections below preserve older experiments and results. Their run
paths, draw counts, and solver settings are historical.

## Earlier presidential screening benchmark

Both 10,000-person pilot fits finished. The [rendered vignette](vignettes/vignette_us_2026.md)
audits the screened national designs and stops before fitting by default.
The candidate design includes `on_ballot` in both party stages so the
pre-fit rules determine what survives. The metrics below are from the
earlier scalar-screening pilot.

The pilot sampled 10,000 of the 57,603 complete CES presidential
respondents using seed `20261124`. The remaining 47,603 were used for
held-out multiclass log-loss. Both variants fit the same training rows
with stock `vglmer` 1.0.8, cyclical updates, parameter-change tolerance
0.01, and ELBO stopping disabled. Pilot depths were 2, 2, and 1 for No Vote,
Democratic, and Republican. Depth 2 was forced for the first two stages to
exercise the interactions on this sample. All six fits reached tolerance.
The pre-rake comparison used 30 draws and the full frame of 50 states plus
DC. Fit timings are wall-clock seconds from separate single-threaded R
processes running concurrently.

| Stage | Ordinary iterations / seconds | Screened iterations / seconds | Columns removed |
| --- | ---: | ---: | ---: |
| No Vote | 34 / 312.490 | 29 / 272.474 | 9,104 |
| Democratic | 54 / 539.568 | 46 / 429.319 | 9,047 |
| Republican | 10 / 3.229 | 14 / 4.277 | 242 |
| Total | 98 / 855.287 | 89 / 706.070 | 18,393 |

Screening saved 149.217 seconds (17.4%) in this run. Held-out log-loss was
0.947866 ordinary and 0.951027 screened. Each entry below gives ordinary
then screened. Bias and RMSE are in percentage points. Coverage is for
the 90% intervals. The election benchmarks also inform area predictors.

| Outcome | Bias | RMSE | Spearman | Coverage |
| --- | ---: | ---: | ---: | ---: |
| Overall party shares | 0.00 / 0.00 | 7.81 / 7.64 | 0.844 / 0.837 | 24.2% / 30.7% |
| No Vote | −25.26 / −24.52 | 25.62 / 24.89 | 0.900 / 0.896 | 0.0% / 0.0% |
| Democratic | 7.30 / 6.50 | 8.71 / 8.44 | 0.921 / 0.917 | 35.3% / 49.0% |
| Republican | −9.09 / −8.55 | 10.15 / 9.92 | 0.932 / 0.922 | 17.6% / 29.4% |
| Other | 1.79 / 2.05 | 2.06 / 2.34 | 0.275 / 0.237 | 19.6% / 13.7% |

The prototype counts support in VoteFrame and encodes a screened scalar
term as `(0 + keep | fit_group)`. Screened rows keep their survey outcomes,
receive a placeholder retained group label, and have `keep = 0`.
Stock `vglmer` therefore builds only the retained columns. The synthetic
coefficient name has no fixed-effect counterpart for mean recentering.
For prediction, rebuild the original group labels and set `keep = 1`.
Screened levels then use the existing new-level variance draws. The
benchmark only screened scalar random levels. It did not exercise fully
screened terms, omitted fixed columns, or screened correlated levels
shared with a grouped smooth in the formula-based implementation.

The benchmark scripts, stage and iteration logs, predictions, plots, and
full metrics are in `/tmp/voteframe-10k-no-ballot.uhcwul`. This path can
disappear on workspace recycle. An Alabama profile with 30 draws spent
55% of prediction time building random-effect and spline design blocks
and about 9% in matrix multiplication. Investigate reusing basis values
for repeated area covariates and reducing design rebuilds across batches.
When loading a saved joint-spline fit in a fresh R process, load the
`gKRLS` namespace so its `Predict.matrix` method is registered.

## Fixed-effect near-collinearity

The pre-fit screen uses respondent-weighted, unit-norm fixed columns and a
condition-number cutoff of 30. It starts from the full supported fixed
design and removes columns from the weakest singular direction, preferring
to remove higher-order interactions. Each deletion also prunes dependent
terms. The stage log records affected columns, other columns in the same weak
direction, and conditioning before and after selection.

In the 10,000-person CES presidential pilot, `house_share` and
`house_share:on_ballot` formed the nearly dependent pair. Keep
`house_share` in this case. An early pilot omitted `on_ballot` and its
interactions, bringing the scaled Democratic condition number from 21.4
million to 81.7 on that sample. The current national vignette includes
`on_ballot` and relies on automatic pruning.

The earlier national design preflight with a cutoff of one million selected depth 2 for all six stages and
screened 41,729 random coefficient columns at minimum support 10. The
vignette specification screened zero fixed columns and zero smooths, with
scaled fixed-design condition numbers from 80.6 to 121.2. Restoring
`on_ballot` to the full presidential Democratic stage for a separate
design check reduced the condition number from about 1.41 × 10¹⁴ to
142.5 by dropping `house_share:on_ballot` and `incumbent:on_ballot`.
The main effect `house_share` was retained. This preflight constructed
the screened inputs without starting a national solver run.

With the cutoff at 30, strong hierarchy enabled, and `on_ballot` still
omitted, an earlier preflight selected depth 2 for all six stages. The retained fixed-design
condition numbers ranged from 18.46 to 28.97. Each stage dropped
`house_share:pres_share` and its joint spline. Both Republican stages
also dropped `pres_share:incumbent` and its joint spline. The depth rule
now uses the estimated retained coefficient count. All main effects
were retained. The subsequent within-group slope screen uses an average
of 10 informative respondents per retained level. All six national
designs dropped `pres_share:state_fips` and its grouped spline, with
average within-state support of zero. No other slope fell below 10 in
these designs.

## Minimum support for fitted coefficients

Measure support on the actual stage design, using aggregated binomial trial
counts to count eligible respondents with nonzero entries. The cutoff for
screening is fewer than 10 respondents, subject to the rules below. A column
with exactly 10 respondents meets the cutoff. Keep every survey row in the
likelihood. The US vignette passes `minimum_support = 10L` to both fits.
VoteFrame encodes screened terms in the formula and input columns before
calling stock `vglmer`. The stage fit logs record dropped columns by effect
type before the solver starts.

- A scalar random effect can use the existing frame-only draw from its
  term's fitted variance. For a correlated random intercept and slopes,
  use the intercept's support to decide whether to retain the whole level.
  If the intercept has at least 10 respondents, retain its slopes even
  when their own support falls below 10. If the intercept has fewer than
  10, omit the whole level and use the existing joint frame-only draw.
  A wholly unsupported random term is omitted from fitting and prediction. The diagnostic's columnwise support counts still show
  sparse slopes and do not imply those slopes will be removed.
- Omit a fixed-effect column below the cutoff from both fitting and
  prediction. Its contribution is zero and it requires no prediction
  prior. Apply this to individual design columns for terms with multiple
  contrasts. Ensure the engine does not implicitly add a screened column
  back when constructing a formula's fixed design.
- Drop a grouped spline if every level of its companion random term is
  screened. Also drop any spline using a predictor whose fixed main effect
  is screened. Otherwise keep all spline basis columns produced by the existing
  basis construction and rank handling, including columns below the cutoff. This
  preserves the original spline basis and smoothing penalty. Keep fitted
  knots, transforms, and any kernel sketch for population prediction.
  Continue reporting spline support in the diagnostic. Within a retained
  grouped smooth, fit deviations only for the levels whose linear random
  block survived. Restore screened group labels for prediction so they
  receive deviation draws from the fitted smooth variance.
- Apply the screen before solver updates. Keep the original identities of
  screened random levels and cache their prediction draws by stage, term,
  level, and draw across batches and state partitions.
- Preserve original grouping labels and spline encodings for prediction.
  Fully screened random terms contribute zero. All six national stage designs retain at least one level per
  random term at the vignette cutoff.
- Report eligible rows and respondents, the screened counts by term, and
  retained versus total column counts for each candidate depth. The
  depth rule uses the retained coefficient estimates, and the log also
  reports the nominal counts. Check runtime, convergence, held-out
  predictions, and pre-rake uncertainty. Thresholding creates a hard
  cutoff and can discard real local signal. The screen also depends on the
  chosen contrasts and spline basis, and a nonzero count alone does not
  measure variation or diagnose separation.

### November 24 screened run

The joint and cyclical national runs were interrupted after 5 and 59
iterations of presidential No Vote, respectively. Both used the older
matrix-interception screen. Its recentering step can shift a fixed
intercept without a compensating random-effect shift for respondents in
screened levels. This was the reason to stop the cyclical run and test
formula-based screening. The logs and diagnostics are in
`/tmp/voteframe-screened.s21DCE` and
`/tmp/voteframe-screened-cyclical.Y7ZKFV`.
The installed R dependencies are under `/tmp/voteframe-profile/lib`.
The national No Vote fit log confirms that screening removed 5,237 scalar
random-effect columns and kept the fixed, correlated, and spline columns.
The three-state execution check is in
`/tmp/voteframe-screened-pilot.Th1cqh`. It uses two iterations and three
draws to exercise the workflow on Alabama, California, and Minnesota.
Pre-rake party-share RMSE was 5.59 percentage points for president and
10.80 for House. No Vote RMSE was 25.51 and 24.67 points, respectively.
These are pilot scores from fits run to a two-iteration cap.
The vignette now defaults to pre-rake benchmark comparisons and saves
their metrics as CSVs. Set `VOTEFRAME_US_RAKE=true` for the optional
raked partitions and post-rake checks. A benchmark-only three-state pilot
using this source is in `/tmp/voteframe-pre-rake-pilot.XE6L4R`.

## November 22 run (historical)

This earlier depth-2 experiment was launched from `bfb3aff`. Its
temporary directory was `/tmp/voteframe-us-relaxed-tolerance.b8jACG`.
Presidential No Vote converged at iteration 74 with 3,160 estimated
coefficients. Presidential Democratic was still running when that
checkpoint was written. The experiment did not save completed stages
separately, so its in-process results cannot be resumed from checkpoint
files. The current run above uses a different screened design and saves
each completed stage and model.

## Historical baseline

The completed national baseline uses depth 1 and 30 posterior draws. It
extends the CD120 population frame with 2024 presidential choice and then
with 2024 House choice conditional on presidential choice. A full depth-2
retry with splines stopped during the first presidential fit after the
fixed-effects rank fix.

## Repository snapshot for the earlier depth-1 baseline

- The branch is `package`. Commit `4db4070` is on `origin/package`. It includes
  the US inputs, the eight-draw vignette and plots, the Denmark vignette
  move into `vignettes/`, and removal of `legacy/`.
- The working tree contains the completed 30-draw Markdown and updated
  plots, the source configured for full depth 2, the fixed-effects rank
  fix, and support for reusing posterior draws across prediction batches.
  These changes are local and uncommitted.
- The [rendered vignette](vignettes/vignette_us_2026.md) shows the verified
  depth-1 run, including all six fitted formulas, diagnostics, and a
  six-row preview with every demographic key and draws 1, 15, and 30.
  The [R Markdown source](vignettes/vignette_us_2026.Rmd) requests the
  depth-2 rerun. Its expected output therefore differs from the captured
  baseline until the new run succeeds.

## Inputs and model specification for the earlier baseline

The three original files in [the US input directory](data/input/us%202026/README.md)
come from upstream commit `b302fb476c710896f53f7583ac5528bf31ce2524`. The
input guide records the SHA-256 checksums and the distinction between
observed 2024 House returns and modeled results on redrawn CD120
districts. This is the nine-column election CSV release. The original
files retain their upstream filenames.

| Input | Role |
| --- | --- |
| `ces_2024_harmonized.rds` | 58,627 eligible survey respondents with both reported vote outcomes |
| `pums_cd_cells.rds` | 497,550 original PUMS cells representing about 240,960,972 citizen adults |
| `district_election_inputs_cd120.csv` | 1,744 district-by-choice rows across 436 districts including DC |

All US-specific preparation lives in the vignette. Preserve the two FIPS
codes as text and join on their composite district ID. Cell keys include
district and age, gender, race, Hispanic ethnicity, and education.
The first extension produces 1,990,200 rows. The second produces 7,960,800.
The eventual use is prediction of 2026 House vote. Conditioning 2024
House choice on 2024 presidential choice lets the frame carry their
association forward.

Both fits use the order Democratic, Republican, Other, then No Vote.
Each of the three fitted stages gets the party's district House share,
statewide presidential share, district lineup, ballot presence, and
incumbency. No Vote receives the residual probability. Its ballot and
incumbency fields are blank. The House fit also includes each CES
respondent's 2024 presidential choice. In the population frame, matching
presidential and House draw indices are multiplied to obtain joint counts.

The general fitter accepts a named list of area-by-predictor tables.
Each stage can have its own columns, or several stages can reuse one
table. The interaction and spline specification follows the existing
builder, including crossed random effects, varying slopes, truncated
linear splines, factor-by-curve terms, and the implementation's joint
smooths. Keep these terms enabled for the requested rerun. [Goplerud supplement, pp. 9–10](https://static.cambridge.org/content/id/urn:cambridge.org:id:article:S0003055423000035/resource/name/S0003055423000035sup001.pdf#page=9)

## Calibration and completed results

The three joint constraints are the original district-by-demographic
PUMS count per cell, 2024 House vote shares in every district, and 2024
presidential vote shares in every state. Each vote share sums over the
other vote choice. The mean-count column and all 30 draws are raked
separately. States are computational partitions because all district and
cell constraints are nested inside states.

The comparison plots can use independent target tables and geography for
each vote outcome. Party estimates and targets are conditional on their
own turnout. The Overall panel pools party comparisons. No Vote retains
its population denominator. Draw intervals divide by each draw's own
turnout. Calibration itself uses the original population-share targets.

| 30-draw depth-1 result | Value |
| --- | ---: |
| Pre-rake party RMSE, presidential / House | 6.50 / 7.70 percentage points |
| Largest original-cell count error after raking | 7.28 × 10⁻¹² people |
| Mean / median posterior count variance across joint rows | 40.37 / 0.0129 people² |
| Average 90% interval width, district 2024 House shares | Effectively zero |
| Average 90% interval width, district presidential shares | 2.41 percentage points |
| Average 90% interval width, district joint presidential–House shares | 0.69 percentage points |

The intervals use the 5th and 95th percentiles across 30 draws and average
equally across district–outcome combinations. House margins are fixed by
the rake. District presidential shares can vary because that margin is
fixed at the state level. These are historical frame diagnostics. The
effect on a 2026 forecast remains to be measured by passing the mean
frame and draw-specific frames through the same forecast model.

## Depth-2 failure and changes made

The design audit before rank reduction found that depth 2 met the
respondent-to-coefficient rule for the Democratic and Republican stages
in both fits. The Other stages selected depth 1, and all depth-3
candidates exceeded the stage respondent count.

1. The first full depth-2 spline fit was terminated before returning,
   with memory nearly exhausted. The first stage is presidential
   Democratic, with 57,603 eligible respondents in 42,019 cells.
2. A linear-only diagnostic retry failed during a matrix factorization
   with a non-positive leading minor. The user subsequently confirmed
   that the requested fit should retain the full interactions and splines.
3. The general rank fix now omits constant predictors before term
   construction and checks the unpenalized fixed design for linear
   dependence. It retains estimable columns, preserves surviving
   contrasts for partially aliased fixed factors, and records omissions
   in stage warnings and log columns. The first US stage dropped
   `on_ballot:incumbent`, leaving 10 fixed, 13,379 random, and 2,802 spline
   coefficients, for 16,191 total.
4. The full spline retry after rank reduction again ended with exit
   status 143 during that first stage. The fit log last recorded
   `started`. No fit or prediction was returned. The exit status alone
   does not identify the allocation or confirm an operating-system OOM
   kill. The retry was stopped at this point as requested.

Synthetic checks covered constants, duplicate predictors, dependent
interaction products, partially aliased factor contrasts, and retention
of spline terms. Prediction batching was checked on reused fitted and
frame-only effects. The national depth-2 run never reached prediction.

## Files and environment for the earlier depth-2 retries

| Path | Contents |
| --- | --- |
| [US vignette source](vignettes/vignette_us_2026.Rmd) | Inline input preparation, full spline fit, joint rake, diagnostic plots, and final preview |
| [Formula builder](R/build_sequential_formula.R) and [fitter](R/fit_sequential_logit.R) | Stage-specific rank check, warnings, and logged omissions |
| [Sequential prediction](R/predict_sequential_logit.R) and [effect draws](R/predict_vglmer_levels.R) | State and row batching with an independent shared draw environment for each fit |
| `/tmp/voteframe-us-2026-draw30/raked/` | 51 completed depth-1 state/DC RDS partitions, about 1.6 GiB total |
| `/tmp/voteframe-us-draw30-variance.log` | Draw variance and district interval summary |
| `/tmp/voteframe-us-depth2-rank.log` | Console log for the full spline retry |
| `/tmp/voteframe-us-depth2-rank-results/presidential_fit_log.csv` | First-stage design and omitted column from the stopped retry |

The files under `/tmp` are specific to this workspace and may disappear
when it is recycled. The run used R 4.3.3, `vglmer` 1.0.8, `gKRLS` 1.0.5,
`SQUAREM` 2026.1, and `knitr` 1.52. The temporary R package library is
`/tmp/voteframe-r-lib`. The machine reported about 7.8 GiB of RAM and no swap.
