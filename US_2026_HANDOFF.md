# US 2026 frame extension handoff

## Resume the November 22 run

The user asked to checkpoint while the revised national vignette was
running. It was left active. First check
`/tmp/voteframe-us-relaxed-tolerance.b8jACG/run.log`,
`results/presidential_fit_log.csv`, `results/house_fit_log.csv` if present,
and `exit-status` in that run directory. Its memory trace is `memory.csv`.
The run directory is temporary and can disappear on workspace recycle.
The code checkpoint is commit `bfb3aff` on the local `package` branch.
If the run is still active, continue monitoring it. If it finished, use
its outputs and verify them before publishing. If the run directory is
gone or the fit exited without completing, restart with the code from
the checkpoint and the current vignette source.

- The vignette uses stock `vglmer` 1.0.8 with joint updates and verbose
  per-iteration diagnostics. VoteFrame's default stopping thresholds are
  a maximum parameter change below 10⁻³ or a positive ELBO gain below
  10⁻⁵. Explicit solver controls take precedence.
- Both vote extensions fit No Vote first, then Democratic and Republican,
  and leave Other as the residual. Stage-specific area covariates blank
  across every district are omitted. District and state retain random
  intercepts and are excluded from interactions. Eligible non-geographic
  interactions and splines are enabled with requested depth 2.
- The presidential No Vote fit used 57,603 respondents and 3,160 estimated
  coefficients. It converged at iteration 74 with an ELBO gain of about
  1.6 × 10⁻⁵ and a maximum parameter change of about 8 × 10⁻⁴.
- At 08:18 UTC, the presidential Democratic fit was active with 52,767
  respondents and 3,911 estimated coefficients. Iteration 14 reported
  an ELBO gain of 0.4111 and a maximum parameter change of 2,511.63. The
  run's sampled peak R memory was 3.41 GiB. Prediction and raking had
  yet to start.
- The completed No Vote fit is held in the active R process. This run
  does not serialize individual stage fits. A process exit or workspace
  recycle before vignette completion will require refitting the stages.
  If it completes, the Markdown and plots are under the run directory's
  `vignettes/` subdirectory and state partitions are under `results/raked/`.
  Verify all 51 partitions, 30 draws, and 7,960,800 rows before publishing.
- The temporary stock library was `/tmp/voteframe-profile/lib`, with
  `vglmer` 1.0.8, `gKRLS` 1.0.5, and `SQUAREM` 2026.1. The repository
  contains `MasterP()` for the general pipeline. The live US run uses
  the existing state-partitioned vignette workflow.

To restart after a workspace recycle, restore the stock R dependencies
if needed. From the repository root, create a scratch copy of the
vignette so the existing published Markdown and figures are preserved
until the new results are verified:

```sh
repo_dir="$PWD"
run_dir=$(mktemp -d /tmp/voteframe-us-resume.XXXXXX)
mkdir -p "$run_dir/vignettes"
ln -s "$repo_dir/R" "$run_dir/R"
ln -s "$repo_dir/data" "$run_dir/data"
cp vignettes/vignette_us_2026.Rmd "$run_dir/vignettes/"
cd "$run_dir"
VOTEFRAME_US_OUTPUT_DIR="$run_dir/results" Rscript -e '
knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(error = FALSE)
knitr::knit("vignettes/vignette_us_2026.Rmd", output = "vignettes/vignette_us_2026.md")
' > run.log 2>&1
```

Set `R_LIBS` for the restart if the dependencies are only in a temporary
library. The prior run used one BLAS thread and `R_GC_MEM_GROW=0`.

## Historical baseline

The completed national baseline uses depth 1 and 30 posterior draws. It
extends the CD120 population frame with 2024 presidential choice and then
with 2024 House choice conditional on presidential choice. A full depth-2
rerun with splines stopped during the first presidential fit after the
fixed-effects rank fix. Resume with that fit before running prediction or
raking again.

## Repository and result status

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

## Inputs and model specification

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

## Files and environment for resuming

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

## Where to resume

Start by choosing a larger-memory machine for the full specification or
profiling the first presidential Democratic fit to identify the largest
allocation. Profile the fitting stage before investing in the national
prediction and rake. The rank issue and the termination are distinct
findings. The broad question is whether sparse or lower-memory construction
can preserve the exact hierarchical design and spline bases.

Once the full first stage fits, run the vignette from the repository root
and direct its generated RDS partitions to a fresh output directory.
Render the Markdown to a temporary destination until it finishes. The
figure chunk writes its plots under `vignettes/assets/us-2026/`.

```sh
VOTEFRAME_US_OUTPUT_DIR=/path/to/us-2026-depth2-results Rscript -e '
knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(error = FALSE)
knitr::knit("vignettes/vignette_us_2026.Rmd", output = "/tmp/us-2026-depth2.md")
'
```

Check the selected stages and logged omissions, then confirm that all 51
partitions contain 30 draws and sum to 7,960,800 rows. Compare depth 2 with
the depth-1 baseline using the **pre-rake** turnout-conditional diagnostics
and, if evaluating predictive performance, held-out CES respondents.
The post-rake plots verify calibration to the supplied benchmarks. Publish
the refreshed Markdown and figures together after these checks.
