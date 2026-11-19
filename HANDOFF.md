# Denmark 2026 validation handoff

The next task is extensive validation of the Denmark 2026 case study, followed
by adding a second set of area covariates and measuring what it changes.
The current area predictor is the municipality's conditional share from the
2022 election. The additional covariates and their source are still to be
chosen.

## Starting point

- [Project README](README.md) has the end-to-end R example, the depth rule, and
  the output formats. Source the scripts in `R/` from the repository root.
- [Denmark inputs](data/input/denmark%202026/README.md) documents the raw sources
  and recodes. Run `python3 data/input/build_denmark_2026.py` to rebuild all three
  inputs under the country's `processing/rebuilt/` directory.
- `election_results.csv` has `vote_choice_2022`, `municipality`, and `pop_pct`.
  The election year is 2022. The rebuild produced the same 1,078 election
  values under the updated schema and reproduced the survey and frame data.
- The short checks used `vglmer` 1.0.8. If the temporary environment is still
  present, its R executable is `/tmp/voteframe-r/bin/Rscript`. Joint smooths
  across multiple continuous predictors also require `gKRLS`.

The survey has 2,823 rows. With the four Denmark predictors, 2,602 complete
rows form 2,088 distinct cells and 221 rows are dropped. The frame has 28,980
cells and covers 98 municipalities. The survey has recorded respondents in
94. Fanø, Læsø, Samsø, and Ærø have no recorded respondents.

The deferred harmonisation issue concerns 67 respondents across 62
combinations whose individual municipality, gender, age, and education
labels exist in the frame but whose exact combinations are absent. Of those
respondents, 64 have a recorded vote choice. Keep track of these survey-only
combinations when interpreting validation results.

## Validate the existing case study

1. Run the README fit with `frame = frame`, `no_vote = "Did not vote"`, and
   requested depth 2. Save the fit object and CSV fit log alongside the run's
   outputs. Record R and package versions, the seed, and the fitting controls.
2. Check convergence stage by stage using the fitted `vglmer` diagnostics. A
   stage with status `returned` in the CSV has a model object. Check its
   convergence separately. Review the selected depths, eligible respondent
   counts, estimated coefficients, and frame-only coefficient counts. In the
   short baseline check, the first nine stages selected depth 1. The final
   fitted stage, `Blank vote`, selected depth 0 with 39 respondents and 30
   estimated coefficients. Depth 0 fits a global intercept and an area
   random intercept.
3. Check predictive performance on held-out respondents. Hold out complete
   cells and municipalities in separate checks to exercise prediction for
   unseen interactions and areas. Compare category calibration and log loss
   using the joint outcome probabilities. Inspect rare choices separately.
   Keep the fitting and held-out data separate when computing outcome order
   and selecting depth.
4. Generate the baseline exports with 125 draws, then verify the full output.
   Probabilities should sum to one per cell and draw. Counts should sum back
   to each original cell population per draw. Check national and
   municipality-by-age totals against direct sums from the draw export and
   verify that the collapsed frame preserves population totals. A full
   Denmark integration check with three draws produced 318,780 extended
   rows and passed the probability and population checks. That run used
   short fits. Full convergence and held-out accuracy remain open.
5. Check frame-only effects explicitly. Each stage's `frame_levels` records
   the fitted and frame coefficient names. A frame-only random effect is
   centered at zero on the log-odds scale and gets draws from its fitted
   term's covariance posterior. Cells sharing a level reuse its draw.
   Fixed contrasts with no survey information require an explicit
   `new_fixed_sd`. A term absent from the fitted formula has no covariance
   to sample.

## Add the second area covariate set

Decide the variables, source, reference period, units, and municipality
mapping before fitting. Check coverage against all 98 frame municipalities,
including the four without survey respondents. Record any recoding and
missing values in the Denmark input documentation and make the source
processing reproducible.

Use `process_area_covariates()` with a named list if the new inputs are
area-by-outcome values that should be normalized over the remaining outcome
categories at each stage. Each input must cover the same areas. Its list
name becomes the predictor name. For municipality-level predictors intended
to remain in their original units, join them by municipality to the
historical-share matrices. Pass the resulting named list of stage matrices
to the fitter. A stage matrix uses municipality IDs as row names.

Fit the expanded model with the same validation splits as the baseline.
Compare selected depths alongside predictive performance because additional
covariates and their interactions count toward the per-stage coefficient
limit. Keep baseline and expanded outputs in separate directories.
