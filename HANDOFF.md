# Denmark sequential extension handoff

The Denmark vignette is complete through the single-extension fit, posterior
prediction, raking, and diagnostics for 2022 reported vote choice. The
next task is to build a new Denmark frame that includes 2026 parliamentary
election results and use it to test two sequential extensions on actual
Denmark inputs.

## Current baseline

- [Denmark vignette](vignette_denmark_2026.Rmd) contains the runnable workflow,
  captured output, and a 44-row extract of the final raked CSV. Source the
  scripts in `R/` from the repository root.
- [Denmark input documentation](data/input/denmark%202026/README.md) describes
  the original frame and the 2022 results and survey recodes. The completed
  fit uses the original municipality `pop_pct` values as its historical
  vote-share predictors at every stage.
- All ten fitted stages converged with R 4.5.3, `vglmer` 1.0.8, and seed
  `2026`. The first nine selected depth 1 and `Blank vote` selected depth 0.
  `Other` receives the remaining probability.
- The original frame has 28,980 demographic cells and represents 4,163,940
  people in 98 municipalities. The extended and raked frames each have
  318,780 cell–outcome rows with `N` and 125 draws. The rake preserves the
  original joint cell and municipality totals in every count column.
- [The output directory](data/output/denmark-2026-fit/) contains `fit.rds`,
  `fit_log.csv`, `run_settings.rds`, `posterior_probabilities.rds`,
  `extended_frame.csv`, and the final `raked_extended_frame.csv`. It also
  holds the outcome-target, original-joint, and total-variation figures.
- [The README](README.md) documents the function interfaces. The single
  rake uses `SQUAREM`. `rake_with_diagnostics()` runs that rake and produces
  its three figures. The 2022 target also supplied the area predictor,
  so the target checks measure agreement and the post-rake checks verify
  the imposed constraints.

The 2022 survey has 2,823 rows, of which 2,602 are complete for fitting.
There are 64 complete respondents whose exact demographic cells are
absent from the frame. These respondents still contribute to fitting.
Fanø, Læsø, Samsø, and Ærø appear in the frame and have no recorded survey
respondents.

## Verified two-extension behavior

The two-extension workflow passed an end-to-end test with two real
`vglmer` fits on synthetic surveys. The test used shuffled draw columns
and two target sets grouped by different variables.

- `predict_sequential_logit()` pairs incoming `N_draw_i` with new
  probability draw `i`. It infers the number of draws, rejects an
  explicit mismatch, and computes the new `N` as the mean of the paired
  count products. A mean-only input uses `N` in every draw and produces
  125 draws by default or the explicitly requested number.
- `rake_joint_extended_frame()` takes the twice-extended frame, the
  **original** stratification frame, and a named target table for each
  extension. It enforces both outcome margins together and verifies them
  for each draw. It rakes the mean-count column independently.
- The actual two-election Denmark run is pending. The saved baseline
  `extended_frame.csv` is the starting point when reusing the first
  extension, so both outcomes can be raked jointly after prediction.

## Next session

1. Bring in the 2026 parliamentary results at municipality level. Document
   the source, municipality identifiers, outcome mappings, and denominator.
   Check coverage against the original frame, including the treatment of
   non-voters, blank votes, and any party categories that differ by year.
2. Identify the auxiliary respondent survey and its outcome coding for the
   2026 extension. The aggregate election results can supply raking targets.
   Estimating how the 2026 outcome varies by demographics requires the
   auxiliary survey.
3. Extend the frame first for `vote_choice_2022`, then for
   `vote_choice_2026`. Use separate output directories for the two
   predictions. Verify that summing each second-extension draw across the
   2026 outcomes recovers its incoming 2022-extended count.
4. Rake the twice-extended frame once against both elections' targets.
   Check the 2022 and 2026 outcome margins, the original demographic
   joint, and the municipality and national populations in every draw.
   Keep this run's outputs separate from the completed baseline.
