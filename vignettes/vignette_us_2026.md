# Extending the US 2026 congressional district frame

The results below are from the completed 30-draw, depth-1 run. The R Markdown
source requests depth 2 with splines. The depth-2 retry stopped during the
first presidential fit and produced no replacement results.

The goal is a population frame for predicting the 2026 House vote. We first
estimate 2024 presidential vote in each demographic cell, then estimate
2024 House vote conditional on that presidential choice. Presidential vote
may help explain House voting among people with the same demographics.
Whether it improves predictions is an empirical question. The joint frame
carries both historical outcomes forward for a later 2026 model.

| Frame | Rows |
| --- | ---: |
| Original PUMS demographic cells | 497,550 |
| After adding the four presidential choices | 1,990,200 |
| After adding the four House choices within each presidential choice | 7,960,800 |

Raking keeps the joint row count, including combinations whose calibrated
counts are zero.

All three inputs use the 2026 district map (CD120), including DC. The
[US input guide](../data/input/us%202026/README.md) records the upstream
commit, checksums, and the rules for observed and modeled House returns.
Run the chunks from the repository root with `vglmer` and `SQUAREM` installed.

## 1. Read the survey, district data, and population frame

The CES auxiliary survey records both 2024 choices. The ACS PUMS mother
frame supplies citizen-adult population weights. The district-by-choice CSV
supplies area-level predictors and calibration targets. Its presidential
shares repeat within each state and choice, while House shares belong to
individual districts. Some House shares on the 2026 map come from upstream
modeled scenarios. Agreement with these benchmarks is a calibration check.


``` r
for (file in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(file)

country <- file.path("data", "input", "us 2026")
fips <- c("state_fips", "district_fips")
demographics <- c("age_cat", "gender_cat", "race_cat", "hispanic_cat", "educ_cat")
pres <- "vote_pres_2024"
house <- "vote_house_2024"
choices <- c("Democratic", "Republican", "Other", "No Vote")

survey <- readRDS(file.path(country, "ces_2024_harmonized.rds"))
frame <- readRDS(file.path(country, "pums_cd_cells.rds"))
election <- read.csv(file.path(country, "district_election_inputs_cd120.csv"),
                     colClasses = setNames(rep("character", length(fips)), fips),
                     na.strings = "")

# VoteFrame uses one area ID. A district number alone can repeat across states.
district_id <- function(data) {
  id <- paste(data$state_fips, data$district_fips, sep = "-")
  id[!complete.cases(data[fips])] <- NA_character_
  id
}
survey$district_id <- district_id(survey)
frame$district_id <- district_id(frame)
election$district_id <- district_id(election)

predictors <- c("district_id", demographics)
# The composite key retains district FIPS. Keep state FIPS for state margins.
frame <- frame[order(frame$state_fips), c("state_fips", predictors, "N")]

stopifnot(
  nrow(election) == 1744L, length(unique(election$district_id)) == 436L,
  !anyNA(election$district_id), !anyDuplicated(election[c("district_id", "vote_choice")]),
  all(grepl("^[0-9]{2}-[0-9]{2}$", election$district_id)),
  setequal(election$vote_choice, choices), "11-98" %in% election$district_id,
  setequal(frame$district_id, election$district_id),
  all(survey$district_id[!is.na(survey$district_id)] %in% election$district_id),
  all(vapply(survey[c(pres, house)], function(x) identical(levels(x), choices), logical(1))),
  all(is.na(election[election$vote_choice == "No Vote", c("on_ballot", "incumbent")]))
)
data.frame(election_rows = nrow(election), population_cells = nrow(frame),
           survey_respondents = nrow(survey), citizen_adults = round(sum(frame$N)))
```

```
##   election_rows population_cells survey_respondents citizen_adults
## 1          1744           497550              58627      240960972
```

## 2. Specify stage predictors and targets

Each entry in a fit's area-covariate list corresponds to a choice. Each
entry is a matrix or data frame with district IDs as row names and
predictors as columns. Splitting the CSV by choice directly produces these
tables. In both fits, each party stage uses that choice's district House
share and statewide presidential share, the district's categorical
House lineup, and the choice's ballot presence and incumbency. Preserve
both shares on their original citizen-adult scale. Set the order to
Democratic, Republican, Other, then No Vote so all three party groups
get a fitted stage and No Vote receives the remaining probability.
Ballot presence and incumbency are undefined for No Vote.

An outcome such as five-category happiness can use the same area predictors
at every stage. Given an area-by-predictor table with area IDs as row names,
use `setNames(rep(list(area_predictors), length(choices)), choices)` with
the appropriate response labels. Replace individual entries when a stage
needs its own predictors.


``` r
party_election <- election[election$vote_choice != "No Vote", ]
party_rows <- split(party_election, party_election$vote_choice)
area_covariates <- lapply(party_rows, function(rows) {
  data.frame(house_share = rows[[house]], pres_share = rows[[pres]],
             lineup = factor(rows$lineup), on_ballot = rows$on_ballot,
             incumbent = rows$incumbent, row.names = rows$district_id)
})

pres_targets <- unique(data.frame(state_fips = election$state_fips,
                                 vote_pres_2024 = election$vote_choice,
                                 target = election[[pres]]))
house_targets <- data.frame(district_id = election$district_id,
                            vote_house_2024 = election$vote_choice,
                            target = election[[house]])
rake_targets <- setNames(list(pres_targets, house_targets), c(pres, house))
rake_by <- setNames(list("state_fips", "district_id"), c(pres, house))

# Each outcome can use a different comparison table and grouping for plots.
plot_targets <- setNames(list(
  list(data = pres_targets, by = "state_fips"),
  list(data = house_targets, by = "district_id")
), c(pres, house))

stopifnot(
  nrow(pres_targets) == 51L * length(choices),
  !anyDuplicated(pres_targets[c("state_fips", pres)]),
  all(is.finite(pres_targets$target) & pres_targets$target >= 0 & pres_targets$target <= 1),
  all(is.finite(house_targets$target) & house_targets$target >= 0 & house_targets$target <= 1),
  all(abs(rowsum(pres_targets$target, pres_targets$state_fips) - 1) < 1e-8),
  all(abs(rowsum(house_targets$target, house_targets$district_id) - 1) < 1e-8)
)
data.frame(outcome = names(rake_targets),
           target_rows = vapply(rake_targets, nrow, integer(1)))
```

```
##                         outcome target_rows
## vote_pres_2024   vote_pres_2024         204
## vote_house_2024 vote_house_2024        1744
```

The default plots use the same benchmarks as the rake. To compare either
outcome against a different benchmark, replace its entry in `plot_targets`
with a table keyed by the selected grouping columns and that outcome column.
Its numeric `target` column contains shares on the 0–1 scale. Changing
`plot_targets` leaves the calibration inputs in `rake_targets` independent.

## 3. Fit and extend with 2024 presidential vote

The presidential model uses district and the five demographic predictors.
Each fit uses its own complete cases. For a compact national run, this
vignette uses 30 posterior draws and depth 1. Increase the draw count
for analyses that need more precise posterior intervals. The second extension
pairs draw indices with the first.

### Scaling to more posterior draws

The number of frame rows is independent of the draw count. Each additional
draw adds a count column and another calibration solve per state. At 125
draws, the final joint frame has 995,100,000 draw-specific counts. Stored as
eight-byte numbers, those counts alone take about 8 GB uncompressed,
before cell keys, the mean column, intermediate arrays, or temporary copies.
CSV exports also take disk space and time to write and read. Here,
manageable means keeping peak RAM and scratch storage within the machine's
limits and fitting the prediction, raking, and export into the time
available for the run.

The vignette already partitions both predictions and raking by state
and keeps intermediate predictions in memory only for the current state.
To scale further, run both predictions and the joint rake in small batches
of draws within each state. Keep draw IDs paired across the two extensions
and reuse the same sampled coefficients across all applicable states for a
given draw. Accumulate the unraked joint mean across batches and rake it
once if you want the same mean-column definition used here. Store completed
partitions in a compressed format if the analysis needs the full draws.
For the diagnostic plots, keep only the state presidential and district
House totals per draw and calculate intervals from those totals.


``` r
out <- Sys.getenv("VOTEFRAME_US_OUTPUT_DIR",
                  unset = file.path("data", "output", "us-2026-fit"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
n_draws <- 30L
control <- vglmer::vglmer_control(quiet = TRUE)
set.seed(2026)

pres_fit <- fit_sequential_logit(
  survey, pres, "district_id", area_covariates, no_vote = "No Vote", outcome_order = choices,
  predictors = predictors, depth = 1, control = control, frame = frame,
  log_file = file.path(out, "presidential_fit_log.csv")
)
```

```
## Warning: Removed 1024 incomplete survey rows before aggregation.
```

```
## Stage 1/3 'Democratic' — preparing data and selecting depth.
```

```
## Stage 1/3 'Democratic' — fitting at depth 1 with 57603 eligible respondents and 523 estimated coefficients.
```

```
## Stage 1/3 'Democratic' — registering frame levels.
```

```
## Stage 1/3 'Democratic' — model returned (1 of 3 completed).
```

```
## Stage 2/3 'Republican' — preparing data and selecting depth.
```

```
## Stage 2/3 'Republican' — fitting at depth 1 with 28758 eligible respondents and 523 estimated coefficients.
```

```
## Stage 2/3 'Republican' — registering frame levels.
```

```
## Stage 2/3 'Republican' — model returned (2 of 3 completed).
```

```
## Stage 3/3 'Other' — preparing data and selecting depth.
```

```
## Stage 3/3 'Other' — fitting at depth 1 with 6696 eligible respondents and 523 estimated coefficients.
```

```
## Stage 3/3 'Other' — registering frame levels.
```

```
## Stage 3/3 'Other' — model returned (3 of 3 completed).
```

``` r
pres_fit$fit_log[c("outcome", "observations", "fitted_depth", "estimated_parameters")]
```

```
##      outcome observations fitted_depth estimated_parameters
## 1 Democratic        57603            1                  523
## 2 Republican        28758            1                  523
## 3      Other         6696            1                  523
```

``` r
for (stage in names(pres_fit$stages)) {
  cat(stage, "\n")
  cat(trimws(deparse(pres_fit$stages[[stage]]$formula), which = "right"), sep = "\n")
  cat("\n")
}
```

```
## Democratic
## cbind(.success, .failure) ~ 1 + house_share + pres_share + on_ballot +
##     incumbent + (1 | district_id) + (1 | age_cat) + (1 | gender_cat) +
##     (1 | race_cat) + (1 | hispanic_cat) + (1 | educ_cat) + (1 |
##     lineup) + v_s(house_share) + v_s(pres_share)
##
## Republican
## cbind(.success, .failure) ~ 1 + house_share + pres_share + on_ballot +
##     incumbent + (1 | district_id) + (1 | age_cat) + (1 | gender_cat) +
##     (1 | race_cat) + (1 | hispanic_cat) + (1 | educ_cat) + (1 |
##     lineup) + v_s(house_share) + v_s(pres_share)
##
## Other
## cbind(.success, .failure) ~ 1 + house_share + pres_share + on_ballot +
##     incumbent + (1 | district_id) + (1 | age_cat) + (1 | gender_cat) +
##     (1 | race_cat) + (1 | hispanic_cat) + (1 | educ_cat) + (1 |
##     lineup) + v_s(house_share) + v_s(pres_share)
```


``` r
predict_presidential_state <- function(original) {
  set.seed(2026)
  prediction <- predict_sequential_logit(
    pres_fit, frame = original, n_draws = n_draws, allow_missing_levels = FALSE,
    write_files = FALSE
  )
  aggregate_extended_frame(original, prediction, by = names(prediction$cells))
}
```

## 4. Fit and extend with 2024 House vote

The House model adds the reported presidential choice to the CES predictors.
In the population frame it uses the presidential categories created in the
first extension. Each conditional House probability multiplies the expected
population in its presidential category in the same draw. This produces
16 joint vote combinations per original cell. Fit on the national CES and
predict both outcomes one state at a time to limit memory use. Each model
uses the same prediction seed in every state so a draw reuses the national
fit's coefficient draw. Prediction requires all effect levels to have been
estimated in the national fit. An unseen level raises an error, since
sampling new effects independently by state could change their dependence.


``` r
set.seed(2027)
house_fit <- fit_sequential_logit(
  survey, house, "district_id", area_covariates, no_vote = "No Vote", outcome_order = choices,
  predictors = c(predictors, pres), depth = 1, control = control,
  log_file = file.path(out, "house_fit_log.csv")
)
```

```
## Warning: Removed 3214 incomplete survey rows before aggregation.
```

```
## Stage 1/3 'Democratic' — preparing data and selecting depth.
```

```
## Stage 1/3 'Democratic' — fitting at depth 1 with 55413 eligible respondents and 527 estimated coefficients.
```

```
## Stage 1/3 'Democratic' — model returned (1 of 3 completed).
```

```
## Stage 2/3 'Republican' — preparing data and selecting depth.
```

```
## Stage 2/3 'Republican' — fitting at depth 1 with 28635 eligible respondents and 527 estimated coefficients.
```

```
## Stage 2/3 'Republican' — model returned (2 of 3 completed).
```

```
## Stage 3/3 'Other' — preparing data and selecting depth.
```

```
## Stage 3/3 'Other' — fitting at depth 1 with 8146 eligible respondents and 527 estimated coefficients.
```

```
## ..
```

```
## Stage 3/3 'Other' — model returned (3 of 3 completed).
```

``` r
house_fit$fit_log[c("outcome", "observations", "fitted_depth", "estimated_parameters")]
```

```
##      outcome observations fitted_depth estimated_parameters
## 1 Democratic        55413            1                  527
## 2 Republican        28635            1                  527
## 3      Other         8146            1                  527
```

``` r
for (stage in names(house_fit$stages)) {
  cat(stage, "\n")
  cat(trimws(deparse(house_fit$stages[[stage]]$formula), which = "right"), sep = "\n")
  cat("\n")
}
```

```
## Democratic
## cbind(.success, .failure) ~ 1 + house_share + pres_share + on_ballot +
##     incumbent + (1 | district_id) + (1 | age_cat) + (1 | gender_cat) +
##     (1 | race_cat) + (1 | hispanic_cat) + (1 | educ_cat) + (1 |
##     vote_pres_2024) + (1 | lineup) + v_s(house_share) + v_s(pres_share)
##
## Republican
## cbind(.success, .failure) ~ 1 + house_share + pres_share + on_ballot +
##     incumbent + (1 | district_id) + (1 | age_cat) + (1 | gender_cat) +
##     (1 | race_cat) + (1 | hispanic_cat) + (1 | educ_cat) + (1 |
##     vote_pres_2024) + (1 | lineup) + v_s(house_share) + v_s(pres_share)
##
## Other
## cbind(.success, .failure) ~ 1 + house_share + pres_share + on_ballot +
##     incumbent + (1 | district_id) + (1 | age_cat) + (1 | gender_cat) +
##     (1 | race_cat) + (1 | hispanic_cat) + (1 | educ_cat) + (1 |
##     vote_pres_2024) + (1 | lineup) + v_s(house_share) + v_s(pres_share)
```


``` r
predict_house_state <- function(incoming) {
  set.seed(2028)
  prediction <- predict_sequential_logit(
    house_fit, frame = incoming, allow_missing_levels = FALSE,
    write_files = FALSE
  )
  aggregate_extended_frame(incoming, prediction, by = names(prediction$cells))
}
```

## 5. Rake both outcome margins and preserve the original cells

Jointly calibrate the state presidential targets and district House targets
on every count column. Each district belongs to one state, and all original
cells belong to a single district. The constraints therefore separate by
state. Predict and rake one state at a time to bound memory usage. House
targets of zero constrain their district and choice to zero while calibration
preserves the other outcome margins and the original joint cell totals.


``` r
rake_tolerance <- 1e-8
raked_dir <- file.path(out, "raked")
dir.create(raked_dir, showWarnings = FALSE)
states <- split(seq_len(nrow(frame)), frame$state_fips)
count_columns <- c("N", paste0("N_draw_", seq_len(n_draws)))

# Reduce each block to group-outcome draw totals for the comparison plots.
plot_totals <- function(block, extension) {
  columns <- c(plot_targets[[extension]]$by, extension)
  keys <- block[columns]
  group <- frame_cell_keys(keys, keys)
  cbind(keys[!duplicated(group), , drop = FALSE],
        rowsum(block[count_columns], group, reorder = FALSE))
}

rake_states <- function() {
  lapply(names(states), function(state) {
    original <- frame[states[[state]], , drop = FALSE]
    incoming <- predict_presidential_state(original)
    joint <- predict_house_state(incoming)
    stopifnot(nrow(joint) == nrow(original) * length(choices)^2L)
    # Use the same named configuration for the rake in every block.
    targets <- lapply(names(rake_targets), function(outcome) {
      by <- rake_by[[outcome]]
      table <- rake_targets[[outcome]]
      key <- frame_cell_keys(table[by], original[by])
      keep <- key %in% frame_cell_keys(unique(original[by]), original[by])
      table[keep, , drop = FALSE]
    }) |> setNames(names(rake_targets))

    raked <- rake_joint_extended_frame(joint, original, targets, by = rake_by,
                                       tolerance = rake_tolerance)
    keys <- setdiff(names(original), "N")
    cell <- match(frame_cell_keys(raked[keys], original[keys]),
                  frame_cell_keys(original[keys], original[keys]))
    counts <- rowsum(raked[count_columns], cell)
    error <- max(abs(as.matrix(counts) - original$N))
    stopifnot(error <= rake_tolerance)
    saveRDS(raked, file.path(raked_dir, paste0("state_", state, ".rds")))

    list(before = setNames(lapply(names(plot_targets), function(x) plot_totals(joint, x)),
                           names(plot_targets)),
         after = setNames(lapply(names(plot_targets), function(x) plot_totals(raked, x)),
                          names(plot_targets)),
         cell_error = error)
  })
}

summaries <- rake_states()
max(vapply(summaries, function(x) x$cell_error, numeric(1)))
```

```
## [1] 7.275958e-12
```

## 6. Plot each outcome against its own targets

The panels compare the unraked and raked joint frame to the selected
benchmarks, with posterior intervals from the draw counts. Presidential
shares are marginalized over House vote. House shares are marginalized over
presidential vote. For party panels, divide both the predictions and targets
by turnout at their own comparison geography. Draw intervals divide each
draw's party count by its own turnout before taking quantiles. Overall pools
those turnout-conditional party comparisons. The No Vote panel keeps the
population denominator. The rake still uses population-share targets.
The default benchmarks also enter model fitting as area predictors, so
agreement with them measures calibration.


``` r
figure_dir <- file.path("vignettes", "assets", "us-2026")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
fits <- setNames(list(pres_fit, house_fit), c(pres, house))
for (phase in c("before", "after")) {
  for (outcome in names(plot_targets)) {
    specification <- plot_targets[[outcome]]
    totals <- do.call(rbind, lapply(summaries, function(x) x[[phase]][[outcome]]))
    figure <- file.path(figure_dir, paste0(outcome, "_", phase, "_rake.png"))
    check <- check_extended_frame(
      totals, specification$data, by = specification$by,
      extension_variable = outcome, outcome_order = fits[[outcome]]$outcome_order,
      no_vote = fits[[outcome]]$no_vote,
      plot_file = figure, coverage_tolerance = if (phase == "after") rake_tolerance else 0
    )
    print(check$metrics)
  }
}
```

```
##                  bias_pp   RMSE_pp  spearman coverage_90_pct
## Overall    -1.683405e-15  6.498797 0.9067790        4.575163
## Democratic  6.069814e+00  6.823634 0.9727602        9.803922
## Republican -8.090537e+00  8.708720 0.9709502        3.921569
## Other       2.020723e+00  2.073478 0.8406335        0.000000
## No Vote    -2.575223e+01 26.170325 0.8713122        0.000000
```

```
##                  bias_pp   RMSE_pp  spearman coverage_90_pct
## Overall    -3.227647e-16  7.696671 0.9307125      25.5351682
## Democratic  5.594692e+00  8.979772 0.9511983      26.3761468
## Republican -6.426024e+00  9.446105 0.9550218      25.4587156
## Other       8.313322e-01  2.801968 0.6150587      24.7706422
## No Vote    -2.313553e+01 24.050545 0.8140960       0.2293578
```

```
##                  bias_pp      RMSE_pp spearman coverage_90_pct
## Overall    -4.161069e-16 1.194543e-08        1             100
## Democratic -1.654226e-09 1.454889e-08        1             100
## Republican  1.581327e-09 1.470821e-08        1             100
## Other       7.289760e-11 2.801707e-10        1             100
## No Vote     1.029418e-09 7.207703e-09        1             100
```

```
##                  bias_pp      RMSE_pp  spearman coverage_90_pct
## Overall    -1.180059e-16 3.669187e-10 0.9999997             100
## Democratic  4.684468e-11 4.103290e-10 0.9999961             100
## Republican -5.306658e-11 4.493817e-10 0.9999999             100
## Other       6.221542e-12 1.832327e-10 1.0000000             100
## No Vote    -1.370922e-10 1.269862e-09 1.0000000             100
```

### Presidential vote by state

![State presidential estimates before joint calibration](assets/us-2026/vote_pres_2024_before_rake.png)

![State presidential estimates after joint calibration](assets/us-2026/vote_pres_2024_after_rake.png)

### House vote by district

![District House estimates before joint calibration](assets/us-2026/vote_house_2024_before_rake.png)

![District House estimates after joint calibration](assets/us-2026/vote_house_2024_after_rake.png)

## 7. Inspect the final frame

Each state RDS includes both outcome columns, `N`, and the draw-specific
counts. The original state and district keys are carried by `state_fips`
and `district_id`. Read the partitions together when a later analysis needs
the national joint frame. The rows below sample Alabama and show all
demographic keys, both vote outcomes, and three draw counts.


``` r
alabama <- readRDS(file.path(raked_dir, "state_01.rds"))
preview_rows <- round(seq(1L, nrow(alabama), length.out = 6L))
preview_columns <- c("state_fips", predictors, pres, house, "N",
                     paste0("N_draw_", c(1L, ceiling(n_draws / 2), n_draws)))
knitr::kable(alabama[preview_rows, preview_columns], digits = 3, row.names = FALSE)
```



|state_fips |district_id |age_cat |gender_cat |race_cat        |hispanic_cat |educ_cat     |vote_pres_2024 |vote_house_2024 |      N| N_draw_1| N_draw_15| N_draw_30|
|:----------|:-----------|:-------|:----------|:---------------|:------------|:------------|:--------------|:---------------|------:|--------:|---------:|---------:|
|01         |01-01       |18-22   |Male       |White           |Hispanic     |No HS        |Democratic     |Democratic      |  4.027|    4.517|     4.169|     4.055|
|01         |01-02       |43-47   |Male       |White           |Not Hispanic |Some college |Other          |Republican      | 12.587|   11.854|    12.279|    14.786|
|01         |01-03       |73-77   |Male       |Native American |Not Hispanic |Post-grad    |Democratic     |No Vote         |  0.727|    0.706|     0.692|     0.717|
|01         |01-05       |33-37   |Male       |Asian           |Not Hispanic |4-year       |No Vote        |Democratic      |  0.000|    0.000|     0.000|     0.000|
|01         |01-06       |58-62   |Male       |Native American |Not Hispanic |No HS        |Republican     |Other           |  0.000|    0.000|     0.000|     0.000|
|01         |01-07       |83+     |Female     |Other/Multi     |Not Hispanic |Post-grad    |No Vote        |No Vote         |  3.262|    3.422|     3.094|     3.687|
