# Extending the US 2026 congressional district frame

This run prepares and audits two models before fitting. The goal is a
population frame for predicting the 2026 House vote. We first
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
Run the chunks from the repository root with `vglmer` 1.0.8, `gKRLS`, and
`SQUAREM` installed. VoteFrame screens fixed columns and random-effect levels before
calling `vglmer`.

## 1. Read the survey, district data, and population frame

The CES auxiliary survey records both 2024 choices. The ACS PUMS mother
frame supplies citizen-adult population weights. The district-by-choice CSV
supplies area-level predictors and calibration targets. Its presidential
shares repeat within each state and choice, while House shares belong to
individual districts. Some House shares on the 2026 map come from upstream
modeled scenarios. Agreement with these benchmarks is a calibration check.


``` r
# Load VoteFrame from this checkout so the audit uses the same functions as the fit.
for (file in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(file)

country <- file.path("data", "input", "us 2026")
fips <- c("state_fips", "district_fips")
demographics <- c("age_cat", "gender_cat", "race_cat", "hispanic_cat", "educ_cat")
pres <- "vote_pres_2024"
house <- "vote_house_2024"
# The files use choices as their category order. The model uses vote_order
# to define its three regressions. The final choice receives the remainder.
choices <- c("Democratic", "Republican", "Other", "No Vote")
vote_order <- c("No Vote", "Democratic", "Republican", "Other")

# The survey supplies observed choices. The population frame supplies cell
# sizes. Read FIPS IDs as text to preserve their leading zeros in joins.
survey <- readRDS(file.path(country, "ces_2024_harmonized.rds"))
frame <- readRDS(file.path(country, "pums_cd_cells.rds"))
election <- read.csv(file.path(country, "district_election_inputs_cd120.csv"),
                     colClasses = setNames(rep("character", length(fips)), fips),
                     na.strings = "")

# VoteFrame uses one area ID. A district number alone can repeat across states.
district_id <- function(data) {
  id <- paste(data$state_fips, data$district_fips, sep = "-")
  # Preserve missing geography so the survey complete-case check can catch it.
  id[!complete.cases(data[fips])] <- NA_character_
  id
}
survey$district_id <- district_id(survey)
frame$district_id <- district_id(frame)
election$district_id <- district_id(election)

predictors <- c("state_fips", "district_id", demographics)
# Keep the demographic keys and population count N for each cell. State FIPS
# also lets us partition prediction and match statewide totals later.
frame <- frame[order(frame$state_fips), c(predictors, "N")]

# Check that the inputs use the same map and outcome labels. Require one
# election row per district and choice, including the DC district.
stopifnot(
  nrow(election) == 1744L, length(unique(election$district_id)) == 436L,
  !anyNA(election$district_id), !anyDuplicated(election[c("district_id", "vote_choice")]),
  all(grepl("^[0-9]{2}-[0-9]{2}$", election$district_id)),
  setequal(election$vote_choice, choices), "11-98" %in% election$district_id,
  setequal(frame$district_id, election$district_id),
  all(survey$district_id[!is.na(survey$district_id)] %in% election$district_id),
  all(vapply(survey[c(pres, house)], function(x) identical(levels(x), choices), logical(1))),
  all(is.na(election$incumbent[election$vote_choice == "No Vote"]))
)
data.frame(election_rows = nrow(election), population_cells = nrow(frame),
           survey_respondents = nrow(survey), citizen_adults = round(sum(frame$N)))
```

```
##   election_rows population_cells survey_respondents citizen_adults
## 1          1744           497550              58627      240960972
```

## 2. Specify stage predictors and election benchmarks

Each entry in a fit's area-covariate list corresponds to a choice. Each
entry is a matrix or data frame with district IDs as row names and
predictors as columns. Splitting the CSV by choice directly produces these
tables. In both fits, each party stage uses that choice's district House
share and statewide presidential share, the district's categorical
House lineup, the choice's incumbency, and its ballot indicator. Preserve
both shares on their original citizen-adult scale. Both fits start with
No Vote, using the district House and statewide presidential nonvoting
shares and the lineup. Democratic and Republican follow among voters.
Other receives the remaining probability. Covariates that are blank in
every district for a stage are omitted from that stage. For No Vote,
this applies to incumbency and ballot presence.

Include `on_ballot` in the party-stage candidate designs so the support,
collinearity, and hierarchy rules decide whether its main effect and
interactions enter each fitted stage.


``` r
# Each stage looks up district predictors for the outcome it models.
# District IDs become row names so the survey joins can use them as keys.
choice_rows <- split(election, election$vote_choice)
area_covariates <- lapply(choice_rows, function(rows) {
  covariates <- data.frame(house_share = rows[[house]], pres_share = rows[[pres]],
                           lineup = factor(rows$lineup),
                           incumbent = rows$incumbent, on_ballot = rows$on_ballot,
                           row.names = rows$district_id)
  # No Vote has no candidate, so its incumbency and ballot fields are blank.
  # Omit fields blank across an entire stage.
  covariates[!vapply(covariates, function(x) all(is.na(x)), logical(1))]
})

# Statewide presidential shares repeat on every district row in a state.
# Collapse those copies to one share per state and choice for later raking.
pres_targets <- unique(data.frame(state_fips = election$state_fips,
                                 vote_pres_2024 = election$vote_choice,
                                 target = election[[pres]]))
# House shares are district-specific. Keep one share per district and choice.
house_targets <- data.frame(district_id = election$district_id,
                            vote_house_2024 = election$vote_choice,
                            target = election[[house]])
rake_targets <- setNames(list(pres_targets, house_targets), c(pres, house))
rake_by <- setNames(list("state_fips", "district_id"), c(pres, house))

# Start later comparison plots with the same benchmarks as raking. The
# separate list lets a plot use another comparison table without changing the rake.
plot_targets <- setNames(list(
  list(data = pres_targets, by = "state_fips"),
  list(data = house_targets, by = "district_id")
), c(pres, house))

# Check one presidential share per state and choice. All four shares must
# be between zero and one and sum to one within each state or district.
stopifnot(
  nrow(pres_targets) == 51L * length(choices),
  !anyDuplicated(pres_targets[c("state_fips", pres)]),
  all(is.finite(pres_targets$target) & pres_targets$target >= 0 & pres_targets$target <= 1),
  all(is.finite(house_targets$target) & house_targets$target >= 0 & house_targets$target <= 1),
  all(abs(rowsum(pres_targets$target, pres_targets$state_fips) - 1) < 1e-8),
  all(abs(rowsum(house_targets$target, house_targets$district_id) - 1) < 1e-8)
)
```

Each benchmark is a **vote share for one choice in one geography**. For
example, California's Democratic presidential share is one benchmark, and
its presidential No Vote share is another. There are four shares per
geography, including No Vote, because the denominator is the full
citizen-adult population. These are the shares a later calibration run
would match.


|Election          |Geography               | Geographies| Choices per geography| Benchmark shares|
|:-----------------|:-----------------------|-----------:|---------------------:|----------------:|
|2024 presidential |States and DC           |          51|                     4|              204|
|2024 House        |Congressional districts |         436|                     4|            1,744|

## 3. Build the survey designs

Both models start at requested depth 2, with interactions and splines.
The presidential model uses state, district, five demographics, and the
area predictors above. The House model adds each CES respondent's
presidential choice. Each model uses its own complete survey cases.
Identical predictor rows are grouped into binomial successes and failures.
At each stage, respondents who chose an earlier outcome leave the risk set.
The counts below refer to eligible respondents at that stage.


``` r
# Try two-way interactions. Count support in respondents, not aggregated cells.
requested_depth <- 2L
minimum_support <- 10L
# Draw count for the later prediction run. The pre-fit audit draws nothing.
n_draws <- 125L
# The House model additionally conditions on each respondent’s presidential choice.
model_specs <- list(
  Presidential = list(outcome = pres, predictors = predictors),
  House = list(outcome = house, predictors = c(predictors, pres))
)

prepare_model <- function(spec) {
  # Combine identical predictor rows and count respondents by vote choice.
  # The audit reports incomplete cases separately for each model below.
  aggregated <- suppressWarnings(aggregate_extension_survey(
    survey, spec$outcome, spec$predictors, "No Vote", outcome_order = vote_order
  ))
  # Other receives the probability left over from the three fitted stages.
  stages <- head(vote_order, -1L)
  setNames(lapply(seq_along(stages), function(i) {
    # At this stage, the current choice is a success. Current and later
    # choices form the trial count because earlier choices already exited.
    success <- aggregated$counts[, i]
    trials <- rowSums(aggregated$counts[, i:length(vote_order), drop = FALSE])
    eligible <- trials > 0
    # Join only eligible cells to the district covariates for this choice.
    stage <- join_stage_covariates(aggregated$cells[eligible, , drop = FALSE],
                                  "district_id", area_covariates[[stages[i]]])
    # Use the same design selector as fitting. It screens the candidate and
    # checks the retained coefficient budget without running the solver.
    selected <- select_sequential_stage_design(
      stage, "district_id", requested_depth, splines = TRUE,
      area_predictors = setdiff(names(stage), spec$predictors),
      observations = sum(trials), trials = trials[eligible],
      minimum_support = minimum_support, successes = success[eligible]
    )
    selected$excluded_respondents <- aggregated$omitted
    selected
  }), stages)
}
preflight <- lapply(model_specs, prepare_model)
```

## 4. Screen support, conditioning, and hierarchy

1. **Build the requested design.** Omit constant predictors and
   district-level predictor interactions with district ID, since those
   predictors are constant within district. The starting counts below
   are for the design after those formula rules.
2. **Screen support.** Fixed columns need at least 10 respondents with
   nonzero entries. A random group level needs at least 10 respondents
   for the whole intercept-and-slopes vector to enter the likelihood.
   A varying slope also needs, on average, at least 10 informative
   respondents per retained group. Within a group, this counts respondents
   whose slope value differs from its most common value.
3. **Screen the fixed design for collinearity.** Weight each row by the
   square root of its trial count and scale each column to unit length.
   The condition indices are `largest singular value / each singular
   value`. Their maximum must stay below 30. Try the intercept first,
   then main effects, then interactions of increasing order. Within
   each order prefer the column that adds the most independent
   variation. Belsley, Kuh, and Welsch use 30 as an inspection flag in
   linear regression, alongside variance-decomposition proportions.
   VoteFrame uses it as an automatic cutoff for its pre-fit logistic
   design. The plots below show condition indices. [SAS guide to the
   Belsley diagnostic](https://support.sas.com/documentation/cdl/en/sgug/59902/HTML/default/fit_sect29.htm)
4. **Enforce hierarchy.** Drop descendants of removed main effects or
   interactions. Fit a grouped spline deviation only where the
   corresponding linear random block survives. Survey rows from screened
   groups still inform the overall curve and other retained effects.
5. **Check depth after screening.** Compare the estimated retained
   coefficients with eligible respondents. Reduce depth only if the
   screened candidate still exceeds that budget.



## 5. Account for every removed coefficient

The presidential model excluded 1,024
incomplete survey records. The House model excluded
3,214.
The starting counts are design columns and include the nominal kernel
sketch width. The final solver may use a smaller kernel basis after
its rank checks. Each row reconciles as **start minus removed plus restored equals retained**. A restored column belongs to a standalone global curve that
the starting formula had supplied through a grouped spline.


|Model        |Stage      | Respondents|  Cells| Depth|  Start| Removed| Restored| Retained|
|:------------|:----------|-----------:|------:|-----:|------:|-------:|--------:|--------:|
|Presidential |No Vote    |      57,603| 42,019|     2| 20,013|   6,088|        0|   13,925|
|Presidential |Democratic |      52,767| 39,007|     2| 20,816|   7,103|        0|   13,713|
|Presidential |Republican |      23,922| 19,496|     2| 19,818|   9,932|        0|    9,886|
|House        |No Vote    |      55,413| 46,863|     2| 22,317|   6,938|        0|   15,379|
|House        |Democratic |      48,330| 40,477|     2| 22,852|   8,230|        0|   14,622|
|House        |Republican |      21,552| 18,045|     2| 21,177|  10,626|        0|   10,551|

Here is the coefficient accounting summed across the three fitted stages of
each model. The residual outcome, Other, has no separate stage.


|Model        |  Start| Removed| Restored| Retained|
|:------------|------:|-------:|--------:|--------:|
|House        | 66,346|  25,794|        0|   40,552|
|Presidential | 60,647|  23,123|        0|   37,524|

Random level support counts columns removed with a low-support intercept.
Slope support and linear hierarchy count additional removals among levels
that clear the intercept cutoff. Whole smooths and spline levels report
the associated estimated basis columns. These rows count each removal once.
The substantive pattern is the same in both models. Most removals are
low-support random levels. The slope screen also removes
`pres_share:state_fips` because presidential share is constant within
each state. The conditioning table below identifies the rejected fixed
columns in each stage. The term table shows the corresponding smooth
removals.


|Model        |Stage      | Fixed support| Random level support| Within-group slope support| Fixed collinearity| Linear hierarchy| Whole smooths| Spline level support|
|:------------|:----------|-------------:|--------------------:|--------------------------:|------------------:|----------------:|-------------:|--------------------:|
|Presidential |No Vote    |             0|                5,237|                         51|                  1|                0|           799|                    0|
|Presidential |Democratic |             0|                5,737|                         57|                  5|                0|         1,304|                    0|
|Presidential |Republican |             0|                8,519|                        108|                  6|                0|         1,299|                    0|
|House        |No Vote    |             0|                6,077|                         51|                  1|                0|           809|                    0|
|House        |Democratic |             0|                6,844|                         57|                  5|                0|         1,324|                    0|
|House        |Republican |             0|                9,315|                         51|                  3|               93|         1,164|                    0|


|Model        |Stage      |Fixed interactions removed by hierarchy                          |Sparse slopes (mean respondents)                                             |Whole smooths removed                                                                                                                                                                                                                                            |
|:------------|:----------|:----------------------------------------------------------------|:----------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Presidential |No Vote    |—                                                                |pres_share:state_fips (0)                                                    |v_s(pres_share, by = state_fips), v_s(house_share, pres_share, type = "gKRLS")                                                                                                                                                                                   |
|Presidential |Democratic |—                                                                |pres_share:state_fips (0), lineup:on_ballot (0)                              |v_s(pres_share, by = state_fips), v_s(house_share, pres_share, type = "gKRLS"), v_s(house_share, on_ballot, type = "gKRLS"), v_s(pres_share, incumbent, type = "gKRLS"), v_s(pres_share, on_ballot, type = "gKRLS")                                              |
|Presidential |Republican |—                                                                |pres_share:state_fips (0), on_ballot:state_fips (8.47), lineup:on_ballot (0) |v_s(pres_share, by = state_fips), v_s(house_share, pres_share, type = "gKRLS"), v_s(house_share, incumbent, type = "gKRLS"), v_s(house_share, on_ballot, type = "gKRLS"), v_s(pres_share, incumbent, type = "gKRLS"), v_s(pres_share, on_ballot, type = "gKRLS") |
|House        |No Vote    |—                                                                |pres_share:state_fips (0)                                                    |v_s(pres_share, by = state_fips), v_s(house_share, pres_share, type = "gKRLS")                                                                                                                                                                                   |
|House        |Democratic |—                                                                |pres_share:state_fips (0), lineup:on_ballot (0)                              |v_s(pres_share, by = state_fips), v_s(house_share, pres_share, type = "gKRLS"), v_s(house_share, on_ballot, type = "gKRLS"), v_s(pres_share, incumbent, type = "gKRLS"), v_s(pres_share, on_ballot, type = "gKRLS")                                              |
|House        |Republican |pres_share:on_ballot, house_share:on_ballot, incumbent:on_ballot |pres_share:state_fips (0)                                                    |v_s(pres_share, by = state_fips), v_s(house_share, pres_share, type = "gKRLS"), v_s(house_share, on_ballot, type = "gKRLS"), v_s(pres_share, incumbent, type = "gKRLS"), v_s(pres_share, on_ballot, type = "gKRLS")                                              |

## 6. Inspect support and conditioning before fitting

The left panels show how many candidate random-effect levels have at most
the given number of survey respondents. Each level is counted once per
random term, including crossed factor levels. The right panels show the
full fixed-design condition-index spectrum by stage, after the support
screen and again after collinearity and hierarchy pruning. The horizontal
line marks 30. The spectra are sorted from their largest index, so the
first point gives the maximum. Indices above 200 are labeled at the top
of the plot so the region near the cutoff remains legible.


|Model        |Stage      | Before pruning| After pruning|Fixed columns before → after |Fixed columns rejected                                                                                                                |
|:------------|:----------|--------------:|-------------:|:----------------------------|:-------------------------------------------------------------------------------------------------------------------------------------|
|Presidential |No Vote    |   1.104000e+02|         18.46|4 → 3                        |house_share:pres_share                                                                                                                |
|Presidential |Democratic |   1.408689e+14|         26.92|11 → 6                       |house_share:pres_share, house_share:on_ballot, pres_share:incumbent, pres_share:on_ballot, incumbent:on_ballot                        |
|Presidential |Republican |   1.144379e+15|         24.64|11 → 5                       |house_share:pres_share, house_share:incumbent, house_share:on_ballot, pres_share:incumbent, pres_share:on_ballot, incumbent:on_ballot |
|House        |No Vote    |   1.101400e+02|         18.47|4 → 3                        |house_share:pres_share                                                                                                                |
|House        |Democratic |   1.781587e+16|         28.03|11 → 6                       |house_share:pres_share, house_share:on_ballot, pres_share:incumbent, pres_share:on_ballot, incumbent:on_ballot                        |
|House        |Republican |   8.938414e+14|         25.41|11 → 5                       |on_ballot, house_share:pres_share, pres_share:incumbent                                                                               |

For each pre-pruning component at or above 30, the next table names the
fixed columns with at least half of their variance proportion on that
component. These proportions come from the weighted design's singular
vectors. They diagnose shared weak directions in the design. They are
separate from posterior coefficient variances and do not drive pruning.


|Model        |Stage      | Condition index|Columns with at least 50% on this component                                          |
|:------------|:----------|---------------:|:------------------------------------------------------------------------------------|
|Presidential |No Vote    |    1.104000e+02|(Intercept) (98%), house_share (94%), pres_share (93%), house_share:pres_share (99%) |
|Presidential |Democratic |    3.607000e+01|—                                                                                    |
|Presidential |Democratic |    9.418000e+01|house_share:pres_share (73%)                                                         |
|Presidential |Democratic |    1.552500e+02|(Intercept) (60%), pres_share (63%), on_ballot (87%), pres_share:on_ballot (89%)     |
|Presidential |Democratic |    2.045343e+07|house_share (100%), house_share:on_ballot (100%)                                     |
|Presidential |Democratic |    1.408689e+14|incumbent (100%), incumbent:on_ballot (100%)                                         |
|Presidential |Republican |    3.378000e+01|house_share:incumbent (89%)                                                          |
|Presidential |Republican |    4.743000e+01|pres_share:incumbent (50%)                                                           |
|Presidential |Republican |    1.325400e+02|house_share:pres_share (83%)                                                         |
|Presidential |Republican |    2.390900e+02|(Intercept) (81%), pres_share (82%), on_ballot (95%), pres_share:on_ballot (98%)     |
|Presidential |Republican |    3.870729e+14|incumbent (100%), incumbent:on_ballot (100%)                                         |
|Presidential |Republican |    1.144379e+15|house_share (100%), house_share:on_ballot (100%)                                     |
|House        |No Vote    |    1.101400e+02|(Intercept) (98%), house_share (94%), pres_share (93%), house_share:pres_share (99%) |
|House        |Democratic |    3.619000e+01|—                                                                                    |
|House        |Democratic |    9.705000e+01|house_share:pres_share (80%)                                                         |
|House        |Democratic |    1.691000e+02|(Intercept) (68%), pres_share (71%), on_ballot (87%), pres_share:on_ballot (89%)     |
|House        |Democratic |    2.524383e+07|house_share (100%), house_share:on_ballot (100%)                                     |
|House        |Democratic |    1.781587e+16|incumbent (100%), incumbent:on_ballot (100%)                                         |
|House        |Republican |    3.041000e+01|—                                                                                    |
|House        |Republican |    3.662000e+01|house_share:incumbent (62%)                                                          |
|House        |Republican |    4.963000e+01|pres_share:incumbent (50%)                                                           |
|House        |Republican |    1.390300e+02|house_share:pres_share (92%)                                                         |
|House        |Republican |    3.031900e+02|(Intercept) (89%), pres_share (91%), on_ballot (95%), pres_share:on_ballot (97%)     |
|House        |Republican |    5.588506e+14|house_share (100%), house_share:on_ballot (100%)                                     |
|House        |Republican |    8.938414e+14|incumbent (100%), incumbent:on_ballot (100%)                                         |


![Presidential model support and condition indices](assets/us-2026/prefit-presidential.png)

![House model support and condition indices](assets/us-2026/prefit-house.png)

## 7. Stop before fitting

Both models have prepared designs. The later run is configured for
125 posterior draws. This knit stops before solver iterations,
posterior prediction, or raking. To continue in a separate
run, knit with `VOTEFRAME_US_FIT=true`.
