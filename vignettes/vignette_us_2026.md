# Extending the US 2026 congressional district frame

This vignette prepares, audits, and fits two models. The goal is a
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

*Table 1. Expected frame size at each step. Each extension creates one row
per choice for each incoming cell. Raking adjusts counts, so rows with
zero calibrated counts are still included.*

All three inputs use the 2026 district map (CD120), including DC. The
[US input guide](../data/input/us%202026/README.md) records the upstream
commit, checksums, and the rules for observed and modeled House returns.
Run the chunks from the SynthFrame repository root. The [case-study setup
instructions](../README.md#reproduce-a-case-study) list the R packages and
show how to knit the report. SynthFrame screens fixed columns and random-effect
levels before calling `vglmer`.

## 1. Read the survey, district data, and population frame

The CES auxiliary survey records both 2024 choices. The ACS PUMS mother
frame supplies citizen-adult population weights. The district-by-choice CSV
supplies area-level predictors and calibration targets. Its presidential
shares repeat within each state and choice, while House shares belong to
individual districts. Some House shares on the 2026 map come from upstream
modeled scenarios. Agreement with these benchmarks is a calibration check.

*Snippet 1. Read the three inputs, build district IDs, and check that every input uses CD120.*


``` r
# Load SynthFrame from this checkout so the audit uses the same functions as the fit.
for (file in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(file)

# Declare the shared input path, join keys, and recorded outcomes.
country <- file.path("data", "input", "us 2026")
fips <- c("state_fips", "district_fips")
demographics <- c("age_cat", "gender_cat", "race_cat", "hispanic_cat", "educ_cat")
pres <- "vote_pres_2024"
house <- "vote_house_2024"
# Keep all districts in a state together unless the caller overrides the geography.
parent_area <- Sys.getenv("SYNTHFRAME_PARENT_AREA", "state_fips")
areas_per_batch <- vignette_areas_per_batch()
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

# SynthFrame uses one area ID. A district number alone can repeat across states.
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
```


| District × choice records| Population cells| Survey respondents| Citizen adults represented|
|-------------------------:|----------------:|------------------:|--------------------------:|
|                     1,744|          497,550|             58,627|                240,960,972|

*Table 2. Input sizes before screening. A population cell is one district
and demographic combination. The citizen-adult total sums the population
counts across those cells.*

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

*Snippet 2. Prepare district predictors for each stage and election benchmarks for calibration.*


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
  all(abs(rowsum(pres_targets$target, pres_targets$state_fips) - 1) < 0.00000001),
  all(abs(rowsum(house_targets$target, house_targets$district_id) - 1) < 0.00000001)
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

*Table 3. Benchmark shares available for later calibration. Each count is
the number of geographies multiplied by the four choices, including
No Vote.*

## 3. Build the survey designs

Both models start at requested depth 2, with interactions and splines.
The presidential model uses state, district, five demographics, and the
area predictors above. The House model adds each CES respondent's
presidential choice. Each model uses its own complete survey cases.
Identical predictor rows are grouped into binomial successes and failures.
At each stage, respondents who chose an earlier outcome leave the risk set.
The counts below refer to eligible respondents at that stage.

*Snippet 3. Build the candidate binomial designs and screen them before any solver iterations.*


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

1. **Build the requested design.** Set aside predictors with only one
   observed value in the stage. A one-level factor has no contrast to
   construct, and a constant numeric predictor supplies no main-effect
   variation beyond the intercept. Build all eligible interactions at
   the requested depth. The starting counts below describe this design.
2. **Screen support.** Fixed columns need at least 10 respondents with
   nonzero entries. A random group level needs at least 10 respondents
   for the whole intercept-and-slopes vector to enter the likelihood.
   Keep the group only if at least two levels meet that cutoff.
   A varying slope also needs, on average, at least 10 informative
   respondents per retained group. Within a group, this counts respondents
   whose slope value differs from its most common value.
3. **Screen the fixed design for collinearity.** Weight each row by the
   square root of its trial count and scale each column to unit length.
   The condition indices are `largest singular value / each singular
   value`. Start with the full supported fixed design and prune until its
   maximum is below 30. At each step find the weakest singular direction.
   Among the columns involved, consider the highest-order interactions
   first and keep the intercept. Choose the deletion that yields the
   lowest condition number after enforcing hierarchy. Belsley, Kuh,
   and Welsch use 30 as an inspection flag in
   linear regression, alongside variance-decomposition proportions.
   SynthFrame uses it as an automatic cutoff for its pre-fit logistic
   design. The plots below show condition indices. [SAS guide to the
   Belsley diagnostic](https://support.sas.com/documentation/cdl/en/sgug/59902/HTML/default/fit_sect29.htm)
4. **Enforce hierarchy and check grouping redundancy.** Drop descendants
   whenever a main effect or interaction is removed. Drop a crossed
   random intercept if it gives the same grouping of survey rows as one
   of its parents. For example, district by state duplicates district
   because each district belongs to one state. The general slope rule
   handles numeric predictors that are constant within district.
   Fit a grouped spline deviation only where the
   corresponding linear random block survives. Survey rows from screened
   groups still inform the overall curve and other retained effects.
5. **Check depth after screening.** Compare the estimated retained
   coefficients with eligible respondents. Try a lower depth if the candidate
   exceeds that budget or screening removes every random effect and spline.



## 5. Account for every removed coefficient

The presidential model excluded 1,024
incomplete survey records. The House model excluded
3,214.
The counts below are estimates of **model coefficients**, one per fixed
effect column, random intercept or slope column, and spline basis column.
They exclude variance and covariance parameters. The solver may use a
smaller kernel spline basis after its rank checks.


|Model        |Stage      | Respondents| Survey cells| Depth| Candidate coefficients| Dropped coefficients| Spline coefficients added back†| Coefficients to fit|
|:------------|:----------|-----------:|------------:|-----:|----------------------:|--------------------:|-------------------------------:|-------------------:|
|Presidential |No Vote    |      57,603|       42,019|     2|                 41,860|               28,371|                               0|              13,489|
|Presidential |Democratic |      52,767|       39,007|     2|                 43,535|               30,258|                               0|              13,277|
|Presidential |Republican |      23,922|       19,496|     2|                 42,537|               33,087|                               0|               9,450|
|House        |No Vote    |      55,413|       46,863|     2|                 44,164|               29,221|                               0|              14,943|
|House        |Democratic |      48,330|       40,477|     2|                 45,571|               31,385|                               0|              14,186|
|House        |Republican |      21,552|       18,045|     2|                 43,896|               33,905|                               0|               9,991|

*Table 4. One row per model and stage. The coefficients to fit equal the
candidate columns minus the dropped columns plus any spline columns added
back. Survey cells counts distinct eligible predictor rows after
aggregation. Respondents counts the people represented by those rows.*

† *Spline coefficients added back.* After all fixed and random terms have
been screened for a candidate depth, SynthFrame checks which eligible
overall curves are still supplied by a grouped spline. If no retained
grouped spline supplies an eligible overall curve, it fits that curve on
its own. This check runs once per candidate depth, after pruning. The
six stages added back 0 spline columns.

The biggest cut came from dropping district-specific curves for vote
shares. Everyone in a district is assigned the same district vote share,
so the survey offers no way to estimate how that district's curve changes
as the share changes. We can still estimate an overall curve by comparing
districts. The same logic rules out a separate slope for presidential
vote share within each state.

The next biggest cut came from groups with fewer than 10 respondents.
For example, a district and age category can each have plenty of survey
respondents, while the particular district–age pair has too few to
estimate its own effect. The table counts the coefficients we dropped
for each reason. One dropped curve can account for many coefficients.


|Model        |Stage      | Fixed support| Random level support| Within-group slope support| Fixed collinearity| Redundant group intercepts| Linear hierarchy| Whole smooths| Spline level support|
|:------------|:----------|-------------:|--------------------:|--------------------------:|------------------:|--------------------------:|----------------:|-------------:|--------------------:|
|Presidential |No Vote    |             0|                5,237|                        923|                  1|                        872|                0|        21,338|                    0|
|Presidential |Democratic |             0|                5,737|                      1,801|                  5|                        872|                0|        21,843|                    0|
|Presidential |Republican |             0|                8,519|                      1,852|                  6|                        872|                0|        21,838|                    0|
|House        |No Vote    |             0|                6,077|                        923|                  1|                        872|                0|        21,348|                    0|
|House        |Democratic |             0|                6,844|                      1,801|                  5|                        872|                0|        21,863|                    0|
|House        |Republican |             0|                9,375|                      1,323|                  7|                        848|              514|        21,838|                    0|

*Table 5. Why coefficients were dropped.* Each row identifies the **model**
(presidential or House) and the **stage** (the choice being modeled).
The other columns count coefficients dropped for the reasons below.
Their sum matches the dropped-coefficient count in Table 4.

- **Fixed support.** Fewer than 10 respondents had a nonzero entry in
  that fixed-effect column.
- **Random level support.** A group, such as a district–age pair, had
  fewer than 10 respondents. Its random intercept and any slopes were
  left out of the fit together.
- **Within-group slope support.** A varying slope averaged fewer than
  10 informative respondents per retained group, so the slope was
  dropped across all groups. A respondent counts as informative if
  their slope value differs from the group's most common value.
- **Fixed collinearity.** A fixed coefficient was removed to bring the
  maximum condition index below 30.
- **Redundant group intercepts.** A crossed grouping repeated one of
  its parent groupings. For example, district × state makes the same
  groups as district alone.
- **Linear hierarchy.** An interaction was dropped because a main
  effect or lower-order interaction it depended on was removed.
- **Whole smooths.** Estimated spline coefficients dropped when the
  entire curve term was removed.
- **Spline level support.** Estimated spline coefficients left out for
  a particular group whose linear random effects were screened. The
  curve still has fitted deviations for the groups that qualified.


|Model        |Stage      |Redundant crossed groups                   |Fixed interactions removed by hierarchy |Sparse slopes (mean respondents)                                                                                                                                                            |Whole smooths removed                                                                                                                                                                                                                                                                                                                   |
|:------------|:----------|:------------------------------------------|:---------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Presidential |No Vote    |district_id:state_fips, district_id:lineup |—                                       |pres_share:state_fips (0), district_id:house_share (0), district_id:pres_share (0)                                                                                                          |v_s(pres_share, by = state_fips), v_s(house_share, by = district_id), v_s(pres_share, by = district_id), v_s(house_share, pres_share, type = "gKRLS")                                                                                                                                                                                   |
|Presidential |Democratic |district_id:state_fips, district_id:lineup |—                                       |pres_share:state_fips (0), district_id:house_share (0), district_id:pres_share (0), district_id:incumbent (0), district_id:on_ballot (0), lineup:on_ballot (0)                              |v_s(pres_share, by = state_fips), v_s(house_share, by = district_id), v_s(pres_share, by = district_id), v_s(house_share, pres_share, type = "gKRLS"), v_s(house_share, on_ballot, type = "gKRLS"), v_s(pres_share, incumbent, type = "gKRLS"), v_s(pres_share, on_ballot, type = "gKRLS")                                              |
|Presidential |Republican |district_id:state_fips, district_id:lineup |—                                       |pres_share:state_fips (0), on_ballot:state_fips (8.47), district_id:house_share (0), district_id:pres_share (0), district_id:incumbent (0), district_id:on_ballot (0), lineup:on_ballot (0) |v_s(pres_share, by = state_fips), v_s(house_share, by = district_id), v_s(pres_share, by = district_id), v_s(house_share, pres_share, type = "gKRLS"), v_s(house_share, incumbent, type = "gKRLS"), v_s(house_share, on_ballot, type = "gKRLS"), v_s(pres_share, incumbent, type = "gKRLS"), v_s(pres_share, on_ballot, type = "gKRLS") |
|House        |No Vote    |district_id:state_fips, district_id:lineup |—                                       |pres_share:state_fips (0), district_id:house_share (0), district_id:pres_share (0)                                                                                                          |v_s(pres_share, by = state_fips), v_s(house_share, by = district_id), v_s(pres_share, by = district_id), v_s(house_share, pres_share, type = "gKRLS")                                                                                                                                                                                   |
|House        |Democratic |district_id:state_fips, district_id:lineup |—                                       |pres_share:state_fips (0), district_id:house_share (0), district_id:pres_share (0), district_id:incumbent (0), district_id:on_ballot (0), lineup:on_ballot (0)                              |v_s(pres_share, by = state_fips), v_s(house_share, by = district_id), v_s(pres_share, by = district_id), v_s(house_share, pres_share, type = "gKRLS"), v_s(house_share, on_ballot, type = "gKRLS"), v_s(pres_share, incumbent, type = "gKRLS"), v_s(pres_share, on_ballot, type = "gKRLS")                                              |
|House        |Republican |district_id:state_fips, district_id:lineup |—                                       |pres_share:state_fips (0), district_id:house_share (0), district_id:pres_share (0), district_id:incumbent (0)                                                                               |v_s(pres_share, by = state_fips), v_s(house_share, by = district_id), v_s(pres_share, by = district_id), v_s(house_share, pres_share, type = "gKRLS"), v_s(house_share, incumbent, type = "gKRLS"), v_s(house_share, on_ballot, type = "gKRLS"), v_s(pres_share, incumbent, type = "gKRLS"), v_s(pres_share, on_ballot, type = "gKRLS") |

*Table 6. Names of the affected terms. One term can account for many
coefficient columns in Table 5. For a sparse slope, the parenthesized
number is its mean number of informative respondents per retained group.*

## 6. Inspect support and conditioning before fitting

The tables report the condition indices before and after pruning and show
which coefficients are involved. The figures below show how far pruning
moves each stage away from the cutoff of 30.


|Model        |Stage      |Before pruning         |After pruning |Fixed columns before → after |Fixed columns rejected                                                                                                                           |
|:------------|:----------|:----------------------|:-------------|:----------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------|
|Presidential |No Vote    |110.40                 |18.46         |4 → 3                        |house_share:pres_share                                                                                                                           |
|Presidential |Democratic |140,868,925,592,265    |26.92         |11 → 6                       |incumbent:on_ballot, house_share:on_ballot, pres_share:on_ballot, house_share:pres_share, pres_share:incumbent                                   |
|Presidential |Republican |1,144,378,722,009,732  |24.64         |11 → 5                       |house_share:on_ballot, incumbent:on_ballot, pres_share:on_ballot, house_share:pres_share, pres_share:incumbent, house_share:incumbent            |
|House        |No Vote    |110.14                 |18.47         |4 → 3                        |house_share:pres_share                                                                                                                           |
|House        |Democratic |17,815,865,119,032,030 |28.03         |11 → 6                       |incumbent:on_ballot, house_share:on_ballot, pres_share:on_ballot, house_share:pres_share, pres_share:incumbent                                   |
|House        |Republican |893,841,368,873,835    |19.13         |11 → 4                       |incumbent:on_ballot, house_share:on_ballot, pres_share:on_ballot, house_share:pres_share, pres_share:incumbent, house_share:incumbent, on_ballot |

*Table 7. Maximum condition index for each stage’s weighted fixed design
before and after backward pruning. The column counts describe that fixed
design alone. Random effects and spline bases are outside this check.*

Each row in Table 8 describes a weak direction in the weighted, scaled
fixed design before pruning. The singular value decomposition gives a
singular value for each direction. The condition index is the largest
singular value divided by the singular value for that direction. A
larger index means a weaker direction.


|Model        |Stage      |Condition index        |Affected coefficients and their individual variance shares                                                     |
|:------------|:----------|:----------------------|:--------------------------------------------------------------------------------------------------------------|
|Presidential |No Vote    |110.40                 |(Intercept) (98%), house_share (94%), pres_share (93%), house_share:pres_share (99%)                           |
|Presidential |Democratic |36.07                  |house_share:incumbent (42%), pres_share:incumbent (49%)                                                        |
|Presidential |Democratic |94.18                  |(Intercept) (21%), pres_share (22%), house_share:pres_share (73%), pres_share:incumbent (38%)                  |
|Presidential |Democratic |155.25                 |(Intercept) (60%), pres_share (63%), on_ballot (87%), house_share:pres_share (26%), pres_share:on_ballot (89%) |
|Presidential |Democratic |20,453,431             |(Intercept) (15%), house_share (100%), pres_share (11%), house_share:on_ballot (100%)                          |
|Presidential |Democratic |140,868,925,592,265    |incumbent (100%), incumbent:on_ballot (100%)                                                                   |
|Presidential |Republican |33.78                  |house_share:incumbent (89%)                                                                                    |
|Presidential |Republican |47.43                  |pres_share:incumbent (50%)                                                                                     |
|Presidential |Republican |132.54                 |(Intercept) (15%), pres_share (16%), house_share:pres_share (83%), pres_share:incumbent (43%)                  |
|Presidential |Republican |239.09                 |(Intercept) (81%), pres_share (82%), on_ballot (95%), house_share:pres_share (16%), pres_share:on_ballot (98%) |
|Presidential |Republican |387,072,888,929,741    |incumbent (100%), incumbent:on_ballot (100%)                                                                   |
|Presidential |Republican |1,144,378,722,009,732  |house_share (100%), house_share:on_ballot (100%)                                                               |
|House        |No Vote    |110.14                 |(Intercept) (98%), house_share (94%), pres_share (93%), house_share:pres_share (99%)                           |
|House        |Democratic |36.19                  |house_share:incumbent (42%), pres_share:incumbent (49%)                                                        |
|House        |Democratic |97.05                  |(Intercept) (16%), pres_share (17%), house_share:pres_share (80%), pres_share:incumbent (40%)                  |
|House        |Democratic |169.10                 |(Intercept) (68%), pres_share (71%), on_ballot (87%), house_share:pres_share (19%), pres_share:on_ballot (89%) |
|House        |Democratic |25,243,827             |(Intercept) (13%), house_share (100%), house_share:on_ballot (100%)                                            |
|House        |Democratic |17,815,865,119,032,030 |incumbent (100%), incumbent:on_ballot (100%)                                                                   |
|House        |Republican |30.41                  |house_share:incumbent (30%)                                                                                    |
|House        |Republican |36.62                  |house_share:incumbent (62%)                                                                                    |
|House        |Republican |49.63                  |pres_share:incumbent (50%)                                                                                     |
|House        |Republican |139.03                 |house_share:pres_share (92%), pres_share:incumbent (46%)                                                       |
|House        |Republican |303.19                 |(Intercept) (89%), pres_share (91%), on_ballot (95%), pres_share:on_ballot (97%)                               |
|House        |Republican |558,850,570,096,225    |house_share (100%), house_share:on_ballot (100%)                                                               |
|House        |Republican |893,841,368,873,835    |incumbent (100%), incumbent:on_ballot (100%)                                                                   |

*Table 8. Share of each coefficient's estimation variance accounted for
by a weak direction. In the first row (Presidential, No Vote), the
weakest direction has a condition index of 110.40. It accounts for 94%
of the House-share coefficient's estimation variance and 99% of the
interaction coefficient's estimation variance. Each percentage is a
share of that coefficient's own variance. The design is held fixed
while outcome noise varies in a hypothetical linear regression. The
table shows directions with a condition index of at least 30. The
**table display cutoff** is **10% of a coefficient's estimation
variance**. A dash means no coefficient reached 10%. The **pruning
candidate cutoff** is **1% of the largest absolute entry** in the
right singular vector for the weakest direction. The intercept is
always kept.*

Let $X$ be the supported fixed design with each row weighted by the
square root of its trial count and each column scaled to unit length.
For Table 8, $X$ is the design before pruning. At each pruning step, $X$
contains the columns still retained after enforcing hierarchy. The
program computes $X=U\cdot\mathrm{diag}(s_1,\ldots,s_p)\cdot V^\top$, with the singular
values ordered largest to smallest.

**The loading of design column $j$ on direction $k$ is $V_{jk}$.**
This is the entry in row $j$ and column $k$ of $V$. For pruning, the
program uses the last column of $V$, which describes the weakest
direction. A non-intercept column $j$ enters the candidate set when

$$
|V_{jp}| \;\ge\; 0.01\cdot\max_i |V_{ip}|.
$$

The rule asks, “Is this column’s absolute loading at least 1/100 as
large as the biggest absolute loading in this direction?”
Using the largest loading as the reference lets the cutoff adapt to how
concentrated or spread out the loadings are in that direction.

The intercept participates in this calculation and is always kept. If
the cutoff yields an empty candidate set, all remaining non-intercept
columns enter the candidate set.

The **110.40** in the first row comes from $s_1/s_p$. To calculate
the **94%**, pick the House-share coefficient. For each singular
direction, square the House-share loading and multiply it by that
direction's squared condition index. The product is that direction's
contribution to the House-share coefficient's estimation variance, on
a common scale. The squared condition index measures how much the
direction amplifies noise variance, and the squared loading measures
how strongly that amplified noise carries into the House-share
estimate. Add those contributions across all directions, then divide
the weakest direction's contribution by that total.

In general, with condition index $\kappa_k=s_1/s_k$, the share of
coefficient $j$'s estimation variance attributable to direction $k$ is

$$
\frac{V_{jk}^2\cdot\kappa_k^2}{\sum_\ell \left(V_{j\ell}^2\cdot\kappa_\ell^2\right)}.
$$

The code uses $V_{jk}^2/s_k^2$ for each contribution. Since
$\kappa_k^2=s_1^2/s_k^2$, the common factor $s_1^2$ cancels in the ratio.

### How to read the figures

Start with the top-right panel of Figure 1, for presidential No Vote.
We want to know whether the survey can tell the intercept, the two vote
shares, and their interaction apart. A high condition index means there
is a way to change several coefficients together while barely changing
the predictions. That makes it hard to tell how much of the prediction
belongs to each coefficient.

The orange dots list those indices before pruning. The largest goes at
position 1, the second-largest at position 2, and so on. The first orange
dot is **110.40**, well above the dashed line at 30. After dropping the
interaction between the two vote shares, we calculate a fresh list and
sort it again. The first green dot is **18.46**. Since even the largest
green value is below 30, all the other green values are below 30 too.
The horizontal positions are places in a sorted list. They do not track
a particular predictor or show the order in which we removed terms.


![Presidential model support and condition indices](assets/us-2026/prefit-presidential.png)

*Figure 1. Pre-fit checks for the presidential model.* Each row shows one vote choice being modeled. **Left.** Pick a respondent count on the horizontal axis. The curve tells you what fraction of the groups has that many respondents or fewer. A group might be a district or a district–age pair. Groups with fewer than 10 respondents are screened. **Right.** Orange shows the indices before pruning, and green shows them after pruning. Each color puts its largest value first. Read the first green dot in each row to check that the retained design clears the cutoff. Open triangles mark values above 200. Table 8 gives those values in full. The largest green value in each row ranges from 18.46 to 26.92.


![House model support and condition indices](assets/us-2026/prefit-house.png)

*Figure 2. Pre-fit checks for the house model.* Each row shows one vote choice being modeled. **Left.** Pick a respondent count on the horizontal axis. The curve tells you what fraction of the groups has that many respondents or fewer. A group might be a district or a district–age pair. Groups with fewer than 10 respondents are screened. **Right.** Orange shows the indices before pruning, and green shows them after pruning. Each color puts its largest value first. Read the first green dot in each row to check that the retained design clears the cutoff. Open triangles mark values above 200. Table 8 gives those values in full. The largest green value in each row ranges from 18.47 to 28.03.

## 7. Run the national models

The national run uses 125 posterior draws. Set
`SYNTHFRAME_US_FIT=true` to include the model results and predictions.
A fresh full run fits both national models from the three tracked inputs,
then predicts and calibrates the joint frame. On a memory-limited Codespace,
you can inspect the completed Markdown report and its figures directly or run
the pre-fit checks. For a pre-fit render, set `SYNTHFRAME_US_FIT=false`
immediately before `knitr::knit()` and use
`output = "vignettes/vignette_us_2026_prefit.md"` to preserve the full report.
The pre-fit render regenerates the two pre-fit figures in
`vignettes/assets/us-2026/`. A full render writes its figures there as well.

To use fits produced on another machine, place `pres_fit.rds` and
`house_fit.rds` under `data/output/us 2026/models/`, or in the `models/`
subdirectory of your `SYNTHFRAME_US_OUTPUT_DIR`. Set both
`SYNTHFRAME_US_FIT=true` and `SYNTHFRAME_US_REUSE_FITS=true` before knitting.
The vignette reuses each model whose inputs, settings and fitting code match.
A missing or unverified file starts a fresh fit for that model. Reusing the
fits still runs prediction and, by default, calibration.



### Read the fitted formulas and coefficient counts

The next snippet defines two reporting functions. They translate the fitted
formula's group labels back to the predictor names and use the fitted
coefficient vectors to count the effects each stage actually estimated.

*Snippet 4. Decode fitted formula labels and count actual coefficient rows by predictor combination.*


``` r
group_variables <- function(stage, group) {
  # A crossed group records its component variables. A single factor names itself.
  components <- stage$grouping_columns[[group]]
  if (is.null(components)) group else components
}

display_fitted_formula <- function(stage) {
  fit <- stage$fit
  # Use the solver formula after screening and substitute labels for display.
  decode <- function(term) {
    if (is.symbol(term) && as.character(term) %in% names(stage$grouping_columns)) {
      return(as.name(paste(group_variables(stage, as.character(term)), collapse = " × ")))
    }
    if (!is.call(term)) return(term)
    if (identical(term[[1L]], as.name("|"))) {
      # Recover the intercept and slopes encoded in a support-masked random term.
      screened <- fit$screened_random[[as.character(term[[3L]])]]
      if (!is.null(screened)) {
        term[[2L]] <- str2lang(paste(screened$slopes, collapse = " + "))
        label <- paste(group_variables(stage, screened$group), collapse = " × ")
        term[[3L]] <- as.name(paste(label, "[supported levels]"))
      }
    }
    if (identical(term[[1L]], as.name("v_s"))) {
      # A grouped smooth may also use a masked factor for supported levels.
      args <- as.list(term)
      by <- as.character(args$by)
      if (length(by) && by %in% names(fit$screened_smooth_groups)) {
        group <- unname(fit$screened_smooth_groups[by])
        label <- paste(group_variables(stage, group), collapse = " × ")
        args$by <- as.name(paste(label, "[supported levels]"))
        term <- as.call(args)
      }
    }
    as.call(lapply(as.list(term), decode))
  }
  cat(deparse(decode(fit$formula$formula), width.cutoff = 90L), sep = "\n")
}

retained_stage_counts <- function(stage) {
  fit <- stage$fit
  # Apply one predictor order so, for example, age × state and state × age combine.
  combination <- function(variables) {
    variables <- setdiff(variables, c("1", "(Intercept)"))
    ordered <- names(stage$predictor_names)[stage$predictor_names %in% variables]
    stopifnot(length(ordered) == length(unique(variables)))
    if (length(ordered)) paste(ordered, collapse = " × ") else "(Intercept)"
  }
  # Match the fitted fixed coefficients to their terms using the design's assignment index.
  fixed_terms <- stats::delete.response(fit$formula$fe_terms)
  design <- stats::model.matrix(fixed_terms, stage$data)
  fixed_columns <- match(rownames(fit$beta$mean), colnames(design))
  stopifnot(!anyNA(fixed_columns))
  terms <- c("(Intercept)", attr(fixed_terms, "term.labels"))
  labels <- terms[attr(design, "assign")[fixed_columns] + 1L]
  fixed <- vapply(labels, function(label) {
    if (label == "(Intercept)") return(combination(character()))
    contrast <- stage$fixed_contrasts[[label]]
    combination(if (is.null(contrast)) all.vars(str2lang(label)) else contrast$predictor)
  }, character(1))

  # Each fitted random or spline block points to its rows in the coefficient vector.
  blocks <- fit$internal_parameters$cyclical_pos
  names(blocks) <- names(fit$internal_parameters$names_of_RE)
  random_and_smooth <- unlist(lapply(names(blocks), function(block) {
    indices <- blocks[[block]]
    if (startsWith(block, "spline-")) {
      # A spline base uses its numeric predictors. A deviation also uses its by-factor.
      number <- as.integer(sub("^.*-([0-9]+)-(base|int)$", "\\1", block))
      spec <- fit$formula$interpret_gam$smooth.spec[[number]]
      by <- if (endsWith(block, "-int")) spec$by
      if (length(by) && by %in% names(fit$screened_smooth_groups)) {
        by <- unname(fit$screened_smooth_groups[by])
      }
      variables <- c(spec$term, if (length(by)) group_variables(stage, by))
      return(rep(combination(variables), length(indices)))
    }
    # Random coefficient labels carry the group, slope, and retained level.
    names_at_level <- rownames(fit$alpha$mean)[indices]
    slopes <- vapply(strsplit(names_at_level, " @ ", fixed = TRUE), `[[`, character(1), 2L)
    screened <- fit$screened_random[[block]]
    group <- block
    if (!is.null(screened)) {
      group <- screened$group
      slopes <- screened$slopes[match(slopes, screened$columns)]
      stopifnot(!anyNA(slopes))
    }
    vapply(strsplit(slopes, ":", fixed = TRUE), function(slope) {
      combination(c(group_variables(stage, group), slope))
    }, character(1))
  }), use.names = FALSE)
  # Every coefficient row must appear exactly once in the count.
  counts <- base::table(c(fixed, random_and_smooth))
  stopifnot(sum(counts) == nrow(fit$beta$mean) + nrow(fit$alpha$mean))
  counts
}
```

## 8. Fit and extend with 2024 presidential vote

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

The formulas show the solver terms after screening. Each stage compares
successes for its named outcome with the remaining eligible respondents.
A crossed group displays both predictors, such as `district_id × age_cat`.
`[supported levels]` marks groups fitted only at levels that cleared the
survey-support cutoff.

*Snippet 5. Fit or load the presidential stages, then report convergence and the terms that entered each fit.*


``` r
# Use the shared country output folder unless the caller specifies a different path.
out <- Sys.getenv("SYNTHFRAME_US_OUTPUT_DIR",
                  unset = file.path("data", "output", basename(country)))
outputs <- case_study_outputs(out)
requested_depth <- 2L
max_iterations <- 100L
parameter_tolerance <- 0.01
minimum_support <- 10L
depth_support_dir <- file.path(outputs$diagnostics, "depth_support")
stage_columns <- c("outcome", "observations", "fitted_depth", "estimated_parameters",
                   "dropped_fixed", "dropped_scalar_random", "dropped_correlated_random",
                   "dropped_smooth", "estimated_retained_parameters", "fixed_condition_after",
                   "iterations_used", "max_parameter_change")
control <- vglmer::vglmer_control(quiet = FALSE, quiet_rho = FALSE, print_prog = 1L)
# Both elections use the same fitting specification with their own survey columns and seed.
fit_us_model <- function(filename, extension, predictors, seed, label) {
  args <- list(
    auxiliary_survey = survey, extension_variable = extension, area_variable = "district_id",
    area_covariates = area_covariates, no_vote = "No Vote", outcome_order = vote_order,
    predictors = predictors, depth = requested_depth, splines = TRUE, control = control,
    tolerance_parameters = parameter_tolerance, tolerance_elbo = 0, max_iterations = max_iterations,
    minimum_support = minimum_support, depth_support_dir = depth_support_dir,
    interaction_support_log = file.path(outputs$diagnostics, paste0(label, "_interaction_support.csv")),
    log_file = file.path(outputs$diagnostics, paste0(label, "_fit_log.csv"))
  )
  load_or_fit_us_model(file.path(outputs$models, filename), args, seed)
}
pres_fit <- fit_us_model("pres_fit.rds", pres, predictors, 2026L, "presidential")
# Report the depth, screening counts, and convergence outcome for each stage.
knitr::kable(pres_fit$fit_log[stage_columns], format = "pipe")
```



|outcome    | observations| fitted_depth| estimated_parameters| dropped_fixed| dropped_scalar_random| dropped_correlated_random| dropped_smooth| estimated_retained_parameters| fixed_condition_after| iterations_used| max_parameter_change|
|:----------|------------:|------------:|--------------------:|-------------:|---------------------:|-------------------------:|--------------:|-----------------------------:|---------------------:|---------------:|--------------------:|
|No Vote    |        57603|            2|                41860|             0|                  6109|                       923|          21338|                         13489|              18.46013|              27|            0.0069440|
|Democratic |        52767|            2|                43534|             0|                  6609|                      1801|          21843|                         13277|              26.91919|              47|            0.0069129|
|Republican |        23922|            2|                42535|             0|                  9391|                      1852|          21838|                          9450|              24.63738|              80|            0.0046068|

*Table 9. Presidential stage design and convergence diagnostics.*

*Snippet 6. Display the three fitted presidential formulas with crossed groups named explicitly.*


``` r
# Show the formula used after screening, with original names for crossed groups.
for (stage in names(pres_fit$stages)) {
  cat(stage, "\n")
  display_fitted_formula(pres_fit$stages[[stage]])
  cat("\n")
}
```

```
## No Vote
## cbind(.success, .failure) ~ 1 + house_share + pres_share + (1 | district_id) + (1 + house_share |
##     state_fips) + (1 + house_share + pres_share | age_cat) + (1 + house_share + pres_share |
##     gender_cat) + (1 + house_share + pres_share | race_cat) + (1 + house_share + pres_share |
##     hispanic_cat) + (1 + house_share + pres_share | educ_cat) + (1 + house_share + pres_share |
##     lineup) + (1 | `district_id × age_cat [supported levels]`) + (1 | `district_id × gender_cat`) +
##     (1 | `district_id × race_cat [supported levels]`) + (1 | `district_id × hispanic_cat [supported levels]`) +
##     (1 | `district_id × educ_cat [supported levels]`) + (1 | `state_fips × age_cat [supported levels]`) +
##     (1 | `state_fips × gender_cat`) + (1 | `state_fips × race_cat [supported levels]`) +
##     (1 | `state_fips × hispanic_cat [supported levels]`) + (1 | `state_fips × educ_cat [supported levels]`) +
##     (1 | `state_fips × lineup`) + (1 | `age_cat × gender_cat`) + (1 | `age_cat × race_cat [supported levels]`) +
##     (1 | `age_cat × hispanic_cat`) + (1 | `age_cat × educ_cat`) + (1 | `age_cat × lineup [supported levels]`) +
##     (1 | `gender_cat × race_cat`) + (1 | `gender_cat × hispanic_cat`) + (1 | `gender_cat × educ_cat`) +
##     (1 | `gender_cat × lineup`) + (1 | `race_cat × hispanic_cat`) + (1 | `race_cat × educ_cat`) +
##     (1 | `race_cat × lineup [supported levels]`) + (1 | `hispanic_cat × educ_cat`) + (1 |
##     `hispanic_cat × lineup`) + (1 | `educ_cat × lineup`) + v_s(house_share, by = state_fips) +
##     v_s(house_share, by = age_cat) + v_s(pres_share, by = age_cat) + v_s(house_share, by = gender_cat) +
##     v_s(pres_share, by = gender_cat) + v_s(house_share, by = race_cat) + v_s(pres_share, by = race_cat) +
##     v_s(house_share, by = hispanic_cat) + v_s(pres_share, by = hispanic_cat) + v_s(house_share,
##     by = educ_cat) + v_s(pres_share, by = educ_cat) + v_s(house_share, by = lineup) + v_s(pres_share,
##     by = lineup)
##
## Democratic
## cbind(.success, .failure) ~ 1 + house_share + pres_share + incumbent + on_ballot + house_share:incumbent +
##     (1 | district_id) + (1 + house_share + incumbent + on_ballot | state_fips) + (1 + house_share +
##     pres_share + incumbent + on_ballot | age_cat) + (1 + house_share + pres_share + incumbent +
##     on_ballot | gender_cat) + (1 + house_share + pres_share + incumbent + on_ballot | race_cat) +
##     (1 + house_share + pres_share + incumbent + on_ballot | hispanic_cat) + (1 + house_share +
##     pres_share + incumbent + on_ballot | educ_cat) + (1 + house_share + pres_share + incumbent |
##     lineup) + (1 | `district_id × age_cat [supported levels]`) + (1 | `district_id × gender_cat`) +
##     (1 | `district_id × race_cat [supported levels]`) + (1 | `district_id × hispanic_cat [supported levels]`) +
##     (1 | `district_id × educ_cat [supported levels]`) + (1 | `state_fips × age_cat [supported levels]`) +
##     (1 | `state_fips × gender_cat`) + (1 | `state_fips × race_cat [supported levels]`) +
##     (1 | `state_fips × hispanic_cat [supported levels]`) + (1 | `state_fips × educ_cat [supported levels]`) +
##     (1 | `state_fips × lineup`) + (1 | `age_cat × gender_cat`) + (1 | `age_cat × race_cat [supported levels]`) +
##     (1 | `age_cat × hispanic_cat`) + (1 | `age_cat × educ_cat`) + (1 | `age_cat × lineup [supported levels]`) +
##     (1 | `gender_cat × race_cat`) + (1 | `gender_cat × hispanic_cat`) + (1 | `gender_cat × educ_cat`) +
##     (1 | `gender_cat × lineup`) + (1 | `race_cat × hispanic_cat`) + (1 | `race_cat × educ_cat`) +
##     (1 | `race_cat × lineup [supported levels]`) + (1 | `hispanic_cat × educ_cat`) + (1 |
##     `hispanic_cat × lineup`) + (1 | `educ_cat × lineup [supported levels]`) + v_s(house_share,
##     by = state_fips) + v_s(house_share, by = age_cat) + v_s(pres_share, by = age_cat) + v_s(house_share,
##     by = gender_cat) + v_s(pres_share, by = gender_cat) + v_s(house_share, by = race_cat) +
##     v_s(pres_share, by = race_cat) + v_s(house_share, by = hispanic_cat) + v_s(pres_share,
##     by = hispanic_cat) + v_s(house_share, by = educ_cat) + v_s(pres_share, by = educ_cat) +
##     v_s(house_share, by = lineup) + v_s(pres_share, by = lineup) + v_s(house_share, incumbent,
##     type = "gKRLS")
##
## Republican
## cbind(.success, .failure) ~ 1 + house_share + pres_share + incumbent + on_ballot + (1 | district_id) +
##     (1 + house_share + incumbent | state_fips) + (1 + house_share + pres_share + incumbent +
##     on_ballot | age_cat) + (1 + house_share + pres_share + incumbent + on_ballot | gender_cat) +
##     (1 + house_share + pres_share + incumbent + on_ballot | race_cat) + (1 + house_share +
##     pres_share + incumbent + on_ballot | hispanic_cat) + (1 + house_share + pres_share + incumbent +
##     on_ballot | educ_cat) + (1 + house_share + pres_share + incumbent | lineup) + (1 | `district_id × age_cat [supported levels]`) +
##     (1 | `district_id × gender_cat [supported levels]`) + (1 | `district_id × race_cat [supported levels]`) +
##     (1 | `district_id × hispanic_cat [supported levels]`) + (1 | `district_id × educ_cat [supported levels]`) +
##     (1 | `state_fips × age_cat [supported levels]`) + (1 | `state_fips × gender_cat [supported levels]`) +
##     (1 | `state_fips × race_cat [supported levels]`) + (1 | `state_fips × hispanic_cat [supported levels]`) +
##     (1 | `state_fips × educ_cat [supported levels]`) + (1 | `state_fips × lineup`) + (1 |
##     `age_cat × gender_cat`) + (1 | `age_cat × race_cat [supported levels]`) + (1 | `age_cat × hispanic_cat`) +
##     (1 | `age_cat × educ_cat [supported levels]`) + (1 | `age_cat × lineup [supported levels]`) +
##     (1 | `gender_cat × race_cat`) + (1 | `gender_cat × hispanic_cat`) + (1 | `gender_cat × educ_cat`) +
##     (1 | `gender_cat × lineup`) + (1 | `race_cat × hispanic_cat`) + (1 | `race_cat × educ_cat [supported levels]`) +
##     (1 | `race_cat × lineup [supported levels]`) + (1 | `hispanic_cat × educ_cat`) + (1 |
##     `hispanic_cat × lineup`) + (1 | `educ_cat × lineup [supported levels]`) + v_s(house_share,
##     by = state_fips) + v_s(house_share, by = age_cat) + v_s(pres_share, by = age_cat) + v_s(house_share,
##     by = gender_cat) + v_s(pres_share, by = gender_cat) + v_s(house_share, by = race_cat) +
##     v_s(pres_share, by = race_cat) + v_s(house_share, by = hispanic_cat) + v_s(pres_share,
##     by = hispanic_cat) + v_s(house_share, by = educ_cat) + v_s(pres_share, by = educ_cat) +
##     v_s(house_share, by = lineup) + v_s(pres_share, by = lineup)
```

In the presidential fit, 3
of 3 stages recorded a final maximum parameter
change below 0.01.

*Snippet 7. Expand presidential choice within one state and reuse the same coefficient draws across states.*


``` r
# Retain draws for fitted coefficients and new levels across all state batches.
pres_draw_state <- new.env(parent = emptyenv())
predict_presidential_state <- function(original) {
  prediction <- predict_sequential_logit(
    pres_fit, frame = original, n_draws = n_draws, write_files = FALSE,
    draw_state = pres_draw_state, batch_size = 4000L
  )
  # Convert presidential probabilities to counts on the mother-frame scale.
  aggregate_extended_frame(original, prediction, by = names(prediction$cells))
}
```

## 9. Fit and extend with 2024 House vote

The House model adds the reported presidential choice to the CES predictors.
In the population frame it uses the presidential categories created in the
first extension. Each conditional House probability multiplies the expected
population in its presidential category in the same draw. This produces
16 joint vote combinations per original cell. Fit on the national CES and
predict both outcomes one state at a time to limit memory use. Each model
reuses the national coefficient draws in every prediction batch. For
interaction levels found only in the population frame, prediction draws
from the fitted variance distribution and reuses the sampled effect
whenever that level appears again.

*Snippet 8. Fit or load House choice conditional on presidential choice, then report convergence and fitted terms.*


``` r
# Release the saved presidential fit while fitting the House stages.
rm(pres_fit)
invisible(gc())
house_fit <- fit_us_model("house_fit.rds", house, c(predictors, pres), 2027L, "house")
# Both models are needed for the coefficient table and joint prediction.
pres_fit <- readRDS(file.path(outputs$models, "pres_fit.rds"))
# Report the same stage diagnostics and decoded formulas as for the presidential fit.
knitr::kable(house_fit$fit_log[stage_columns], format = "pipe")
```



|outcome    | observations| fitted_depth| estimated_parameters| dropped_fixed| dropped_scalar_random| dropped_correlated_random| dropped_smooth| estimated_retained_parameters| fixed_condition_after| iterations_used| max_parameter_change|
|:----------|------------:|------------:|--------------------:|-------------:|---------------------:|-------------------------:|--------------:|-----------------------------:|---------------------:|---------------:|--------------------:|
|No Vote    |        55413|            2|                44164|             0|                  6949|                       923|          21348|                         14943|              18.46600|             100|            0.4310780|
|Democratic |        48330|            2|                45570|             0|                  7716|                      1801|          21863|                         14186|              28.02934|             100|            0.3070906|
|Republican |        21552|            2|                43894|             0|                 10163|                      1897|          21838|                          9991|              19.13335|              95|            0.0074460|

*Table 10. House stage design and convergence diagnostics.*

*Snippet 9. Display the three fitted House formulas with crossed groups named explicitly.*


``` r
for (stage in names(house_fit$stages)) {
  cat(stage, "\n")
  display_fitted_formula(house_fit$stages[[stage]])
  cat("\n")
}
```

```
## No Vote
## cbind(.success, .failure) ~ 1 + house_share + pres_share + (1 | district_id) + (1 + house_share |
##     state_fips) + (1 + house_share + pres_share | age_cat) + (1 + house_share + pres_share |
##     gender_cat) + (1 + house_share + pres_share | race_cat) + (1 + house_share + pres_share |
##     hispanic_cat) + (1 + house_share + pres_share | educ_cat) + (1 + house_share + pres_share |
##     vote_pres_2024) + (1 + house_share + pres_share | lineup) + (1 | `district_id × age_cat [supported levels]`) +
##     (1 | `district_id × gender_cat`) + (1 | `district_id × race_cat [supported levels]`) +
##     (1 | `district_id × hispanic_cat [supported levels]`) + (1 | `district_id × educ_cat [supported levels]`) +
##     (1 | `district_id × vote_pres_2024 [supported levels]`) + (1 | `state_fips × age_cat [supported levels]`) +
##     (1 | `state_fips × gender_cat`) + (1 | `state_fips × race_cat [supported levels]`) +
##     (1 | `state_fips × hispanic_cat [supported levels]`) + (1 | `state_fips × educ_cat [supported levels]`) +
##     (1 | `state_fips × vote_pres_2024 [supported levels]`) + (1 | `state_fips × lineup`) +
##     (1 | `age_cat × gender_cat`) + (1 | `age_cat × race_cat [supported levels]`) + (1 | `age_cat × hispanic_cat`) +
##     (1 | `age_cat × educ_cat`) + (1 | `age_cat × vote_pres_2024`) + (1 | `age_cat × lineup [supported levels]`) +
##     (1 | `gender_cat × race_cat`) + (1 | `gender_cat × hispanic_cat`) + (1 | `gender_cat × educ_cat`) +
##     (1 | `gender_cat × vote_pres_2024`) + (1 | `gender_cat × lineup`) + (1 | `race_cat × hispanic_cat`) +
##     (1 | `race_cat × educ_cat`) + (1 | `race_cat × vote_pres_2024`) + (1 | `race_cat × lineup [supported levels]`) +
##     (1 | `hispanic_cat × educ_cat`) + (1 | `hispanic_cat × vote_pres_2024`) + (1 | `hispanic_cat × lineup`) +
##     (1 | `educ_cat × vote_pres_2024`) + (1 | `educ_cat × lineup`) + (1 | `vote_pres_2024 × lineup [supported levels]`) +
##     v_s(house_share, by = state_fips) + v_s(house_share, by = age_cat) + v_s(pres_share, by = age_cat) +
##     v_s(house_share, by = gender_cat) + v_s(pres_share, by = gender_cat) + v_s(house_share,
##     by = race_cat) + v_s(pres_share, by = race_cat) + v_s(house_share, by = hispanic_cat) +
##     v_s(pres_share, by = hispanic_cat) + v_s(house_share, by = educ_cat) + v_s(pres_share,
##     by = educ_cat) + v_s(house_share, by = vote_pres_2024) + v_s(pres_share, by = vote_pres_2024) +
##     v_s(house_share, by = lineup) + v_s(pres_share, by = lineup)
##
## Democratic
## cbind(.success, .failure) ~ 1 + house_share + pres_share + incumbent + on_ballot + house_share:incumbent +
##     (1 | district_id) + (1 + house_share + incumbent + on_ballot | state_fips) + (1 + house_share +
##     pres_share + incumbent + on_ballot | age_cat) + (1 + house_share + pres_share + incumbent +
##     on_ballot | gender_cat) + (1 + house_share + pres_share + incumbent + on_ballot | race_cat) +
##     (1 + house_share + pres_share + incumbent + on_ballot | hispanic_cat) + (1 + house_share +
##     pres_share + incumbent + on_ballot | educ_cat) + (1 + house_share + pres_share + incumbent +
##     on_ballot | vote_pres_2024) + (1 + house_share + pres_share + incumbent | lineup) + (1 |
##     `district_id × age_cat [supported levels]`) + (1 | `district_id × gender_cat`) + (1 |
##     `district_id × race_cat [supported levels]`) + (1 | `district_id × hispanic_cat [supported levels]`) +
##     (1 | `district_id × educ_cat [supported levels]`) + (1 | `district_id × vote_pres_2024 [supported levels]`) +
##     (1 | `state_fips × age_cat [supported levels]`) + (1 | `state_fips × gender_cat`) + (1 |
##     `state_fips × race_cat [supported levels]`) + (1 | `state_fips × hispanic_cat [supported levels]`) +
##     (1 | `state_fips × educ_cat [supported levels]`) + (1 | `state_fips × vote_pres_2024 [supported levels]`) +
##     (1 | `state_fips × lineup`) + (1 | `age_cat × gender_cat`) + (1 | `age_cat × race_cat [supported levels]`) +
##     (1 | `age_cat × hispanic_cat`) + (1 | `age_cat × educ_cat`) + (1 | `age_cat × vote_pres_2024 [supported levels]`) +
##     (1 | `age_cat × lineup [supported levels]`) + (1 | `gender_cat × race_cat`) + (1 | `gender_cat × hispanic_cat`) +
##     (1 | `gender_cat × educ_cat`) + (1 | `gender_cat × vote_pres_2024`) + (1 | `gender_cat × lineup`) +
##     (1 | `race_cat × hispanic_cat`) + (1 | `race_cat × educ_cat`) + (1 | `race_cat × vote_pres_2024 [supported levels]`) +
##     (1 | `race_cat × lineup [supported levels]`) + (1 | `hispanic_cat × educ_cat`) + (1 |
##     `hispanic_cat × vote_pres_2024`) + (1 | `hispanic_cat × lineup`) + (1 | `educ_cat × vote_pres_2024`) +
##     (1 | `educ_cat × lineup [supported levels]`) + (1 | `vote_pres_2024 × lineup [supported levels]`) +
##     v_s(house_share, by = state_fips) + v_s(house_share, by = age_cat) + v_s(pres_share, by = age_cat) +
##     v_s(house_share, by = gender_cat) + v_s(pres_share, by = gender_cat) + v_s(house_share,
##     by = race_cat) + v_s(pres_share, by = race_cat) + v_s(house_share, by = hispanic_cat) +
##     v_s(pres_share, by = hispanic_cat) + v_s(house_share, by = educ_cat) + v_s(pres_share,
##     by = educ_cat) + v_s(house_share, by = vote_pres_2024) + v_s(pres_share, by = vote_pres_2024) +
##     v_s(house_share, by = lineup) + v_s(pres_share, by = lineup) + v_s(house_share, incumbent,
##     type = "gKRLS")
##
## Republican
## cbind(.success, .failure) ~ 1 + house_share + pres_share + incumbent + (1 | `district_id [supported levels]`) +
##     (1 + house_share + incumbent | state_fips) + (1 + house_share + pres_share + incumbent |
##     age_cat) + (1 + house_share + pres_share + incumbent | gender_cat) + (1 + house_share +
##     pres_share + incumbent | race_cat) + (1 + house_share + pres_share + incumbent | hispanic_cat) +
##     (1 + house_share + pres_share + incumbent | educ_cat) + (1 + house_share + pres_share +
##     incumbent | vote_pres_2024) + (1 + house_share + pres_share + incumbent | lineup) + (1 |
##     `district_id × age_cat [supported levels]`) + (1 | `district_id × gender_cat [supported levels]`) +
##     (1 | `district_id × race_cat [supported levels]`) + (1 | `district_id × hispanic_cat [supported levels]`) +
##     (1 | `district_id × educ_cat [supported levels]`) + (1 | `district_id × vote_pres_2024 [supported levels]`) +
##     (1 | `state_fips × age_cat [supported levels]`) + (1 | `state_fips × gender_cat [supported levels]`) +
##     (1 | `state_fips × race_cat [supported levels]`) + (1 | `state_fips × hispanic_cat [supported levels]`) +
##     (1 | `state_fips × educ_cat [supported levels]`) + (1 | `state_fips × vote_pres_2024 [supported levels]`) +
##     (1 | `state_fips × lineup [supported levels]`) + (1 | `age_cat × gender_cat`) + (1 |
##     `age_cat × race_cat [supported levels]`) + (1 | `age_cat × hispanic_cat`) + (1 | `age_cat × educ_cat [supported levels]`) +
##     (1 | `age_cat × vote_pres_2024 [supported levels]`) + (1 | `age_cat × lineup [supported levels]`) +
##     (1 | `gender_cat × race_cat`) + (1 | `gender_cat × hispanic_cat`) + (1 | `gender_cat × educ_cat`) +
##     (1 | `gender_cat × vote_pres_2024`) + (1 | `gender_cat × lineup`) + (1 | `race_cat × hispanic_cat`) +
##     (1 | `race_cat × educ_cat [supported levels]`) + (1 | `race_cat × vote_pres_2024 [supported levels]`) +
##     (1 | `race_cat × lineup [supported levels]`) + (1 | `hispanic_cat × educ_cat`) + (1 |
##     `hispanic_cat × vote_pres_2024`) + (1 | `hispanic_cat × lineup`) + (1 | `educ_cat × vote_pres_2024`) +
##     (1 | `educ_cat × lineup [supported levels]`) + (1 | `vote_pres_2024 × lineup [supported levels]`) +
##     v_s(house_share, by = state_fips) + v_s(house_share, by = age_cat) + v_s(pres_share, by = age_cat) +
##     v_s(house_share, by = gender_cat) + v_s(pres_share, by = gender_cat) + v_s(house_share,
##     by = race_cat) + v_s(pres_share, by = race_cat) + v_s(house_share, by = hispanic_cat) +
##     v_s(pres_share, by = hispanic_cat) + v_s(house_share, by = educ_cat) + v_s(pres_share,
##     by = educ_cat) + v_s(house_share, by = vote_pres_2024) + v_s(pres_share, by = vote_pres_2024) +
##     v_s(house_share, by = lineup) + v_s(pres_share, by = lineup)
```

In the House fit, 1
of 3 stages recorded a final maximum parameter
change below 0.01. Table 10 gives the iteration count
and final change for each stage.

### Retained coefficients by predictor combination

Each column is one fitted stage. The bottom row totals its fitted coefficients.


|Combination                   | Pres. No Vote| Pres. Democratic| Pres. Republican| House No Vote| House Democratic| House Republican|
|:-----------------------------|-------------:|----------------:|----------------:|-------------:|----------------:|----------------:|
|state_fips × house_share      |         1,836|            1,836|            1,836|         1,836|            1,836|            1,836|
|district_id × educ_cat        |         2,070|            2,010|            1,045|         2,042|            1,951|              930|
|district_id × age_cat         |         2,606|            2,205|              431|         2,438|            1,884|              399|
|district_id × gender_cat      |           872|              872|              837|           872|              872|              787|
|district_id × race_cat        |           998|              954|              486|           978|              890|              445|
|district_id × hispanic_cat    |           686|              668|              513|           672|              639|              463|
|state_fips × age_cat          |           634|              621|              504|           632|              612|              468|
|age_cat × house_share         |           504|              504|              504|           504|              504|              504|
|district_id                   |           436|              436|              436|           436|              436|              424|
|district_id × vote_pres_2024  |             0|                0|                0|         1,117|              862|              448|
|state_fips × educ_cat         |           292|              289|              266|           291|              286|              257|
|house_share                   |           246|              246|              246|           281|              281|              281|
|educ_cat × house_share        |           216|              216|              216|           216|              216|              216|
|house_share × lineup          |           216|              216|              216|           216|              216|              216|
|age_cat × pres_share          |           182|              182|              182|           182|              182|              182|
|race_cat × house_share        |           180|              180|              180|           180|              180|              180|
|state_fips × race_cat         |           191|              181|              139|           189|              177|              131|
|state_fips × gender_cat       |           102|              102|              100|           102|              102|              101|
|state_fips × lineup           |            93|               93|               93|            93|               93|               91|
|state_fips × hispanic_cat     |            96|               96|               89|            95|               93|               86|
|age_cat × educ_cat            |            84|               84|               82|            84|               84|               82|
|pres_share                    |            73|               73|               73|            85|               85|               85|
|educ_cat × pres_share         |            78|               78|               78|            78|               78|               78|
|pres_share × lineup           |            78|               78|               78|            78|               78|               78|
|age_cat × lineup              |            82|               82|               75|            81|               80|               62|
|state_fips × vote_pres_2024   |             0|                0|                0|           188|              151|              122|
|gender_cat × house_share      |            72|               72|               72|            72|               72|               72|
|hispanic_cat × house_share    |            72|               72|               72|            72|               72|               72|
|vote_pres_2024 × house_share  |             0|                0|                0|           144|              144|              144|
|age_cat × race_cat            |            69|               69|               66|            69|               69|               64|
|race_cat × pres_share         |            65|               65|               65|            65|               65|               65|
|state_fips                    |            51|               51|               51|            51|               51|               51|
|educ_cat × lineup             |            36|               35|               35|            36|               34|               32|
|state_fips × incumbent        |             0|               51|               51|             0|               51|               51|
|race_cat × educ_cat           |            30|               30|               29|            30|               30|               29|
|age_cat × gender_cat          |            28|               28|               28|            28|               28|               28|
|age_cat × hispanic_cat        |            28|               28|               28|            28|               28|               28|
|age_cat × vote_pres_2024      |             0|                0|                0|            56|               54|               48|
|gender_cat × pres_share       |            26|               26|               26|            26|               26|               26|
|hispanic_cat × pres_share     |            26|               26|               26|            26|               26|               26|
|vote_pres_2024 × pres_share   |             0|                0|                0|            52|               52|               52|
|race_cat × lineup             |            27|               26|               22|            27|               24|               22|
|state_fips × on_ballot        |             0|               51|                0|             0|               51|                0|
|age_cat                       |            14|               14|               14|            14|               14|               14|
|educ_cat × vote_pres_2024     |             0|                0|                0|            24|               24|               24|
|gender_cat × educ_cat         |            12|               12|               12|            12|               12|               12|
|gender_cat × lineup           |            12|               12|               12|            12|               12|               12|
|hispanic_cat × educ_cat       |            12|               12|               12|            12|               12|               12|
|hispanic_cat × lineup         |            12|               12|               12|            12|               12|               12|
|vote_pres_2024 × lineup       |             0|                0|                0|            23|               20|               19|
|gender_cat × race_cat         |            10|               10|               10|            10|               10|               10|
|race_cat × hispanic_cat       |            10|               10|               10|            10|               10|               10|
|house_share × incumbent       |             0|               29|                0|             0|               30|                0|
|race_cat × vote_pres_2024     |             0|                0|                0|            20|               19|               18|
|age_cat × incumbent           |             0|               14|               14|             0|               14|               14|
|age_cat × on_ballot           |             0|               14|               14|             0|               14|                0|
|educ_cat                      |             6|                6|                6|             6|                6|                6|
|lineup                        |             6|                6|                6|             6|                6|                6|
|race_cat                      |             5|                5|                5|             5|                5|                5|
|educ_cat × incumbent          |             0|                6|                6|             0|                6|                6|
|gender_cat × hispanic_cat     |             4|                4|                4|             4|                4|                4|
|gender_cat × vote_pres_2024   |             0|                0|                0|             8|                8|                8|
|hispanic_cat × vote_pres_2024 |             0|                0|                0|             8|                8|                8|
|lineup × incumbent            |             0|                6|                6|             0|                6|                6|
|race_cat × incumbent          |             0|                5|                5|             0|                5|                5|
|educ_cat × on_ballot          |             0|                6|                6|             0|                6|                0|
|race_cat × on_ballot          |             0|                5|                5|             0|                5|                0|
|gender_cat                    |             2|                2|                2|             2|                2|                2|
|hispanic_cat                  |             2|                2|                2|             2|                2|                2|
|vote_pres_2024                |             0|                0|                0|             4|                4|                4|
|gender_cat × incumbent        |             0|                2|                2|             0|                2|                2|
|hispanic_cat × incumbent      |             0|                2|                2|             0|                2|                2|
|vote_pres_2024 × incumbent    |             0|                0|                0|             0|                4|                4|
|(Intercept)                   |             1|                1|                1|             1|                1|                1|
|gender_cat × on_ballot        |             0|                2|                2|             0|                2|                0|
|hispanic_cat × on_ballot      |             0|                2|                2|             0|                2|                0|
|incumbent                     |             0|                1|                1|             0|                1|                1|
|vote_pres_2024 × on_ballot    |             0|                0|                0|             0|                4|                0|
|on_ballot                     |             0|                1|                1|             0|                1|                0|
|**Total**                     |        13,489|           13,135|            9,450|        14,943|           14,040|            9,991|

*Table 11. Actual fitted coefficients by predictor combination and stage.*

The House No Vote stage has **1,117**
district-by-presidential-choice random intercepts. The House Republican stage
has **448**
after the earlier stages have removed respondents from its risk set.
For `state_fips × house_share`, the House No Vote stage retains
**1,836** coefficients,
counting both state-specific slopes and any fitted spline deviations.

*Snippet 10. Expand House choice within each presidential category while keeping draw indices paired.*


``` r
# Keep the House coefficient draws aligned across all state batches.
house_draw_state <- new.env(parent = emptyenv())
predict_house_state <- function(incoming) {
  prediction <- predict_sequential_logit(
    house_fit, frame = incoming, write_files = FALSE,
    draw_state = house_draw_state, batch_size = 4000L
  )
  # Multiply conditional House probabilities by the matching presidential counts.
  aggregate_extended_frame(incoming, prediction, by = names(prediction$cells))
}
```

## 10. Predict the joint frame by state

The full run scores the pre-rake predictions, calibrates the state
presidential and district House targets, and saves the joint stratification
frame under `raked_frame/` in the run's output directory. Set
`SYNTHFRAME_US_RAKE=false` for a pre-rake benchmark-only run. Prediction
and calibration use state or DC batches by default. Statewide
presidential targets keep every district in a state together, even when
an override requests smaller batches.
The current state's pre-rake frame uses memory or temporary disk
according to available memory. Set `SYNTHFRAME_PRE_RAKE_STORAGE=keep`
to save all joint pre-rake partitions.
Set `SYNTHFRAME_US_RAKED_DIR` to use a different partition directory
when the fit output directory has limited free space.

*Snippet 11. Predict by state, jointly calibrate both vote margins, and save each verified partition.*

``` r
# Apply the same tolerance to election margins and mother-frame cells.
rake_tolerance <- 0.00000001
run_rake <- identical(Sys.getenv("SYNTHFRAME_US_RAKE", unset = "true"), "true")
raked_dir <- Sys.getenv("SYNTHFRAME_US_RAKED_DIR", unset = outputs$raked_frame)
if (run_rake) dir.create(raked_dir, recursive = TRUE, showWarnings = FALSE)
summary_dir <- file.path(outputs$diagnostics, "pre_rake_totals")
dir.create(summary_dir, showWarnings = FALSE)
states <- geographic_batches(frame, "district_id", parent_area, areas_per_batch, rake_by)
count_columns <- c("N", paste0("N_draw_", seq_len(n_draws)))
# Estimate the storage requirement before allocating any predicted partitions.
state_bytes <- pre_rake_frame_bytes(lengths(states) * length(choices)^2,
                                    n_draws, ncol(frame) + 1L)
pre_rake_mode <- Sys.getenv("SYNTHFRAME_PRE_RAKE_STORAGE", "auto")
storage <- vignette_pre_rake_storage(
  out, if (pre_rake_mode == "keep") sum(state_bytes) else max(state_bytes),
  final_directory = if (run_rake) raked_dir else out,
  final_bytes = if (run_rake) sum(state_bytes) else 0
)

# Reduce each block to group-outcome draw totals for the comparison plots.
plot_totals <- function(block, extension) {
  columns <- c(plot_targets[[extension]]$by, extension)
  keys <- block[columns]
  group <- frame_cell_keys(keys, keys)
  cbind(keys[!duplicated(group), , drop = FALSE],
        rowsum(block[count_columns], group, reorder = FALSE))
}

change_summaries <- if (run_rake) {
  # Check demographic margins nationally and selected subgroups within districts.
  groupings <- c(as.list(demographics), list(c("age_cat", "educ_cat")))
  district_groups <- list(c("district_id", "age_cat"), c("district_id", "educ_cat"))
  setNames(lapply(c(pres, house), function(outcome) {
    groups <- c(groupings, if (outcome == pres) list("district_id"), district_groups)
    partitioned_raking_change_summary(frame, "district_id", outcome, "No Vote", groups)
  }), c(pres, house))
}

summarize_states <- function() {
  lapply(names(states), function(state) {
    if (isTRUE(getOption("knitr.in.progress"))) {
      cat("Processing partition ", state, "\n", sep = "", file = stderr())
    }
    original <- frame[states[[state]], , drop = FALSE]
    # Predict presidential choice first, then House choice within each presidential category.
    incoming <- predict_presidential_state(original)
    joint <- predict_house_state(incoming)
    rm(incoming)
    stopifnot(nrow(joint) == nrow(original) * length(choices)^2L)
    # Use memory or scratch disk for the state’s pre-rake counts.
    joint_part <- storage$put(joint, paste0("state_", state))
    if (storage$mode == "temporary") on.exit(unlink(joint_part))
    before <- setNames(lapply(names(plot_targets), function(x) plot_totals(joint, x)),
                       names(plot_targets))
    extended_path <- if (storage$mode == "keep") joint_part
    pre_rake <- list(before = before, extended_path = extended_path)
    save_case_study_rds(pre_rake, file.path(summary_dir, paste0(state, ".rds")))
    if (!run_rake) return(pre_rake)
    rm(joint)
    # Use the same named configuration for the rake in every block.
    targets <- lapply(names(rake_targets), function(outcome) {
      by <- rake_by[[outcome]]
      table <- rake_targets[[outcome]]
      key <- frame_cell_keys(table[by], original[by])
      keep <- key %in% frame_cell_keys(unique(original[by]), original[by])
      table[keep, , drop = FALSE]
    }) |> setNames(names(rake_targets))

    extended <- storage$get(joint_part)
    raked <- rake_joint_extended_frame(extended, original, targets, by = rake_by,
                                       tolerance = rake_tolerance)
    # Sum over both elections and compare the mean and all draws with the source cell.
    cells <- post_rake_cell_comparison(raked, original, c(pres, house), include_max_error = TRUE)
    stopifnot(max(cells$max_abs_error) <= rake_tolerance)
    for (change in change_summaries) change$add(extended, raked, original)
    # Publish the verified partition only after the write succeeds.
    path <- file.path(raked_dir, paste0("state_", state, ".rds"))
    save_case_study_rds(raked, path)

    list(before = before, extended_path = extended_path,
         after = setNames(lapply(names(plot_targets), function(x) plot_totals(raked, x)),
                          names(plot_targets)),
         cells = cells)
  })
}

# Seed once before the national sequence to keep draw IDs aligned across states.
set.seed(2028)
summaries <- summarize_states()
if (storage$mode == "keep") {
  paths <- vapply(summaries, function(x) x$extended_path, character(1))
  prune_geographic_partitions(dirname(paths[1L]), paths, "^state_([0-9]+|batch-[0-9]+)[.]rds$")
}
storage$cleanup()
```



## 11. Compare predictions with election benchmarks

The panels compare the pre-rake predictions to the selected benchmarks,
with posterior intervals from the draw counts. When raking is enabled they
also show the post-rake comparison. The fit output directory contains a
metrics CSV for each comparison. Presidential
shares are marginalized over House vote. House shares are marginalized over
presidential vote. For party panels, divide both the predictions and targets
by turnout at their own comparison geography. Draw intervals divide each
draw's party count by its own turnout before taking quantiles. Overall pools
those turnout-conditional party comparisons. The No Vote panel keeps the
population denominator. The rake still uses population-share targets.
The default benchmarks also enter model fitting as area predictors, so
agreement with them measures calibration.

*Snippet 12. Compare the predicted shares and draw intervals with election benchmarks before and after calibration.*


``` r
figure_dir <- file.path("vignettes", "assets", "us-2026")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
fits <- setNames(list(pres_fit, house_fit), c(pres, house))
elections <- setNames(c("Presidential vote", "House vote"), c(pres, house))
comparison <- 0L
for (phase in c("before", if (run_rake) "after")) {
  for (outcome in names(plot_targets)) {
    # Pool counts at the benchmark geography for this election.
    specification <- plot_targets[[outcome]]
    totals <- do.call(rbind, lapply(summaries, function(x) x[[phase]][[outcome]]))
    figure <- file.path(figure_dir, paste0(outcome, "_", phase, "_rake.png"))
    check <- check_extended_frame(
      totals, specification$data, by = specification$by,
      extension_variable = outcome, outcome_order = fits[[outcome]]$outcome_order,
      no_vote = fits[[outcome]]$no_vote,
      plot_file = figure, coverage_tolerance = if (phase == "after") rake_tolerance else 0
    )
    # Pair the table and figure by election and calibration step.
    comparison <- comparison + 1L
    caption <- paste(elections[[outcome]], phase, "raking")
    print(knitr::kable(check$metrics, format = "pipe", digits = 4))
    cat("\n*Table ", 11L + comparison, ". ", caption,
        " – errors and 90% interval coverage against the election benchmarks.*\n\n", sep = "")
    utils::write.csv(check$metrics, file.path(outputs$diagnostics,
                                             paste0(outcome, "_", phase, "_rake_metrics.csv")))
    figure_caption <- paste(caption, "– benchmark shares versus predictions and 90% intervals.")
    cat("\n![", figure_caption, "](", sub("^vignettes/", "", figure), ")\n\n",
        "*Figure ", 2L + comparison, ". ", figure_caption, "*\n\n", sep = "")
  }
}
```



|           |  bias_pp| RMSE_pp| spearman| coverage_90_pct|
|:----------|--------:|-------:|--------:|---------------:|
|Overall    |   0.0000|  6.8196|   0.8898|         11.7647|
|No Vote    | -25.5521| 25.8782|   0.9619|          0.0000|
|Democratic |   6.3910|  7.2456|   0.9637|         17.6471|
|Republican |  -8.3992|  9.0804|   0.9679|          5.8824|
|Other      |   2.0082|  2.1371|   0.6831|         11.7647|

*Table 12. Presidential vote before raking – errors and 90% interval coverage against the election benchmarks.*


![Presidential vote before raking – benchmark shares versus predictions and 90% intervals.](assets/us-2026/vote_pres_2024_before_rake.png)

*Figure 3. Presidential vote before raking – benchmark shares versus predictions and 90% intervals.*



|           |  bias_pp| RMSE_pp| spearman| coverage_90_pct|
|:----------|--------:|-------:|--------:|---------------:|
|Overall    |   0.0000|  7.5490|   0.9340|         22.0183|
|No Vote    | -22.9109| 23.5498|   0.9238|          0.4587|
|Democratic |   6.0130|  8.7103|   0.9661|         26.6055|
|Republican |  -6.7374|  9.3177|   0.9659|         21.5596|
|Other      |   0.7244|  2.8768|   0.6328|         17.8899|

*Table 13. House vote before raking – errors and 90% interval coverage against the election benchmarks.*


![House vote before raking – benchmark shares versus predictions and 90% intervals.](assets/us-2026/vote_house_2024_before_rake.png)

*Figure 4. House vote before raking – benchmark shares versus predictions and 90% intervals.*



|           | bias_pp| RMSE_pp| spearman| coverage_90_pct|
|:----------|-------:|-------:|--------:|---------------:|
|Overall    |       0|       0|        1|             100|
|No Vote    |       0|       0|        1|             100|
|Democratic |       0|       0|        1|             100|
|Republican |       0|       0|        1|             100|
|Other      |       0|       0|        1|             100|

*Table 14. Presidential vote after raking – errors and 90% interval coverage against the election benchmarks.*


![Presidential vote after raking – benchmark shares versus predictions and 90% intervals.](assets/us-2026/vote_pres_2024_after_rake.png)

*Figure 5. Presidential vote after raking – benchmark shares versus predictions and 90% intervals.*



|           | bias_pp| RMSE_pp| spearman| coverage_90_pct|
|:----------|-------:|-------:|--------:|---------------:|
|Overall    |       0|       0|        1|             100|
|No Vote    |       0|       0|        1|             100|
|Democratic |       0|       0|        1|             100|
|Republican |       0|       0|        1|             100|
|Other      |       0|       0|        1|             100|

*Table 15. House vote after raking – errors and 90% interval coverage against the election benchmarks.*


![House vote after raking – benchmark shares versus predictions and 90% intervals.](assets/us-2026/vote_house_2024_after_rake.png)

*Figure 6. House vote after raking – benchmark shares versus predictions and 90% intervals.*

## 12. Check the original joint stratification cells

Sum over both vote choices within each original district-by-demographic
cell. Compare its raked mean and every draw with the population in the
mother frame. The plot shows the mean and 90% draw interval against the
original count. The maximum error also checks every draw outside that interval.

*Snippet 13. Combine the checks for all mother-frame cells and report the largest count discrepancy.*


``` r
if (run_rake) {
  # Stack the cell comparisons while keeping each original demographic cell distinct.
  joint_cells <- do.call(rbind, lapply(summaries, "[[", "cells"))
  figure <- file.path(figure_dir, "original-cell-totals.png")
  joint_metrics <- plot_post_rake_cells(joint_cells, figure, count_tolerance = rake_tolerance)
  joint_metrics$cells_checked <- nrow(joint_cells)
  joint_metrics$max_abs_count_error <- max(joint_cells$max_abs_error)
  stopifnot(joint_metrics$max_abs_count_error <= rake_tolerance)
  utils::write.csv(joint_metrics, file.path(outputs$diagnostics, "original_cell_totals_metrics.csv"))
  print(knitr::kable(joint_metrics, format = "pipe", digits = 12))
  cat("\n*Table 16. Agreement between raked and original population counts in every mother-frame cell.*\n\n")
  caption <- "Original cell counts versus raked totals and 90% intervals after summing over both votes."
  cat("\n![", caption, "](", sub("^vignettes/", "", figure), ")\n\n",
      "*Figure 7. ", caption, "*\n\n", sep = "")
}
```



|                   | bias_count| RMSE_count|  spearman| coverage_90_pct| cells_checked| max_abs_count_error|
|:------------------|----------:|----------:|---------:|---------------:|-------------:|-------------------:|
|All original cells |          0|          0| 0.9999999|             100|        497550|             1.5e-11|

*Table 16. Agreement between raked and original population counts in every mother-frame cell.*


![Original cell counts versus raked totals and 90% intervals after summing over both votes.](assets/us-2026/original-cell-totals.png)

*Figure 7. Original cell counts versus raked totals and 90% intervals after summing over both votes.*

## 13. Measure how raking changes vote shares

Each row shows a demographic or district grouping. The outcome panels
show subgroup changes in percentage points as dots and population-weighted
averages as squares. Green marks increases and red marks decreases.
All four outcome panels use the citizen voting-age population (CVAP)
in the subgroup as their denominator. Total variation includes all four choices. It is half the sum of their absolute changes in CVAP shares,
in percentage points, weighted across subgroups by CVAP. Its dots show
the paired draws and its square shows the mean-count result.
The presidential plot includes districts because that calibration
target is statewide. The House plot includes district-by-demographic
groups because the district target covers all demographics together.

*Snippet 14. Plot before-to-after vote-share changes by demographic group and district using paired draws.*


``` r
if (run_rake) {
  # Plot the summaries collected while the before and after counts were both available.
  captions <- setNames(c(
    "Presidential vote share changes across demographic and district groupings.",
    "House vote share changes across demographic and district-by-demographic groupings."
  ), c(pres, house))
  for (outcome in names(change_summaries)) {
    figure <- file.path(figure_dir, paste0(outcome, "_raking_changes.png"))
    change_summaries[[outcome]]$plot(figure)
    caption <- captions[[outcome]]
    tv_caption <- paste(
      "Each total-variation dot measures the change for one paired posterior draw.",
      "The square measures the change between the mean-count frames before and after raking.",
      "In those frames, increases in some draws can offset decreases in others before absolute",
      "differences are taken. This can put the square below every draw."
    )
    cat("\n![", caption, "](", sub("^vignettes/", "", figure), ")\n\n",
        "*Figure ", 7L + match(outcome, c(pres, house)), ". ", caption, " ",
        tv_caption, "*\n\n", sep = "")
  }
}
```


![Presidential vote share changes across demographic and district groupings.](assets/us-2026/vote_pres_2024_raking_changes.png)

*Figure 8. Presidential vote share changes across demographic and district groupings. Each total-variation dot measures the change for one paired posterior draw. The square measures the change between the mean-count frames before and after raking. In those frames, increases in some draws can offset decreases in others before absolute differences are taken. This can put the square below every draw.*


![House vote share changes across demographic and district-by-demographic groupings.](assets/us-2026/vote_house_2024_raking_changes.png)

*Figure 9. House vote share changes across demographic and district-by-demographic groupings. Each total-variation dot measures the change for one paired posterior draw. The square measures the change between the mean-count frames before and after raking. In those frames, increases in some draws can offset decreases in others before absolute differences are taken. This can put the square below every draw.*

## 14. Inspect the final frame

Each compressed state RDS includes both outcome columns, `N`, and all
draw-specific counts. The original state and district keys are carried by
`state_fips` and `district_id`. Read the partitions with `readRDS()` to use
the national joint frame in a later analysis. The table shows 100 rows
sampled across the state and DC partitions, with both vote outcomes and
three draw counts.

*Snippet 15. Sample 100 rows from the saved joint frame and record where the partitions are stored.*


``` r
if (run_rake) {
  set.seed(2029)
  raked_paths <- file.path(raked_dir, paste0("state_", names(states), ".rds"))
  stopifnot(all(file.exists(raked_paths)))
  partition_sizes <- lengths(states) * length(choices)^2L
  # Show the calibrated mean and three draw columns from the complete saved frame.
  preview_columns <- c(predictors, pres, house, "N",
                       paste0("N_draw_", c(1L, ceiling(n_draws / 2), n_draws)))
  preview <- sample_extended_frame(raked_paths, preview_columns, partition_sizes)
  prune_geographic_partitions(raked_dir, raked_paths, "^state_([0-9]+|batch-[0-9]+)[.]rds$")
  writeLines(raked_dir, file.path(outputs$diagnostics, "raked-directory.txt"))
  print(knitr::kable(preview, digits = 3, row.names = FALSE))
  cat("\n*Table 17. Sample of 100 raked joint rows with the calibrated mean and three draws.*\n\n")
}
```



|state_fips |district_id |age_cat |gender_cat |race_cat        |hispanic_cat |educ_cat     |vote_pres_2024 |vote_house_2024 |       N| N_draw_1| N_draw_63| N_draw_125|
|:----------|:-----------|:-------|:----------|:---------------|:------------|:------------|:--------------|:---------------|-------:|--------:|---------:|----------:|
|06         |06-34       |23-27   |Male       |Other/Multi     |Not Hispanic |Some college |Other          |Democratic      |   6.678|    6.855|     7.262|      6.720|
|17         |17-05       |23-27   |Female     |Native American |Hispanic     |2-year       |No Vote        |Democratic      |   0.124|    0.105|     0.207|      0.142|
|29         |29-05       |38-42   |Male       |Native American |Hispanic     |Some college |No Vote        |No Vote         |  22.933|   25.412|    23.093|     20.544|
|44         |44-01       |43-47   |Female     |White           |Hispanic     |HS grad      |Republican     |Republican      |   4.446|    3.595|     4.408|      5.270|
|12         |12-22       |73-77   |Female     |Black           |Hispanic     |Post-grad    |No Vote        |Republican      |   0.001|    0.002|     0.001|      0.001|
|18         |18-03       |33-37   |Female     |Black           |Not Hispanic |HS grad      |No Vote        |Democratic      |   2.951|    2.699|     2.879|      3.263|
|48         |48-25       |53-57   |Female     |White           |Not Hispanic |4-year       |Democratic     |No Vote         |  23.511|   19.583|    21.192|     19.225|
|48         |48-33       |53-57   |Female     |Asian           |Not Hispanic |Post-grad    |Democratic     |Other           |   0.000|    0.000|     0.000|      0.000|
|18         |18-09       |68-72   |Female     |Other/Multi     |Hispanic     |Some college |No Vote        |Other           |   0.117|    0.144|     0.112|      0.081|
|30         |30-02       |58-62   |Male       |White           |Not Hispanic |2-year       |Republican     |No Vote         |  25.193|   22.587|    27.915|     24.224|
|12         |12-25       |18-22   |Male       |Native American |Hispanic     |No HS        |Other          |Other           |   0.002|    0.003|     0.002|      0.003|
|12         |12-01       |43-47   |Female     |Other/Multi     |Hispanic     |No HS        |Other          |No Vote         |   0.180|    0.159|     0.175|      0.136|
|56         |56-00       |38-42   |Male       |Other/Multi     |Hispanic     |2-year       |Other          |Other           |   1.832|    2.418|     1.518|      1.986|
|36         |36-26       |73-77   |Female     |White           |Not Hispanic |Post-grad    |Democratic     |Republican      |  30.220|   34.873|    31.799|     29.479|
|48         |48-10       |28-32   |Male       |Other/Multi     |Hispanic     |Some college |Republican     |Republican      | 132.205|  133.808|   143.896|    153.854|
|40         |40-01       |58-62   |Male       |Other/Multi     |Not Hispanic |2-year       |Republican     |Republican      |  35.666|   36.030|    37.794|     38.404|
|04         |04-01       |83+     |Female     |Asian           |Not Hispanic |4-year       |No Vote        |Republican      |   0.141|    0.089|     0.103|      0.187|
|46         |46-00       |28-32   |Male       |White           |Not Hispanic |Some college |No Vote        |Republican      |  94.007|  118.208|    65.176|    103.145|
|06         |06-49       |38-42   |Female     |Native American |Hispanic     |4-year       |Other          |Other           |   0.000|    0.000|     0.000|      0.000|
|45         |45-04       |18-22   |Male       |Asian           |Hispanic     |No HS        |Other          |Democratic      |   0.008|    0.005|     0.014|      0.005|
|48         |48-20       |18-22   |Female     |Native American |Hispanic     |2-year       |No Vote        |No Vote         |  26.667|   29.446|    24.441|     26.164|
|12         |12-12       |53-57   |Male       |Black           |Hispanic     |HS grad      |No Vote        |No Vote         |   0.655|    0.630|     0.564|      0.650|
|49         |49-01       |33-37   |Female     |White           |Not Hispanic |Post-grad    |Democratic     |Other           |  81.695|   78.783|   106.267|     70.616|
|06         |06-43       |78-82   |Male       |White           |Hispanic     |Some college |Republican     |Other           |   0.000|    0.000|     0.000|      0.000|
|24         |24-05       |78-82   |Male       |Asian           |Not Hispanic |Some college |Republican     |Republican      |   3.089|    3.432|     2.991|      3.406|
|34         |34-09       |23-27   |Female     |Other/Multi     |Hispanic     |Some college |No Vote        |Republican      |   5.164|    5.920|     7.380|      5.002|
|04         |04-02       |83+     |Female     |White           |Hispanic     |No HS        |Democratic     |Democratic      |  21.944|   26.554|    18.566|     24.067|
|21         |21-05       |18-22   |Male       |Black           |Hispanic     |2-year       |No Vote        |No Vote         |   0.161|    0.151|     0.161|      0.168|
|06         |06-51       |78-82   |Male       |Other/Multi     |Not Hispanic |Post-grad    |Other          |Other           |   0.000|    0.000|     0.000|      0.000|
|08         |08-07       |58-62   |Female     |Native American |Not Hispanic |Some college |Democratic     |Other           |   0.197|    0.192|     0.154|      0.177|
|30         |30-01       |28-32   |Female     |Other/Multi     |Not Hispanic |4-year       |Republican     |Other           |   6.001|    4.793|     7.309|      2.943|
|08         |08-05       |28-32   |Female     |White           |Not Hispanic |HS grad      |Democratic     |Democratic      | 296.194|  275.063|   304.716|    296.783|
|06         |06-05       |28-32   |Male       |Native American |Hispanic     |4-year       |Republican     |Other           |   0.000|    0.000|     0.000|      0.000|
|04         |04-04       |33-37   |Female     |Other/Multi     |Hispanic     |No HS        |Democratic     |Democratic      |  28.793|   29.829|    30.592|     30.467|
|47         |47-08       |33-37   |Female     |Asian           |Not Hispanic |4-year       |Republican     |Democratic      |   2.086|    1.628|     2.477|      1.438|
|25         |25-04       |48-52   |Male       |Other/Multi     |Hispanic     |4-year       |Republican     |Other           |   1.980|    2.030|     1.477|      1.636|
|34         |34-12       |48-52   |Female     |Black           |Hispanic     |4-year       |Democratic     |Other           |   0.096|    0.082|     0.106|      0.085|
|26         |26-09       |28-32   |Female     |White           |Not Hispanic |4-year       |Other          |Republican      |  37.482|   57.871|    31.608|     30.798|
|48         |48-30       |78-82   |Female     |White           |Hispanic     |No HS        |No Vote        |Republican      |   0.182|    0.219|     0.184|      0.129|
|09         |09-03       |83+     |Male       |White           |Hispanic     |No HS        |Democratic     |Republican      |   0.134|    0.171|     0.096|      0.129|
|25         |25-07       |53-57   |Female     |White           |Not Hispanic |Post-grad    |Republican     |No Vote         | 268.130|  239.178|   291.488|    312.142|
|55         |55-07       |43-47   |Male       |White           |Not Hispanic |4-year       |Democratic     |Other           |   1.135|    1.293|     1.143|      1.148|
|20         |20-03       |48-52   |Male       |White           |Hispanic     |2-year       |Republican     |Democratic      |   0.520|    0.417|     0.398|      0.652|
|06         |06-06       |28-32   |Male       |Other/Multi     |Hispanic     |4-year       |Democratic     |Republican      |   0.000|    0.000|     0.000|      0.000|
|26         |26-09       |23-27   |Male       |White           |Hispanic     |Some college |No Vote        |No Vote         |  41.362|   42.856|    40.761|     41.927|
|51         |51-02       |73-77   |Female     |Other/Multi     |Hispanic     |No HS        |Republican     |Other           |   0.163|    0.234|     0.092|      0.221|
|19         |19-03       |43-47   |Male       |Other/Multi     |Hispanic     |2-year       |No Vote        |No Vote         |   7.648|    8.498|     8.102|      7.182|
|18         |18-03       |63-67   |Male       |Other/Multi     |Not Hispanic |HS grad      |Other          |Republican      |   0.379|    0.292|     0.419|      0.291|
|17         |17-15       |38-42   |Female     |Native American |Not Hispanic |Post-grad    |Other          |Democratic      |   0.000|    0.000|     0.000|      0.000|
|06         |06-43       |53-57   |Male       |White           |Not Hispanic |Post-grad    |Democratic     |Other           |   0.000|    0.000|     0.000|      0.000|
|25         |25-02       |58-62   |Male       |Other/Multi     |Hispanic     |4-year       |Other          |No Vote         |   0.051|    0.067|     0.053|      0.017|
|22         |22-04       |23-27   |Female     |Asian           |Not Hispanic |Post-grad    |Democratic     |No Vote         |   1.771|    1.510|     1.582|      1.677|
|23         |23-01       |28-32   |Female     |Other/Multi     |Hispanic     |Some college |Democratic     |Republican      |   0.201|    0.216|     0.229|      0.200|
|06         |06-07       |33-37   |Male       |White           |Hispanic     |2-year       |Republican     |No Vote         |  10.773|    8.113|    14.605|     10.096|
|36         |36-16       |53-57   |Male       |Other/Multi     |Hispanic     |4-year       |No Vote        |Republican      |   3.594|    3.251|     2.389|      3.984|
|12         |12-27       |63-67   |Female     |Other/Multi     |Hispanic     |4-year       |No Vote        |Republican      |  21.561|   25.094|    22.883|     29.786|
|53         |53-03       |18-22   |Male       |Asian           |Hispanic     |2-year       |Democratic     |Republican      |   0.350|    0.376|     0.312|      0.307|
|42         |42-13       |63-67   |Female     |Black           |Not Hispanic |Some college |Republican     |Democratic      |   0.028|    0.037|     0.023|      0.026|
|34         |34-11       |78-82   |Female     |Other/Multi     |Hispanic     |Post-grad    |No Vote        |Other           |   0.001|    0.000|     0.000|      0.000|
|26         |26-08       |68-72   |Female     |Other/Multi     |Not Hispanic |4-year       |Republican     |Republican      |  10.216|   10.450|     8.481|      8.927|
|45         |45-03       |83+     |Female     |White           |Not Hispanic |Post-grad    |No Vote        |No Vote         |  18.610|   19.292|    21.987|     18.442|
|19         |19-02       |38-42   |Male       |Native American |Not Hispanic |No HS        |No Vote        |Other           |   0.003|    0.002|     0.003|      0.002|
|24         |24-04       |73-77   |Male       |White           |Hispanic     |Post-grad    |Democratic     |Republican      |   0.011|    0.013|     0.012|      0.013|
|48         |48-01       |48-52   |Female     |Black           |Not Hispanic |4-year       |Democratic     |Republican      |  11.332|   12.269|    10.024|      9.205|
|48         |48-34       |58-62   |Female     |Native American |Not Hispanic |2-year       |No Vote        |No Vote         |   3.108|    2.759|     2.863|      3.387|
|08         |08-02       |58-62   |Female     |Other/Multi     |Not Hispanic |2-year       |Republican     |Democratic      |   0.967|    1.281|     1.162|      1.129|
|06         |06-08       |73-77   |Male       |White           |Hispanic     |HS grad      |Republican     |Other           |   0.000|    0.000|     0.000|      0.000|
|12         |12-21       |78-82   |Male       |White           |Hispanic     |Post-grad    |Republican     |No Vote         |   0.067|    0.064|     0.099|      0.074|
|12         |12-04       |33-37   |Female     |Other/Multi     |Hispanic     |Post-grad    |No Vote        |Democratic      |   1.478|    1.860|     1.837|      1.520|
|12         |12-25       |48-52   |Female     |Asian           |Hispanic     |Some college |Republican     |No Vote         |   0.221|    0.216|     0.291|      0.228|
|25         |25-09       |68-72   |Female     |Other/Multi     |Not Hispanic |HS grad      |Republican     |Democratic      |   8.471|    8.056|     7.437|     10.196|
|51         |51-02       |48-52   |Female     |Native American |Hispanic     |4-year       |Republican     |No Vote         |   0.325|    0.355|     0.244|      0.255|
|53         |53-08       |43-47   |Male       |Black           |Not Hispanic |Some college |Other          |Republican      |   1.093|    1.232|     1.602|      0.927|
|12         |12-06       |23-27   |Female     |Other/Multi     |Not Hispanic |Post-grad    |No Vote        |Other           |   0.000|    0.000|     0.000|      0.000|
|42         |42-14       |43-47   |Male       |Other/Multi     |Hispanic     |No HS        |Democratic     |No Vote         |   1.355|    1.142|     1.780|      1.468|
|29         |29-05       |83+     |Male       |Other/Multi     |Hispanic     |No HS        |Republican     |No Vote         |   3.930|    4.329|     4.737|      4.795|
|13         |13-14       |58-62   |Male       |Black           |Hispanic     |HS grad      |No Vote        |Democratic      |   0.097|    0.077|     0.092|      0.096|
|17         |17-06       |18-22   |Male       |Black           |Not Hispanic |Some college |No Vote        |Democratic      |  10.869|    7.803|    10.277|     11.492|
|06         |06-47       |73-77   |Female     |Native American |Hispanic     |HS grad      |No Vote        |Republican      |   0.058|    0.051|     0.045|      0.053|
|28         |28-04       |38-42   |Female     |Black           |Not Hispanic |HS grad      |No Vote        |Democratic      |  25.469|   22.807|    26.219|     28.034|
|12         |12-08       |38-42   |Male       |White           |Not Hispanic |Some college |Other          |Democratic      |  12.174|    8.137|    13.476|     15.097|
|01         |01-06       |68-72   |Female     |Other/Multi     |Not Hispanic |4-year       |Other          |Democratic      |   1.941|    2.093|     1.289|      1.578|
|39         |39-15       |33-37   |Female     |White           |Not Hispanic |Some college |Other          |Republican      |   6.869|    5.531|     7.440|      4.995|
|32         |32-03       |63-67   |Female     |White           |Hispanic     |Post-grad    |Democratic     |No Vote         |   0.000|    0.000|     0.000|      0.000|
|06         |06-39       |83+     |Male       |Other/Multi     |Hispanic     |Post-grad    |Republican     |Other           |   0.000|    0.000|     0.000|      0.000|
|25         |25-09       |83+     |Female     |White           |Not Hispanic |Post-grad    |Republican     |Democratic      |  56.581|   51.947|    49.124|     56.967|
|04         |04-03       |38-42   |Female     |Asian           |Not Hispanic |2-year       |Republican     |No Vote         |   0.868|    0.683|     0.730|      0.769|
|06         |06-27       |83+     |Female     |Other/Multi     |Not Hispanic |HS grad      |Other          |Democratic      |   0.496|    0.493|     0.503|      0.461|
|34         |34-11       |23-27   |Male       |Black           |Hispanic     |No HS        |Democratic     |No Vote         |   0.449|    0.428|     0.548|      0.541|
|38         |38-00       |78-82   |Female     |White           |Hispanic     |Some college |No Vote        |Other           |   0.000|    0.001|     0.000|      0.000|
|48         |48-28       |53-57   |Female     |Black           |Not Hispanic |HS grad      |Democratic     |Other           |   0.005|    0.004|     0.006|      0.004|
|37         |37-07       |63-67   |Male       |Black           |Hispanic     |HS grad      |Democratic     |No Vote         |   0.537|    0.559|     0.765|      0.515|
|55         |55-02       |68-72   |Female     |Native American |Not Hispanic |Some college |Republican     |Democratic      |   0.031|    0.034|     0.028|      0.030|
|17         |17-07       |83+     |Female     |Native American |Hispanic     |No HS        |Republican     |Democratic      |   0.141|    0.100|     0.126|      0.154|
|26         |26-02       |23-27   |Female     |Native American |Not Hispanic |Some college |Democratic     |Republican      |   1.090|    1.149|     0.995|      1.138|
|17         |17-11       |23-27   |Female     |Other/Multi     |Not Hispanic |No HS        |Democratic     |Democratic      |   3.928|    4.327|     2.903|      3.664|
|06         |06-15       |53-57   |Female     |Native American |Hispanic     |2-year       |Republican     |Democratic      |   0.314|    0.374|     0.347|      0.458|
|34         |34-07       |43-47   |Female     |Black           |Not Hispanic |Post-grad    |Other          |Republican      |   1.043|    0.857|     0.846|      0.661|
|28         |28-03       |43-47   |Male       |Asian           |Not Hispanic |No HS        |Republican     |Other           |   0.000|    0.000|     0.000|      0.000|
|19         |19-02       |23-27   |Male       |Other/Multi     |Hispanic     |HS grad      |No Vote        |Other           |   0.191|    0.188|     0.197|      0.134|

*Table 17. Sample of 100 raked joint rows with the calibrated mean and three draws.*
