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

*Table 1. Expected frame size at each step. Each extension creates one row
per choice for each incoming cell. Raking adjusts counts, so rows with
zero calibrated counts are still included.*

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
   VoteFrame uses it as an automatic cutoff for its pre-fit logistic
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
   coefficients with eligible respondents. Reduce depth only if the
   screened candidate still exceeds that budget.



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
been screened for a candidate depth, VoteFrame checks which eligible
overall curves are still supplied by a grouped spline. If no retained
grouped spline supplies an eligible overall curve, it fits that curve on
its own. This check runs once per candidate depth, after pruning. No
spline columns were added back in this audit.

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

Let $X$ be the weighted, column-scaled design matrix. The program
computes $X=U\,\mathrm{diag}(s_1,\ldots,s_p)V^\top$, with the singular
values ordered largest to smallest.

**The loading of column $j$ on direction $k$ is $V_{jk}$.** For pruning,
the program uses the last column of $V$, which describes the weakest
direction. Column $j$ qualifies as a removal candidate when

$$
|V_{jp}| \;\ge\; 0.01\,\max_i |V_{ip}|.
$$

The intercept participates in this calculation and is always kept. The
**110.40** in the first row comes from $s_1/s_p$. The **94%** uses a
different formula. Divide the House-share coefficient's contribution
$V_{jp}^2/s_p^2$ by the sum of its contributions across all directions,
$\sum_k V_{jk}^2/s_k^2$.

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

## 7. Stop before fitting

Both models have prepared designs. The later run is configured for
125 posterior draws. This knit stops before solver iterations,
posterior prediction, or raking. To continue in a separate
run, knit with `VOTEFRAME_US_FIT=true`.
