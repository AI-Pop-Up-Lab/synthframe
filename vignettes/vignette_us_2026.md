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
for (file in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(file)

country <- file.path("data", "input", "us 2026")
fips <- c("state_fips", "district_fips")
demographics <- c("age_cat", "gender_cat", "race_cat", "hispanic_cat", "educ_cat")
pres <- "vote_pres_2024"
house <- "vote_house_2024"
choices <- c("Democratic", "Republican", "Other", "No Vote")
vote_order <- c("No Vote", "Democratic", "Republican", "Other")

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

predictors <- c("state_fips", "district_id", demographics)
# The composite key retains district FIPS. Keep state FIPS for state margins.
frame <- frame[order(frame$state_fips), c(predictors, "N")]

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

## 2. Specify stage predictors and targets

Each entry in a fit's area-covariate list corresponds to a choice. Each
entry is a matrix or data frame with district IDs as row names and
predictors as columns. Splitting the CSV by choice directly produces these
tables. In both fits, each party stage uses that choice's district House
share and statewide presidential share, the district's categorical
House lineup, and the choice's incumbency. Preserve
both shares on their original citizen-adult scale. Both fits start with
No Vote, using the district House and statewide presidential nonvoting
shares and the lineup. Democratic and Republican follow among voters.
Other receives the remaining probability. Covariates that are blank in
every district for a stage are omitted from that stage. For No Vote,
this applies to incumbency.

The vignette omits `on_ballot`. An earlier design check found that its
interaction with district House share nearly duplicated the House-share
main effect. The audit below checks the fixed design that will actually
be passed to the solver.


``` r
choice_rows <- split(election, election$vote_choice)
area_covariates <- lapply(choice_rows, function(rows) {
  covariates <- data.frame(house_share = rows[[house]], pres_share = rows[[pres]],
                           lineup = factor(rows$lineup),
                           incumbent = rows$incumbent, row.names = rows$district_id)
  covariates[!vapply(covariates, function(x) all(is.na(x)), logical(1))]
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

The statewide presidential and district House benchmarks also define the
targets for a later calibration run. This pre-fit audit uses them only to
construct the predictors and check the input tables.

## 3. Build the survey designs

Both models start at requested depth 2, with interactions and splines.
The presidential model uses state, district, five demographics, and the
area predictors above. The House model adds each CES respondent's
presidential choice. Each model uses its own complete survey cases.
Identical predictor rows are grouped into binomial successes and failures.
At each stage, respondents who chose an earlier outcome leave the risk set.
The counts below refer to eligible respondents at that stage.


``` r
requested_depth <- 2L
minimum_support <- 10L
n_draws <- 125L
model_specs <- list(
  Presidential = list(outcome = pres, predictors = predictors),
  House = list(outcome = house, predictors = c(predictors, pres))
)

prepare_model <- function(spec) {
  aggregated <- suppressWarnings(aggregate_extension_survey(
    survey, spec$outcome, spec$predictors, "No Vote", outcome_order = vote_order
  ))
  stages <- head(vote_order, -1L)
  setNames(lapply(seq_along(stages), function(i) {
    success <- aggregated$counts[, i]
    trials <- rowSums(aggregated$counts[, i:length(vote_order), drop = FALSE])
    eligible <- trials > 0
    stage <- join_stage_covariates(aggregated$cells[eligible, , drop = FALSE],
                                  "district_id", area_covariates[[stages[i]]])
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
|Presidential |Democratic |      52,767| 39,007|     2| 20,386|   6,583|        0|   13,803|
|Presidential |Republican |      23,922| 19,496|     2| 19,458|   9,466|        0|    9,992|
|House        |No Vote    |      55,413| 46,863|     2| 22,317|   6,938|        0|   15,379|
|House        |Democratic |      48,330| 40,477|     2| 22,408|   7,695|        0|   14,713|
|House        |Republican |      21,552| 18,045|     2| 20,813|  10,262|        0|   10,551|

Here is the coefficient accounting summed across the three fitted stages of
each model. The residual outcome, Other, has no separate stage.


|Model        |  Start| Removed| Restored| Retained|
|:------------|------:|-------:|--------:|--------:|
|House        | 65,538|  24,895|        0|   40,643|
|Presidential | 59,857|  22,137|        0|   37,720|

Random level support counts columns removed with a low-support intercept.
Slope support and linear hierarchy count additional removals among levels
that clear the intercept cutoff. Whole smooths and spline levels report
the associated estimated basis columns. These rows count each removal once.
The substantive pattern is the same in both models. Most removals are
low-support random levels. The slope screen also removes
`pres_share:state_fips` because presidential share is constant within
each state. The condition screen removes `house_share:pres_share` in all
stages and `pres_share:incumbent` in both Republican stages. Their
corresponding smooths are removed as well.


|Model        |Stage      | Fixed support| Random level support| Within-group slope support| Fixed collinearity| Linear hierarchy| Whole smooths| Spline level support|
|:------------|:----------|-------------:|--------------------:|--------------------------:|------------------:|----------------:|-------------:|--------------------:|
|Presidential |No Vote    |             0|                5,237|                         51|                  1|                0|           799|                    0|
|Presidential |Democratic |             0|                5,737|                         51|                  1|                0|           794|                    0|
|Presidential |Republican |             0|                8,519|                         51|                  2|                0|           894|                    0|
|House        |No Vote    |             0|                6,077|                         51|                  1|                0|           809|                    0|
|House        |Democratic |             0|                6,844|                         51|                  1|                0|           799|                    0|
|House        |Republican |             0|                9,315|                         51|                  2|                0|           894|                    0|


|Model        |Stage      |Sparse slopes (mean respondents) |Whole smooths removed                                                                                                      |
|:------------|:----------|:--------------------------------|:--------------------------------------------------------------------------------------------------------------------------|
|Presidential |No Vote    |pres_share:state_fips (0)        |v_s(pres_share, by = state_fips), v_s(house_share, pres_share, type = "gKRLS")                                             |
|Presidential |Democratic |pres_share:state_fips (0)        |v_s(pres_share, by = state_fips), v_s(house_share, pres_share, type = "gKRLS")                                             |
|Presidential |Republican |pres_share:state_fips (0)        |v_s(pres_share, by = state_fips), v_s(house_share, pres_share, type = "gKRLS"), v_s(pres_share, incumbent, type = "gKRLS") |
|House        |No Vote    |pres_share:state_fips (0)        |v_s(pres_share, by = state_fips), v_s(house_share, pres_share, type = "gKRLS")                                             |
|House        |Democratic |pres_share:state_fips (0)        |v_s(pres_share, by = state_fips), v_s(house_share, pres_share, type = "gKRLS")                                             |
|House        |Republican |pres_share:state_fips (0)        |v_s(pres_share, by = state_fips), v_s(house_share, pres_share, type = "gKRLS"), v_s(pres_share, incumbent, type = "gKRLS") |

## 6. Inspect support and conditioning before fitting

The left panels show how many candidate random-effect levels have at most
the given number of survey respondents. Each level is counted once per
random term, including crossed factor levels. The right panels show the
full fixed-design condition-index spectrum by stage, after the support
screen and again after collinearity and hierarchy pruning. The horizontal
line marks 30. The spectra are sorted from their largest index, so the
first point gives the reported maximum.


|Model        |Stage      | Before pruning| After pruning|Fixed columns before → after |Fixed columns rejected                       |
|:------------|:----------|--------------:|-------------:|:----------------------------|:--------------------------------------------|
|Presidential |No Vote    |         110.40|         18.46|4 → 3                        |house_share:pres_share                       |
|Presidential |Democratic |          80.56|         28.81|7 → 6                        |house_share:pres_share                       |
|Presidential |Republican |         108.86|         23.74|7 → 5                        |house_share:pres_share, pres_share:incumbent |
|House        |No Vote    |         110.14|         18.47|4 → 3                        |house_share:pres_share                       |
|House        |Democratic |          84.19|         28.97|7 → 6                        |house_share:pres_share                       |
|House        |Republican |         121.21|         25.41|7 → 5                        |house_share:pres_share, pres_share:incumbent |

For each pre-pruning component at or above 30, the next table names the
fixed columns with at least half of their variance proportion on that
component. These proportions come from the weighted design's singular
vectors. They diagnose shared weak directions in the design. They are
separate from posterior coefficient variances and do not drive pruning.


|Model        |Stage      | Condition index|Columns with at least 50% on this component                                          |
|:------------|:----------|---------------:|:------------------------------------------------------------------------------------|
|Presidential |No Vote    |          110.40|(Intercept) (98%), house_share (94%), pres_share (93%), house_share:pres_share (99%) |
|Presidential |Democratic |           31.33|pres_share:incumbent (56%)                                                           |
|Presidential |Democratic |           80.56|(Intercept) (85%), house_share (94%), pres_share (77%), house_share:pres_share (98%) |
|Presidential |Republican |           45.10|pres_share:incumbent (55%)                                                           |
|Presidential |Republican |          108.86|(Intercept) (83%), house_share (95%), pres_share (78%), house_share:pres_share (98%) |
|House        |No Vote    |          110.14|(Intercept) (98%), house_share (94%), pres_share (93%), house_share:pres_share (99%) |
|House        |Democratic |           31.50|pres_share:incumbent (56%)                                                           |
|House        |Democratic |           84.19|(Intercept) (86%), house_share (94%), pres_share (79%), house_share:pres_share (98%) |
|House        |Republican |           47.47|pres_share:incumbent (53%)                                                           |
|House        |Republican |          121.21|(Intercept) (85%), house_share (95%), pres_share (79%), house_share:pres_share (98%) |


![Presidential model support and condition indices](assets/us-2026/prefit-presidential.png)

![House model support and condition indices](assets/us-2026/prefit-house.png)

## 7. Stop before fitting

Both models have prepared designs. The later run is configured for
125 posterior draws. This knit stops before solver iterations,
posterior prediction, or raking. To continue in a separate
run, knit with `VOTEFRAME_US_FIT=true`.
