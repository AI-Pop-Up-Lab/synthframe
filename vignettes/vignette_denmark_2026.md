# Extending the Denmark 2026 population frame

This example extends the Denmark population frame with reported 2022
parliamentary vote choice. It fits one model per sequential stage,
predicts into the population frame, and calibrates the resulting counts
to municipal election shares. The
[input guide](../data/input/denmark%202026/README.md) documents the
source data and outcome recodes.

## Read and inspect the inputs


``` r
for (file in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(file)

country <- file.path("data", "input", "denmark 2026")
survey <- read.csv(file.path(country, "auxiliary_survey.csv"), fileEncoding = "UTF-8")
frame <- read.csv(file.path(country, "frame.csv"), fileEncoding = "UTF-8")
election <- read.csv(file.path(country, "election_results.csv"), fileEncoding = "UTF-8")
area <- "municipality"
outcome <- "vote_choice_2022"
no_vote <- "No Vote"
predictors <- c(area, "gender", "age_group", "education")
out <- Sys.getenv("VOTEFRAME_DENMARK_OUTPUT_DIR",
                  unset = file.path("data", "output", "denmark-2026-sequential"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
figure_dir <- file.path("vignettes", "assets", "denmark-2026")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(!anyDuplicated(frame[predictors]),
          setequal(frame[[area]], election[[area]]))
cells <- aggregate_extension_survey(
  survey, extension_variable = outcome, predictors = predictors, no_vote = no_vote
)
```

```
## Warning: Removed 221 incomplete survey rows before aggregation.
```

``` r
c(survey_rows = nrow(survey), complete_respondents = sum(cells$counts),
  omitted_respondents = cells$omitted, population_cells = nrow(frame),
  municipalities = length(unique(frame[[area]])))
```

```
##          survey_rows complete_respondents  omitted_respondents
##                 2823                 2602                  221
##     population_cells       municipalities
##                28980                   98
```

## Prepare the municipality predictors

`process_area_covariates()` makes a matrix of historical shares with one
row per municipality. Each fitted stage uses the historical population
share for its own outcome. The first stage models nonvoting. Later
stages use respondents whose choices remain after removing the earlier
outcomes. The final choice receives the remaining probability.


``` r
historical <- process_area_covariates(election, area, outcome, "share_eligible")
stage_outcomes <- head(cells$outcome_order, -1L)
area_covariates <- setNames(lapply(stage_outcomes, function(choice) {
  share <- historical[, choice, drop = FALSE]
  colnames(share) <- "vote_share_2022"
  share
}), stage_outcomes)
cells$outcome_order
```

```
##  [1] "No Vote"                         "A: Socialdemokratiet"
##  [3] "Other - Left"                    "V: Venstre"
##  [5] "F: SF - Socialistisk Folkeparti" "Other - Right"
##  [7] "M: Moderaterne"                  "I: Liberal Alliance"
##  [9] "Æ: Danmarksdemokraterne"         "Invalid"
## [11] "Other"
```

## Select and fit the sequential models

`fit_sequential_logit()` starts each stage's search at **depth 4**.
It screens support, prunes collinear fixed columns while preserving
hierarchy, and reduces depth if the retained design exceeds the
respondent budget. The fit log records the selected designs, the
columns removed, and the solver diagnostics. A stage can return a fit
at the iteration cap, so inspect the solver status before interpreting
its results.


``` r
requested_depth <- 4L
minimum_support <- 10L
set.seed(2026)
control <- vglmer::vglmer_control(quiet = FALSE, print_prog = 1L)
fit <- withCallingHandlers(
  fit_sequential_logit(
    survey, outcome, area, area_covariates,
    no_vote = no_vote, outcome_order = cells$outcome_order,
    predictors = predictors, frame = frame, depth = requested_depth,
    splines = TRUE, control = control, minimum_support = minimum_support,
    tolerance_parameters = 0.01, tolerance_elbo = 0, max_iterations = 100L,
    interaction_support_log = file.path(out, "interaction_support.csv"),
    log_file = file.path(out, "fit_log.csv")
  ),
  message = function(m) cat(conditionMessage(m), file = stderr())
)
saveRDS(fit, file.path(out, "fit.rds"))
```


``` r
fields <- c("outcome", "observations", "fitted_depth", "estimated_retained_parameters",
            "dropped_fixed", "dropped_scalar_random", "dropped_correlated_random",
            "dropped_smooth", "fixed_condition_after", "iterations_used",
            "max_parameter_change", "status")
knitr::kable(fit$fit_log[fields])
```



|outcome                         | observations| fitted_depth| estimated_retained_parameters| dropped_fixed| dropped_scalar_random| dropped_correlated_random| dropped_smooth| fixed_condition_after| iterations_used| max_parameter_change|status   |
|:-------------------------------|------------:|------------:|-----------------------------:|-------------:|---------------------:|-------------------------:|--------------:|---------------------:|---------------:|--------------------:|:--------|
|No Vote                         |         2602|            2|                          1145|             0|                  1720|                       107|           2185|             12.714249|             100|            0.0241435|returned |
|A: Socialdemokratiet            |         2503|            2|                          1134|             0|                  1698|                       109|           2185|              9.259361|              16|            0.0053718|returned |
|Other - Left                    |         1840|            2|                          1032|             0|                  1544|                       124|           2185|              3.207179|              17|            0.0056380|returned |
|V: Venstre                      |         1456|            2|                           991|             0|                  1473|                       127|           2185|              6.870647|              11|            0.0080692|returned |
|F: SF - Socialistisk Folkeparti |         1129|            2|                           942|             0|                  1326|                       143|           2185|              6.710431|              25|            0.0037753|returned |
|Other - Right                   |          855|            1|                            81|             0|                    66|                         0|              0|              8.067551|              10|            0.0098956|returned |
|M: Moderaterne                  |          599|            1|                            69|             0|                    76|                         0|              0|              8.153983|               7|            0.0009223|returned |
|I: Liberal Alliance             |          349|            1|                            55|             0|                    82|                         0|              0|              7.643608|              10|            0.0041993|returned |
|Æ: Danmarksdemokraterne         |          190|            1|                            39|             0|                    81|                         0|              0|              4.407712|              16|            0.0086571|returned |
|Invalid                         |           39|            4|                            28|             0|                    39|                       741|           2688|             16.719593|              11|            0.0075036|returned |

In this run, the No Vote stage reached the 100-iteration cap with a
maximum parameter change of 0.02414 against the 0.01 threshold. The
remaining nine fitted stages converged. The diagnostics below use the
returned No Vote fit, so its estimates should be read with that
convergence result in mind.

## Predict and check municipal vote shares before raking

`predict_sequential_logit()` draws the outcomes' cell probabilities.
`aggregate_extended_frame()` multiplies probabilities by population
counts and keeps a row for every original cell and vote choice. The
municipal comparison uses shares **among voters** for party choices and
shares **of the full population** for nonvoting. The among-voters
denominator includes invalid ballots. Posterior intervals
use each draw's own turnout denominator.

The 2022 municipal results also enter as predictors. This comparison
measures agreement with the calibration targets.


``` r
set.seed(2027)
posterior <- predict_sequential_logit(fit, n_draws = 125L, write_files = FALSE)
extended <- aggregate_extended_frame(frame, posterior, by = setdiff(names(frame), "N"))
rm(posterior)
stopifnot(nrow(extended) == nrow(frame) * length(fit$outcome_order))
saveRDS(extended, file.path(out, "extended_frame.rds"))
```


``` r
figure <- function(name) file.path(figure_dir, paste0(name, ".png"))
before <- check_extended_frame(
  extended, election, by = area, extension_variable = outcome, target_variable = "share_eligible",
  no_vote = no_vote, outcome_order = fit$outcome_order,
  plot_file = figure("municipal-before")
)
knitr::kable(before$metrics, digits = 4)
```



|                                |  bias_pp| RMSE_pp| spearman| coverage_90_pct|
|:-------------------------------|--------:|-------:|--------:|---------------:|
|Overall                         |   0.0000|  2.3109|   0.9259|         90.0000|
|No Vote                         | -11.4988| 11.6258|   0.9852|          0.0000|
|A: Socialdemokratiet            |   0.5687|  2.5125|   0.8695|         93.8776|
|Other - Left                    |   0.9206|  2.0751|   0.9475|         70.4082|
|V: Venstre                      |   1.1617|  1.4368|   0.9682|         98.9796|
|F: SF - Socialistisk Folkeparti |   2.0572|  2.2605|   0.9515|         87.7551|
|Other - Right                   |  -2.4229|  2.7384|   0.8563|         87.7551|
|M: Moderaterne                  |   0.6006|  1.2061|   0.9055|         97.9592|
|I: Liberal Alliance             |  -0.3442|  2.7189|  -0.0852|         89.7959|
|Æ: Danmarksdemokraterne         |  -2.9450|  4.3559|   0.9084|         75.5102|
|Invalid                         |   0.0477|  0.3672|   0.7012|        100.0000|
|Other                           |   0.3555|  0.3925|   0.0859|         97.9592|

``` r
knitr::include_graphics(sub("^vignettes/", "", figure("municipal-before")), error = FALSE)
```

![plot of chunk before-rake](assets/denmark-2026/municipal-before.png)

## Rake to the municipal targets

`rake_extended_frame()` takes shares on the population scale. It
reallocates counts across vote choices within each demographic cell,
preserving that cell's total. The same constraints apply to the mean
counts and each posterior draw.


``` r
rake_tolerance <- 1e-8
raked <- rake_extended_frame(
  extended, frame, election, by = area, extension_variable = outcome,
  target_variable = "share_eligible", tolerance = rake_tolerance
)
saveRDS(raked, file.path(out, "raked_frame.rds"))
```

## Check both margins after raking

`check_post_rake()` repeats the municipal comparisons using the same
denominators and checks every original joint demographic cell's total.


``` r
after <- check_post_rake(
  raked, frame, election, by = area, extension_variable = outcome, target_variable = "share_eligible",
  no_vote = no_vote, outcome_order = fit$outcome_order,
  share_tolerance = 2 * rake_tolerance,
  outcome_plot_file = figure("municipal-after"),
  joint_plot_file = figure("original-cell-totals")
)
knitr::kable(after$outcome$metrics, digits = 4)
```



|                                | bias_pp| RMSE_pp| spearman| coverage_90_pct|
|:-------------------------------|-------:|-------:|--------:|---------------:|
|Overall                         |       0|       0|        1|             100|
|No Vote                         |       0|       0|        1|             100|
|A: Socialdemokratiet            |       0|       0|        1|             100|
|Other - Left                    |       0|       0|        1|             100|
|V: Venstre                      |       0|       0|        1|             100|
|F: SF - Socialistisk Folkeparti |       0|       0|        1|             100|
|Other - Right                   |       0|       0|        1|             100|
|M: Moderaterne                  |       0|       0|        1|             100|
|I: Liberal Alliance             |       0|       0|        1|             100|
|Æ: Danmarksdemokraterne         |       0|       0|        1|             100|
|Invalid                         |       0|       0|        1|             100|
|Other                           |       0|       0|        1|             100|

``` r
knitr::include_graphics(sub("^vignettes/", "", figure("municipal-after")), error = FALSE)
```

![plot of chunk after-rake](assets/denmark-2026/municipal-after.png)

``` r
knitr::include_graphics(sub("^vignettes/", "", figure("original-cell-totals")), error = FALSE)
```

![plot of chunk after-rake](assets/denmark-2026/original-cell-totals.png)

## Measure how raking changes the estimates

The municipal outcomes are constrained by the rake. Vote shares for
age, sex, and education groups are free to move. Compare those margins
before and after raking, using the full group population for nonvoting
and the number of voters for the other choices. The table gives mean
absolute and maximum absolute changes in percentage points across the
groups for each outcome. It uses the posterior mean count column.


``` r
shares_by <- function(data, variable) {
  keys <- c(variable, outcome)
  totals <- aggregate(data$N, data[keys], sum)
  group <- totals[[variable]]
  population <- tapply(totals$x, group, sum)
  voters <- totals[[outcome]] != no_vote
  turnout <- tapply(totals$x[voters], group[voters], sum)
  denominator <- unname(population[group])
  denominator[voters] <- turnout[group[voters]]
  totals$share <- ifelse(denominator > 0, totals$x / denominator, NA_real_)
  totals[c(keys, "share")]
}
changes <- do.call(rbind, lapply(c(area, "gender", "age_group", "education"), function(variable) {
  paired <- merge(shares_by(extended, variable), shares_by(raked, variable),
                  by = c(variable, outcome), suffixes = c("_before", "_after"))
  absolute_change <- 100 * abs(paired$share_after - paired$share_before)
  measured <- is.finite(absolute_change)
  by_outcome <- split(absolute_change[measured], paired[[outcome]][measured])
  data.frame(grouping = variable, outcome = names(by_outcome),
             mean_change_pp = vapply(by_outcome, mean, numeric(1)),
             max_change_pp = vapply(by_outcome, max, numeric(1)), row.names = NULL)
}))
write.csv(changes, file.path(out, "raking_changes.csv"), row.names = FALSE)
knitr::kable(changes, digits = 3)
```



|grouping     |outcome                         | mean_change_pp| max_change_pp|
|:------------|:-------------------------------|--------------:|-------------:|
|municipality |A: Socialdemokratiet            |          1.940|         6.721|
|municipality |Æ: Danmarksdemokraterne         |          3.281|        11.812|
|municipality |F: SF - Socialistisk Folkeparti |          2.057|         5.077|
|municipality |I: Liberal Alliance             |          1.891|        11.804|
|municipality |Invalid                         |          0.271|         1.328|
|municipality |M: Moderaterne                  |          0.907|         5.619|
|municipality |No Vote                         |         11.499|        14.689|
|municipality |Other                           |          0.356|         0.794|
|municipality |Other - Left                    |          1.750|         5.343|
|municipality |Other - Right                   |          2.447|         7.234|
|municipality |V: Venstre                      |          1.267|         2.856|
|gender       |A: Socialdemokratiet            |          0.372|         0.742|
|gender       |Æ: Danmarksdemokraterne         |          2.246|         2.569|
|gender       |F: SF - Socialistisk Folkeparti |          2.162|         2.662|
|gender       |I: Liberal Alliance             |          0.662|         0.770|
|gender       |Invalid                         |          0.073|         0.115|
|gender       |M: Moderaterne                  |          0.770|         1.031|
|gender       |No Vote                         |         11.696|        11.900|
|gender       |Other                           |          0.349|         0.361|
|gender       |Other - Left                    |          1.072|         1.209|
|gender       |Other - Right                   |          2.044|         2.154|
|gender       |V: Venstre                      |          0.898|         1.117|
|age_group    |A: Socialdemokratiet            |          0.371|         0.875|
|age_group    |Æ: Danmarksdemokraterne         |          2.237|         3.108|
|age_group    |F: SF - Socialistisk Folkeparti |          2.129|         2.581|
|age_group    |I: Liberal Alliance             |          0.723|         1.989|
|age_group    |Invalid                         |          0.067|         0.140|
|age_group    |M: Moderaterne                  |          0.727|         0.965|
|age_group    |No Vote                         |         11.517|        15.883|
|age_group    |Other                           |          0.358|         0.621|
|age_group    |Other - Left                    |          0.979|         1.667|
|age_group    |Other - Right                   |          2.076|         2.564|
|age_group    |V: Venstre                      |          0.908|         1.238|
|education    |A: Socialdemokratiet            |          0.775|         1.713|
|education    |Æ: Danmarksdemokraterne         |          1.832|         3.673|
|education    |F: SF - Socialistisk Folkeparti |          2.241|         3.951|
|education    |I: Liberal Alliance             |          0.956|         2.322|
|education    |Invalid                         |          0.082|         0.165|
|education    |M: Moderaterne                  |          0.834|         1.279|
|education    |No Vote                         |         13.154|        21.546|
|education    |Other                           |          0.398|         0.583|
|education    |Other - Left                    |          1.242|         1.961|
|education    |Other - Right                   |          2.099|         2.608|
|education    |V: Venstre                      |          0.807|         1.551|

## Inspect the extended frame


``` r
knitr::kable(head(raked[c(predictors, outcome, "N", "N_draw_1", "N_draw_2")]), digits = 2)
```



|municipality |gender |age_group |education                          |vote_choice_2022                |     N| N_draw_1| N_draw_2|
|:------------|:------|:---------|:----------------------------------|:-------------------------------|-----:|--------:|--------:|
|Aabenraa     |Men    |20-24     |H1020 Primary school 7th-9th grade |No Vote                         | 24.59|    21.53|    16.85|
|Aabenraa     |Men    |20-24     |H1020 Primary school 7th-9th grade |A: Socialdemokratiet            | 24.70|    25.13|    21.39|
|Aabenraa     |Men    |20-24     |H1020 Primary school 7th-9th grade |Other - Left                    |  6.68|     7.18|     7.22|
|Aabenraa     |Men    |20-24     |H1020 Primary school 7th-9th grade |V: Venstre                      | 20.23|    24.82|    27.79|
|Aabenraa     |Men    |20-24     |H1020 Primary school 7th-9th grade |F: SF - Socialistisk Folkeparti |  4.54|     4.59|     6.49|
|Aabenraa     |Men    |20-24     |H1020 Primary school 7th-9th grade |Other - Right                   | 31.12|    33.42|    24.58|
