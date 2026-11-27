# Extending the Denmark 2026 population frame

This example extends the Denmark population frame with reported 2022
parliamentary vote choice. It fits one model per sequential stage,
predicts into the population frame, and calibrates the resulting counts
to municipal election shares. The
[input guide](../data/input/denmark%202026/README.md) documents the
source data and outcome recodes.

## Read and inspect the inputs

*Snippet 1. Read the population frame, survey, and election results and check their keys.*


``` r
knitr::opts_chunk$set(error = FALSE)
for (file in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(file)

country <- file.path("data", "input", "denmark 2026")
survey <- read.csv(file.path(country, "auxiliary_survey.csv"), fileEncoding = "UTF-8")
frame <- read.csv(file.path(country, "frame.csv"), fileEncoding = "UTF-8")
election <- read.csv(file.path(country, "election_results.csv"), fileEncoding = "UTF-8")
area <- "municipality"
parent_area <- Sys.getenv("SYNTHFRAME_PARENT_AREA", "constituency")
areas_per_batch <- vignette_areas_per_batch()
outcome <- "vote_choice_2022"
no_vote <- "No Vote"
predictors <- c(area, "gender", "age_group", "education")
out <- Sys.getenv("SYNTHFRAME_DENMARK_OUTPUT_DIR",
                  unset = file.path("data", "output", basename(country)))
outputs <- case_study_outputs(out)
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

*Snippet 2. Report how many respondents and population cells enter the example.*


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

*Snippet 3. Build municipality-level predictors from the earlier election.*


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
columns removed, and the solver diagnostics. In Table 1, `returned` means
the solver produced a model. With the settings below, convergence requires
`max_parameter_change` to fall below 0.01. If a stage reaches 100
iterations at or above that threshold, its predictions use the last iterate.

*Snippet 4. Fit and save the sequential vote-choice stages.*


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
    interaction_support_log = file.path(outputs$diagnostics, "interaction_support.csv"),
    log_file = file.path(outputs$diagnostics, "fit_log.csv")
  ),
  message = function(m) cat(conditionMessage(m), file = stderr())
)
save_case_study_rds(fit, file.path(outputs$models, "fit.rds"))
```

*Snippet 5. Display the selected designs and convergence results for each fitted stage.*


``` r
fields <- c("outcome", "observations", "fitted_depth", "retained_order",
            "estimated_retained_parameters",
            "dropped_fixed", "dropped_scalar_random", "dropped_correlated_random",
            "dropped_smooth", "fixed_condition_after", "iterations_used",
            "max_parameter_change", "status")
knitr::kable(fit$fit_log[fields])
```



|outcome                         | observations| fitted_depth| retained_order| estimated_retained_parameters| dropped_fixed| dropped_scalar_random| dropped_correlated_random| dropped_smooth| fixed_condition_after| iterations_used| max_parameter_change|status   |
|:-------------------------------|------------:|------------:|--------------:|-----------------------------:|-------------:|---------------------:|-------------------------:|--------------:|---------------------:|---------------:|--------------------:|:--------|
|No Vote                         |         2602|            2|              2|                          1145|             0|                  1720|                       107|           2185|             12.714249|             100|            0.0241435|returned |
|A: Socialdemokratiet            |         2503|            2|              2|                          1134|             0|                  1698|                       109|           2185|              9.259361|              16|            0.0053718|returned |
|Other - Left                    |         1840|            2|              2|                          1032|             0|                  1544|                       124|           2185|              3.207179|              17|            0.0056380|returned |
|V: Venstre                      |         1456|            2|              2|                           991|             0|                  1473|                       127|           2185|              6.870647|              11|            0.0080692|returned |
|F: SF - Socialistisk Folkeparti |         1129|            2|              2|                           942|             0|                  1326|                       143|           2185|              6.710431|              25|            0.0037753|returned |
|Other - Right                   |          855|            1|              1|                            81|             0|                    66|                         0|              0|              8.067551|              10|            0.0098956|returned |
|M: Moderaterne                  |          599|            1|              1|                            69|             0|                    76|                         0|              0|              8.153983|               7|            0.0009223|returned |
|I: Liberal Alliance             |          349|            1|              1|                            55|             0|                    82|                         0|              0|              7.643608|              10|            0.0041993|returned |
|Æ: Danmarksdemokraterne         |          190|            1|              1|                            39|             0|                    81|                         0|              0|              4.407712|              16|            0.0086571|returned |
|Invalid                         |           39|            4|              2|                            27|             0|                    39|                       742|           2688|             16.719593|              13|            0.0012434|returned |

*Table 1. Fitted stages, with sample sizes, retained design, screened coefficients, and convergence diagnostics.*

The `fitted_depth` column gives the selected candidate depth. The
`retained_order` column counts the predictors in the largest surviving term.
For example, `fit$stages[["Invalid"]]$formula` shows the terms fitted for
invalid ballots.

## Predict and check municipal vote shares before raking

`predict_sequential_logit()` draws the outcomes' cell probabilities.
`aggregate_extended_frame()` multiplies probabilities by population
counts and keeps a row for every original cell and vote choice. The
municipal comparison uses shares **among voters** for party choices and
shares **of the full population** for nonvoting. The among-voters
denominator includes invalid ballots. Posterior intervals
use each draw's own turnout denominator.

The 2022 municipal results also enter as predictors. This comparison
measures agreement with the calibration targets. Prediction and raking
batch by constituency by default, keeping each municipality intact.

*Snippet 6. Predict vote choice in geographic batches and expand population counts into choice categories.*


``` r
n_draws <- 125L
frame_rows <- geographic_batches(frame, area, parent_area, areas_per_batch)
part_bytes <- pre_rake_frame_bytes(lengths(frame_rows) * length(fit$outcome_order),
                                   n_draws, ncol(frame))
storage <- vignette_pre_rake_storage(
  out, sum(part_bytes), largest_part_bytes = max(part_bytes),
  final_directory = out, final_bytes = sum(part_bytes)
)
set.seed(2027)
draw_state <- new.env(parent = emptyenv())
extended_parts <- lapply(seq_along(frame_rows), function(i) {
  cells <- frame[frame_rows[[i]], , drop = FALSE]
  posterior <- predict_sequential_logit(
    fit, frame = cells, n_draws = n_draws, draw_state = draw_state, write_files = FALSE
  )
  extended <- aggregate_extended_frame(cells, posterior, by = setdiff(names(cells), "N"))
  stopifnot(nrow(extended) == nrow(cells) * length(fit$outcome_order))
  storage$put(extended, sprintf("part-%02d", i))
})
rm(draw_state)
```

*Snippet 7. Pool the predicted counts by municipality and compare their shares with the targets.*


``` r
figure <- function(name) file.path(figure_dir, paste0(name, ".png"))
municipal_counts <- municipal_partition_counts(extended_parts, storage$get, area, outcome)
before <- check_extended_frame(
  municipal_counts, election, by = area, extension_variable = outcome, target_variable = "share_eligible",
  no_vote = no_vote, outcome_order = fit$outcome_order,
  plot_file = figure("municipal-before")
)
utils::write.csv(before$metrics, file.path(outputs$diagnostics, "before_rake_metrics.csv"))
knitr::kable(before$metrics, digits = 4)
```



|                                |  bias_pp| RMSE_pp| spearman| coverage_90_pct|
|:-------------------------------|--------:|-------:|--------:|---------------:|
|Overall                         |   0.0000|  2.2851|   0.9270|         91.3265|
|No Vote                         | -11.4983| 11.6256|   0.9860|          0.0000|
|A: Socialdemokratiet            |   0.7885|  2.5030|   0.8742|         93.8776|
|Other - Left                    |   0.9033|  2.0886|   0.9410|         71.4286|
|V: Venstre                      |   0.9620|  1.3406|   0.9687|        100.0000|
|F: SF - Socialistisk Folkeparti |   2.0252|  2.2222|   0.9501|         92.8571|
|Other - Right                   |  -2.1841|  2.4936|   0.8619|         93.8776|
|M: Moderaterne                  |   0.6924|  1.2027|   0.9183|         98.9796|
|I: Liberal Alliance             |  -0.5668|  2.7834|  -0.0923|         87.7551|
|Æ: Danmarksdemokraterne         |  -2.9655|  4.3822|   0.8959|         75.5102|
|Invalid                         |   0.1939|  0.4434|   0.7041|        100.0000|
|Other                           |   0.1511|  0.2066|   0.0634|         98.9796|

*Table 2. Before-rake municipal vote-share errors and 90% interval coverage against the election benchmarks.*

![Before raking, predicted municipal shares and 90% draw intervals against the election benchmarks.](assets/denmark-2026/municipal-before.png)

*Figure 1. Before raking, predicted municipal shares and 90% draw intervals against the election benchmarks.*

## Rake to the municipal targets

`rake_extended_frame()` takes shares on the population scale. It
reallocates counts across vote choices within each demographic cell,
preserving that cell's total. The same constraints apply to the mean
counts and each posterior draw.

*Snippet 8. Calibrate each municipality’s vote totals while preserving the original population cells.*


``` r
rake_tolerance <- 1e-8
raked_parts <- lapply(seq_along(frame_rows), function(i) {
  cells <- frame[frame_rows[[i]], , drop = FALSE]
  targets <- election[election[[area]] %in% cells[[area]], , drop = FALSE]
  rake_extended_frame(
    storage$get(extended_parts[[i]]), cells, targets, by = area, extension_variable = outcome,
    target_variable = "share_eligible", tolerance = rake_tolerance
  )
})
raked <- do.call(rbind, raked_parts)
row_order <- order(rep(unlist(frame_rows, use.names = FALSE), each = length(fit$outcome_order)))
raked <- raked[row_order, , drop = FALSE]
rownames(raked) <- NULL
dir.create(outputs$raked_frame, showWarnings = FALSE)
raked_path <- file.path(outputs$raked_frame, "part-01.rds")
save_case_study_rds(raked, raked_path)
```

The final raked frame is saved to
`data/output/denmark 2026/raked_frame/part-01.rds`. The pre-rake frame uses memory or temporary
disk according to available memory. Set `SYNTHFRAME_PRE_RAKE_STORAGE=keep`
to save it. Use the saved `models/fit.rds` and the prediction chunk to recreate it.

## Check both margins after raking

`check_post_rake()` repeats the municipal comparisons using the same
denominators and checks every original joint demographic cell's total.

*Snippet 9. Check the calibrated election margins and compare counts with the original stratification joint.*


``` r
after <- check_post_rake(
  raked, frame, election, by = area, extension_variable = outcome, target_variable = "share_eligible",
  no_vote = no_vote, outcome_order = fit$outcome_order,
  share_tolerance = 2 * rake_tolerance,
  outcome_plot_file = figure("municipal-after"),
  joint_plot_file = figure("original-cell-totals")
)
utils::write.csv(after$outcome$metrics, file.path(outputs$diagnostics, "after_rake_metrics.csv"))
utils::write.csv(after$joint$metrics, file.path(outputs$diagnostics, "original_cell_totals_metrics.csv"))
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

*Table 3. After-rake municipal vote-share errors and 90% interval coverage against the same benchmarks.*

![After raking, calibrated municipal shares and 90% draw intervals against the election benchmarks.](assets/denmark-2026/municipal-after.png)

*Figure 2. After raking, calibrated municipal shares and 90% draw intervals against the election benchmarks.*

![Raked counts and 90% draw intervals versus the mother-frame population in each original cell.](assets/denmark-2026/original-cell-totals.png)

*Figure 3. Raked counts and 90% draw intervals versus the mother-frame population in each original cell.*

## Measure how raking changes the estimates

`plot_partitioned_raking_changes()` shows how calibration changed vote shares on
demographic margins. Each row shows a grouping, such as age group or
municipality × education. In the outcome panels, dots are subgroup
changes in percentage points and squares are population-weighted
averages. Green marks increases and red marks decreases. Every outcome, including No Vote, is expressed as a share of the
eligible population in that group. Total variation is half the sum of
the absolute changes in these shares, population-weighted across groups.
Its dots show paired draws and its square uses the mean counts.

*Snippet 10. Compare pre-rake and post-rake vote shares across demographic groupings.*


``` r
plot_partitioned_raking_changes(
  extended_parts, raked_parts, frame[c(predictors, "N")], frame_rows,
  area = area, outcome = outcome, no_vote = no_vote, plot_file = figure("raking-changes"),
  read_extended = storage$get, read_raked = identity
)
```

![Changes in vote shares after raking across demographic and municipal groupings.](assets/denmark-2026/raking-changes.png)

*Figure 4. Changes in vote shares after raking across demographic and municipal groupings.*

## Inspect the extended frame

The table shows 100 randomly selected rows from the raked frame.

*Snippet 11. Sample 100 rows from the raked stratification frame.*


``` r
set.seed(2029)
columns <- c(predictors, outcome, "N", "N_draw_1", "N_draw_2")
knitr::kable(sample_extended_frame(raked, columns), digits = 2, row.names = FALSE)
```



|municipality      |gender |age_group |education                                                           |vote_choice_2022                |      N| N_draw_1| N_draw_2|
|:-----------------|:------|:---------|:-------------------------------------------------------------------|:-------------------------------|------:|--------:|--------:|
|Faaborg-Midtfyn   |Women  |75-79     |H2010 Upper secondary education, General (stx, hf, student courses) |F: SF - Socialistisk Folkeparti |   0.97|     1.79|     0.40|
|Haderslev         |Men    |80-84     |H60 Bachelors programs                                              |Other - Left                    |   0.13|     0.18|     0.14|
|Dragør            |Women  |60-64     |H70 Masters programs                                                |F: SF - Socialistisk Folkeparti |   5.12|     5.77|     9.19|
|Solrød            |Women  |75-79     |H1020 Primary school 7th-9th grade                                  |Other - Right                   |  13.51|    10.68|    14.03|
|Haderslev         |Men    |25-29     |H2020 Upper secondary education, General (hhx, htx)                 |F: SF - Socialistisk Folkeparti |   1.73|     1.93|     0.80|
|Rødovre           |Men    |35-39     |H70 Masters programs                                                |M: Moderaterne                  |  27.58|    27.90|    21.73|
|Odense            |Men    |55-59     |H40 Short cycle higher education                                    |Invalid                         |  19.40|    12.51|     9.81|
|Hjørring          |Men    |20-24     |H1020 Primary school 7th-9th grade                                  |Other - Right                   |  35.92|    45.02|    32.22|
|Høje-Taastrup     |Women  |30-34     |H40 Short cycle higher education                                    |No Vote                         |  18.03|    15.13|    11.80|
|Randers           |Men    |50-54     |H40 Short cycle higher education                                    |I: Liberal Alliance             |  39.24|    59.31|    27.28|
|Gladsaxe          |Men    |90-94     |H70 Masters programs                                                |Other                           |   0.01|     0.00|     0.00|
|Billund           |Men    |45-49     |H60 Bachelors programs                                              |Invalid                         |   0.03|     0.02|     0.02|
|Assens            |Men    |20-24     |H1030 Primary school 10th grade                                     |M: Moderaterne                  |  11.22|     5.57|    16.27|
|Ishøj             |Women  |75-79     |H2010 Upper secondary education, General (stx, hf, student courses) |I: Liberal Alliance             |   0.02|     0.04|     0.03|
|Hedensted         |Women  |85-89     |H30 Vocational Education and Training (VET)                         |Æ: Danmarksdemokraterne         |  16.67|    12.89|    17.42|
|Dragør            |Men    |25-29     |H30 Vocational Education and Training (VET)                         |Æ: Danmarksdemokraterne         |   1.01|     0.62|     0.64|
|Hjørring          |Women  |65-69     |H2020 Upper secondary education, General (hhx, htx)                 |I: Liberal Alliance             |   0.33|     0.32|     0.31|
|Solrød            |Women  |55-59     |H50 Vocational bachelors educations                                 |M: Moderaterne                  |  18.31|    14.15|    10.82|
|Lejre             |Men    |45-49     |H70 Masters programs                                                |Invalid                         |   1.05|     1.62|     0.53|
|Ishøj             |Men    |45-49     |H1020 Primary school 7th-9th grade                                  |Other - Right                   |   5.22|     4.34|     6.34|
|Fredensborg       |Women  |75-79     |H2020 Upper secondary education, General (hhx, htx)                 |Æ: Danmarksdemokraterne         |   0.48|     0.78|     1.07|
|Halsnæs           |Men    |20-24     |H50 Vocational bachelors educations                                 |M: Moderaterne                  |   0.20|     0.17|     0.19|
|Assens            |Men    |35-39     |H60 Bachelors programs                                              |M: Moderaterne                  |   1.36|     1.24|     1.38|
|Nordfyns          |Men    |85-89     |H50 Vocational bachelors educations                                 |A: Socialdemokratiet            |   5.58|     5.25|     3.63|
|Rødovre           |Women  |60-64     |H2020 Upper secondary education, General (hhx, htx)                 |Other - Right                   |   2.32|     2.63|     2.69|
|Ringsted          |Women  |60-64     |H40 Short cycle higher education                                    |I: Liberal Alliance             |   1.71|     1.83|     1.94|
|Hørsholm          |Men    |25-29     |H2020 Upper secondary education, General (hhx, htx)                 |Invalid                         |   0.28|     0.35|     0.25|
|Faaborg-Midtfyn   |Women  |20-24     |H50 Vocational bachelors educations                                 |I: Liberal Alliance             |   0.85|     0.72|     0.40|
|Favrskov          |Men    |20-24     |H90 Not stated                                                      |A: Socialdemokratiet            |   0.22|     0.18|     0.42|
|Fredensborg       |Men    |55-59     |H30 Vocational Education and Training (VET)                         |F: SF - Socialistisk Folkeparti |  10.14|    18.52|    13.77|
|Ringsted          |Men    |45-49     |H40 Short cycle higher education                                    |F: SF - Socialistisk Folkeparti |   2.98|     4.93|     0.89|
|Guldborgsund      |Men    |35-39     |H60 Bachelors programs                                              |Other                           |   0.01|     0.00|     0.01|
|Vallensbæk        |Men    |70-74     |H1020 Primary school 7th-9th grade                                  |I: Liberal Alliance             |   1.37|     1.26|     1.08|
|Skive             |Women  |20-24     |H1030 Primary school 10th grade                                     |F: SF - Socialistisk Folkeparti |   6.21|     1.87|     4.31|
|Roskilde          |Women  |65-69     |H70 Masters programs                                                |Invalid                         |   0.83|     0.37|     0.23|
|Tønder            |Women  |20-24     |H70 Masters programs                                                |F: SF - Socialistisk Folkeparti |   0.10|     0.07|     0.13|
|Aalborg           |Men    |60-64     |H50 Vocational bachelors educations                                 |A: Socialdemokratiet            | 228.87|   273.19|   242.87|
|Morsø             |Women  |60-64     |H2020 Upper secondary education, General (hhx, htx)                 |Other - Left                    |   0.53|     0.20|     0.38|
|Vallensbæk        |Women  |30-34     |H60 Bachelors programs                                              |I: Liberal Alliance             |   1.00|     0.63|     1.28|
|Ringkøbing-Skjern |Men    |20-24     |H1020 Primary school 7th-9th grade                                  |Invalid                         |   1.80|     2.10|     0.02|
|Copenhagen        |Women  |75-79     |H1030 Primary school 10th grade                                     |Æ: Danmarksdemokraterne         |   5.52|    10.75|     4.17|
|Sønderborg        |Women  |55-59     |H40 Short cycle higher education                                    |Æ: Danmarksdemokraterne         |  10.31|     7.45|    11.69|
|Copenhagen        |Women  |40-44     |H30 Vocational Education and Training (VET)                         |No Vote                         | 345.17|   311.46|   340.50|
|Gentofte          |Men    |20-24     |H1030 Primary school 10th grade                                     |Invalid                         |   1.48|     2.59|     0.03|
|Svendborg         |Men    |50-54     |H40 Short cycle higher education                                    |Other                           |   0.18|     0.03|     0.01|
|Ishøj             |Women  |50-54     |H1030 Primary school 10th grade                                     |V: Venstre                      |   1.59|     1.29|     0.98|
|Greve             |Men    |70-74     |H40 Short cycle higher education                                    |F: SF - Socialistisk Folkeparti |   1.61|     0.95|     0.72|
|Greve             |Men    |35-39     |H50 Vocational bachelors educations                                 |Other - Right                   |  21.75|    27.86|    17.70|
|Vesthimmerlands   |Men    |20-24     |H2010 Upper secondary education, General (stx, hf, student courses) |Æ: Danmarksdemokraterne         |   3.04|     4.57|     1.83|
|Fanø              |Men    |45-49     |H2010 Upper secondary education, General (stx, hf, student courses) |M: Moderaterne                  |   0.33|     0.26|     0.41|
|Herning           |Women  |90-94     |H1030 Primary school 10th grade                                     |M: Moderaterne                  |   0.49|     0.31|     0.27|
|Gribskov          |Men    |55-59     |H1030 Primary school 10th grade                                     |No Vote                         |  41.91|    39.37|    33.79|
|Roskilde          |Men    |25-29     |H50 Vocational bachelors educations                                 |V: Venstre                      |  27.73|     6.60|    36.52|
|Lemvig            |Men    |60-64     |H40 Short cycle higher education                                    |Other - Right                   |   3.80|     5.14|     3.13|
|Gribskov          |Men    |45-49     |H60 Bachelors programs                                              |M: Moderaterne                  |   3.01|     1.91|     3.78|
|Lyngby-Taarbæk    |Men    |20-24     |H2020 Upper secondary education, General (hhx, htx)                 |I: Liberal Alliance             | 119.76|   116.41|    94.54|
|Tønder            |Women  |55-59     |H60 Bachelors programs                                              |Other                           |   0.01|     0.01|     0.01|
|Vejen             |Women  |35-39     |H90 Not stated                                                      |I: Liberal Alliance             |   0.33|     0.17|     0.16|
|Høje-Taastrup     |Women  |75-79     |H30 Vocational Education and Training (VET)                         |I: Liberal Alliance             |   3.55|     7.49|     4.40|
|Roskilde          |Men    |60-64     |H2010 Upper secondary education, General (stx, hf, student courses) |F: SF - Socialistisk Folkeparti |  10.45|     9.25|     5.20|
|Kalundborg        |Men    |85-89     |H1020 Primary school 7th-9th grade                                  |No Vote                         |  21.39|    24.38|    10.76|
|Herlev            |Men    |35-39     |H40 Short cycle higher education                                    |Other                           |   0.06|     0.01|     0.06|
|Odsherred         |Men    |50-54     |H1020 Primary school 7th-9th grade                                  |M: Moderaterne                  |  14.62|    19.43|    15.97|
|Lejre             |Women  |40-44     |H1030 Primary school 10th grade                                     |I: Liberal Alliance             |   0.64|     0.65|     0.52|
|Holstebro         |Men    |90-94     |H50 Vocational bachelors educations                                 |Other - Left                    |   0.65|     0.51|     0.83|
|Greve             |Women  |55-59     |H60 Bachelors programs                                              |F: SF - Socialistisk Folkeparti |   1.09|     1.01|     0.92|
|Stevns            |Men    |90-94     |H40 Short cycle higher education                                    |F: SF - Socialistisk Folkeparti |   0.07|     0.01|     0.06|
|Nyborg            |Men    |35-39     |H1020 Primary school 7th-9th grade                                  |I: Liberal Alliance             |   5.34|     4.99|     3.02|
|Frederikssund     |Women  |50-54     |H2020 Upper secondary education, General (hhx, htx)                 |Æ: Danmarksdemokraterne         |   1.59|     1.63|     3.78|
|Vallensbæk        |Men    |20-24     |H1020 Primary school 7th-9th grade                                  |Other                           |   0.01|     0.01|     0.00|
|Kerteminde        |Women  |85-89     |H90 Not stated                                                      |Æ: Danmarksdemokraterne         |   0.23|     0.29|     0.14|
|Gribskov          |Men    |50-54     |H60 Bachelors programs                                              |Other                           |   0.01|     0.00|     0.00|
|Holbæk            |Women  |90-94     |H30 Vocational Education and Training (VET)                         |V: Venstre                      |   9.68|     6.82|    13.02|
|Dragør            |Men    |75-79     |H90 Not stated                                                      |Other                           |   0.01|     0.00|     0.00|
|Horsens           |Men    |70-74     |H1030 Primary school 10th grade                                     |Other - Right                   |   6.62|     6.16|     7.89|
|Rudersdal         |Men    |45-49     |H40 Short cycle higher education                                    |I: Liberal Alliance             |  25.61|    37.39|    33.94|
|Middelfart        |Women  |20-24     |H1030 Primary school 10th grade                                     |M: Moderaterne                  |   5.96|     2.90|     7.69|
|Frederikssund     |Men    |75-79     |H30 Vocational Education and Training (VET)                         |Other - Left                    |  25.25|    32.55|    18.58|
|Tårnby            |Women  |85-89     |H60 Bachelors programs                                              |F: SF - Socialistisk Folkeparti |   0.07|     0.04|     0.09|
|Norddjurs         |Men    |40-44     |H60 Bachelors programs                                              |No Vote                         |   0.31|     0.20|     0.44|
|Copenhagen        |Men    |50-54     |H2020 Upper secondary education, General (hhx, htx)                 |Other - Right                   |  43.22|    42.64|    41.55|
|Lemvig            |Men    |80-84     |H70 Masters programs                                                |Other - Left                    |   1.89|     3.55|     2.66|
|Hvidovre          |Women  |80-84     |H30 Vocational Education and Training (VET)                         |Invalid                         |   3.82|     4.33|     1.02|
|Vejen             |Women  |60-64     |H40 Short cycle higher education                                    |V: Venstre                      |   8.69|     5.69|     4.75|
|Roskilde          |Men    |75-79     |H90 Not stated                                                      |Æ: Danmarksdemokraterne         |   1.23|     0.94|     0.98|
|Svendborg         |Men    |70-74     |H60 Bachelors programs                                              |Other - Left                    |   0.75|     0.55|     0.61|
|Fanø              |Women  |75-79     |H40 Short cycle higher education                                    |I: Liberal Alliance             |   0.08|     0.23|     0.06|
|Ballerup          |Men    |65-69     |H70 Masters programs                                                |M: Moderaterne                  |  10.95|    10.67|     9.99|
|Ringsted          |Women  |50-54     |H90 Not stated                                                      |Æ: Danmarksdemokraterne         |   0.07|     0.10|     0.15|
|Gribskov          |Women  |35-39     |H60 Bachelors programs                                              |F: SF - Socialistisk Folkeparti |   1.26|     0.38|     0.61|
|Holbæk            |Women  |25-29     |H60 Bachelors programs                                              |Other - Left                    |   5.61|     7.56|     4.29|
|Frederikssund     |Women  |25-29     |H40 Short cycle higher education                                    |Æ: Danmarksdemokraterne         |   1.31|     0.12|     0.28|
|Favrskov          |Men    |30-34     |H40 Short cycle higher education                                    |Invalid                         |   2.08|     0.81|     1.37|
|Norddjurs         |Men    |60-64     |H40 Short cycle higher education                                    |M: Moderaterne                  |   4.37|     3.71|     3.31|
|Fredensborg       |Men    |35-39     |H70 Masters programs                                                |I: Liberal Alliance             |  38.10|    37.30|    39.79|
|Brønderslev       |Women  |35-39     |H1030 Primary school 10th grade                                     |V: Venstre                      |   6.06|     7.93|     5.90|
|Favrskov          |Men    |65-69     |H50 Vocational bachelors educations                                 |Other - Right                   |  13.05|    15.25|    10.63|
|Sorø              |Women  |35-39     |H50 Vocational bachelors educations                                 |Other - Right                   |  19.80|    26.66|    15.51|
|Stevns            |Men    |80-84     |H1020 Primary school 7th-9th grade                                  |Other - Right                   |  25.56|    19.93|    32.99|
|Roskilde          |Men    |50-54     |H40 Short cycle higher education                                    |F: SF - Socialistisk Folkeparti |  14.14|     6.18|    37.30|

*Table 4. Sample of 100 raked joint rows with their calibrated mean and two posterior draws.*

*Snippet 12. Keep the requested output partitions and release the remaining pre-rake storage.*


``` r
if (storage$mode == "keep") {
  paths <- unlist(extended_parts, use.names = FALSE)
  prune_geographic_partitions(dirname(paths[1L]), paths, "^part-[0-9]+[.]rds$")
}
storage$cleanup()
rm(extended_parts, raked_parts)
```
