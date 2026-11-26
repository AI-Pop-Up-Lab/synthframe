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
parent_area <- Sys.getenv("VOTEFRAME_PARENT_AREA", "constituency")
areas_per_batch <- vignette_areas_per_batch()
outcome <- "vote_choice_2022"
no_vote <- "No Vote"
predictors <- c(area, "gender", "age_group", "education")
out <- Sys.getenv("VOTEFRAME_DENMARK_OUTPUT_DIR",
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
columns removed, and the solver diagnostics. A stage can return a fit
at the iteration cap, so inspect the solver status before interpreting
its results.

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
|Invalid                         |           39|            4|              2|                            28|             0|                    39|                       741|           2688|             16.719593|              11|            0.0075036|returned |

*Table 1. Fitted stages, with sample sizes, retained design, screened coefficients, and convergence diagnostics.*

The Invalid stage accepted candidate depth 4. Its largest retained terms
combine gender and 2022 vote share, so its retained order is 2.

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
municipal_counts <- do.call(rbind, lapply(extended_parts, function(part) {
  counts <- storage$get(part)
  keys <- counts[c(area, outcome)]
  group <- frame_cell_keys(keys, keys)
  columns <- c("N", extended_frame_draw_columns(counts, "N"))
  cbind(keys[!duplicated(group), , drop = FALSE],
        rowsum(counts[columns], group, reorder = FALSE))
}))
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
|Overall                         |   0.0000|  2.2832|   0.9281|         90.9184|
|No Vote                         | -11.5011| 11.6288|   0.9863|          0.0000|
|A: Socialdemokratiet            |   0.7978|  2.5061|   0.8760|         94.8980|
|Other - Left                    |   0.9213|  2.0847|   0.9406|         70.4082|
|V: Venstre                      |   0.9741|  1.3384|   0.9720|        100.0000|
|F: SF - Socialistisk Folkeparti |   2.0672|  2.2628|   0.9501|         92.8571|
|Other - Right                   |  -2.2257|  2.5442|   0.8522|         92.8571|
|M: Moderaterne                  |   0.7115|  1.2028|   0.9167|         97.9592|
|I: Liberal Alliance             |  -0.6263|  2.8022|  -0.0829|         86.7347|
|Æ: Danmarksdemokraterne         |  -2.9448|  4.3074|   0.9108|         75.5102|
|Invalid                         |  -0.0087|  0.3626|   0.7756|        100.0000|
|Other                           |   0.3336|  0.3660|   0.1232|         97.9592|

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
disk according to available memory. Set `VOTEFRAME_PRE_RAKE_STORAGE=keep`
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
averages. Green marks increases and red marks decreases. Shares use
the number of voters for ballot choices and the full group population
for No Vote. The total variation panel summarizes changes across the
whole distribution among voters. Its dots are posterior draws and its
square uses the mean counts.

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
|Faaborg-Midtfyn   |Women  |75-79     |H2010 Upper secondary education, General (stx, hf, student courses) |F: SF - Socialistisk Folkeparti |   0.99|     0.96|     0.85|
|Haderslev         |Men    |80-84     |H60 Bachelors programs                                              |Other - Left                    |   0.13|     0.18|     0.11|
|Dragør            |Women  |60-64     |H70 Masters programs                                                |F: SF - Socialistisk Folkeparti |   4.91|     2.68|     3.22|
|Solrød            |Women  |75-79     |H1020 Primary school 7th-9th grade                                  |Other - Right                   |  14.12|    13.41|    23.18|
|Haderslev         |Men    |25-29     |H2020 Upper secondary education, General (hhx, htx)                 |F: SF - Socialistisk Folkeparti |   1.81|     0.79|     0.30|
|Rødovre           |Men    |35-39     |H70 Masters programs                                                |M: Moderaterne                  |  27.78|    27.18|    17.84|
|Odense            |Men    |55-59     |H40 Short cycle higher education                                    |Invalid                         |  19.45|    11.38|    27.15|
|Hjørring          |Men    |20-24     |H1020 Primary school 7th-9th grade                                  |Other - Right                   |  36.31|    28.07|    33.05|
|Høje-Taastrup     |Women  |30-34     |H40 Short cycle higher education                                    |No Vote                         |  17.94|    14.95|    10.44|
|Randers           |Men    |50-54     |H40 Short cycle higher education                                    |I: Liberal Alliance             |  39.92|    60.36|    33.08|
|Gladsaxe          |Men    |90-94     |H70 Masters programs                                                |Other                           |   0.01|     0.02|     0.00|
|Billund           |Men    |45-49     |H60 Bachelors programs                                              |Invalid                         |   0.04|     0.01|     0.02|
|Assens            |Men    |20-24     |H1030 Primary school 10th grade                                     |M: Moderaterne                  |  10.67|     5.47|     4.29|
|Ishøj             |Women  |75-79     |H2010 Upper secondary education, General (stx, hf, student courses) |I: Liberal Alliance             |   0.02|     0.06|     0.03|
|Hedensted         |Women  |85-89     |H30 Vocational Education and Training (VET)                         |Æ: Danmarksdemokraterne         |  16.97|    19.02|    19.52|
|Dragør            |Men    |25-29     |H30 Vocational Education and Training (VET)                         |Æ: Danmarksdemokraterne         |   1.03|     0.96|     1.39|
|Hjørring          |Women  |65-69     |H2020 Upper secondary education, General (hhx, htx)                 |I: Liberal Alliance             |   0.31|     0.29|     0.44|
|Solrød            |Women  |55-59     |H50 Vocational bachelors educations                                 |M: Moderaterne                  |  17.72|    27.35|    12.45|
|Lejre             |Men    |45-49     |H70 Masters programs                                                |Invalid                         |   0.84|     0.83|     0.56|
|Ishøj             |Men    |45-49     |H1020 Primary school 7th-9th grade                                  |Other - Right                   |   5.21|     3.65|     6.23|
|Fredensborg       |Women  |75-79     |H2020 Upper secondary education, General (hhx, htx)                 |Æ: Danmarksdemokraterne         |   0.49|     0.39|     0.60|
|Halsnæs           |Men    |20-24     |H50 Vocational bachelors educations                                 |M: Moderaterne                  |   0.21|     0.17|     0.35|
|Assens            |Men    |35-39     |H60 Bachelors programs                                              |M: Moderaterne                  |   1.37|     1.03|     1.41|
|Nordfyns          |Men    |85-89     |H50 Vocational bachelors educations                                 |A: Socialdemokratiet            |   5.60|     3.92|     4.42|
|Rødovre           |Women  |60-64     |H2020 Upper secondary education, General (hhx, htx)                 |Other - Right                   |   2.41|     2.10|     2.67|
|Ringsted          |Women  |60-64     |H40 Short cycle higher education                                    |I: Liberal Alliance             |   1.67|     0.99|     1.94|
|Hørsholm          |Men    |25-29     |H2020 Upper secondary education, General (hhx, htx)                 |Invalid                         |   0.30|     0.02|     0.33|
|Faaborg-Midtfyn   |Women  |20-24     |H50 Vocational bachelors educations                                 |I: Liberal Alliance             |   0.83|     0.59|     0.69|
|Favrskov          |Men    |20-24     |H90 Not stated                                                      |A: Socialdemokratiet            |   0.22|     0.19|     0.38|
|Fredensborg       |Men    |55-59     |H30 Vocational Education and Training (VET)                         |F: SF - Socialistisk Folkeparti |   9.99|    10.14|    16.02|
|Ringsted          |Men    |45-49     |H40 Short cycle higher education                                    |F: SF - Socialistisk Folkeparti |   3.45|     3.62|     4.31|
|Guldborgsund      |Men    |35-39     |H60 Bachelors programs                                              |Other                           |   0.01|     0.01|     0.00|
|Vallensbæk        |Men    |70-74     |H1020 Primary school 7th-9th grade                                  |I: Liberal Alliance             |   1.41|     1.26|     1.22|
|Skive             |Women  |20-24     |H1030 Primary school 10th grade                                     |F: SF - Socialistisk Folkeparti |   6.71|     1.81|    10.76|
|Roskilde          |Women  |65-69     |H70 Masters programs                                                |Invalid                         |   0.70|     0.36|     0.29|
|Tønder            |Women  |20-24     |H70 Masters programs                                                |F: SF - Socialistisk Folkeparti |   0.11|     0.11|     0.05|
|Aalborg           |Men    |60-64     |H50 Vocational bachelors educations                                 |A: Socialdemokratiet            | 231.05|   289.59|   137.97|
|Morsø             |Women  |60-64     |H2020 Upper secondary education, General (hhx, htx)                 |Other - Left                    |   0.53|     0.35|     0.17|
|Vallensbæk        |Women  |30-34     |H60 Bachelors programs                                              |I: Liberal Alliance             |   1.01|     0.54|     1.09|
|Ringkøbing-Skjern |Men    |20-24     |H1020 Primary school 7th-9th grade                                  |Invalid                         |   1.54|     1.38|     0.01|
|Copenhagen        |Women  |75-79     |H1030 Primary school 10th grade                                     |Æ: Danmarksdemokraterne         |   5.42|     9.49|     5.79|
|Sønderborg        |Women  |55-59     |H40 Short cycle higher education                                    |Æ: Danmarksdemokraterne         |  10.96|     5.84|    16.19|
|Copenhagen        |Women  |40-44     |H30 Vocational Education and Training (VET)                         |No Vote                         | 344.49|   306.79|   346.02|
|Gentofte          |Men    |20-24     |H1030 Primary school 10th grade                                     |Invalid                         |   1.64|     2.85|     0.04|
|Svendborg         |Men    |50-54     |H40 Short cycle higher education                                    |Other                           |   0.27|     0.03|     0.11|
|Ishøj             |Women  |50-54     |H1030 Primary school 10th grade                                     |V: Venstre                      |   1.77|     2.64|     1.87|
|Greve             |Men    |70-74     |H40 Short cycle higher education                                    |F: SF - Socialistisk Folkeparti |   1.88|     0.86|     1.47|
|Greve             |Men    |35-39     |H50 Vocational bachelors educations                                 |Other - Right                   |  21.69|    33.10|     8.01|
|Vesthimmerlands   |Men    |20-24     |H2010 Upper secondary education, General (stx, hf, student courses) |Æ: Danmarksdemokraterne         |   3.16|     4.37|     4.35|
|Fanø              |Men    |45-49     |H2010 Upper secondary education, General (stx, hf, student courses) |M: Moderaterne                  |   0.35|     0.20|     0.32|
|Herning           |Women  |90-94     |H1030 Primary school 10th grade                                     |M: Moderaterne                  |   0.49|     0.31|     0.37|
|Gribskov          |Men    |55-59     |H1030 Primary school 10th grade                                     |No Vote                         |  42.22|    55.72|    48.15|
|Roskilde          |Men    |25-29     |H50 Vocational bachelors educations                                 |V: Venstre                      |  27.65|    33.52|    38.51|
|Lemvig            |Men    |60-64     |H40 Short cycle higher education                                    |Other - Right                   |   3.77|     4.67|     3.41|
|Gribskov          |Men    |45-49     |H60 Bachelors programs                                              |M: Moderaterne                  |   2.97|     1.66|     2.13|
|Lyngby-Taarbæk    |Men    |20-24     |H2020 Upper secondary education, General (hhx, htx)                 |I: Liberal Alliance             | 118.82|   116.88|    94.24|
|Tønder            |Women  |55-59     |H60 Bachelors programs                                              |Other                           |   0.01|     0.06|     0.00|
|Vejen             |Women  |35-39     |H90 Not stated                                                      |I: Liberal Alliance             |   0.33|     0.11|     0.14|
|Høje-Taastrup     |Women  |75-79     |H30 Vocational Education and Training (VET)                         |I: Liberal Alliance             |   3.62|     5.11|     6.77|
|Roskilde          |Men    |60-64     |H2010 Upper secondary education, General (stx, hf, student courses) |F: SF - Socialistisk Folkeparti |  10.40|     5.86|     6.95|
|Kalundborg        |Men    |85-89     |H1020 Primary school 7th-9th grade                                  |No Vote                         |  21.15|    33.05|    17.76|
|Herlev            |Men    |35-39     |H40 Short cycle higher education                                    |Other                           |   0.08|     0.01|     0.26|
|Odsherred         |Men    |50-54     |H1020 Primary school 7th-9th grade                                  |M: Moderaterne                  |  14.36|    17.44|    11.15|
|Lejre             |Women  |40-44     |H1030 Primary school 10th grade                                     |I: Liberal Alliance             |   0.62|     0.92|     0.63|
|Holstebro         |Men    |90-94     |H50 Vocational bachelors educations                                 |Other - Left                    |   0.63|     0.55|     0.67|
|Greve             |Women  |55-59     |H60 Bachelors programs                                              |F: SF - Socialistisk Folkeparti |   1.04|     0.70|     2.67|
|Stevns            |Men    |90-94     |H40 Short cycle higher education                                    |F: SF - Socialistisk Folkeparti |   0.08|     0.09|     0.05|
|Nyborg            |Men    |35-39     |H1020 Primary school 7th-9th grade                                  |I: Liberal Alliance             |   5.35|     6.25|     2.85|
|Frederikssund     |Women  |50-54     |H2020 Upper secondary education, General (hhx, htx)                 |Æ: Danmarksdemokraterne         |   1.54|     1.69|     3.90|
|Vallensbæk        |Men    |20-24     |H1020 Primary school 7th-9th grade                                  |Other                           |   0.01|     0.00|     0.00|
|Kerteminde        |Women  |85-89     |H90 Not stated                                                      |Æ: Danmarksdemokraterne         |   0.23|     0.14|     0.28|
|Gribskov          |Men    |50-54     |H60 Bachelors programs                                              |Other                           |   0.01|     0.01|     0.00|
|Holbæk            |Women  |90-94     |H30 Vocational Education and Training (VET)                         |V: Venstre                      |   9.92|     4.98|     5.55|
|Dragør            |Men    |75-79     |H90 Not stated                                                      |Other                           |   0.01|     0.00|     0.00|
|Horsens           |Men    |70-74     |H1030 Primary school 10th grade                                     |Other - Right                   |   6.47|     3.80|     8.40|
|Rudersdal         |Men    |45-49     |H40 Short cycle higher education                                    |I: Liberal Alliance             |  24.99|    34.12|    29.72|
|Middelfart        |Women  |20-24     |H1030 Primary school 10th grade                                     |M: Moderaterne                  |   5.94|     3.70|     4.25|
|Frederikssund     |Men    |75-79     |H30 Vocational Education and Training (VET)                         |Other - Left                    |  24.96|    28.74|    23.38|
|Tårnby            |Women  |85-89     |H60 Bachelors programs                                              |F: SF - Socialistisk Folkeparti |   0.07|     0.03|     0.06|
|Norddjurs         |Men    |40-44     |H60 Bachelors programs                                              |No Vote                         |   0.31|     0.20|     0.32|
|Copenhagen        |Men    |50-54     |H2020 Upper secondary education, General (hhx, htx)                 |Other - Right                   |  43.69|    41.54|    51.05|
|Lemvig            |Men    |80-84     |H70 Masters programs                                                |Other - Left                    |   1.85|     2.41|     1.76|
|Hvidovre          |Women  |80-84     |H30 Vocational Education and Training (VET)                         |Invalid                         |   4.09|     5.48|     1.94|
|Vejen             |Women  |60-64     |H40 Short cycle higher education                                    |V: Venstre                      |   9.63|    17.27|     6.68|
|Roskilde          |Men    |75-79     |H90 Not stated                                                      |Æ: Danmarksdemokraterne         |   1.23|     1.37|     0.89|
|Svendborg         |Men    |70-74     |H60 Bachelors programs                                              |Other - Left                    |   0.75|     0.64|     0.44|
|Fanø              |Women  |75-79     |H40 Short cycle higher education                                    |I: Liberal Alliance             |   0.08|     0.20|     0.04|
|Ballerup          |Men    |65-69     |H70 Masters programs                                                |M: Moderaterne                  |  10.77|     8.44|     6.46|
|Ringsted          |Women  |50-54     |H90 Not stated                                                      |Æ: Danmarksdemokraterne         |   0.07|     0.05|     0.12|
|Gribskov          |Women  |35-39     |H60 Bachelors programs                                              |F: SF - Socialistisk Folkeparti |   1.31|     1.67|     3.13|
|Holbæk            |Women  |25-29     |H60 Bachelors programs                                              |Other - Left                    |   5.60|    10.04|     8.63|
|Frederikssund     |Women  |25-29     |H40 Short cycle higher education                                    |Æ: Danmarksdemokraterne         |   1.24|     0.30|     0.75|
|Favrskov          |Men    |30-34     |H40 Short cycle higher education                                    |Invalid                         |   1.86|     0.81|     2.13|
|Norddjurs         |Men    |60-64     |H40 Short cycle higher education                                    |M: Moderaterne                  |   4.41|     5.07|     2.84|
|Fredensborg       |Men    |35-39     |H70 Masters programs                                                |I: Liberal Alliance             |  38.76|    25.94|    28.97|
|Brønderslev       |Women  |35-39     |H1030 Primary school 10th grade                                     |V: Venstre                      |   5.47|     5.63|     2.88|
|Favrskov          |Men    |65-69     |H50 Vocational bachelors educations                                 |Other - Right                   |  12.75|    14.13|    10.96|
|Sorø              |Women  |35-39     |H50 Vocational bachelors educations                                 |Other - Right                   |  19.28|    17.98|    21.36|
|Stevns            |Men    |80-84     |H1020 Primary school 7th-9th grade                                  |Other - Right                   |  25.46|    18.58|    29.43|
|Roskilde          |Men    |50-54     |H40 Short cycle higher education                                    |F: SF - Socialistisk Folkeparti |  13.26|     5.31|    13.50|

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
