# Extending the Denmark 2026 population frame

This example extends the Denmark population frame with reported 2022
parliamentary vote choice. It fits one model per sequential stage,
predicts into the population frame, and calibrates the resulting counts
to municipal election shares. The
[input guide](../data/input/denmark%202026/README.md) documents the
source data and outcome recodes.

## Read and inspect the inputs


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
raked_path <- file.path(out, "raked_frame.rds")
pending <- paste0(raked_path, ".part")
saveRDS(raked, pending)
stopifnot(file.rename(pending, raked_path))
```

The final raked frame is saved to
`data/output/denmark 2026/raked_frame.rds`. The pre-rake frame uses memory or temporary
disk according to available memory. Set `VOTEFRAME_PRE_RAKE_STORAGE=keep`
to save it. Use the saved `fit.rds` and the prediction chunk to recreate it.

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

`plot_partitioned_raking_changes()` shows how calibration changed vote shares on
demographic margins. Each row shows a grouping, such as age group or
municipality × education. In the outcome panels, dots are subgroup
changes in percentage points and squares are population-weighted
averages. Green marks increases and red marks decreases. Shares use
the number of voters for ballot choices and the full group population
for No Vote. The total variation panel summarizes changes across the
whole distribution among voters. Its dots are posterior draws and its
square uses the mean counts.


``` r
change_plot <- plot_partitioned_raking_changes(
  extended_parts, raked_parts, frame[c(predictors, "N")], frame_rows,
  area = area, outcome = outcome, no_vote = no_vote, plot_file = figure("raking-changes"),
  read_extended = storage$get, read_raked = identity
)
knitr::include_graphics(sub("^vignettes/", "", change_plot), error = FALSE)
```

![plot of chunk raking-changes](assets/denmark-2026/raking-changes.png)

## Inspect the extended frame

The table shows 100 randomly selected rows from the raked frame.


``` r
set.seed(2029)
columns <- c(predictors, outcome, "N", "N_draw_1", "N_draw_2")
knitr::kable(sample_extended_frame(raked, columns), digits = 2, row.names = FALSE)
```



|municipality      |gender |age_group |education                                                           |vote_choice_2022                |      N| N_draw_1| N_draw_2|
|:-----------------|:------|:---------|:-------------------------------------------------------------------|:-------------------------------|------:|--------:|--------:|
|Faaborg-Midtfyn   |Women  |75-79     |H2010 Upper secondary education, General (stx, hf, student courses) |F: SF - Socialistisk Folkeparti |   1.00|     0.75|     0.33|
|Haderslev         |Men    |80-84     |H60 Bachelors programs                                              |Other - Left                    |   0.15|     0.15|     0.13|
|Dragør            |Women  |60-64     |H70 Masters programs                                                |F: SF - Socialistisk Folkeparti |   5.03|     6.30|     5.56|
|Solrød            |Women  |75-79     |H1020 Primary school 7th-9th grade                                  |Other - Right                   |  13.80|    16.93|     6.38|
|Haderslev         |Men    |25-29     |H2020 Upper secondary education, General (hhx, htx)                 |F: SF - Socialistisk Folkeparti |   1.75|     1.18|     0.66|
|Rødovre           |Men    |35-39     |H70 Masters programs                                                |M: Moderaterne                  |  27.95|    31.17|    30.39|
|Odense            |Men    |55-59     |H40 Short cycle higher education                                    |Invalid                         |  18.12|    26.25|    28.90|
|Hjørring          |Men    |20-24     |H1020 Primary school 7th-9th grade                                  |Other - Right                   |  37.34|    35.34|    27.90|
|Høje-Taastrup     |Women  |30-34     |H40 Short cycle higher education                                    |No Vote                         |  17.80|    17.78|    11.02|
|Randers           |Men    |50-54     |H40 Short cycle higher education                                    |I: Liberal Alliance             |  41.12|    51.30|    36.29|
|Gladsaxe          |Men    |90-94     |H70 Masters programs                                                |Other                           |   0.01|     0.00|     0.00|
|Billund           |Men    |45-49     |H60 Bachelors programs                                              |Invalid                         |   0.04|     0.14|     0.07|
|Assens            |Men    |20-24     |H1030 Primary school 10th grade                                     |M: Moderaterne                  |  12.04|     7.98|     9.81|
|Ishøj             |Women  |75-79     |H2010 Upper secondary education, General (stx, hf, student courses) |I: Liberal Alliance             |   0.02|     0.01|     0.01|
|Hedensted         |Women  |85-89     |H30 Vocational Education and Training (VET)                         |Æ: Danmarksdemokraterne         |  16.08|    15.00|     7.33|
|Dragør            |Men    |25-29     |H30 Vocational Education and Training (VET)                         |Æ: Danmarksdemokraterne         |   1.00|     0.78|     1.31|
|Hjørring          |Women  |65-69     |H2020 Upper secondary education, General (hhx, htx)                 |I: Liberal Alliance             |   0.37|     0.48|     0.23|
|Solrød            |Women  |55-59     |H50 Vocational bachelors educations                                 |M: Moderaterne                  |  17.04|    22.66|    16.30|
|Lejre             |Men    |45-49     |H70 Masters programs                                                |Invalid                         |   0.95|     0.24|     0.21|
|Ishøj             |Men    |45-49     |H1020 Primary school 7th-9th grade                                  |Other - Right                   |   5.33|     4.89|     6.04|
|Fredensborg       |Women  |75-79     |H2020 Upper secondary education, General (hhx, htx)                 |Æ: Danmarksdemokraterne         |   0.48|     0.67|     0.03|
|Halsnæs           |Men    |20-24     |H50 Vocational bachelors educations                                 |M: Moderaterne                  |   0.20|     0.14|     0.18|
|Assens            |Men    |35-39     |H60 Bachelors programs                                              |M: Moderaterne                  |   1.38|     1.49|     1.31|
|Nordfyns          |Men    |85-89     |H50 Vocational bachelors educations                                 |A: Socialdemokratiet            |   5.71|     9.90|     6.43|
|Rødovre           |Women  |60-64     |H2020 Upper secondary education, General (hhx, htx)                 |Other - Right                   |   2.40|     2.05|     1.66|
|Ringsted          |Women  |60-64     |H40 Short cycle higher education                                    |I: Liberal Alliance             |   1.71|     1.42|     1.85|
|Hørsholm          |Men    |25-29     |H2020 Upper secondary education, General (hhx, htx)                 |Invalid                         |   0.27|     0.01|     1.82|
|Faaborg-Midtfyn   |Women  |20-24     |H50 Vocational bachelors educations                                 |I: Liberal Alliance             |   0.88|     0.80|     0.63|
|Favrskov          |Men    |20-24     |H90 Not stated                                                      |A: Socialdemokratiet            |   0.21|     0.19|     0.27|
|Fredensborg       |Men    |55-59     |H30 Vocational Education and Training (VET)                         |F: SF - Socialistisk Folkeparti |   8.92|     3.91|     6.30|
|Ringsted          |Men    |45-49     |H40 Short cycle higher education                                    |F: SF - Socialistisk Folkeparti |   3.55|     5.95|     2.48|
|Guldborgsund      |Men    |35-39     |H60 Bachelors programs                                              |Other                           |   0.01|     0.00|     0.00|
|Vallensbæk        |Men    |70-74     |H1020 Primary school 7th-9th grade                                  |I: Liberal Alliance             |   1.55|     0.37|     2.31|
|Skive             |Women  |20-24     |H1030 Primary school 10th grade                                     |F: SF - Socialistisk Folkeparti |   5.87|     5.36|     5.90|
|Roskilde          |Women  |65-69     |H70 Masters programs                                                |Invalid                         |   0.86|     0.44|     0.24|
|Tønder            |Women  |20-24     |H70 Masters programs                                                |F: SF - Socialistisk Folkeparti |   0.10|     0.06|     0.06|
|Aalborg           |Men    |60-64     |H50 Vocational bachelors educations                                 |A: Socialdemokratiet            | 226.66|   169.59|   241.82|
|Morsø             |Women  |60-64     |H2020 Upper secondary education, General (hhx, htx)                 |Other - Left                    |   0.52|     1.01|     0.34|
|Vallensbæk        |Women  |30-34     |H60 Bachelors programs                                              |I: Liberal Alliance             |   1.00|     1.54|     1.24|
|Ringkøbing-Skjern |Men    |20-24     |H1020 Primary school 7th-9th grade                                  |Invalid                         |   1.66|     8.59|     0.38|
|Copenhagen        |Women  |75-79     |H1030 Primary school 10th grade                                     |Æ: Danmarksdemokraterne         |   5.84|     6.10|     4.66|
|Sønderborg        |Women  |55-59     |H40 Short cycle higher education                                    |Æ: Danmarksdemokraterne         |  10.29|     8.82|     4.09|
|Copenhagen        |Women  |40-44     |H30 Vocational Education and Training (VET)                         |No Vote                         | 344.77|   291.49|   351.89|
|Gentofte          |Men    |20-24     |H1030 Primary school 10th grade                                     |Invalid                         |   1.47|     4.03|     0.00|
|Svendborg         |Men    |50-54     |H40 Short cycle higher education                                    |Other                           |   0.22|     0.05|     1.73|
|Ishøj             |Women  |50-54     |H1030 Primary school 10th grade                                     |V: Venstre                      |   1.53|     0.64|     1.18|
|Greve             |Men    |70-74     |H40 Short cycle higher education                                    |F: SF - Socialistisk Folkeparti |   1.85|     1.24|     0.76|
|Greve             |Men    |35-39     |H50 Vocational bachelors educations                                 |Other - Right                   |  21.86|    29.79|    19.63|
|Vesthimmerlands   |Men    |20-24     |H2010 Upper secondary education, General (stx, hf, student courses) |Æ: Danmarksdemokraterne         |   2.92|     1.74|     0.50|
|Fanø              |Men    |45-49     |H2010 Upper secondary education, General (stx, hf, student courses) |M: Moderaterne                  |   0.35|     0.33|     0.60|
|Herning           |Women  |90-94     |H1030 Primary school 10th grade                                     |M: Moderaterne                  |   0.50|     0.56|     0.66|
|Gribskov          |Men    |55-59     |H1030 Primary school 10th grade                                     |No Vote                         |  42.37|    44.91|    31.80|
|Roskilde          |Men    |25-29     |H50 Vocational bachelors educations                                 |V: Venstre                      |  27.07|    19.14|    20.05|
|Lemvig            |Men    |60-64     |H40 Short cycle higher education                                    |Other - Right                   |   3.94|     4.38|     3.95|
|Gribskov          |Men    |45-49     |H60 Bachelors programs                                              |M: Moderaterne                  |   3.03|     3.05|     3.62|
|Lyngby-Taarbæk    |Men    |20-24     |H2020 Upper secondary education, General (hhx, htx)                 |I: Liberal Alliance             | 117.05|   129.91|   132.64|
|Tønder            |Women  |55-59     |H60 Bachelors programs                                              |Other                           |   0.01|     0.00|     0.00|
|Vejen             |Women  |35-39     |H90 Not stated                                                      |I: Liberal Alliance             |   0.33|     0.22|     0.53|
|Høje-Taastrup     |Women  |75-79     |H30 Vocational Education and Training (VET)                         |I: Liberal Alliance             |   3.88|     0.99|     0.91|
|Roskilde          |Men    |60-64     |H2010 Upper secondary education, General (stx, hf, student courses) |F: SF - Socialistisk Folkeparti |  10.21|     7.62|     4.95|
|Kalundborg        |Men    |85-89     |H1020 Primary school 7th-9th grade                                  |No Vote                         |  20.61|    31.89|     8.18|
|Herlev            |Men    |35-39     |H40 Short cycle higher education                                    |Other                           |   0.10|     0.00|     0.16|
|Odsherred         |Men    |50-54     |H1020 Primary school 7th-9th grade                                  |M: Moderaterne                  |  14.72|    11.53|    12.62|
|Lejre             |Women  |40-44     |H1030 Primary school 10th grade                                     |I: Liberal Alliance             |   0.59|     0.23|     0.70|
|Holstebro         |Men    |90-94     |H50 Vocational bachelors educations                                 |Other - Left                    |   0.67|     1.15|     0.62|
|Greve             |Women  |55-59     |H60 Bachelors programs                                              |F: SF - Socialistisk Folkeparti |   1.04|     0.96|     1.42|
|Stevns            |Men    |90-94     |H40 Short cycle higher education                                    |F: SF - Socialistisk Folkeparti |   0.08|     0.11|     0.01|
|Nyborg            |Men    |35-39     |H1020 Primary school 7th-9th grade                                  |I: Liberal Alliance             |   5.48|     3.40|     8.11|
|Frederikssund     |Women  |50-54     |H2020 Upper secondary education, General (hhx, htx)                 |Æ: Danmarksdemokraterne         |   1.39|     1.93|     0.00|
|Vallensbæk        |Men    |20-24     |H1020 Primary school 7th-9th grade                                  |Other                           |   0.01|     0.01|     0.01|
|Kerteminde        |Women  |85-89     |H90 Not stated                                                      |Æ: Danmarksdemokraterne         |   0.22|     0.01|     0.20|
|Gribskov          |Men    |50-54     |H60 Bachelors programs                                              |Other                           |   0.01|     0.00|     0.00|
|Holbæk            |Women  |90-94     |H30 Vocational Education and Training (VET)                         |V: Venstre                      |   9.75|    18.95|    11.27|
|Dragør            |Men    |75-79     |H90 Not stated                                                      |Other                           |   0.01|     0.02|     0.00|
|Horsens           |Men    |70-74     |H1030 Primary school 10th grade                                     |Other - Right                   |   6.35|     3.48|     7.97|
|Rudersdal         |Men    |45-49     |H40 Short cycle higher education                                    |I: Liberal Alliance             |  25.04|    27.79|    29.63|
|Middelfart        |Women  |20-24     |H1030 Primary school 10th grade                                     |M: Moderaterne                  |   6.37|     5.95|     4.81|
|Frederikssund     |Men    |75-79     |H30 Vocational Education and Training (VET)                         |Other - Left                    |  25.95|    33.92|    20.57|
|Tårnby            |Women  |85-89     |H60 Bachelors programs                                              |F: SF - Socialistisk Folkeparti |   0.07|     0.06|     0.10|
|Norddjurs         |Men    |40-44     |H60 Bachelors programs                                              |No Vote                         |   0.31|     0.17|     0.27|
|Copenhagen        |Men    |50-54     |H2020 Upper secondary education, General (hhx, htx)                 |Other - Right                   |  44.52|    32.41|    49.58|
|Lemvig            |Men    |80-84     |H70 Masters programs                                                |Other - Left                    |   1.82|     2.29|     2.39|
|Hvidovre          |Women  |80-84     |H30 Vocational Education and Training (VET)                         |Invalid                         |   4.67|     0.46|     4.91|
|Vejen             |Women  |60-64     |H40 Short cycle higher education                                    |V: Venstre                      |   9.12|     9.47|    10.98|
|Roskilde          |Men    |75-79     |H90 Not stated                                                      |Æ: Danmarksdemokraterne         |   1.33|     0.44|     3.63|
|Svendborg         |Men    |70-74     |H60 Bachelors programs                                              |Other - Left                    |   0.81|     0.57|     0.81|
|Fanø              |Women  |75-79     |H40 Short cycle higher education                                    |I: Liberal Alliance             |   0.09|     0.02|     0.04|
|Ballerup          |Men    |65-69     |H70 Masters programs                                                |M: Moderaterne                  |  11.09|    11.55|    10.37|
|Ringsted          |Women  |50-54     |H90 Not stated                                                      |Æ: Danmarksdemokraterne         |   0.06|     0.00|     0.01|
|Gribskov          |Women  |35-39     |H60 Bachelors programs                                              |F: SF - Socialistisk Folkeparti |   1.40|     1.59|     3.19|
|Holbæk            |Women  |25-29     |H60 Bachelors programs                                              |Other - Left                    |   5.87|     5.42|     4.41|
|Frederikssund     |Women  |25-29     |H40 Short cycle higher education                                    |Æ: Danmarksdemokraterne         |   1.33|     0.41|     0.15|
|Favrskov          |Men    |30-34     |H40 Short cycle higher education                                    |Invalid                         |   1.75|     1.91|     0.67|
|Norddjurs         |Men    |60-64     |H40 Short cycle higher education                                    |M: Moderaterne                  |   4.63|     3.52|     4.45|
|Fredensborg       |Men    |35-39     |H70 Masters programs                                                |I: Liberal Alliance             |  37.21|    22.95|    46.31|
|Brønderslev       |Women  |35-39     |H1030 Primary school 10th grade                                     |V: Venstre                      |   5.75|     7.16|     2.81|
|Favrskov          |Men    |65-69     |H50 Vocational bachelors educations                                 |Other - Right                   |  12.66|    16.20|     7.96|
|Sorø              |Women  |35-39     |H50 Vocational bachelors educations                                 |Other - Right                   |  18.89|    28.38|    16.09|
|Stevns            |Men    |80-84     |H1020 Primary school 7th-9th grade                                  |Other - Right                   |  25.29|    36.60|    23.92|
|Roskilde          |Men    |50-54     |H40 Short cycle higher education                                    |F: SF - Socialistisk Folkeparti |  13.14|    12.34|     5.99|

``` r
legacy_extended_path <- file.path(out, "extended_frame.rds")
if (storage$mode != "keep" && file.exists(legacy_extended_path)) {
  stopifnot(file.remove(legacy_extended_path))
}
if (storage$mode == "keep") {
  paths <- unlist(extended_parts, use.names = FALSE)
  prune_geographic_partitions(dirname(paths[1L]), paths, "^part-[0-9]+[.]rds$")
}
storage$cleanup()
rm(extended_parts, raked_parts)
```
