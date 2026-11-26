# Extending the Sweden 2026 population frame

This example extends the Sweden population frame with reported 2022
Riksdag vote choice. It fits one model per sequential stage,
predicts into the population frame, and calibrates the resulting counts
to municipal election shares. The
[input guide](../data/input/sweden%202026/README.md) documents the
source data and outcome recodes. The auxiliary survey comes from the
[Swedish National Election Study 2022 – CSES Edition](https://doi.org/10.5878/2ty4-tk98)
(Oscarsson, Karlsson, Lindskog, and Oleskog Tryggvason, 2024).

## Read and inspect the inputs


*Snippet 1. Read the population frame, survey, and election results and check their keys.*

``` r
knitr::opts_chunk$set(error = FALSE)
for (file in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(file)

country <- file.path("data", "input", "sweden 2026")
survey <- read.csv(file.path(country, "auxiliary_survey.csv"), fileEncoding = "UTF-8")
frame <- read.csv(file.path(country, "frame.csv"), fileEncoding = "UTF-8")
election <- read.csv(file.path(country, "election_results.csv"), fileEncoding = "UTF-8")
area <- "municipality"
parent_area <- Sys.getenv("VOTEFRAME_PARENT_AREA", "constituency")
areas_per_batch <- vignette_areas_per_batch()
outcome <- "vote_choice_2022"
no_vote <- "No Vote"
predictors <- c(area, "sex", "age", "education")
out <- Sys.getenv("VOTEFRAME_SWEDEN_OUTPUT_DIR",
                  unset = file.path("data", "output", basename(country)))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
figure_dir <- file.path("vignettes", "assets", "sweden-2026")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Keep the municipality's constituency as a frame key. The model uses
# municipality as its area, since the constituency is determined by it.
stopifnot(is.numeric(survey$age), is.numeric(frame$age),
          !anyDuplicated(frame[predictors]),
          setequal(frame[[area]], election[[area]]))
cells <- aggregate_extension_survey(
  survey, extension_variable = outcome, predictors = predictors, no_vote = no_vote
)
```

```
## Warning: Removed 476 incomplete survey rows before aggregation.
```

*Snippet 2. Report how many respondents and population cells enter the example.*

``` r
c(survey_rows = nrow(survey), complete_respondents = sum(cells$counts),
  omitted_respondents = cells$omitted, population_cells = nrow(frame),
  municipalities = length(unique(frame[[area]])))
```

```
##          survey_rows complete_respondents  omitted_respondents
##                 2845                 2369                  476
##     population_cells       municipalities
##               361920                  290
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
##  [1] "No Vote" "S"       "M"       "SD"      "C"       "MP"      "L"
##  [8] "V"       "KD"      "Other"   "Invalid"
```

## Select and fit the sequential models

`fit_sequential_logit()` starts each stage's search at **depth 4**.
It screens support, prunes collinear fixed columns while preserving
hierarchy, and reduces depth if the retained design exceeds the
respondent budget. The fit log records the selected designs, the
columns removed, and the solver diagnostics. A stage can return a fit
at the iteration cap, so inspect the solver status before interpreting
its results.

Age enters the model in years, with 95 representing ages 95 and older.
With splines enabled, the fitter considers a smooth age effect and
interactions involving that smooth. The same screening and coefficient
budget apply to these terms.




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
    interaction_support_log = file.path(out, "interaction_support.csv"),
    log_file = file.path(out, "fit_log.csv")
  ),
  message = function(m) cat(conditionMessage(m), file = stderr())
)
saveRDS(fit, file.path(out, "fit.rds"))
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



|outcome | observations| fitted_depth| retained_order| estimated_retained_parameters| dropped_fixed| dropped_scalar_random| dropped_correlated_random| dropped_smooth| fixed_condition_after| iterations_used| max_parameter_change|status   |
|:-------|------------:|------------:|--------------:|-----------------------------:|-------------:|---------------------:|-------------------------:|--------------:|---------------------:|---------------:|--------------------:|:--------|
|No Vote |         2369|            2|              2|                          2214|             0|                  1271|                       670|          13355|             12.388868|             100|            0.0359261|returned |
|S       |         2262|            2|              2|                          2187|             0|                  1252|                       669|          13315|             15.560200|             100|            0.0336359|returned |
|M       |         1476|            2|              2|                          1450|             0|                  1019|                       666|          12695|             11.037620|             100|            0.0779111|returned |
|SD      |         1063|            2|              2|                          1045|             0|                   865|                       637|          11673|              8.666298|             100|            0.0774935|returned |
|C       |          745|            1|              1|                            75|             0|                   180|                         0|              0|             10.793597|              14|            0.0004620|returned |
|MP      |          577|            1|              1|                            71|             0|                   167|                         0|              0|              8.396833|              26|            0.0012906|returned |
|L       |          431|            1|              1|                            67|             0|                   152|                         0|              0|              8.191447|              13|            0.0042818|returned |
|V       |          289|            1|              1|                            63|             0|                   131|                         0|              0|              8.034208|              11|            0.0048167|returned |
|KD      |          158|            1|              1|                            49|             0|                    97|                         0|              0|              8.021605|              20|            0.0079681|returned |
|Other   |           31|            1|              1|                            17|             0|                    29|                         0|              0|              9.159624|              22|            0.0067292|returned |

*Table 1. Fitted stages, with sample sizes, retained design, screened coefficients, and convergence diagnostics.*

In this run, the No Vote, S, M, and SD stages kept age curves by sex,
education, and supported municipalities. Each reached the 100-iteration
cap above the 0.01 maximum parameter change threshold. The other six
stages converged and kept a global age curve. The joint smooth of age
and municipal vote share did not survive screening in any stage.

## Predict and check municipal vote shares before raking

`predict_sequential_logit()` draws the outcomes' cell probabilities.
`aggregate_extended_frame()` multiplies probabilities by population
counts and keeps a row for every original cell and vote choice. The
municipal comparison uses shares **among voters** for party choices and
shares **of the full population** for nonvoting. The among-voters
denominator includes invalid ballots. Posterior intervals
use each draw's own turnout denominator.

The 2022 municipal results also enter as predictors. This comparison
measures agreement with the calibration targets. Sweden uses 125
posterior draws per cell. Batches use constituencies by default and keep
each municipality intact. Without a usable parent geography, the default
is ten municipalities per batch. A shared draw state keeps fitted
coefficients and effects for previously seen levels aligned across
batches.




*Snippet 6. Predict vote choice in geographic batches and expand population counts into choice categories.*

``` r
n_draws <- 125L
frame_rows <- geographic_batches(frame, area, parent_area, areas_per_batch)
part_paths <- function(name) {
  directory <- file.path(out, name)
  dir.create(directory, showWarnings = FALSE)
  file.path(directory, sprintf("part-%02d.rds", seq_along(frame_rows)))
}
save_part <- function(part, path) {
  pending <- paste0(path, ".part")
  saveRDS(part, pending)
  stopifnot(file.rename(pending, path))
}
part_bytes <- pre_rake_frame_bytes(lengths(frame_rows) * length(fit$outcome_order),
                                   n_draws, length(predictors) + 2L)
storage <- vignette_pre_rake_storage(
  out, sum(part_bytes), largest_part_bytes = max(part_bytes),
  final_directory = out, final_bytes = sum(part_bytes)
)
extended_parts <- vector("list", length(frame_rows))
set.seed(2027)
draw_state <- new.env(parent = emptyenv())
for (i in seq_along(frame_rows)) {
  cells <- frame[frame_rows[[i]], , drop = FALSE]
  posterior <- predict_sequential_logit(
    fit, frame = cells, n_draws = n_draws, draw_state = draw_state, write_files = FALSE
  )
  extended <- aggregate_extended_frame(cells, posterior, by = setdiff(names(cells), "N"))
  rm(posterior)
  stopifnot(nrow(extended) == nrow(cells) * length(fit$outcome_order))
  extended_parts[[i]] <- storage$put(extended, sprintf("part-%02d", i))
  rm(extended)
}
rm(draw_state)
```


*Snippet 7. Pool the predicted counts by municipality and compare their shares with the targets.*

``` r
figure <- function(name) file.path(figure_dir, paste0(name, ".png"))
municipal_counts <- function(parts, read_part = readRDS) {
  do.call(rbind, lapply(parts, function(part) {
    counts <- read_part(part)
    keys <- counts[c(area, outcome)]
    group <- frame_cell_keys(keys, keys)
    columns <- c("N", extended_frame_draw_columns(counts, "N"))
    cbind(keys[!duplicated(group), , drop = FALSE],
          rowsum(counts[columns], group, reorder = FALSE))
  }))
}
before <- check_extended_frame(
  municipal_counts(extended_parts, storage$get), election, by = area, extension_variable = outcome,
  target_variable = "share_eligible", no_vote = no_vote, outcome_order = fit$outcome_order,
  plot_file = figure("municipal-before")
)
knitr::kable(before$metrics, digits = 4)
```



|        | bias_pp| RMSE_pp| spearman| coverage_90_pct|
|:-------|-------:|-------:|--------:|---------------:|
|Overall |  0.0000|  2.6142|   0.9724|         99.0000|
|No Vote | -9.6489|  9.8806|   0.9286|          0.0000|
|S       |  3.9370|  4.3017|   0.9455|        100.0000|
|M       |  1.0563|  1.9423|   0.8969|        100.0000|
|SD      | -5.7921|  6.1661|   0.9416|         92.7586|
|C       | -0.3271|  1.0790|   0.8300|        100.0000|
|MP      |  0.2491|  0.6234|   0.9225|        100.0000|
|L       |  1.3182|  1.3731|   0.9664|         99.6552|
|V       |  0.4643|  1.1385|   0.8233|         99.6552|
|KD      | -0.7860|  1.7000|   0.6716|         98.9655|
|Other   |  0.1325|  0.4980|  -0.0082|         99.6552|
|Invalid | -0.2521|  0.4109|   0.0675|         99.3103|

*Table 2. Before-rake municipal vote-share errors and 90% interval coverage against the election benchmarks.*

![Before raking, predicted municipal shares and 90% draw intervals against the election benchmarks.](assets/sweden-2026/municipal-before.png)

*Figure 1. Before raking, predicted municipal shares and 90% draw intervals against the election benchmarks.*

## Rake to the municipal targets

`rake_extended_frame()` takes shares on the population scale. It
reallocates counts across vote choices within each demographic cell,
preserving that cell's total. Raking uses the same constraints for the
mean counts and all 125 draws. Each municipality is constrained
independently, so batches can be raked separately.




*Snippet 8. Calibrate each municipality’s vote totals while preserving the original population cells.*

``` r
rake_tolerance <- 1e-8
raked_paths <- part_paths("raked_frame")
for (i in seq_along(frame_rows)) {
  cells <- frame[frame_rows[[i]], , drop = FALSE]
  targets <- election[election[[area]] %in% cells[[area]], , drop = FALSE]
  raked <- rake_extended_frame(
    storage$get(extended_parts[[i]]), cells, targets, by = area, extension_variable = outcome,
    target_variable = "share_eligible", tolerance = rake_tolerance
  )
  save_part(raked, raked_paths[i])
  rm(raked)
}
```

The final raked frame is stored as RDS partitions under
`data/output/sweden 2026/raked_frame/`. The pre-rake partitions use memory or temporary disk storage according to
available memory. Temporary partitions are removed after the raked output
and diagnostics finish. Set `VOTEFRAME_PRE_RAKE_STORAGE=keep` to save all
pre-rake partitions. Use the saved
`fit.rds` and the prediction chunk to recreate them.

## Check both margins after raking

The post-rake diagnostics repeat the municipality comparisons using
the same denominators and check the total of each original joint
demographic cell.


*Snippet 9. Check the calibrated election margins and compare counts with the original stratification joint.*

``` r
after <- check_extended_frame(
  municipal_counts(raked_paths), election, by = area, extension_variable = outcome,
  target_variable = "share_eligible", no_vote = no_vote, outcome_order = fit$outcome_order,
  coverage_tolerance = 2 * rake_tolerance, plot_file = figure("municipal-after")
)
joint <- do.call(rbind, lapply(seq_along(frame_rows), function(i) {
  post_rake_cell_comparison(readRDS(raked_paths[i]), frame[frame_rows[[i]], , drop = FALSE], outcome)
}))
joint_metrics <- plot_post_rake_cells(joint, figure("original-cell-totals"))
knitr::kable(after$metrics, digits = 4)
```



|        | bias_pp| RMSE_pp| spearman| coverage_90_pct|
|:-------|-------:|-------:|--------:|---------------:|
|Overall |       0|       0|        1|             100|
|No Vote |       0|       0|        1|             100|
|S       |       0|       0|        1|             100|
|M       |       0|       0|        1|             100|
|SD      |       0|       0|        1|             100|
|C       |       0|       0|        1|             100|
|MP      |       0|       0|        1|             100|
|L       |       0|       0|        1|             100|
|V       |       0|       0|        1|             100|
|KD      |       0|       0|        1|             100|
|Other   |       0|       0|        1|             100|
|Invalid |       0|       0|        1|             100|

*Table 3. After-rake municipal vote-share errors and 90% interval coverage against the same benchmarks.*

![After raking, calibrated municipal shares and 90% draw intervals against the election benchmarks.](assets/sweden-2026/municipal-after.png)

*Figure 2. After raking, calibrated municipal shares and 90% draw intervals against the election benchmarks.*

![Raked counts and 90% draw intervals versus the mother-frame population in each original cell.](assets/sweden-2026/original-cell-totals.png)

*Figure 3. Raked counts and 90% draw intervals versus the mother-frame population in each original cell.*

## Measure how raking changes the estimates

`plot_partitioned_raking_changes()` shows how calibration changed vote shares on
demographic margins. Each row shows a grouping, such as age or
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
  extended_parts, raked_paths, frame[c(predictors, "N")], frame_rows,
  area = area, outcome = outcome, no_vote = no_vote, plot_file = figure("raking-changes"),
  read_extended = storage$get
)
```

![Changes in vote shares after raking across demographic and municipal groupings.](assets/sweden-2026/raking-changes.png)

*Figure 4. Changes in vote shares after raking across demographic and municipal groupings.*

## Inspect the extended frame

The table shows 100 randomly selected rows across all raked partitions.


*Snippet 11. Sample 100 rows from the raked stratification frame.*

``` r
set.seed(2029)
columns <- c(predictors, outcome, "N", "N_draw_1", "N_draw_2")
partition_sizes <- lengths(frame_rows) * length(fit$outcome_order)
preview <- sample_extended_frame(raked_paths, columns, partition_sizes)
knitr::kable(preview, digits = 2, row.names = FALSE)
```



|municipality |sex   | age|education                           |vote_choice_2022 |     N| N_draw_1| N_draw_2|
|:------------|:-----|---:|:-----------------------------------|:----------------|-----:|--------:|--------:|
|Hultsfred    |Men   |  45|Compulsory education, under 9 years |MP               |  0.01|     0.01|     0.01|
|Säffle       |Men   |  84|Post-secondary, 3 years or more     |Invalid          |  0.11|     0.46|     0.04|
|Värmdö       |Men   |  26|Post-secondary, under 3 years       |C                |  2.17|     2.05|     3.29|
|Piteå        |Women |  18|Unknown                             |SD               |  0.52|     0.48|     1.41|
|Essunga      |Men   |  28|Compulsory education, under 9 years |Invalid          |  0.00|     0.00|     0.00|
|Vara         |Men   |  40|Post-secondary, under 3 years       |L                |  0.35|     0.23|     0.45|
|Fagersta     |Women |  37|Upper secondary, up to 2 years      |SD               |  3.44|     2.72|     2.56|
|Karlskoga    |Women |  26|Upper secondary, 3 years            |SD               |  8.05|    11.88|     6.38|
|Gällivare    |Men   |  23|Doctoral education                  |V                |  0.00|     0.00|     0.00|
|Arboga       |Men   |  38|Post-secondary, 3 years or more     |No Vote          |  0.56|     0.44|     0.46|
|Rättvik      |Women |  57|Upper secondary, 3 years            |Invalid          |  0.09|     0.00|     0.04|
|Haninge      |Men   |  27|Compulsory education, 9 or 10 years |MP               |  0.66|     0.63|     1.41|
|Töreboda     |Men   |  44|Unknown                             |M                |  0.00|     0.00|     0.00|
|Laholm       |Women |  57|Upper secondary, 3 years            |SD               |  7.13|    10.39|     5.80|
|Umeå         |Women |  38|Post-secondary, 3 years or more     |KD               | 11.98|    12.69|     8.57|
|Torsås       |Men   |  34|Post-secondary, under 3 years       |Invalid          |  0.09|     0.75|     0.14|
|Torsby       |Women |  74|Post-secondary, 3 years or more     |SD               |  0.80|     0.79|     0.75|
|Båstad       |Women |  61|Compulsory education, under 9 years |M                |  0.33|     0.47|     0.62|
|Upplands-Bro |Men   |  48|Compulsory education, 9 or 10 years |V                |  0.55|     0.33|     0.31|
|Vara         |Women |  71|Compulsory education, under 9 years |Invalid          |  0.06|     0.00|     0.02|
|Lomma        |Women |  95|Post-secondary, 3 years or more     |No Vote          |  0.53|     0.86|     0.48|
|Mellerud     |Women |  41|Compulsory education, 9 or 10 years |Invalid          |  0.04|     0.00|     0.04|
|Laxå         |Men   |  50|Post-secondary, under 3 years       |C                |  0.05|     0.04|     0.08|
|Ale          |Women |  74|Post-secondary, under 3 years       |C                |  0.83|     0.65|     0.99|
|Smedjebacken |Men   |  50|Upper secondary, up to 2 years      |S                |  3.34|     2.93|     2.80|
|Svalöv       |Women |  21|Post-secondary, under 3 years       |V                |  1.72|     1.62|     1.92|
|Berg         |Men   |  89|Unknown                             |MP               |  0.00|     0.00|     0.00|
|Enköping     |Men   |  41|Upper secondary, 3 years            |Other            |  3.51|     2.96|     7.51|
|Nykvarn      |Women |  48|Post-secondary, under 3 years       |Other            |  0.04|     0.01|     0.00|
|Ljusdal      |Men   |  45|Compulsory education, under 9 years |KD               |  0.00|     0.00|     0.00|
|Höör         |Men   |  39|Compulsory education, under 9 years |V                |  0.00|     0.00|     0.00|
|Helsingborg  |Men   |  61|Compulsory education, 9 or 10 years |C                |  1.97|     1.27|     1.20|
|Botkyrka     |Women |  35|Unknown                             |L                |  0.33|     0.21|     0.09|
|Båstad       |Women |  65|Compulsory education, 9 or 10 years |M                |  1.60|     1.78|     2.37|
|Gnesta       |Men   |  52|Doctoral education                  |Other            |  0.02|     0.01|     0.01|
|Danderyd     |Women |  65|Post-secondary, under 3 years       |S                |  3.96|     3.40|     2.97|
|Vänersborg   |Women |  20|Doctoral education                  |V                |  0.00|     0.00|     0.00|
|Storuman     |Men   |  34|Post-secondary, 3 years or more     |C                |  0.22|     0.13|     0.20|
|Nyköping     |Women |  70|Upper secondary, up to 2 years      |S                | 50.57|    46.10|    39.49|
|Älvsbyn      |Men   |  75|Upper secondary, 3 years            |MP               |  0.09|     0.12|     0.09|
|Västerås     |Men   |  52|Doctoral education                  |No Vote          |  3.48|     4.76|     2.60|
|Simrishamn   |Men   |  32|Upper secondary, 3 years            |MP               |  1.24|     2.02|     0.80|
|Vilhelmina   |Men   |  25|Compulsory education, 9 or 10 years |MP               |  0.04|     0.03|     0.10|
|Vilhelmina   |Men   |  74|Unknown                             |Invalid          |  0.00|     0.00|     0.00|
|Hedemora     |Women |  48|Doctoral education                  |V                |  0.00|     0.00|     0.00|
|Nyköping     |Women |  76|Compulsory education, under 9 years |L                |  0.61|     0.56|     0.55|
|Älvsbyn      |Men   |  53|Upper secondary, up to 2 years      |M                |  2.87|     2.67|     1.91|
|Ockelbo      |Men   |  65|Upper secondary, 3 years            |M                |  0.58|     0.45|     0.43|
|Smedjebacken |Women |  43|Compulsory education, under 9 years |KD               |  0.03|     0.06|     0.03|
|Köping       |Men   |  39|Post-secondary, under 3 years       |Invalid          |  0.15|     0.70|     0.31|
|Surahammar   |Men   |  75|Upper secondary, 3 years            |C                |  0.27|     0.18|     0.20|
|Sölvesborg   |Women |  94|Doctoral education                  |MP               |  0.00|     0.00|     0.00|
|Robertsfors  |Women |  52|Post-secondary, 3 years or more     |MP               |  1.39|     1.45|     1.31|
|Ånge         |Men   |  85|Compulsory education, 9 or 10 years |S                |  0.00|     0.00|     0.00|
|Kramfors     |Men   |  75|Post-secondary, 3 years or more     |V                |  0.42|     0.31|     0.27|
|Flen         |Women |  18|Upper secondary, 3 years            |V                |  0.00|     0.00|     0.00|
|Värnamo      |Women |  25|Upper secondary, up to 2 years      |C                |  0.33|     0.41|     0.33|
|Lysekil      |Men   |  45|Upper secondary, up to 2 years      |C                |  0.09|     0.10|     0.09|
|Kramfors     |Women |  36|Post-secondary, under 3 years       |Invalid          |  0.31|     0.00|     0.45|
|Färgelanda   |Women |  87|Compulsory education, 9 or 10 years |L                |  0.03|     0.03|     0.01|
|Nyköping     |Men   |  28|Compulsory education, 9 or 10 years |KD               |  0.67|     0.20|     0.26|
|Gällivare    |Women |  87|Compulsory education, 9 or 10 years |V                |  0.05|     0.07|     0.00|
|Mellerud     |Women |  34|Upper secondary, up to 2 years      |Invalid          |  0.05|     0.00|     0.13|
|Älvdalen     |Men   |  89|Compulsory education, 9 or 10 years |No Vote          |  0.09|     0.08|     0.07|
|Åre          |Men   |  21|Unknown                             |S                |  0.25|     0.38|     0.15|
|Borås        |Men   |  68|Post-secondary, under 3 years       |Invalid          |  0.43|     1.18|     0.14|
|Vansbro      |Women |  29|Post-secondary, 3 years or more     |S                |  1.85|     2.45|     2.31|
|Perstorp     |Men   |  52|Upper secondary, up to 2 years      |SD               |  6.16|     4.68|     6.26|
|Eskilstuna   |Women |  91|Unknown                             |L                |  0.05|     0.07|     0.00|
|Svenljunga   |Women |  49|Compulsory education, under 9 years |No Vote          |  0.45|     0.58|     0.47|
|Härryda      |Women |  22|Compulsory education, 9 or 10 years |L                |  0.51|     0.58|     0.35|
|Töreboda     |Women |  83|Compulsory education, 9 or 10 years |MP               |  0.00|     0.00|     0.00|
|Lycksele     |Men   |  28|Post-secondary, 3 years or more     |M                |  2.29|     2.10|     2.79|
|Ludvika      |Women |  68|Compulsory education, under 9 years |KD               |  0.13|     0.15|     0.13|
|Berg         |Men   |  37|Doctoral education                  |C                |  0.00|     0.00|     0.00|
|Öckerö       |Women |  73|Post-secondary, 3 years or more     |C                |  1.48|     1.68|     1.39|
|Ale          |Women |  39|Compulsory education, 9 or 10 years |S                |  2.53|     2.84|     2.88|
|Värmdö       |Women |  72|Doctoral education                  |L                |  0.00|     0.00|     0.00|
|Storfors     |Women |  54|Compulsory education, under 9 years |S                |  0.34|     0.28|     0.38|
|Lekeberg     |Women |  59|Upper secondary, up to 2 years      |SD               |  2.82|     3.06|     2.55|
|Bjuv         |Women |  26|Unknown                             |MP               |  0.04|     0.01|     0.00|
|Orust        |Women |  72|Post-secondary, under 3 years       |S                |  7.26|     7.22|     5.88|
|Huddinge     |Women |  86|Unknown                             |No Vote          |  1.91|     1.17|     2.66|
|Åstorp       |Men   |  23|Compulsory education, under 9 years |L                |  0.01|     0.01|     0.01|
|Södertälje   |Men   |  32|Upper secondary, 3 years            |SD               | 30.18|    46.87|    13.14|
|Gotland      |Men   |  69|Unknown                             |L                |  0.09|     0.06|     0.01|
|Lycksele     |Men   |  59|Unknown                             |C                |  0.00|     0.00|     0.00|
|Täby         |Men   |  72|Upper secondary, 3 years            |V                |  0.76|     1.12|     1.39|
|Lessebo      |Men   |  94|Post-secondary, 3 years or more     |L                |  0.00|     0.00|     0.00|
|Vingåker     |Women |  81|Upper secondary, up to 2 years      |L                |  0.22|     0.20|     0.19|
|Burlöv       |Women |  43|Compulsory education, 9 or 10 years |No Vote          |  1.86|     1.88|     1.38|
|Surahammar   |Women |  81|Compulsory education, 9 or 10 years |C                |  0.18|     0.16|     0.07|
|Västervik    |Women |  74|Post-secondary, under 3 years       |KD               |  1.52|     1.49|     1.11|
|Älvsbyn      |Men   |  93|Upper secondary, 3 years            |M                |  0.00|     0.00|     0.00|
|Malå         |Men   |  34|Upper secondary, 3 years            |KD               |  0.76|     0.97|     0.53|
|Degerfors    |Women |  20|Upper secondary, 3 years            |KD               |  0.65|     0.42|     0.17|
|Skellefteå   |Women |  49|Upper secondary, up to 2 years      |KD               |  0.67|     0.91|     0.76|
|Kumla        |Women |  78|Doctoral education                  |SD               |  0.00|     0.00|     0.00|
|Linköping    |Women |  94|Post-secondary, 3 years or more     |M                |  3.24|     2.21|     3.89|
|Heby         |Women |  64|Post-secondary, 3 years or more     |V                |  0.88|     0.75|     1.15|

*Table 4. Sample of 100 raked joint rows with their calibrated mean and two posterior draws.*

*Snippet 12. Keep the requested output partitions and release the remaining pre-rake storage.*

``` r
if (storage$mode == "keep") {
  paths <- unlist(extended_parts, use.names = FALSE)
  prune_geographic_partitions(dirname(paths[1L]), paths, "^part-[0-9]+[.]rds$")
}
prune_geographic_partitions(dirname(raked_paths[1L]), raked_paths, "^part-[0-9]+[.]rds$")
storage$cleanup()
rm(extended_parts)
```
