# VoteFrame

VoteFrame aims to extend a population stratification frame with a categorical
variable observed in survey data. The planned workflow fits a multilevel model
using shared covariates and area-level predictors, then uses posterior draws
to estimate the population count in each cell for each outcome category.

## Country input layout

Each country and election cycle has its own folder under [data/input/](data/input/).
The folder contains three modeling inputs and two subfolders.

| Entry | Contents |
| --- | --- |
| `election_results.csv` | Area-level results from the last general election before the study election |
| `frame.csv` | Deepest available population stratification frame before extension |
| `auxiliary_survey.csv` | Processed national election study |
| `raw/` | Downloaded source extracts and metadata |
| `processing/` | Recoding code, intermediate files, and candidate rebuilds |

## Turnout and past vote

For each area, turnout is the sum of votes for the modeled vote choices
divided by that area's total population in the frame. The `Did not vote` share
is the remaining frame population divided by the same total. Include valid
votes and any separately recorded blank ballots when the auxiliary survey
offers a blank-vote choice. Other invalid ballots contribute to `Did not vote`.

## Denmark 2026

The [Denmark 2026 README](data/input/denmark%202026/README.md) documents the
population frame, the 2022 election results, and the auxiliary survey. It
includes the source selections, recoding rules, and rebuild instructions.

[data/output/](data/output/) is the destination for generated modeling files.
