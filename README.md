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

## Denmark 2026

The [Denmark 2026 inputs](data/input/denmark%202026/) were prepared for studies
before the 2026 election. Past vote refers to the 1 November 2022 election.

| Input snapshot | Rows |
| --- | ---: |
| [`election_results.csv`](data/input/denmark%202026/election_results.csv) | 988 |
| [`frame.csv`](data/input/denmark%202026/frame.csv) | 28,984 |
| [`auxiliary_survey.csv`](data/input/denmark%202026/auxiliary_survey.csv) | 2,823 |

### Rebuild from public sources

Run from the repository root with Python 3.10 or later.

```sh
python3 data/input/build_denmark_2026.py
```

The [build script](data/input/build_denmark_2026.py) downloads into
`raw/` and runs the [recodes](data/input/denmark%202026/processing/recodes.py).
It writes the three candidate CSVs to `processing/rebuilt/` for comparison.
Use `--refresh` to fetch fresh extracts. The archive and generated files are
ignored by Git.

- **Frame.** FOLK1D supplies 2026 Q1 Danish-citizen counts by municipality, sex
  and age. HFUDD11 supplies 2025 educational attainment counts across all
  ancestries. The rebuild allocates the FOLK1D counts using the HFUDD11
  education proportions within each municipality, sex, and five-year age
  band. For ages 70–94 it uses the local 65–69 distribution. Christiansø is
  outside the frame, matching the snapshot. All 2,940
  municipality-by-sex-by-age population totals match the snapshot, including
  the total of 4,163,940. The education allocation differs and produces
  31,208 rows. The original education extract and older-age allocation
  rules have yet to be recovered.
  [FOLK1D](https://www.statbank.dk/FOLK1D) ·
  [HFUDD11](https://www.statbank.dk/HFUDD11)
- **Election results.** FVKOM supplies 2022 municipality-level votes and
  electorate counts. The rebuild computes nonvoting as electorate minus
  ballots cast, groups minor parties and invalid ballots, and divides
  by the electorate. It produces 990 rows across 99 areas, including
  Christiansø. The snapshot uses a different share calculation.
  [FVKOM](https://www.statbank.dk/FVKOM) ·
  [Official 2022 results](https://www.dst.dk/valg/Valg1968094/valgopg/valgopg.htm)
- **Auxiliary survey.** The rebuild reads the archived DNES 2022 release,
  table `FV22_MAIN_8feb2024`. It exports the five modeling columns and
  reproduces gender, age group, education level, and past vote for all
  2,823 respondents. Municipality labels match for 2,791 respondents.
  [Rigsarkivet, DNES 2022](https://doi.org/10.5279/dk-ra-51035)

### Verify the exact snapshots

The country folder contains the archived inputs. Verify their bytes against
the original commit with these commands.

```bash
archive=315b2c165d08cf9178aec7b0073002335f504866:python_test/data/denmark
country='data/input/denmark 2026'
git show "$archive/frame.csv" | cmp - "$country/frame.csv"
git show "$archive/election_results.csv" | cmp - "$country/election_results.csv"
git show "$archive/survey_DNES.csv" | cmp - "$country/auxiliary_survey.csv"
```

[data/output/](data/output/) is the destination for generated modeling files.

## Legacy workflow

[legacy/](legacy/) contains the previous R modules, scripts, Python harness,
and documentation. The [previously extended Denmark frame](legacy/python_test/data/denmark/denmark_strat_frame_extended.csv)
is available there as a reference.
