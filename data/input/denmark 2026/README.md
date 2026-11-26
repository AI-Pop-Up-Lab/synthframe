# Denmark 2026

These inputs support a study of voting in Denmark’s 2026 parliamentary
election. The population frame describes Danish citizens by municipality, sex,
age, and estimated education. The area-level election results and the individual
survey responses both refer to the preceding parliamentary election on
1 November 2022.

| File | What each row describes | Modeling columns |
| --- | --- | --- |
| `frame.csv` | A population cell | `municipality`, `gender`, `age_group`, `education`, `N`, `constituency_code`, `constituency` |
| `election_results.csv` | A 2022 election outcome in an area | `municipality_code`, `municipality`, `constituency`, `vote_choice_2022`, `share_eligible` |
| `auxiliary_survey.csv` | One survey respondent | `municipality`, `gender`, `age_group`, `education`, `vote_choice_2022`, `constituency_code`, `constituency` |

## Rebuild the inputs

Run this command from the repository root with Python 3.10 or later.

```sh
python3 "data/input/denmark 2026/processing/build_denmark_2026.py"
```

The [build script](processing/build_denmark_2026.py) downloads source extracts and
Statbank table metadata into `raw/`. It passes the inputs to
[recodes.py](processing/recodes.py), which transforms them into CSVs in
`processing/rebuilt/`. The checked-in input files and the rebuild outputs
have separate paths. Use `--refresh` to download fresh copies of the same
source selections. The source and generated files are ignored by Git.
Statbank can revise historical figures, so keep the downloaded files if you
need to reproduce a particular run.

## Population frame

Danish citizenship limits the population to the group the parliamentary study
aims to represent. The general rule for voting in a Folketing election also
requires residence and a minimum age of 18. This frame covers ages 20–94 in
98 municipalities and excludes Christiansø. It uses a population count as
of 2026 Q1 and does not enumerate people on the election register.
[Danish Parliament voting rules](https://www.thedanishparliament.dk/en/democracy/elections-and-voting)

1. **Count Danish citizens.** In FOLK1D, select the municipal codes under
   `OMRÅDE`, Danish citizenship (`STATSB=DANSK`), both sexes (`KØN=1,2`),
   single ages 20–94, and quarter `Tid=2026K1`. Sum the single-age counts
   into five-year bands separately for each municipality and sex.
   [Statistics Denmark FOLK1D](https://www.statbank.dk/FOLK1D)
2. **Estimate education shares.** HFUDD11 reports education by origin.
   Select people of Danish origin (`HERKOMST=5`) as a proxy for the
   education distribution of Danish citizens. Use each municipality
   (`BOPOMR`), both sexes (`KØN=M,K`), and the age bands and years below.
   This assumes the Danish-origin shares also describe citizens of other
   origins within the same municipality, sex, and age group.
   [Statistics Denmark HFUDD11](https://www.statbank.dk/HFUDD11)
3. **Align the education categories.** Select `H1010`, `H1020`, `H1030`,
   `H2010`, `H2020`, `H2030`, `H30`, `H40`, `H50`, `H60`,
   `H70`, and `H90`. Combine international upper secondary (`H2030`)
   with the HHX/HTX modeling category (`H2020`). Keep education not
   stated (`H90`). The separate PhD category (`H80`) is outside the
   shared education categories and is excluded from the share denominator.
4. **Allocate the population.** Within each municipality, sex, and age
   band, divide each selected education count by the sum of the selected
   education counts after recoding. Multiply by the Danish-citizen
   population total. Keep fractional counts so the cells sum to that total.
   Emit rows for education categories with a positive source count.

HFUDD11 stops at ages 65–69. For ages 70–84, use the same municipality and
sex’s 65–69 profile from when those cohorts were younger, measured from the
2024 education reference year. The corresponding years for ages 85–94
precede the HFUDD11 series, so use the 80–84 profile for both older bands.
This assumes the historical education mix is a usable estimate for the cohort
today, even as its membership and place of residence change.

| Age in frame | HFUDD11 age | HFUDD11 year |
| --- | --- | --- |
| 20–69 | Same five-year band | 2024 |
| 70–74 | 65–69 | 2019 |
| 75–79 | 65–69 | 2014 |
| 80–84 | 65–69 | 2009 |
| 85–89 | 65–69 | 2009 |
| 90–94 | 65–69 | 2009 |

For example, the 2024 profile for men aged 20–24 in Aabenraa has 359 vocational
education records out of 1,156 selected records. The 2026 Q1 citizen count for
that group is 1,202, giving `N = 1,202 × 359 / 1,156 = 373.285467…` for
vocational education. The frame contains 28,980 education cells across
2,940 municipality-by-sex-by-age groups and represents 4,163,940 citizens.
The rebuild checks that the education allocations sum to the citizen
population in every group.

## Election results

Request all areas and result categories from FVKOM for 2022
(`OMRÅDE=*`, `VALRES=*`, `Tid=2022`).
Use the 98 municipalities in the population frame. The code keeps the six
named parties, groups other parties into `Other - Left` or `Other - Right`
where mapped, and counts valid votes for independent candidates under
`Other`. The grouped counts must sum to FVKOM's valid vote total.

The denominator is the official 2022 electoral-roll count
(`VAELG`) from FVKOM. Divide each party or party group's valid vote
count by that denominator for `share_eligible`. `Invalid` uses FVKOM's
total invalid ballots, including blanks. `No Vote` is the number on
the electoral roll minus all valid and invalid ballots cast. The
shares sum to one in every municipality and use the same schema as
Sweden's election results. The calibration applies these shares to
the population in the Denmark frame.

Each municipality maps to one of the ten parliamentary multi-member
constituencies (*storkredse*). The frame and survey carry both the
2022 Statistics Denmark constituency code and the constituency name.
The election results carry the constituency name and the municipality
code as in the Sweden results. Survey rows with unavailable municipality
also have unavailable constituency.
[2022 constituency schedule](https://www.retsinformation.dk/eli/lta/2022/294)

[Statistics Denmark FVKOM](https://www.statbank.dk/FVKOM) ·
[Official 2022 results](https://www.dst.dk/valg/Valg1968094/valgopg/valgopg.htm)

## Auxiliary survey

Use the Danish National Election Study 2022 release `FV22_MAIN_8feb2024`
from Rigsarkivet. Read `Data/CSV/Dataset.csv` from the download using its
Windows-1252 encoding. The survey supplies individual-level education and
past vote for estimating their relationship with the shared covariates.

- Map `kommunekode` to municipality names using the FOLK1D area metadata.
- Map the survey’s `gender` codes and derive the 2026 age band from
  `2026 - birthyear`.
- Use `Q91` to choose between the higher education response (`Q92`) and
  the school education response (`Q90`). Map the response to a shared
  education label.
- Use `Q17` for the reported 2022 vote. Map code `32` (`Stemte blankt`)
  to `Invalid`, codes `30` (another party) and `31` (an independent
  candidate) to `Other`, and code `33` to `No Vote`. Use `Q16`
  codes `2`, `3`, and `4` to identify respondents who said they did not
  vote, including those who considered voting.

The survey uses the frame's names and labels for `municipality`, `gender`,
`age_group`, and `education`. The survey exports those four covariates and
`vote_choice_2022`, which uses the same labels in the survey and election results.
The frame's `N` is an aggregate population count. Empty survey cells mark
missing or unmapped responses, including gender responses outside the frame's
two categories and education responses
without a mapping. The explicit `H90 Not stated` education code remains a
category. The rebuild checks all populated survey labels against the
reference inputs and keeps respondents with missing fields.

The code contains the survey response mappings.
[Rigsarkivet DNES 2022](https://doi.org/10.5279/dk-ra-51035) ·
[Survey and election recodes](processing/recodes.py)
