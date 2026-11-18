# Denmark 2026

These inputs support a study of voting in Denmark’s 2026 parliamentary
election. The population frame describes Danish citizens by municipality, sex,
age, and estimated education. The area-level election results and the individual
survey responses both refer to the preceding parliamentary election on
1 November 2022.

| File | What each row describes | Modeling columns |
| --- | --- | --- |
| `frame.csv` | A population cell | `municipality`, `gender`, `age_group`, `education`, `N` |
| `election_results.csv` | An election outcome in an area | `party`, `municipality`, `year`, `pop_pct` |
| `auxiliary_survey.csv` | One survey respondent | `municipality`, `gender`, `age_group`, `education`, `vote_choice_2022` |

## Rebuild the inputs

Run this command from the repository root with Python 3.10 or later.

```sh
python3 data/input/build_denmark_2026.py
```

The [build script](../build_denmark_2026.py) downloads source extracts and
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

FVKOM reports two blank-ballot counts under the English label `Blanks`.
Add codes `1` (polling station) and `15` (advance voting) to get the
`Blank vote` count. Include these ballots in modeled turnout because the
survey offers an explicit blank-vote choice.

For each municipality, sum `N` across the 2026 Q1 frame. Divide each party's
2022 valid vote count and the blank-ballot count by that population to get
`pop_pct`. Subtract both the valid and blank ballots from the frame population
and divide the remainder by the same population to get `Did not vote`.
Other invalid ballots contribute to that remainder. This is a frame-based
past-vote measure that uses counts from two reference years.
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
  to `Blank vote`, codes `30` (another party) and `31` (an independent
  candidate) to `Other`, and code `33` to `Did not vote`. Use `Q16`
  codes `2`, `3`, and `4` to identify respondents who said they did not
  vote, including those who considered voting.

The survey uses the frame's names and labels for `municipality`, `gender`,
`age_group`, and `education`. The survey exports those four covariates and
`vote_choice_2022`, whose labels match the election results' `party` column.
The frame's `N` is an aggregate population count. Empty survey cells mark
missing or unmapped responses, including gender responses outside the frame's
two categories and education responses
without a mapping. The explicit `H90 Not stated` education code remains a
category. The rebuild checks all populated survey labels against the
reference inputs and keeps respondents with missing fields.

The code contains the survey response mappings.
[Rigsarkivet DNES 2022](https://doi.org/10.5279/dk-ra-51035) ·
[Survey and election recodes](processing/recodes.py)
