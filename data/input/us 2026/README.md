# US 2026 input handoff

This case study uses the 2026 congressional district map (CD120) with 2024
House and presidential vote outcomes. The source is the
[published Polling Without People handoff at `b302fb4`](https://github.com/robertocerinaprojects/polling-without-people/blob/b302fb476c710896f53f7583ac5528bf31ce2524/get_us_2024_xframe_data.md).

## Transfer

The three published files are tracked here under their original names.
Replacement copies are available in the [upstream handoff directory at `b302fb4`](https://github.com/robertocerinaprojects/polling-without-people/tree/b302fb476c710896f53f7583ac5528bf31ce2524/01_frame/data/output/10_handoff).
To rebuild them, run `Rscript 01_frame/scripts/10_prepare_handoff.R` from
the upstream repository root with the saved outputs from Steps 05–07 and 09.

From the VoteFrame repository root, verify the three input files against
the SHA-256 checksums for this handoff.

```sh
cd 'data/input/us 2026'
sha256sum -c <<'CHECKSUMS'
e29d5b82786dd1331526dd473ec906f175f63229086824b365fcaeb0c426ad6f  ces_2024_harmonized.rds
24eedb7512adbb7ae13f7e3156400f7807da9ae40925e13c1a38cfa13ada74d9  district_election_inputs_cd120.csv
495a43f97c0e2f9ec45a6e61462fbd6861f32eb9fd857dcf993d762f9c2613c0  pums_cd_cells.rds
CHECKSUMS
```

| File | Contents |
| --- | --- |
| `district_election_inputs_cd120.csv` | 1,744 rows, one for each of four choices in each of 436 districts, including DC |
| `pums_cd_cells.rds` | 497,550 positive-population cells from 2020–2024 ACS PUMS allocated to CD120. `N` sums to 240,960,972 allocated citizen-adult weights, rounded to the nearest person. |
| `ces_2024_harmonized.rds` | 58,627 eligible CES respondents. The two vote columns refer to the original 2024 ballots. District keys use CD120. |

The survey is derived from Schaffner, Shih, Ansolabehere, and Pope (2025),
*Cooperative Election Study Common Content, 2024*, Harvard Dataverse V9,
released under CC0 1.0. [Source dataset](https://doi.org/10.7910/DVN/X11EP6)

## Geography and outcomes

Join by both `state_fips` and `district_fips`. Preserve leading zeros in
both fields. Use `state_name` for display. The seven
shared frame and survey keys consist of the two FIPS fields and
`age_cat`, `gender_cat`, `race_cat`, `hispanic_cat`, and `educ_cat`.
The two RDS exports preserve missing values and factor levels.

Both survey outcomes use Democratic, Republican, Other, and No Vote.
The election CSV indexes shares by the same labels in `vote_choice`.
Both share columns use an estimated citizen-adult denominator.
`vote_house_2024` is the final district share. `vote_pres_2024` is the
statewide share repeated across districts.

## House scenarios

The upstream pipeline documents these scenarios for the district shares.

| Upstream scenario | Districts | Lineup year | Nominees |
| --- | ---: | ---: | --- |
| `observed_same_footprint` | 290 | 2024 | Actual 2024 nominees in unchanged districts with tabulated returns |
| `model_redrawn` | 144 | 2026 | The 2026 ballot lineup for the hypothetical 2024 contest on CD120 |
| `model_unopposed_2024` | 2 | 2024 | Sole declared 2024 winners Sheila Cherfilus-McCormick (D, FL-20) and Frank D. Lucas (R, OK-03), both incumbents |

`lineup` encodes the party groups represented on the ballot, such as `DR`
for Democratic and Republican nominees. It repeats across all four choices.
`on_ballot` indicates whether a choice has an eligible nominee.
`incumbent` indicates whether at least one such nominee
served in Congress on November 4, 2024, including service in another
district. This date applies to all three scenarios. Both choice-level
ballot fields are blank for No Vote.

## Read in VoteFrame

The [US 2026 vignette](../../../vignettes/vignette_us_2026.md) shows how to read
the three original files and prepare their keys, stage predictors, and
calibration targets directly in the analysis.
