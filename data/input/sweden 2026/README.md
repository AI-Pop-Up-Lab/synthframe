# Sweden 2026

## Population frame

[frame.csv](frame.csv) estimates the number of Swedish citizens aged 18
and older who lived in each municipality on 31 December 2025, by age, sex,
and education. Municipality and constituency names match the `municipality` and
`constituency` columns in [election_results.csv](election_results.csv). The builder uses SCB codes
to match the source tables before writing the names. This is a resident
population estimate for poststratification. Age and residence are
measured at year-end, so the frame excludes people who were 17 then and
turned 18 by election day, and Swedish voters living abroad.

### Population sources

All three extracts are from Statistics Sweden (SCB) and use the same
reference date.

| Raw filename | Contents used |
| --- | --- |
| [TAB4320_sv.zip](https://www.statistikdatabasen.scb.se/Resources/PX/bulk/ssd/sv/TAB4320_sv.zip) | Resident counts by municipality, single year of age through 94 (then 95+), sex, and educational attainment. |
| [TAB5557_sv.zip](https://www.statistikdatabasen.scb.se/Resources/PX/bulk/ssd/sv/TAB5557_sv.zip) | Resident counts by municipality, sex, and age. Select total marital status and the population count. |
| [TAB6659_en.zip](https://www.statistikdatabasen.scb.se/Resources/PX/bulk/ssd/en/TAB6659_en.zip) | Foreign citizens by municipality, sex, and age band. SCB excludes dual citizens who hold Swedish citizenship from this count. |

[SCB education metadata](https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__UF__UF0506/UtbBefRegionR/) ·
[SCB population metadata](https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101A/BefolkningCKM/) ·
[SCB foreign citizenship metadata](https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BE__BE0101__BE0101F/UtlmedbTotNKCKM/)

### Processing

1. Keep the 290 municipality codes from the election results. For each
   municipality and sex, match the resident and foreign-citizen counts
   for ages 10–19, 20–29, 30–39, 40–49, 50–59, 60–69, and 70+. Use the
   published resident totals for the ten-year groups. For 70+, sum the
   resident counts for ages 70–99 and 100+.
2. Subtract foreign citizens from residents in each matched group to
   estimate the number of Swedish citizens. Allocate that estimate to
   single ages in proportion to their resident population counts
   within the group. This assumes the citizenship rate is constant
   across ages within each municipality, sex, and age band. In the
   10–19 band, retain only the shares assigned to ages 18 and 19.
   Combine ages 95 and older to match the education table.
3. For each municipality, sex, and adult age, calculate the shares of
   residents in SCB's eight education categories, including unknown
   education. Multiply the estimated number of citizens at that age
   by each share. This assumes that, at the same age and sex within
   a municipality, Swedish citizens have the same education distribution
   as all residents. If a cell has estimated citizens and its education
   counts sum to zero, put its entire estimate in the unknown category.

SCB applies statistical disclosure protection to the population and
citizenship tables. Independently published totals and their component
counts can differ. Step 2 uses each age's share of the sum of the
single-age counts, so the allocated estimates reconcile to the published
band total after subtracting foreign citizens. Keep fractional `N`
values to preserve the totals. [SCB disclosure protection](https://www.scb.se/en/documentation/statististical-methods/protection-methods-for-disclosure-control/)

### Worked example from Upplands Väsby

The tables follow men aged 10–19 in municipality `0114` through the
three processing steps. Estimated counts are rounded here for display.

**1. Match the population and citizenship tables**

| Before matching | Municipality | Sex | Age band | Published count |
| --- | --- | --- | --- | ---: |
| Resident population | `0114` | Men | 10–19 | 3,364 |
| Foreign citizens | `0114` | Men | 10–19 | 314 |

| After matching | Sex | Age band | Residents | Foreign citizens |
| --- | --- | --- | ---: | ---: |
| Upplands Väsby | Men | 10–19 | 3,364 | 314 |

**2. Subtract foreign citizens and allocate to adult ages**

| Before allocation | Published resident count |
| --- | ---: |
| Ages 10–19, published band total | 3,364 |
| Ages 10–19, sum of published single ages | 3,371 |
| Age 18 | 331 |
| Age 19 | 342 |

Subtract 314 from the published band total of 3,364 to estimate **3,050**
Swedish citizens aged 10–19. Allocate that total using the single-age
resident counts and retain ages 18 and 19 for the adult frame.

| After allocation | Share of single-age resident total | Estimated Swedish citizens |
| --- | ---: | ---: |
| Age 18 | 331 / 3,371 | 299.48 |
| Age 19 | 342 / 3,371 | 309.43 |
| **Adult subtotal** | **673 / 3,371** | **608.91** |

**3. Split the adult estimate by education**

The education register reports 332 residents across its categories for
18-year-old men in Upplands Väsby. Use that total to calculate education
shares, then apply those shares to the estimated 299.48 Swedish citizens
at age 18. The table shows the levels with positive resident counts.

| Education | Before, resident count | Share of education counts | After, estimated Swedish citizens |
| --- | ---: | ---: | ---: |
| Compulsory education, 9 or 10 years | 314 | 314 / 332 | 283.24 |
| Upper secondary, 3 years | 2 | 2 / 332 | 1.80 |
| Unknown | 16 | 16 / 332 | 14.43 |
| **Total** | **332** | **100%** | **299.48** |

### Frame output and validation

| Column | Meaning |
| --- | --- |
| `municipality` | Swedish municipality name, as spelled in the election results. |
| `constituency` | Riksdag constituency name, as spelled in the election results. |
| `age` | Numeric age in years. The integer 95 represents ages 95 and older. |
| `sex` | SCB's recorded sex category, translated as `Men` or `Women`. |
| `education` | SCB education level translated into English, including `Unknown`. |
| `N` | Estimated resident Swedish citizens in the cell. |

The script checks that the source tables cover the same municipality and
sex groups, that every adult age has all eight education levels, and that
citizen totals reconcile within each group after allocation. For ages
18–19, the target is the adult share of the estimated 10–19 citizen
total. The frame contains 361,920 cells across 290 municipalities, with
an estimated total of 7,785,905.81 adult citizens. The fallback for
empty education breakdowns was unused in this build.

### Rebuild the population frame

Download the three archives above into `raw/` under the stated filenames.
With `election_results.csv` present, run this from the repository root.

```sh
python3 "data/input/sweden 2026/processing/build_population_frame.py"
```

The script uses the Python standard library and writes `frame.csv`.

## Auxiliary survey

[auxiliary_survey.csv](auxiliary_survey.csv) has one row per respondent
to the 2022 Swedish National Election Study CSES edition. Its columns
are `municipality`, `constituency`, `age`, `sex`, `education`, and
`vote_choice_2022`. The geography and demographic labels match the population
frame, and the vote choice labels match the election results.

The respondent and municipality source files were joined on `lopnr`.
The extract omits that key. Constituency names come from the municipality
mapping in the official 2022 election results. Sex and age come from
registers. Age is numeric and is capped at 95 to match the frame. Self-reported
education (`v7301`) is recoded to approximate the frame categories.
Reported party choice (`v7000`) and register turnout (`v7020`) identify
party choice and nonvoting. Self-reported blank ballots map to
`Invalid`. The official results' `Invalid` category also covers other
rejected ballots. Missing responses remain empty, and both geography
fields are empty for the five respondents without a usable municipality.

The extract contains 2,845 respondents, of whom 2,369 have all six fields
populated. The original study files are private and stored in the
Git-ignored `raw/` folder.

**Data credit.** Oscarsson, H., Karlsson, R., Lindskog, H., and Oleskog
Tryggvason, P. (2024). *The Swedish National Election Study 2022 – CSES
Edition* (Version 1). University of Gothenburg. Data collection by the
SOM Institute on behalf of the Swedish National Election Studies
Program. [Dataset DOI](https://doi.org/10.5878/2ty4-tk98)

## 2022 Riksdag results

[election_results.csv](election_results.csv) contains the final 2022
parliamentary returns for all 290 municipalities. There is one row
per municipality and `vote_choice_2022`. Join on the four-character
`municipality_code`, keeping leading zeros. The file includes each
municipality's Riksdag constituency name.

The eight parliamentary parties have separate rows. `Other` pools valid
votes for other participating parties. `Invalid` includes blank ballots
and other invalid ballots. `No Vote` is the number of eligible voters
minus ballots cast. Use `share_eligible` as the municipality target for all 11 choices.
Its denominator is the official electoral-roll count. The model treats
`Invalid` as a ballot-casting outcome separate from `No Vote`. A voter
can deliberately cast a blank ballot, but the combined invalid count
does not tell us which ballots were deliberate. The 11 shares are
fractions from 0 to 1 and sum to one within each municipality.
[Swedish Election Authority ballot guide](https://www.val.se/det-svenska-valsystemet/sa-funkar-rostning-i-svenska-val/sa-funkar-valsedlar-och-personrostning)

## Constituencies and candidate lists

The Riksdag has 29 multi-member constituencies. Of its 349 seats, 310 are
fixed constituency seats and 39 are adjustment seats that bring the overall
distribution closer to national party vote shares. A party qualifies with
at least 4% nationwide or, for fixed seats in a particular constituency,
at least 12% there. Seats won by each party go to its candidates. Personal
votes can move a candidate ahead of the list order once the candidate
receives at least 5% of the party's votes in that constituency. Remaining
seats are filled under the list-order rules.
[Swedish Parliament election guide](https://www.riksdagen.se/en/how-the-riksdag-works/democracy/elections-to-the-riksdag/)

## Rebuild

Download the Swedish Election Authority (Valmyndigheten) files below into
`raw/` under the indicated names, then run
`python3 "data/input/sweden 2026/processing/build_election_results.py"`
from the repository root. The script uses the standard Python library.
It reads the 2022 columns of the results workbook and takes
municipality names and codes from SCB’s `TAB6659_en.zip`, listed under
Population sources above.

| Local filename | Official source |
| --- | --- |
| `results-2022.xlsx` | [Final Riksdag results workbook](https://www.val.se/download/18.162047b519a91d05331197bd/1786611369096/slutligt-valresultat-riksdagen-jamforande-statistik-2018-2022.xlsx) |
