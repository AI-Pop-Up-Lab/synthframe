# Denmark 2026 harmonisation handoff

Start with the 67 DNES respondents whose municipality, gender, age group, and
education labels are all present in the frame individually, but whose exact
combination is absent. They span 62 distinct combinations. Of the 67, 64
have a recorded vote choice. The education labels are H1010 for 52
respondents, H90 for 13, and H60 for 2. Check the source counts and the DNES
education recode for these combinations before deciding whether the frame
or the survey needs a change.

The frame and election results cover the same 98 municipalities. DNES has
recorded respondents in 94. Fanø, Læsø, Samsø, and Ærø have no recorded
respondents. Survey blanks are 11 for municipality, 14 for gender, 10 for
age group, 158 for education, and 51 for vote choice. These counts overlap.

The published survey has five columns, `municipality`, `gender`,
`age_group`, `education`, and `vote_choice_2022`. All populated survey
labels match the frame or the election results, as appropriate. The
election results use `party` for the same vote-choice labels. Explicit
blank votes are their own choice and count toward turnout, which is
measured against the frame population. The DNES turnout difference is
expected and requires no adjustment.

Run `python3 data/input/build_denmark_2026.py` from the repository root.
The last rebuild reproduced all three checked-in CSVs. See the
[Denmark 2026 README](data/input/denmark%202026/README.md) for sources and
the current recoding rules.
