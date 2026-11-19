"""Transform official Denmark extracts into the three modeling inputs."""
from collections import Counter, defaultdict
from math import fsum, isclose


PARTIES = {
    "A": "A: Socialdemokratiet",
    "F": "F: SF - Socialistisk Folkeparti",
    "I": "I: Liberal Alliance",
    "M": "M: Moderaterne",
    "V": "V: Venstre",
    "Æ": "Æ: Danmarksdemokraterne",
    **dict.fromkeys("BQØÅ", "Other - Left"),
    **dict.fromkeys("CDKO", "Other - Right"),
}


def age_band(age):
    lower = age // 5 * 5
    return f"{lower}-{lower + 4}"


# For ages above the HFUDD11 limit, follow the cohorts back from the 2024
# education reference year to when they were 65–69. Reuse the 80–84
# profile for ages 85–94 because their corresponding cohort years, 2004
# and 1999, precede the HFUDD11 series.
OLDER_EDUCATION_PROFILE_YEARS = {
    "70-74": "2019",
    "75-79": "2014",
    "80-84": "2009",
    "85-89": "2009",
    "90-94": "2009",
}


def build_frame(population, attainment, labels):
    """Allocate Danish-citizen totals using Danish-origin education shares."""
    population_totals = Counter()
    for row in population:
        age = int(row["ALDER"].split()[0])
        group = (row["OMRÅDE"], row["KØN"], age_band(age))
        population_totals[group] += int(row["INDHOLD"])

    education_counts = defaultdict(Counter)
    for row in attainment:
        source_age_band = row["ALDER"].split()[0]
        profile_key = (row["BOPOMR"], row["KØN"], source_age_band, row["TID"])
        education_code = row["HFUDD"].split()[0]

        # International upper secondary shares the HHX/HTX modeling category.
        # The shared education categories have no separate PhD category.
        if education_code == "H80":
            continue
        if education_code == "H2030":
            education_code = "H2020"
        education_counts[profile_key][education_code] += int(row["INDHOLD"])

    rows = []
    for (municipality, gender, ages), citizen_total in sorted(population_totals.items()):
        source_year = OLDER_EDUCATION_PROFILE_YEARS.get(ages, "2024")
        source_age_band = "65-69" if ages in OLDER_EDUCATION_PROFILE_YEARS else ages
        profile = education_counts[municipality, gender, source_age_band, source_year]
        source_total = profile.total()
        if source_total <= 0:
            raise ValueError(
                f"Missing education profile for {municipality}, {gender}, "
                f"ages {source_age_band} in {source_year}"
            )

        # Keep fractional counts so the education cells sum to the citizen total.
        allocated_total = 0
        for education_code, source_count in sorted(profile.items()):
            if source_count == 0:
                continue
            allocated_count = citizen_total * source_count / source_total
            rows.append({
                "municipality": municipality,
                "gender": gender,
                "age_group": ages,
                "education": labels[education_code],
                "N": allocated_count,
            })
            allocated_total += allocated_count

        if not isclose(allocated_total, citizen_total, rel_tol=0, abs_tol=1e-6):
            raise ValueError(
                f"Education allocation changed the total for {municipality}, {gender}, {ages}"
            )
    return rows


def build_election(election, frame, labels):
    """Express modeled vote counts as shares of each municipality's frame population."""
    frame_counts = defaultdict(list)
    for row in frame:
        frame_counts[row["municipality"]].append(row["N"])

    election_counts = defaultdict(dict)
    blank_ballot_counts = Counter()
    blank_ballot_labels = {labels["1"], labels["15"]}
    for row in election:
        if row["TID"] != "2022":
            raise ValueError("Election extract must contain only 2022")
        municipality = row["OMRÅDE"]
        result_label = row["VALRES"]
        count = int(row["INDHOLD"])

        # FVKOM uses the same English label for polling-station and advance
        # blank ballots. Add both rows before indexing the other results.
        if result_label in blank_ballot_labels:
            blank_ballot_counts[municipality] += count
        else:
            election_counts[municipality][result_label] = count

    rows = []
    for municipality, cell_counts in sorted(frame_counts.items()):
        # Education splits integer population counts into fractional cells.
        # Recover the integer total after summing away the education split.
        allocated_population = fsum(cell_counts)
        frame_population = round(allocated_population)
        if frame_population <= 0 or not isclose(
            allocated_population, frame_population, rel_tol=0, abs_tol=1e-6
        ):
            raise ValueError(f"Invalid frame population for {municipality}")
        if municipality not in election_counts:
            raise ValueError(f"Missing election results for {municipality}")
        votes = election_counts[municipality]

        valid_votes_by_party = Counter()
        for label, count in votes.items():
            party_code, separator, _ = label.partition(". ")
            if separator and len(party_code) == 1 and party_code.isalpha():
                party = PARTIES.get(party_code, "Other")
                valid_votes_by_party[party] += count
        valid_votes_by_party["Other"] += votes[labels["UDENFOR_IALT"]]

        valid_vote_total = valid_votes_by_party.total()
        if valid_vote_total != votes[labels["GYLD_IALT"]] or min(valid_votes_by_party.values()) < 0:
            raise ValueError(f"Valid vote counts do not add up in {municipality}")
        blank_ballots = blank_ballot_counts[municipality]
        if not 0 <= blank_ballots <= votes[labels["UGYLD_IALT"]]:
            raise ValueError(f"Invalid blank ballot count in {municipality}")
        modeled_vote_total = valid_vote_total + blank_ballots
        if modeled_vote_total > frame_population:
            raise ValueError(f"Modeled votes exceed the frame population in {municipality}")

        # DNES offers a blank-vote choice, so count blank ballots separately.
        # Other invalid ballots stay in the frame-based nonvoter residual.
        outcome_counts = valid_votes_by_party.copy()
        outcome_counts["Blank vote"] = blank_ballots
        outcome_counts["Did not vote"] = frame_population - modeled_vote_total
        for outcome, count in sorted(outcome_counts.items()):
            rows.append({
                "vote_choice_2022": outcome,
                "municipality": municipality,
                "pop_pct": count / frame_population,
            })
    return rows


def build_survey(survey, areas, labels):
    # DNES reports school and higher education in separate questions.
    school = {
        "1": "H1010", "2": "H1020", "3": "H1030",
        **dict.fromkeys(("4", "5", "6"), "H2010"),
        **dict.fromkeys(("7", "8"), "H2020"),
        "9": "H30", "10": "H90", "11": "H90",
    }
    higher = {
        **dict.fromkeys(map(str, range(1, 7)), "H30"),
        "7": "H40", "8": "H50", "9": "H60", "10": "H70", "11": "H90",
    }
    # Translate the DNES 2022 past-vote codes before grouping minor parties.
    vote_codes = {
        "1": "A", "2": "B", "3": "C", "4": "D", "6": "F", "9": "I", "11": "K",
        "13": "M", "15": "O", "17": "Q", "22": "V", "27": "Æ", "28": "Ø", "29": "Å",
    }
    rows = []
    for row in survey:
        age = 2026 - int(row["birthyear"]) if row["birthyear"].isdigit() else None
        vote = row["Q17"]
        if vote in vote_codes:
            outcome = PARTIES[vote_codes[vote]]
        elif vote == "32":  # DNES explicitly offers “Stemte blankt”.
            outcome = "Blank vote"
        elif vote == "33" or row["Q16"] in {"2", "3", "4"}:
            outcome = "Did not vote"
        elif vote in {"30", "31"}:  # Another party or an independent candidate.
            outcome = "Other"
        else:
            outcome = ""
        education = higher.get(row["Q92"]) if row["Q91"] == "1" else None
        if education is None and row["Q91"] != "1":
            education = school.get(row["Q90"])
        # The frame has two gender categories. Leave responses outside that
        # classification blank rather than assigning them to either category.
        gender = {"1": "Men", "2": "Women"}.get(row["gender"], "")
        rows.append({
            "municipality": areas.get(row["kommunekode"], ""),
            "gender": gender,
            "age_group": age_band(age) if age is not None else "",
            "education": labels.get(education, ""),
            "vote_choice_2022": outcome,
        })
    return rows


def validate_input_labels(frame, election_results, survey):
    """Check that every recorded survey label appears in its reference input."""
    frame_municipalities = {row["municipality"] for row in frame}
    election_municipalities = {row["municipality"] for row in election_results}
    if election_municipalities != frame_municipalities:
        raise ValueError("Election and frame municipalities differ")

    allowed_labels = {
        column: {row[column] for row in frame}
        for column in ("municipality", "gender", "age_group", "education")
    }
    allowed_labels["vote_choice_2022"] = {row["vote_choice_2022"] for row in election_results}
    for column, labels in allowed_labels.items():
        survey_labels = {row[column] for row in survey if row[column]}
        unexpected_labels = survey_labels - labels
        if unexpected_labels:
            raise ValueError(f"Survey column {column} has unmatched labels: {sorted(unexpected_labels)}")
