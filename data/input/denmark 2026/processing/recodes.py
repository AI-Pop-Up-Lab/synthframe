"""Transform official Denmark extracts into the three modeling inputs."""
from collections import Counter, defaultdict
from math import isclose


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


def build_frame(population, attainment, labels):
    totals = Counter()
    for row in population:
        key = row["OMRÅDE"], row["KØN"], age_band(int(row["ALDER"].split()[0]))
        totals[key] += int(row["INDHOLD"])

    counts = defaultdict(Counter)
    for row in attainment:
        key = row["BOPOMR"], row["KØN"], row["ALDER"].split()[0]
        label = row["HFUDD"].split()[0]
        label = {"H2030": "H2010", "H80": "H70"}.get(label, label)
        counts[key][label] += int(row["INDHOLD"])

    rows = []
    for (municipality, gender, ages), total in sorted(totals.items()):
        source_band = ages if int(ages.split("-")[0]) < 70 else "65-69"
        profile = counts[municipality, gender, source_band]
        denominator = profile.total()
        if denominator <= 0:
            raise ValueError(f"Missing education profile for {municipality}, {gender}, {source_band}")
        for education, count in sorted(profile.items()):
            if count:
                rows.append(dict(municipality=municipality, gender=gender, age_group=ages,
                                 education=labels[education], N=total * count / denominator))
    if not isclose(sum(row["N"] for row in rows), totals.total(), abs_tol=1e-6):
        raise ValueError("Education allocation changed the population total")
    return rows


def build_election(election, labels):
    counts = defaultdict(dict)
    for row in election:
        if row["TID"] != "2022":
            raise ValueError("Election extract must contain only 2022")
        counts[row["OMRÅDE"]][row["VALRES"]] = int(row["INDHOLD"])

    rows = []
    for municipality, votes in sorted(counts.items()):
        electorate = votes[labels["VAELG"]]
        valid = votes[labels["GYLD_IALT"]]
        cast = valid + votes[labels["UGYLD_IALT"]]
        grouped = Counter()
        for label, count in votes.items():
            prefix, separator, _ = label.partition(". ")
            if separator and len(prefix) == 1 and prefix.isalpha():
                grouped[PARTIES.get(prefix, "Other")] += count
        grouped["Other"] += votes[labels["UDENFOR_IALT"]] + cast - valid
        grouped["Did not vote"] = electorate - cast
        if electorate <= 0 or grouped.total() != electorate or min(grouped.values()) < 0:
            raise ValueError(f"Election outcomes do not add up in {municipality}")
        for party, count in sorted(grouped.items()):
            rows.append(dict(party=party, municipality=municipality, year=2022,
                             pop_pct=count / electorate))
    return rows


def build_survey(survey, areas, labels):
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
        elif vote == "33" or row["Q16"] in {"3", "4"}:
            outcome = "Did not vote"
        elif vote in {"30", "31", "32"}:
            outcome = "Other"
        else:
            outcome = ""
        education = higher.get(row["Q92"]) if row["Q91"] == "1" else None
        if education is None and row["Q91"] != "1":
            education = school.get(row["Q90"])
        rows.append(dict(municipality=areas.get(row["kommunekode"], ""),
                         gender={"1": "Men", "2": "Women", "3": "Other"}.get(row["gender"], ""),
                         age_group=age_band(age) if age is not None else "",
                         education_level=labels.get(education, "Unknown"), past_vote=outcome))
    return rows
