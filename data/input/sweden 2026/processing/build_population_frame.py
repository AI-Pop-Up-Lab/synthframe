"""Estimate the 2025 adult citizen population by municipality, age, sex and education."""
import csv
import io
import math
import re
from collections import defaultdict
from pathlib import Path
from zipfile import ZipFile

ROOT = Path(__file__).resolve().parents[1]
YEAR = "2025"
SEX = {"män": "Men", "kvinnor": "Women", "men": "Men", "women": "Women"}
EDUCATION = {
    "förgymnasial utbildning kortare än 9 år": "Compulsory education, under 9 years",
    "förgymnasial utbildning, 9 (10) år": "Compulsory education, 9 or 10 years",
    "gymnasial utbildning, högst 2 år": "Upper secondary, up to 2 years",
    "gymnasial utbildning, 3 år": "Upper secondary, 3 years",
    "eftergymnasial utbildning, mindre än 3 år": "Post-secondary, under 3 years",
    "eftergymnasial utbildning, 3 år eller mer": "Post-secondary, 3 years or more",
    "forskarutbildning": "Doctoral education",
    "uppgift om utbildningsnivå saknas": "Unknown",
}
BANDS = tuple(range(10, 71, 10))


def scb_rows(name):
    with ZipFile(ROOT / "raw" / f"{name}.zip") as archive:
        with io.TextIOWrapper(archive.open(f"{name}.csv"), encoding="cp1252", newline="") as file:
            rows = csv.reader(file)
            next(rows)
            yield from rows


def municipality(region):
    code = region.split(" ", 1)[0]
    return code if len(code) == 4 and code.isdigit() else None


def age_band(age):
    return min(age // 10 * 10, 70)


def published_count(value):
    count = int(value)
    if count < 0:
        raise ValueError(f"Negative population count: {count}")
    return count


def municipality_geography():
    locations = {}
    with (ROOT / "election_results.csv").open(encoding="utf-8", newline="") as file:
        for row in csv.DictReader(file):
            code = row["municipality_code"]
            location = row["municipality"], row["constituency"]
            if not all(location) or locations.setdefault(code, location) != location:
                raise ValueError(f"Missing or conflicting geography for municipality {code}")
    return locations


def residents():
    single_age = defaultdict(int)
    bands = {}
    for region, marital_status, age, sex, year, measure, count in scb_rows("TAB5557_sv"):
        code = municipality(region)
        if (not code or year != YEAR or measure != "Folkmängd"
                or marital_status != "totalt, samtliga civilstånd" or sex not in SEX):
            continue
        match = re.fullmatch(r"(\d+)(?:\+)? år", age)
        if match and int(match[1]) >= 10:
            single_age[code, SEX[sex], int(match[1])] += published_count(count)
        match = re.fullmatch(r"(\d+)-(\d+) år", age)
        if match and int(match[1]) in BANDS[:-1] and int(match[2]) == int(match[1]) + 9:
            bands[code, SEX[sex], int(match[1])] = published_count(count)
    for code, sex, age in single_age:
        if age >= 70:
            key = code, sex, 70
            bands[key] = bands.get(key, 0) + single_age[code, sex, age]
    return single_age, bands


def foreign_citizens():
    counts = {}
    for region, age, sex, year, count in scb_rows("TAB6659_en"):
        code = municipality(region)
        match = re.match(r"^(\d+)[+-]", age)
        if code and year == YEAR and sex in SEX and match and int(match[1]) in BANDS:
            counts[code, SEX[sex], int(match[1])] = published_count(count)
    return counts


def education_counts():
    counts = defaultdict(dict)
    for region, education, age, sex, year, count in scb_rows("TAB4320_sv"):
        if year != YEAR or age in {"16 år", "17 år"}:
            continue
        code = municipality(region)
        if code:
            counts[code, SEX[sex], int(age.split()[0].rstrip("+"))][EDUCATION[education]] = (
                published_count(count)
            )
    return counts


def build_frame():
    geography = municipality_geography()
    codes = geography.keys()
    if len(codes) != 290 or len({name for name, _ in geography.values()}) != 290:
        raise ValueError("Expected the 290 municipality codes in election_results.csv")
    resident_ages, resident_bands = residents()
    foreign = foreign_citizens()
    education = education_counts()
    expected_bands = {(code, sex, band) for code in codes for sex in ("Men", "Women")
                      for band in BANDS}
    expected_cells = {(code, sex, age) for code in codes for sex in ("Men", "Women")
                      for age in range(18, 96)}
    if resident_bands.keys() != expected_bands or foreign.keys() != expected_bands:
        raise ValueError("Population and foreign-citizen counts must cover matching municipality, sex and age bands")
    if education.keys() != expected_cells:
        raise ValueError("Education counts must cover every municipality, sex and adult age")
    expected_resident_ages = {(code, sex, age) for code in codes for sex in ("Men", "Women")
                              for age in range(10, 101)}
    if resident_ages.keys() != expected_resident_ages:
        raise ValueError("Resident counts must cover ages 10–99 and 100+ in every municipality by sex")
    for key, population in resident_bands.items():
        if not 0 <= foreign[key] <= population or population == 0:
            raise ValueError(f"Invalid citizenship counts for {key}: {foreign[key]} of {population}")

    # Use the published band total where available. Normalize single ages within
    # each band because SCB independently perturbs published counts for privacy.
    single_age_bands = defaultdict(int)
    for (code, sex, age), population in resident_ages.items():
        single_age_bands[code, sex, age_band(age)] += population
    rows = []
    totals = defaultdict(list)
    unknown_cells = 0
    for (code, sex, age), breakdown in sorted(education.items()):
        if set(breakdown) != set(EDUCATION.values()):
            raise ValueError(f"Incomplete education breakdown for {(code, sex, age)}")
        key = code, sex, age_band(age)
        if age == 95:
            population = sum(resident_ages[code, sex, a] for a in range(95, 101))
        else:
            population = resident_ages[code, sex, age]
        citizens = population / single_age_bands[key] * (resident_bands[key] - foreign[key])
        education_total = sum(breakdown.values())
        if citizens > 0 and education_total == 0:
            # Independently perturbed counts can disagree in very small cells.
            breakdown = dict.fromkeys(EDUCATION.values(), 0)
            breakdown["Unknown"] = 1
            education_total = 1
            unknown_cells += 1
        name, constituency = geography[code]
        for level, count in breakdown.items():
            estimate = citizens * count / education_total if education_total else 0.0
            totals[key].append(estimate)
            rows.append(dict(municipality=name, constituency=constituency,
                             age_group="95+" if age == 95 else str(age), sex=sex,
                             education=level, N=estimate))
    for key, estimates in totals.items():
        code, sex, band = key
        expected = resident_bands[key] - foreign[key]
        if band == 10:
            expected *= sum(resident_ages[code, sex, age] for age in (18, 19)) / single_age_bands[key]
        if not math.isclose(math.fsum(estimates), expected, rel_tol=1e-12):
            raise ValueError(f"Frame does not reconcile to the estimated citizen count for {key}")
    return rows, unknown_cells


if __name__ == "__main__":
    frame, unknown_cells = build_frame()
    with (ROOT / "frame.csv").open("w", encoding="utf-8", newline="") as file:
        writer = csv.DictWriter(file, fieldnames=list(frame[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(frame)
    print(f"{len(frame):,} cells, {math.fsum(row['N'] for row in frame):,.2f} estimated adult citizens")
    print(f"{unknown_cells} age-by-sex cells assigned to Unknown because no education counts were available")
