"""Download and rebuild the Denmark inputs for review."""
import argparse
import csv
import io
from importlib import import_module
import json
from pathlib import Path
import shutil
import sys
from urllib.parse import urlencode
from urllib.request import urlopen
from zipfile import ZipFile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--refresh", action="store_true", help="Download fresh source extracts.")
    args = parser.parse_args()
    country = Path(__file__).resolve().parent
    raw = country / "raw"
    raw.mkdir(exist_ok=True)

    def download(name, url):
        path = raw / name
        if args.refresh or not path.exists():
            print(f"Downloading {name}", flush=True)
            pending = path.with_suffix(path.suffix + ".part")
            with urlopen(url, timeout=120) as response, pending.open("wb") as stream:
                shutil.copyfileobj(response, stream)
            pending.replace(path)
        return path

    api = "https://api.statbank.dk/v1"
    metadata = {}
    for table in ("FOLK1D", "HFUDD11", "FVKOM"):
        path = download(f"{table}.json", f"{api}/tableinfo/{table}?lang=en")
        metadata[table] = {
            variable["id"]: {value["id"]: value["text"] for value in variable["values"]}
            for variable in json.loads(path.read_text(encoding="utf-8-sig"))["variables"]
        }

    def extract(table, selections, *, filename=None):
        query = urlencode({"lang": "en", **selections})
        path = download(filename or f"{table}.csv", f"{api}/data/{table}/CSV?{query}")
        with path.open(encoding="utf-8-sig", newline="") as stream:
            return list(csv.DictReader(stream, delimiter=";"))

    areas = {
        code: name for code, name in metadata["FOLK1D"]["OMRÅDE"].items()
        if code.isdecimal() and int(code) >= 100 and name != "Christiansø"
    }
    # Citizenship defines the population we aim to represent in the parliamentary study.
    population = extract("FOLK1D", {
        "OMRÅDE": ",".join(areas), "KØN": "1,2", "ALDER": ",".join(map(str, range(20, 95))),
        "STATSB": "DANSK", "Tid": "2026K1",
    })
    area_names = set(areas.values())
    population = [row for row in population if row["OMRÅDE"] in area_names]
    # HFUDD11 has origin, so use Danish-origin education shares as a proxy
    # for the education distribution of citizens within each population group.
    education_codes = "H1010,H1020,H1030,H2010,H2020,H2030,H30,H40,H50,H60,H70,H90"
    current_age_bands = ",".join(f"{age}-{age + 4}" for age in range(20, 70, 5))
    education_age_bands_by_year = {
        "2024": current_age_bands,
        "2019": "65-69",
        "2014": "65-69",
        "2009": "65-69",
    }

    # Use distinct cache names so a prior all-origin extract cannot be reused here.
    attainment = []
    for year, age_bands in education_age_bands_by_year.items():
        attainment.extend(extract("HFUDD11", {
            "BOPOMR": ",".join(areas),
            "HERKOMST": "5",
            "HFUDD": education_codes,
            "ALDER": age_bands,
            "KØN": "M,K",
            "Tid": year,
        }, filename=f"HFUDD11_danish_origin_{year}.csv"))

    # Use the preceding parliamentary election and the corresponding election study.
    election = extract("FVKOM", {"VALRES": "*", "OMRÅDE": "*", "Tid": "2022"})
    archive = download("DNES-2022.zip", "https://digidata.rigsarkivet.dk/download/51035")
    with ZipFile(archive) as bundle, bundle.open("Data/CSV/Dataset.csv") as source:
        survey = list(csv.DictReader(io.TextIOWrapper(source, encoding="cp1252", newline="")))

    # The processing module lives alongside the country inputs it builds.
    sys.path.insert(0, str(country / "processing"))
    process = import_module("recodes")
    output = country / "processing" / "rebuilt"
    output.mkdir(exist_ok=True)
    print("Building frame.csv", flush=True)
    frame = process.build_frame(population, attainment, metadata["HFUDD11"]["HFUDD"])

    geography = process.municipality_geography(areas)
    process.add_constituencies(frame, geography)

    # Compute past-vote shares on the official 2022 electoral roll.
    print("Building election_results.csv", flush=True)
    election_results = process.build_election(election, geography, metadata["FVKOM"]["VALRES"])

    print("Building auxiliary_survey.csv", flush=True)
    auxiliary_survey = process.build_survey(survey, areas, metadata["HFUDD11"]["HFUDD"])
    process.add_constituencies(auxiliary_survey, geography)
    process.validate_input_labels(frame, election_results, auxiliary_survey)

    outputs = {
        "frame.csv": frame,
        "election_results.csv": election_results,
        "auxiliary_survey.csv": auxiliary_survey,
    }
    for name, rows in outputs.items():
        if not rows:
            raise ValueError(f"{name} produced no rows")
        with (output / name).open("w", encoding="utf-8", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=list(rows[0]), lineterminator="\n")
            writer.writeheader()
            writer.writerows(rows)
        print(f"Wrote {len(rows):,} rows to {output / name}", flush=True)


if __name__ == "__main__":
    main()
