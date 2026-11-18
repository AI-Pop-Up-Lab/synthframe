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
    country = Path(__file__).resolve().parent / "denmark 2026"
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

    def extract(table, selections):
        query = urlencode({"lang": "en", **selections})
        path = download(f"{table}.csv", f"{api}/data/{table}/CSV?{query}")
        with path.open(encoding="utf-8-sig", newline="") as stream:
            return list(csv.DictReader(stream, delimiter=";"))

    areas = {
        code: name for code, name in metadata["FOLK1D"]["OMRÅDE"].items()
        if code.isdecimal() and int(code) >= 100 and name != "Christiansø"
    }
    education = "H1010,H1020,H1030,H2010,H2020,H2030,H30,H40,H50,H60,H70,H80,H90"
    population = extract("FOLK1D", {
        "OMRÅDE": ",".join(areas), "KØN": "1,2", "ALDER": ",".join(map(str, range(20, 95))),
        "STATSB": "DANSK", "Tid": "2026K1",
    })
    area_names = set(areas.values())
    population = [row for row in population if row["OMRÅDE"] in area_names]
    attainment = extract("HFUDD11", {
        "BOPOMR": ",".join(areas), "HERKOMST": "TOT", "HFUDD": education,
        "ALDER": ",".join(f"{age}-{age + 4}" for age in range(20, 70, 5)),
        "KØN": "M,K", "Tid": "2025",
    })
    election = extract("FVKOM", {"VALRES": "*", "OMRÅDE": "*", "Tid": "2022"})
    archive = download("DNES-2022.zip", "https://digidata.rigsarkivet.dk/download/51035")
    with ZipFile(archive) as bundle, bundle.open("Data/CSV/Dataset.csv") as source:
        survey = list(csv.DictReader(io.TextIOWrapper(source, encoding="cp1252", newline="")))

    # The processing module lives alongside the country inputs it builds.
    sys.path.insert(0, str(country / "processing"))
    process = import_module("recodes")
    output = country / "processing" / "rebuilt"
    output.mkdir(exist_ok=True)
    jobs = [
        ("frame.csv", process.build_frame, (population, attainment, metadata["HFUDD11"]["HFUDD"])),
        ("election_results.csv", process.build_election, (election, metadata["FVKOM"]["VALRES"])),
        ("auxiliary_survey.csv", process.build_survey, (survey, areas, metadata["HFUDD11"]["HFUDD"])),
    ]
    for name, build, inputs in jobs:
        print(f"Building {name}", flush=True)
        rows = build(*inputs)
        if not rows:
            raise ValueError(f"{name} produced no rows")
        with (output / name).open("w", encoding="utf-8", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
            writer.writeheader()
            writer.writerows(rows)
        print(f"Wrote {len(rows):,} rows to {output / name}", flush=True)


if __name__ == "__main__":
    main()
