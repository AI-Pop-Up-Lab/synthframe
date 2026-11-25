"""Build municipality Riksdag results."""
import csv
import re
import xml.etree.ElementTree as ET
from collections import defaultdict
from pathlib import Path
from zipfile import ZipFile

from build_population_frame import municipality, scb_rows

ROOT = Path(__file__).resolve().parents[1]
RAW = ROOT / "raw"
PARTIES = {
    "M": "Moderaterna",
    "C": "Centerpartiet",
    "L": "Liberalerna (tidigare Folkpartiet)",
    "KD": "Kristdemokraterna",
    "S": "Arbetarepartiet-Socialdemokraterna",
    "V": "Vänsterpartiet",
    "MP": "Miljöpartiet de gröna",
    "SD": "Sverigedemokraterna",
}


def workbook_rows(filename, sheet):
    with ZipFile(RAW / filename) as book:
        strings = [
            "".join(node.text or "" for node in item.findall(".//{*}t"))
            for item in ET.fromstring(book.read("xl/sharedStrings.xml"))
        ]
        worksheet = ET.fromstring(book.read(f"xl/worksheets/sheet{sheet}.xml"))
    for row in worksheet.findall(".//{*}row"):
        cells = {}
        for cell in row:
            value = cell.find("{*}v")
            if value is not None:
                column = re.match("[A-Z]+", cell.attrib["r"])[0]
                cells[column] = strings[int(value.text)] if cell.get("t") == "s" else value.text
        yield cells


def municipality_results():
    municipality_codes = {
        region.split(" ", 1)[1]: code
        for region, *_ in scb_rows("TAB6659_en")
        if (code := municipality(region)) is not None
    }
    assert len(municipality_codes) == 290
    rows = workbook_rows("results-2022.xlsx", 3)
    next(rows)
    results = defaultdict(dict)
    for row in rows:
        if row.get("B") and row.get("C"):
            results[row["B"], row["A"]][row["C"]] = int(row["D"])
    assert len(results) == 290
    assert {name for name, _ in results} == municipality_codes.keys()
    assert len({constituency for _, constituency in results}) == 29
    output = []
    for (municipality_name, constituency), votes in results.items():
        code = municipality_codes[municipality_name]
        counts = {party: votes[name] for party, name in PARTIES.items()}
        counts["Other"] = votes["Övriga anmälda partier"]
        eligible = votes["Röstberättigade"]
        valid = votes["Giltiga Röster"]
        cast = votes["Valdeltagande"]
        counts["Invalid"] = sum(
            count for label, count in votes.items() if label.startswith("Ogiltiga röster")
        )
        counts["No Vote"] = eligible - cast
        assert sum(counts[party] for party in (*PARTIES, "Other")) == valid
        assert valid + counts["Invalid"] == cast <= eligible
        for choice, count in counts.items():
            output.append(dict(
                municipality_code=code, municipality=municipality_name,
                constituency=constituency, vote_choice_2022=choice,
                share_eligible=count / eligible,
            ))
    return sorted(output, key=lambda row: row["municipality_code"])


def write_csv(filename, rows):
    with (ROOT / filename).open("w", encoding="utf-8", newline="") as file:
        writer = csv.DictWriter(file, fieldnames=list(rows[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


if __name__ == "__main__":
    write_csv("election_results.csv", municipality_results())
