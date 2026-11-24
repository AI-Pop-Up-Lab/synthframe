from pathlib import Path
from tempfile import NamedTemporaryFile

root = Path(__file__).resolve().parent
begin = "<!-- checkpoint status -->"
end = "<!-- end checkpoint status -->"
models = (
    ("Presidential", "vote_pres_2024", "pres_fit.rds"),
    ("House", "vote_house_2024", "house_fit.rds"),
)
for notes in (root / "RESUME.md", root.parents[2] / "US_2026_RESUME.md"):
    status = [
        "## Saved checkpoints",
        "",
        "| Model | Saved stages | Complete model |",
        "| --- | ---: | --- |",
    ]
    for label, stage_prefix, filename in models:
        n_stages = sum((root / f"{stage_prefix}-stage-{i}.rds").is_file() for i in range(1, 4))
        relative_path = (root / filename).relative_to(notes.parent)
        model = f"[Ready]({relative_path})" if (root / filename).is_file() else "Pending"
        status.append(f"| {label} | {n_stages} of 3 | {model} |")
    old = notes.read_text()
    prefix, rest = old.split(begin, 1)
    _, suffix = rest.split(end, 1)
    updated = prefix + begin + "\n" + "\n".join(status) + "\n" + end + suffix
    if updated != old:
        with NamedTemporaryFile(mode="w", encoding="utf-8", dir=notes.parent, delete=False) as temporary:
            temporary.write(updated)
        Path(temporary.name).replace(notes)
