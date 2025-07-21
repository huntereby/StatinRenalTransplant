import csv
import os
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DOCKING_DIR = ROOT / 'Docking Results'

DRUGS = {
    'Atorva': {
        'PharmaMapper': DOCKING_DIR / 'PharmaMapper' / 'AtorvoPharmMapper.csv',
        'SuperPred': DOCKING_DIR / 'SuperPre' / 'AtorvaSuperPred.csv',
        'Swiss': DOCKING_DIR / 'Swiss' / 'AtorvaSwiss.csv',
    },
    'Rosuva': {
        'PharmaMapper': DOCKING_DIR / 'PharmaMapper' / 'RosuvaPharmMapper.csv',
        'SuperPred': DOCKING_DIR / 'SuperPre' / 'RosuvaSuperPred.csv',
        'Swiss': DOCKING_DIR / 'Swiss' / 'RosuvaSwiss.csv',
    }
}

def read_uniprot(path: Path) -> set[str]:
    """Read csv and return a set of UniProt IDs."""
    ids = set()
    with open(path, newline='') as f:
        reader = csv.reader(f)
        # Skip lines until we get header containing 'Uni' or 'Uniplot'
        header = next(reader)
        while header and not any('Uni' in h for h in header):
            header = next(reader)
        # Normalize column name for uniprot
        try:
            idx = header.index('UniProt ID')
        except ValueError:
            try:
                idx = header.index('Uniprot ID')
            except ValueError:
                try:
                    idx = header.index('Uniplot')
                except ValueError:
                    raise RuntimeError(f"Can't find UniProt column in {path}")
        for row in reader:
            if not row:
                continue
            val = row[idx].strip()
            if val:
                ids.add(val)
    return ids

def venn_counts(A: set[str], B: set[str], C: set[str]):
    return {
        'A_only': len(A - B - C),
        'B_only': len(B - A - C),
        'C_only': len(C - A - B),
        'A_B': len((A & B) - C),
        'A_C': len((A & C) - B),
        'B_C': len((B & C) - A),
        'A_B_C': len(A & B & C),
    }

def make_svg(counts: dict, labels: tuple[str, str, str], title: str, out_path: Path):
    width, height = 400, 320
    r = 100
    cx1, cy1 = 150, 150
    cx2, cy2 = 250, 150
    cx3, cy3 = 200, 230
    svg = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        f'<svg width="{width}" height="{height}" xmlns="http://www.w3.org/2000/svg">',
        f'<title>{title}</title>',
        # circles
        f'<circle cx="{cx1}" cy="{cy1}" r="{r}" fill="red" fill-opacity="0.4" stroke="black"/>',
        f'<circle cx="{cx2}" cy="{cy2}" r="{r}" fill="green" fill-opacity="0.4" stroke="black"/>',
        f'<circle cx="{cx3}" cy="{cy3}" r="{r}" fill="blue" fill-opacity="0.4" stroke="black"/>',
        # labels for sets
        f'<text x="{cx1-r+10}" y="{cy1-r-10}" font-size="16">{labels[0]}</text>',
        f'<text x="{cx2+r-60}" y="{cy2-r-10}" font-size="16">{labels[1]}</text>',
        f'<text x="{cx3-40}" y="{cy3+r+20}" font-size="16">{labels[2]}</text>',
        # counts
        # unique regions
        f'<text x="{cx1-40}" y="{cy1}" font-size="20">{counts["A_only"]}</text>',
        f'<text x="{cx2+20}" y="{cy2}" font-size="20">{counts["B_only"]}</text>',
        f'<text x="{cx3-10}" y="{cy3+10}" font-size="20">{counts["C_only"]}</text>',
        # pair intersections
        f'<text x="{(cx1+cx2)/2-10}" y="{cy1-20}" font-size="20">{counts["A_B"]}</text>',
        f'<text x="{(cx1+cx3)/2-30}" y="{(cy1+cy3)/2+10}" font-size="20">{counts["A_C"]}</text>',
        f'<text x="{(cx2+cx3)/2+10}" y="{(cy2+cy3)/2+10}" font-size="20">{counts["B_C"]}</text>',
        # triple intersection
        f'<text x="{cx1+30}" y="{cy1+30}" font-size="20">{counts["A_B_C"]}</text>',
        '</svg>'
    ]
    out_path.write_text('\n'.join(svg))

def main():
    figures_dir = ROOT / 'Figures'
    figures_dir.mkdir(exist_ok=True)
    for drug, files in DRUGS.items():
        sets = {}
        for method, path in files.items():
            sets[method] = read_uniprot(path)
        counts = venn_counts(sets['PharmaMapper'], sets['SuperPred'], sets['Swiss'])
        out_file = figures_dir / f'{drug}_venn.svg'
        make_svg(counts, ('PharmaMapper', 'SuperPred', 'Swiss'), f'{drug} Docking Overlap', out_file)
        print(f'Wrote {out_file}')

if __name__ == '__main__':
    main()
