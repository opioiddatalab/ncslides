#!/usr/bin/env python3
"""Split the exported Claude Design deck into per-slide templates.

Run this first (then tools/annotate_slides.py) whenever the design is
re-exported from the canvas. It overwrites r/slides/*.html, so any markers
inserted by the annotator are lost -- that is the point: the design is the
source of truth for layout, the annotator re-applies the wiring.

Preserves: r/slides/spec.csv (hand-authored, not derived).
"""
import re, os, csv

SRC = 'Design/NC Drug Checking Nightly Deck.dc.html'
OUT = 'r/slides'

def slug(lab):
    s = lab.lower().replace('§', 's').replace('·', '-')
    return re.sub(r'[^a-z0-9]+', '_', s).strip('_')

def main():
    src = open(SRC, encoding='utf-8').read()
    os.makedirs(OUT, exist_ok=True)

    helmet = re.search(r'<helmet>\s*<style>([\s\S]*?)</style>\s*</helmet>', src)
    open(f'{OUT}/_helmet.css', 'w', encoding='utf-8').write(helmet.group(1).strip() + '\n')

    secs = re.findall(r'(<section [\s\S]*?)(?=<section |</x-import>)', src)
    rows = []
    for i, body in enumerate(secs, start=1):
        lab = re.search(r'data-label="([^"]+)"', body).group(1)
        fn = f'{i:02d}_{slug(lab)}.html'
        open(f'{OUT}/{fn}', 'w', encoding='utf-8').write(body.rstrip() + '\n')
        rows.append(dict(order=i, file=fn, label=lab, bytes=len(body)))

    with open(f'{OUT}/manifest.csv', 'w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader(); w.writerows(rows)
    print(f'split {len(rows)} slides into {OUT}/')

if __name__ == '__main__':
    main()
