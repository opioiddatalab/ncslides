#!/usr/bin/env python3
"""Flag mock values left behind in the slide templates (CI gate).

The design's placeholder numbers are plausible fabrications. Any that survive
tools/annotate_slides.py get published as though they were real and nothing
downstream notices -- so this is the safety net.

Flags, on data-bearing slides only:
  * text nodes that are entirely a number ("612", "78%", "1,204")
  * date and quarter tokens ("26Q2", "Aug 2026", "25 Aug 2026")

Ignores: static slides (their numbers are editorial copy about the card and
methods), the r/ provenance footers (Menlo-styled, they cite filenames), and
variable names, which legitimately contain digits.

Exit 1 if anything is flagged, so `python3 tools/audit_slides.py` works as a
build gate.
"""
import csv, re, os, sys

SLIDES = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'r', 'slides')

BARE_NUMBER = re.compile(r'^[+\-−]?\d[\d,]*(?:\.\d+)?%?$')
DATE_TOKEN = re.compile(
    r'\b\d{2}Q[1-4]\b'
    r'|\b\d{1,2} (?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) \d{4}\b'
    r'|\b(?:January|February|March|April|May|June|July|August|September|October'
    r'|November|December) \d{1,2}, \d{4}\b'
    r'|\b(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) \d{4}\b')

def scrub(html):
    """Remove marker contents and provenance footers before scanning."""
    html = re.sub(r'\{\{[^}]*\}\}', '\x00', html)
    html = re.sub(r'<p style="[^"]*Menlo[^"]*"[^>]*>[\s\S]*?</p>', '', html)
    return html

def main():
    spec = {r['file']: r['archetype'] for r in
            csv.DictReader(open(os.path.join(SLIDES, 'spec.csv')))}
    findings = []
    for fn, arch in sorted(spec.items()):
        if arch == 'static':
            continue
        html = scrub(open(os.path.join(SLIDES, fn), encoding='utf-8').read())
        for t in re.findall(r'>([^<>]+)<', html):
            t = t.strip()
            if not t or '\x00' in t:
                continue
            if BARE_NUMBER.match(t):
                findings.append((fn, 'bare number', t))
            for d in DATE_TOKEN.findall(t):
                findings.append((fn, 'date token', d))

    for fn, kind, t in findings:
        print(f'{fn[:46]:46} {kind:12} {t[:60]}')
    print(f'\n{len(findings)} mock value(s) still in the templates')
    return 1 if findings else 0

if __name__ == '__main__':
    sys.exit(main())
