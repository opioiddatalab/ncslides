#!/usr/bin/env python3
"""Insert build-time markers into the slide templates in r/slides/.

Run after tools/split_slides.py. Rewrites r/slides/*.html in place, replacing
the design's PLACEHOLDER chart scaffolding and mock values with markers that
r/deck.R substitutes at build time:

    {{CHART:<fig-slug>}}   inline SVG chart (PNG for the dashboard facet grid)
    {{STAT:<key>}}         a scalar or a whole generated sentence
    {{TABLE:<id>}}         a full <table> rendered by render_table()

The design's own mustache vars ({{ noteDisplay }}, {{ rTagDisplay }}) are left
alone -- deck.R resolves those from r/config.yml. Markers here carry no inner
spaces, so the two families never collide.

Every mock number the design invented has to end up behind a marker. Anything
missed ships as if it were real and nothing downstream notices, so
tools/audit_slides.py re-checks the result and CI runs it.

Deliberate simplification: on multi-chart slides the whole chart column is
replaced, so data-derived commentary the design baked in there (e.g. "positive
samples decreased 4%") gives way to the ggplot caption. Prose stating a number
must come from the data or it goes stale.
"""
import csv, re, os

SLIDES = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'r', 'slides')

# ------------------------------------------------------------------ DOM utils
def _sec_body(html):
    return html.index('>', html.index('<section')) + 1

def _spans(html, a, b):
    """Spans of the <div> elements that are direct children of region [a,b)."""
    out, depth, start = [], 0, None
    for m in re.finditer(r'<div\b|</div>', html[a:b]):
        pos = a + m.start()
        if m.group(0) == '<div':
            if depth == 0:
                start = pos
            depth += 1
        else:
            depth -= 1
            if depth == 0:
                out.append((start, a + m.end()))
    return out

def top_divs(html):
    return _spans(html, _sec_body(html), len(html))

def inner(html, span):
    return (html.index('>', span[0]) + 1, span[1] - len('</div>'))

def set_inner(html, span, text):
    a, b = inner(html, span)
    return html[:a] + text + html[b:]

def find_body(html):
    """The growing content div: first top-level div with flex:1 in its style."""
    for s in top_divs(html):
        head = html[s[0]:html.index('>', s[0])]
        st = re.search(r'style="([^"]*)"', head)
        if st and 'flex:1' in st.group(1):
            return s
    return None

def is_sidebar(html, span):
    return bool(re.search(r'width:\d{3}px', html[span[0]:html.index('>', span[0])]))

# ------------------------------------------------------------ chart targeting
def replace_chart(html, region, slug):
    """Swap the chart inside `region` for a marker.

    The design draws placeholder charts three ways: an inline <svg>, a nested
    chart <div>, or bars directly in the region.
    """
    a, b = inner(html, region)
    seg = html[a:b]
    m = re.search(r'<svg[\s\S]*?</svg>', seg)
    if m:
        return html[:a] + seg[:m.start()] + '{{CHART:%s}}' % slug + seg[m.end():] + html[b:]
    return set_inner(html, region, '{{CHART:%s}}' % slug)

# --------------------------------------------------------------- sidebar stats
BIG_RE = re.compile(
    r'(<p style="font-size:(?:96|84|72|64)px;font-weight:700;color:#B45D08[^"]*"[^>]*>)'
    r'([^<]{1,40})(</p>)')

# caption paragraph: may contain <strong>, so match lazily up to </p>
CAP_RE = re.compile(r'(<p style="font-size:2\dpx;[^"]*line-height:1\.5"[^>]*>)([\s\S]*?)(</p>)')

def mark_sidebar(seg, key):
    """Mark the big orange stat, and hand the caption sentence to R wholesale.

    The caption embeds numbers inside <strong> tags ("... 26Q2 - 578 in any
    abundance"), so templating fragments of it is hopeless; R generates the
    whole sentence including its markup.
    """
    hits = 0
    m = BIG_RE.search(seg)
    if m:
        seg = seg[:m.start()] + m.group(1) + '{{STAT:%s}}' % key + m.group(3) + seg[m.end():]
        hits += 1
    # Mark every caption paragraph that states a number. Prose without a
    # number is editorial copy and stays; prose with one must be generated or
    # it goes stale. Right-to-left so earlier match offsets stay valid.
    caps = [c for c in CAP_RE.finditer(seg) if re.search(r'\d', c.group(2))]
    for i, c in enumerate(reversed(caps)):
        n = len(caps) - i
        suffix = '_label' if n == 1 else '_label%d' % n
        seg = seg[:c.start()] + c.group(1) + '{{STAT:%s%s}}' % (key, suffix) + c.group(3) + seg[c.end():]
        hits += 1
    return seg, hits

# --------------------------------------------------------------- stats slide
STATS_SUBS = [
    (r'N = 8,412 NC samples analyzed', 'N = {{STAT:n_samples}} NC samples analyzed'),
    (r'(>)86(<)', r'\1{{STAT:n_programs}}\2'),
    (r'78 counties', '{{STAT:n_counties}} counties'),
    (r'463 distinct substances', '{{STAT:n_substances}} distinct substances'),
]

# ------------------------------------- data tokens the design hardcoded inline
# Order matters: longest/most specific first, so "25 Aug 2026" is consumed
# before the bare "Aug 2026" rule can bite into it.
GLOBAL_TOKENS = [
    (r'March 2022 to Tuesday, August 25, 2026', '{{STAT:date_range}}'),
    (r'\b07 Jun 2022\b',  '{{STAT:first_date}}'),
    (r'\b25 Aug 2026\b',  '{{STAT:latest_date}}'),
    (r'\b26 Aug 2026\b',  '{{STAT:run_date}}'),
    (r'\b26 Aug 2025\b',  '{{STAT:run_date_prior_year}}'),
    (r'\b25 Aug 2025\b',  '{{STAT:latest_date_prior_year}}'),
    (r'\b26Q2\b',         '{{STAT:latest_q}}'),
    (r'\b26Q1\b',         '{{STAT:prev_q}}'),
    (r'\bAug 2026\b',     '{{STAT:latest_month}}'),
    (r'\bAug 2025\b',     '{{STAT:latest_month_prior_year}}'),
]

# Licensing + attribution, appended to the two Fine Print slides. Kept here
# rather than hand-edited into the templates so that re-splitting from a fresh
# design export does not silently revert it. The wording lives in r/config.yml:
# the repo is CC0-1.0, which WAIVES attribution, so the deck requests it rather
# than claiming it is required.
INSERTS = {
    '56_55_data_access.html':
        '  <p style="font-size:25px;color:#5B6770;margin:28px 0 0;line-height:1.55;'
        'max-width:1500px">{{STAT:license_line}}</p>\n',
    '63_56_credits.html':
        '  <p style="font-size:24px;color:#7BAFD4;margin:34px 0 0;max-width:1200px;'
        'line-height:1.6">{{STAT:license_line}}</p>\n',
}


def main():
    spec = list(csv.DictReader(open(os.path.join(SLIDES, 'spec.csv'))))
    report = []

    for row in spec:
        path = os.path.join(SLIDES, row['file'])
        html = open(path, encoding='utf-8').read()
        arch, note = row['archetype'], ''
        charts = [c for c in row['charts'].split(';') if c]

        if arch == 'static':
            note = 'untouched'

        elif arch == 'stats':
            n = 0
            for pat, rep in STATS_SUBS:
                html, k = re.subn(pat, rep, html, count=1)
                n += k
            note = f'{n}/{len(STATS_SUBS)} stats'

        elif arch == 'table':
            ids = [row['table_id'], row['table_id'] + 'b', row['table_id'] + 'c']
            n = 0
            while re.search(r'<table\b', html) and n < len(ids):
                html = re.sub(r'<table\b[\s\S]*?</table>',
                              '{{TABLE:%s}}' % ids[n], html, count=1)
                n += 1
            if n:
                note = f'{n} table(s) replaced'
            else:
                html = set_inner(html, find_body(html), '{{TABLE:%s}}' % ids[0])
                note = 'div-table body replaced'

        elif arch == 'kpi12':
            # Twelve metric tiles, each: static label, value, unit, delta badge,
            # sparkline. The delta badge changes colour with direction, so R
            # emits the whole <p> rather than just its text.
            tiles = _spans(html, *inner(html, find_body(html)))
            if len(tiles) != len(charts):
                note = f'NEED {len(charts)} TILES, FOUND {len(tiles)}'
            else:
                for tile, slug in zip(reversed(tiles), reversed(charts)):
                    seg = html[tile[0]:tile[1]]
                    key = slug[len('dash_'):]
                    seg = re.sub(r'(<p style="font-size:44px;font-weight:700;color:#13294B;margin:0">)[^<]*(</p>)',
                                 r'\1{{STAT:dash_%s_val}}\2' % key, seg, count=1)
                    seg = re.sub(r'<p style="font-size:24px;font-weight:700;color:#[0-9A-F]{6};background:#[0-9A-F]{6};padding:2px 12px;margin:0 0 0 auto">[^<]*</p>',
                                 '{{STAT:dash_%s_badge}}' % key, seg, count=1)
                    inner_span = (seg.index('>', seg.rindex('<div')) + 1, len(seg) - len('</div>'))
                    seg = seg[:inner_span[0]] + '{{CHART:%s}}' % slug + seg[inner_span[1]:]
                    html = html[:tile[0]] + seg + html[tile[1]:]
                note = f'{len(charts)} KPI tiles'

        elif arch == 'chart1':
            body = find_body(html)
            html = replace_chart(html, body, charts[0]) if body else html
            note = 'body -> chart' if body else 'NO BODY'

        elif arch == 'chart1_side':
            body = find_body(html)
            if not body:
                note = 'NO BODY'
            else:
                kids = _spans(html, *inner(html, body))
                side = next((k for k in kids if is_sidebar(html, k)), None)
                hits = 0
                if side:
                    seg, hits = mark_sidebar(html[side[0]:side[1]], charts[0])
                    html = html[:side[0]] + seg + html[side[1]:]
                    body = find_body(html)                 # spans shifted
                    kids = _spans(html, *inner(html, body))
                chart_div = next((k for k in kids if not is_sidebar(html, k)), None)
                html = replace_chart(html, chart_div or body, charts[0])
                note = f'chart + sidebar({hits} stats)' if side else 'chart only'

        elif arch in ('chart2', 'maps2'):
            imgs = re.findall(r'<img src="assets/maps/[^"]+"[^>]*>', html)
            if imgs and len(imgs) == len(charts):
                for img in imgs:
                    name = re.search(r'assets/maps/([a-z0-9_]+)\.png', img).group(1)
                    suffix = name.split('_')[-1]
                    slug = next((c for c in charts if c.endswith('_' + suffix)), charts[0])
                    html = html.replace(img, '{{CHART:%s}}' % slug, 1)
                note = f'img swap x{len(imgs)}'
            else:
                body = find_body(html)
                kids = _spans(html, *inner(html, body))
                if len(kids) < len(charts):
                    note = f'NEED {len(charts)} COLS, FOUND {len(kids)}'
                else:
                    for k, slug in zip(reversed(kids[:len(charts)]), reversed(charts)):
                        html = set_inner(html, k, '{{CHART:%s}}' % slug)
                    note = f'cols -> charts x{len(charts)}'

        # inline data tokens, and the provenance footer's unplaced third figure
        if arch != 'static':
            for pat, rep in GLOBAL_TOKENS:
                html = re.sub(pat, rep, html)
        html = re.sub(r'figs/state_([a-z]+)_\{line,years,mix\}\.png',
                      r'figs/state_\1_{years,line}.png', html)

        if row['file'] in INSERTS and '{{STAT:license_line}}' not in html:
            html = html.replace('</section>', INSERTS[row['file']] + '</section>')
            note = (note + '; +licence').lstrip('; ')

        open(path, 'w', encoding='utf-8').write(html)
        report.append((row['file'], arch, note,
                       len(re.findall(r'\{\{CHART:', html)),
                       len(re.findall(r'\{\{STAT:', html)),
                       len(re.findall(r'\{\{TABLE:', html))))

    print(f'{"file":44} {"arch":12} {"C":>2}{"S":>3}{"T":>3}  note')
    bad = 0
    for f, a, note, c, s, t in report:
        flag = any(w in note.upper() for w in ('NO ', 'NEED'))
        bad += flag
        if a != 'static':
            print(f'{f[:44]:44} {a:12} {c:2}{s:3}{t:3}  {"!! " if flag else ""}{note}')
    tot = [sum(r[i] for r in report) for i in (3, 4, 5)]
    print(f'\nmarkers: {tot[0]} charts, {tot[1]} stats, {tot[2]} tables   problem slides: {bad}')

if __name__ == '__main__':
    main()
