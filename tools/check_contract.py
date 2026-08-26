#!/usr/bin/env python3
"""Check the slide templates and the R code agree on every marker.

r/deck.R aborts the build if a {{CHART:...}}, {{STAT:...}} or {{TABLE:...}}
marker has no value, which is the real guard. This is the cheap static version:
it runs without R, so CI catches a renamed figure or a dropped stat before
spending minutes on a build.

It reads the naming rules out of the R sources rather than duplicating them, so
adding a substance in one place does not silently break the other.
"""
import re, csv, glob, os, sys, collections

ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..')
def rd(p): return open(os.path.join(ROOT, p), encoding='utf-8').read()

# ---------------------------------------------------- what the templates need
need_chart, need_stat, need_table = set(), set(), set()
for f in glob.glob(os.path.join(ROOT, 'r/slides/*.html')):
    c = open(f, encoding='utf-8').read()
    need_chart |= set(re.findall(r'\{\{CHART:([^}]+)\}\}', c))
    need_stat  |= set(re.findall(r'\{\{STAT:([^}]+)\}\}',  c))
    need_table |= set(re.findall(r'\{\{TABLE:([^}]+)\}\}', c))

figs, deck, build = rd('r/figs.R'), rd('r/deck.R'), rd('r/build_nightly.R')

# ------------------------------------------------- what the R code can produce
# Parse the runner: it is the single place that names every figure.
runner = figs[figs.index('run_all_figs <- function'):]

# one(acc, "slug", ...) and slug = "..." inside fig functions
have_chart = set(re.findall(r'one\(acc,\s*"([^"]+)"', runner))
have_chart |= set(re.findall(r'slug\s*=\s*"([^"]+)"', figs))
# many(acc, c("a", "b"), ...)
for block in re.findall(r'many\(acc,\s*c\(([^)]*)\)', runner):
    have_chart |= set(re.findall(r'"([^"]+)"', block))
# draw(<window>, "slug") inside fig_county_coverage
have_chart |= set(re.findall(r'draw\([^,]*,\s*"([^"]+)"\)', figs))
# the trends table: c("<slug>", "<metric>")
trend_slugs = re.findall(r'c\("(\d+_[a-z_]+_trend)",\s*"[a-z_]+"\)', runner)
have_chart |= set(trend_slugs)

dash_keys = re.findall(r'key\s*=\s*"([a-z_]+)"', figs)
have_chart |= {'dash_' + k for k in dash_keys}

# the state-view / map loop: for (s in list(c("fentanyl", "fentanyl"), ...))
loop = re.search(r'for \(s in list\(([\s\S]*?)\)\) \{', runner)
subs = re.findall(r'c\("([a-z]+)",\s*"[a-z_]+"\)', loop.group(1)) if loop else []
for sub in set(subs):
    have_chart |= {f'state_{sub}_years', f'state_{sub}_line',
                   f'maps_{sub}_prev', f'maps_{sub}_chg'}

# ------------------------------------------------------------------- stats
have_stat = set()
# global_stats() in deck.R: `name = ` entries inside its list(...)
g = deck[deck.index('global_stats <- function'):]
g = g[:g.index('\n}\n')]
have_stat |= set(re.findall(r'^\s{4}([a-z_0-9]+)\s*=', g, re.M))
# setNames(list(...), c("a","b")) blocks in figs.R
for block in re.findall(r'setNames\(list\([\s\S]*?\),\s*(c\([^)]*\)|paste0\([^)]*\))\)', figs):
    have_stat |= set(re.findall(r'"([^"]+)"', block))
# slug-derived trend stats
trend_slugs = re.findall(r'c\("(\d+_[a-z_]+_trend)",\s*"[a-z_]+"\)', figs)
for s in trend_slugs:
    have_stat |= {s, s + '_label', s + '_label2'}
# substances-per-sample label
have_stat |= {'15_substances_per_sample_label', '19_substances_per_sample_12mo_label'}
# KPI tile stats, assigned by paste0()
have_stat |= {f'dash_{k}_{sfx}' for k in dash_keys for sfx in ('val', 'badge')}

# ------------------------------------------------------------------- tables
have_table = set(re.findall(r'tables\$([a-z0-9_]+)\s*<-', build))
have_table |= set(re.findall(r'a4b?\s*=', figs))
have_table |= {'a4', 'a4b'} if 'a4b' in figs else set()

# ------------------------------------------------------------------- report
def report(kind, need, have):
    missing = sorted(need - have)
    print(f'{kind:8} templates need {len(need):3}   R provides {len(have):3}   '
          f'missing {len(missing)}')
    for m in missing:
        print(f'           MISSING {m}')
    return missing

bad = []
bad += report('CHART', need_chart, have_chart)
bad += report('STAT',  need_stat,  have_stat)
bad += report('TABLE', need_table, have_table)

# a figure produced but never placed is waste, not breakage: report, don't fail
extra = sorted(have_chart - need_chart)
if extra:
    print('\nfigures produced but not placed on a slide (harmless):')
    for e in extra:
        print('   ', e)

print('\ncontract:', 'OK' if not bad else f'{len(bad)} MISSING')
sys.exit(1 if bad else 0)
