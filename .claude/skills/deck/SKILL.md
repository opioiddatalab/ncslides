---
name: deck
description: Edit the nightly NC drug checking deck — slide copy and layout, charts, stats, tables, the PowerPoint export, or the CI that publishes it. Use for any request to change what a slide says or shows, add or fix a figure, adjust a metric, or diagnose a number that looks wrong. Handles the build-and-verify loop; does not commit unless asked.
---

# Editing the nightly deck

This repo builds a 63-slide deck from live North Carolina drug checking data.
`Rscript r/build_nightly.R` regenerates every figure, the HTML deck, the PPTX and
the month-end archive in about 20 seconds.

A GitHub Action runs the same command at 07:30 UTC, commits the output to `docs/`
on `main`, and GitHub Pages serves it. **The deck is public.** A wrong number
reaches an audience of harm reduction workers who make decisions with it.

## Default behaviour

Work directly and quickly. Edit, build, verify, report. **Do not commit or push
unless the user explicitly says to** — they commit deliberately, usually after a
batch of edits.

Commit straight to `main` when asked; that is the intended flow for slide work.
Suggest a branch only when the change touches `r/data.R`, the enricher of a
number rather than its presentation, or `.github/workflows/` — places where being
wrong is expensive and CI should vet it first.

## The loop

```bash
DECK_OUT=build Rscript r/build_nightly.R    # never write to docs/
python3 tools/audit_slides.py
python3 tools/check_contract.py
```

`DECK_OUT` redirects every output — figures, deck, PPTX, archive, library page.
Use it always. `docs/` is a **tracked** directory that the nightly bot rewrites;
writing there dirties ~90 tracked files and invites committing a stale build over
the bot's newer one, with conflicts on binary PNGs.

If `docs/` does get dirtied: `git checkout -- docs`.

A healthy build ends with:

```
figures: 55 made, 0 skipped
wrote build/index.html (63 slides, 5.5 Mb)
done in 0.3 min
```

`0 skipped` matters. A skipped figure means a marker somewhere has no value.

## Hard rules

- **Never `git add -A`.** Name every file. That is how `docs/` gets committed.
- **Never commit `docs/`.** A pre-commit hook blocks it, but do not rely on that.
- **Never invent a number.** Every value on a slide comes from the data via a
  `{{STAT:...}}` marker. If a figure needs a number the pipeline does not
  produce, add it to `r/figs.R` — do not hardcode it into a template.
- **`show_placeholder_notes` stays `false`** in `r/config.yml` for any real build.

## Where things live

| To change | Edit |
|---|---|
| Slide copy, headings, layout | `r/slides/NN_*.html` (numbered in deck order) |
| Which charts/tables a slide shows | `r/slides/spec.csv` |
| How a chart is computed or drawn | `r/figs.R` |
| The 12 KPI tiles | `DASH_METRICS` in `r/figs.R` |
| Columns read from the extract | `r/data.R` |
| Colours, fonts, figure sizes | `r/theme_unc.R` |
| Programme count, windows, toggles | `r/config.yml` |
| HTML assembly, marker substitution | `r/deck.R` |
| PowerPoint export | `r/pptx.R` |
| Archive + library page | `r/library.R` |
| "New to NC" logic | `r/new_detections.R` |
| R version, packages, schedule | `.github/workflows/nightly.yml` |
| County polygons (one-time, needs sf/tigris) | `r/prep_counties.R` |

## The marker contract

Templates hold placeholders that `r/deck.R` fills:

```
{{CHART:dash_fentanyl}}   {{STAT:dash_fentanyl_val}}   {{TABLE:a1}}
```

An unresolved marker **aborts the build** — by design, so a half-filled slide
cannot publish. Adding a marker to a template means adding its value in
`r/figs.R`. Renaming means renaming both. `tools/check_contract.py` catches the
mismatch in seconds without running R; run it after any marker change.

## Conventions worth preserving

Several charts share behaviour that is easy to destroy in a rewrite. Keep it
unless the user asks otherwise:

- **Partial periods** are plotted, starred, and named in the caption; the LOESS
  is fit on **complete periods only**, so the in-progress month cannot drag a
  trend.
- **Rates are per 1,000 samples**, denominated by `n_distinct(sampleid)`.
- **Primary vs trace** abundance is a two-level split with a fixed fill scale
  (`scale_fill_primarytrace`), solid = primary, light = trace.
- **Every figure carries alt text.** The deck targets WCAG 2.2 AA and the alt
  text is generated from the data, not written by hand — see how the existing
  `alt = sprintf(...)` strings name the actual top values.
- **Never reach into the source data frame from inside `aes()`.** Hoist the
  constant out; ggplot2 4.x warns about it.

## When a number looks wrong

Check the data before changing code. Rare analytes genuinely read zero: the
levamisole, carfentanil and nitazene KPI tiles all read 0 in August 2026 because
the last complete month truly had no detections of any of them, across a
2,767-sample history. Inspect the underlying column and the monthly counts, say
which it is, and only then propose a change.

## Adding a slide

1. Create `r/slides/NN_*.html`, numbered where it should appear. Match the
   surrounding templates' markup — they are design exports, so copy structure
   rather than inventing it.
2. Add its row to `r/slides/spec.csv` (`file,archetype,charts,table_id`).
   Archetypes in use: `static`, `chart1`, `chart1_side`, `chart2`, `maps2`,
   `table`, `stats`, `kpi12`.
3. Wire any chart in `r/figs.R` so `check_contract.py` passes.
4. Verify the template's `<div>`s balance. Slide 38 once shipped with 12
   unclosed tile divs, which nested the whole grid 12 deep and rendered as a
   blank slide.

## Verifying before you report done

- Build exits 0, `55 made, 0 skipped`, 63 slides.
- Both `tools/` gates pass.
- `grep -c '{{' build/index.html` is 0 — no unresolved markers.
- `grep -ci placeholder build/index.html` is 0 — CI fails on this.
- `git status` shows no `docs/`.
- For a visual change, say what to look at: the slide number and what changed.

## CI

`.github/workflows/check.yml` runs the gates and a no-publish build on every push
to `main` and every PR. `.github/workflows/nightly.yml` is the one that publishes;
it runs on a schedule and on demand.

Two traps in the nightly workflow, both already fixed and worth not undoing:

- The `RSPM` URL needs its `__linux__/noble/` segment or every run compiles from
  source for ~20 minutes. `runs-on` is pinned to `ubuntu-24.04` to match.
- `dependencies: '"hard"'` keeps `sf`/`tigris` out. The action's default pulls
  them in, along with an apt-installed GDAL stack the nightly never loads.

"Re-run all jobs" replays the **original** commit and workflow file. To pick up a
change, use **Run workflow**, which reads the branch's latest.
