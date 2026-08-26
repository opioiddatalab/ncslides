# ncslides

Daily slide deck and R code for North Carolina drug checking data results.

Every night the pipeline pulls the day's NC extracts, regenerates all 55 figures,
and publishes a 63-slide brief plus a PowerPoint export and a month-end archive.
Nothing is hand-updated.

| Output | Path |
|---|---|
| Current deck (canonical, accessible) | `docs/index.html` |
| Current deck (PowerPoint) | `docs/nc-drug-checking-latest.pptx` |
| Month-end snapshots | `docs/archive/YYYY-MM-DD.{html,pptx}` |
| File library | `docs/archive/index.html` |
| Figures | `docs/figs/*.svg` (deck) and `*.png` (PPTX) |
| Machine-readable summary | `docs/figs/stats.json` |

## Data

Two files, refreshed daily upstream, linked on `sampleid`:

- `https://data.streetsafe.supply/datasets/NC/analysis_dataset.csv` — one row per sample
- `https://data.streetsafe.supply/datasets/NC/lab_detail.csv` — one row per substance per sample

[Codebook](https://github.com/opioiddatalab/drugchecking/blob/main/datasets/unc_druchecking_codebook.txt)
· [schema](https://github.com/opioiddatalab/drugchecking/blob/main/datasets/technical_details.md).

Naming: `lab_X` = primary substance, `lab_X_any` = primary or trace, trace-only =
`lab_X_any & !lab_X`.

### Two traps worth knowing

**The CSV is a Stata export, so value-labelled variables arrive as label text,
not the numeric codes the codebook documents.** `consumed` is `"taken"`/`"unused"`,
not `1`/`0`; `sen_strength` is `"weaker"`/`"normal"`/`"stronger"`, not `-1/0/1`.
Comparing those to `1` matches nothing, produces an empty chart, and exits 0.
Everything affected goes through a normaliser in `r/data.R` that accepts both forms.

**Prefer the lab's own derived flags over regexes on substance names.** The
extract ships `lab_btmps_any`, `lab_nitazene_any`, `lab_medetomidine_any`,
`lab_fentanyl_impurities_any` and more, all traceable to the chemical dictionary.
A regex like `"BTMPS|bis\\(2,2,6,6"` matches none of the 243 substance strings —
another silent empty chart. Where a flag has no primary counterpart (levamisole,
fentanyl impurities), the primary side is derived from `lab_detail`, which repeats
the flag per substance row and adds `abundance`. See `LAB_FLAGS` in `r/data.R`.

Everything is anchored to the **data's** last date, never `Sys.Date()`: the extract
lags the calendar, so anchoring to the run date flags an empty period as "partial"
and quietly shortens rolling windows by the lag.

## Layout

```
r/slides/            slide templates (the design) + spec.csv + _helmet.css
r/theme_unc.R        palette, theme, SVG+PNG export
r/data.R             loading, normalisers, periods, denominators, rates
r/figs.R             55 figures; each returns list(plot, alt, table, stats)
r/new_detections.R   substances new to NC in the trailing window
r/deck.R             HTML assembly, render_table(), marker substitution
r/pptx.R             hybrid PowerPoint export
r/library.R          month-end archive + library page
r/build_nightly.R    entry point
r/prep_counties.R    ONE-TIME, needs sf/tigris (see Maps)
tools/               design split, marker annotation, and two CI gates
```

## How the deck is generated

The templates in `r/slides/` **are** the design, exported from the Claude Design
canvas. `deck-stage.js` is a plain custom element with no React dependency, and
slides are inline-styled `<section>` siblings, so R can emit the finished HTML
with every value baked in — no client-side fetch, so the deck can't flash
placeholder numbers or fail on a missing JSON file, and its alt text and data
tables are correct by construction.

R supplies values through markers that `tools/annotate_slides.py` inserted once:

| Marker | Filled with |
|---|---|
| `{{CHART:<slug>}}` | inline SVG, so chart text stays selectable and searchable |
| `{{STAT:<key>}}` | a scalar or a whole generated sentence |
| `{{TABLE:<id>}}` | a real `<table>` from `render_table()` |
| `{{ noteDisplay }}`, `{{ rTagDisplay }}` | the design's own vars, from `r/config.yml` |

`r/deck.R` **aborts** if any marker has no value.

### Static vs generated slides — read before editing

22 slides are static and pass through verbatim; the other 41 are generated.

**Editing a generated slide in the Claude Design canvas will be overwritten by the
next nightly build.** Change the R that produces it instead. Static slides can be
edited in the canvas, but see below.

If the design is re-exported, run both steps — the second re-applies the wiring
the first wipes:

```bash
python3 tools/split_slides.py      # design -> r/slides/*.html
python3 tools/annotate_slides.py   # re-insert markers
python3 tools/audit_slides.py      # no mock values survived
python3 tools/check_contract.py    # templates and R agree
```

`r/slides/spec.csv` is hand-authored (archetype and figure list per slide) and is
**not** regenerated — keep it in step when slides are added or removed.

### Why the audit exists

The design's placeholder values are plausible-looking fabrications — "8,412
samples", "78 counties", "463 distinct substances". Any that survive annotation
publish as though they were real, and nothing downstream notices.
`tools/audit_slides.py` fails the build on a leftover bare number or date token,
and CI runs it before R starts.

## Accessibility

The deck ships under UNC's Accessibility of Digital Content and Materials
Standard (WCAG 2.2 Level AA, ADA Title II). The **HTML is the conforming
version**: contrast-checked palette (ratios recorded in `r/theme_unc.R`), alt
text regenerated nightly alongside each figure, data tables in Appendix A1–A5,
no colour-only encoding — the in-progress period is a distinct texture and every
primary/trace segment is directly labelled.

The PPTX is a convenience export and is deliberately **hybrid**: chart areas are
images, but titles, captions, footers and appendix tables are real text boxes, so
most of the text survives. Distribute the HTML where conformance matters, and
never Print-to-PDF — that strips tags, alt text and reading order.

## Maps without a geo stack

`r/prep_counties.R` projects North Carolina's 100 counties **once** and vendors
the result as `r/nc_counties.rds`; the nightly run draws them with
`geom_polygon`. That keeps GDAL/PROJ/GEOS and the Census API out of the nightly
path entirely — county boundaries don't change, and a nightly job shouldn't
depend on an external geo service to render. `sf` and `tigris` are dev-only
(`Suggests`), needed just to regenerate that file:

```bash
Rscript r/prep_counties.R   # run locally, commit the .rds
```

## Running it

```bash
Rscript r/build_nightly.R
```

That writes `docs/`, which is **tracked** — the nightly job commits it and GitHub
Pages serves it from `main`. So a local build dirties ~90 tracked files, and
committing one means merging a stale build over the bot's newer one (with
conflicts on binary PNGs). For a throwaway build, point `DECK_OUT` somewhere
gitignored:

```bash
DECK_OUT=build Rscript r/build_nightly.R
```

Everything — figures, deck, PPTX, archive, library page — moves with it; the two
paths produce byte-identical output. CI sets nothing and keeps writing `docs/`.
If you did already build into `docs/`, `git checkout -- docs` discards it.

**R 4.4.3**, matching the Deepnote environment used for interactive development.
Every dependency's current CRAN floor is `R >= 4.1` or lower, so nothing needs a
dated-snapshot downgrade. CI pins package *versions* through a dated Posit
Package Manager snapshot (`RSPM` in the workflow) — bump that date deliberately,
never incidentally. `ggplot2 4.0.x` is a major S7 rewrite while this code is 3.x
idiom; it has built the deck cleanly since 4.0.2, but if it misbehaves, pin
3.5.x.

Two things about that `RSPM` URL that are easy to get wrong:

- **Keep the `__linux__/noble/` segment.** It is what makes Posit serve prebuilt
  Linux *binaries*. The plain `/cran/<date>/` path is source-only for Linux
  clients, which means ~20 minutes of compiling `stringi`, `data.table` and
  friends on every cold run instead of ~2 minutes of downloading.
- **`runs-on` is pinned to `ubuntu-24.04`, not `ubuntu-latest`,** because
  `noble` is 24.04's codename. If the runner image moves to a newer Ubuntu while
  the path still says `noble`, the binaries 404 and pak silently falls back to
  source builds. Change both together or neither.

CI installs **hard dependencies only** (`dependencies: '"hard"'`). The action's
default is `'"all"'`, which includes `Suggests` — that pulled in `sf` and
`tigris`, and with them `s2`, `wk`, `units` and an apt-installed GDAL/PROJ/GEOS,
none of which the nightly path ever loads. See "Maps without a geo stack" above.

## Publishing

`.github/workflows/nightly.yml` runs at 07:30 UTC (~03:30 ET, after the upstream
refresh) and on `workflow_dispatch`. It uses the injected `GITHUB_TOKEN`, so
there is no credential to create or rotate, and commits `docs/` for GitHub Pages
to serve.

Two things to know:

- **Pages needs enabling once**: Settings → Pages → branch `main`, folder `/docs`.
- **GitHub disables scheduled workflows after 60 days of repo inactivity.** The
  nightly commit counts as activity, so this self-sustains — but a long pause in
  the data upstream could stall it.

The build exits non-zero on an empty or short download, and CI fails on
unresolved markers or leftover placeholder text, so a broken run fails loudly
instead of publishing a half-built deck.

## Licence

[CC0 1.0](LICENSE) — public domain dedication. Attribution is **not required**,
but is requested: **UNC Street Drug Analysis Lab** · **OpioidData.org**.
