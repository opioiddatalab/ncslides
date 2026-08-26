# HANDOFF — nightly R build for the NC Drug Checking deck

## What exists
- `NC Drug Checking Nightly Deck.dc.html` — the 35-slide deck design (1920×1080). Every chart on it
  shows PLACEHOLDER values styled to spec; each chart slide's footer names the R function and the
  PNG it should be replaced by (e.g. `r/figs.R::fig_drug_trend("17_fentanyl", …) → figs/17_fentanyl_trend.png`).
- `r/` — working R skeleton: `theme_unc.R` (palette + theme), `data.R` (nightly loaders, quarter
  denominators, per-1,000 rate builder), `figs.R` (one function per figure), `new_detections.R`,
  `build_nightly.R` (cron entrypoint).

## Analytic contract (do not change without asking)
- Rates are **per 1,000 unique samples completed that calendar quarter** (denominator =
  `n_distinct(sampleid)` with `date_complete` in the quarter).
- Primary vs trace is **stacked**: solid blue #3A6D99 = primary (`lab_X`), light blue
  #C9DEEE with a `unc$body` outline = trace-only increment (`lab_X_any` − `lab_X`). Every figure
  caption must name the exact variables/regex used. Segments get direct % / n labels — hue alone
  must never carry primary vs trace (WCAG 1.4.1).
- The current calendar quarter is always shown, flagged **"partial"** (diagonal stripe fill + label
  — NOT low alpha; see the striped bars on slides 11/20).
- "New detection" = substance whose **first-ever NC detection** falls in the last 91 days.
- Substances without derived flags in analysis_dataset (medetomidine, nitazenes, BTMPS, levamisole
  primary split, fentanyl impurities) are flagged from `lab_detail.substance` regex — see
  `flag_from_lab()`; verify regexes against the live substance list before shipping.

## Claude Code tasks
1. Wire the deck to real output: replace each placeholder chart block with
   `<img src="figs/<name>.png">` (footers name the file), and hydrate slide 6 stats + the title-slide
   date from `figs/stats.json`, slide 29's table from `figs/29_new_detections.json`.
2. Harden `data.R` against schema drift (missing cols → warn, don't crash) and add a
   `renv.lock` (deps: dplyr readr tidyr lubridate zoo forcats stringr scales tidytext ggplot2 jsonlite).
3. Add `logs/` + failure notification (non-zero exit on empty download).
4. Cron: `30 2 * * * Rscript r/build_nightly.R` (upstream CSVs refresh daily).
5. Keep slide copy static — only figures, stats.json numbers, and the new-detections table change nightly.

## Accessibility (WCAG 2.2 AA — required, UNC Digital Accessibility Standard)
The deck ships under UNC's Accessibility of Digital Content and Materials Standard (WCAG 2.2 Level AA,
ADA Title II). The HTML side is already compliant (colors in `theme_unc.R` carry the ratios in
comments; #7BAFD4 only on navy, never text on white). The nightly build must not regress it:

6. **Programmatic alt text for every figure.** Each `fig_*()` returns `list(plot, alt, table)`.
   `alt` is a `sprintf()` on the already-computed dataframe stating chart type, variable, time
   range, and takeaway numbers (peak, latest, direction) — e.g. "Bar chart, unique samples per
   quarter, 24Q3 to 26Q3. Range 380 to 612; latest complete quarter 26Q2: 612. 26Q3 partial: 350."
   Never "chart of data." Maps summarize by region: "Map of NC, fentanyl per 1,000 by region,
   26Q2: East 610, Central 540, West 465; darker = higher."
   `build_nightly.R` writes all of them to `figs/alt.json`; the deck hydrates each `<img alt>` from
   it (same fetch pattern as `stats.json`).
7. **Data-table fallback = Appendix slides, in the deck.** Do NOT emit separate CSVs. (Slides A1–A5 + divider are ALREADY BUILT in the deck design with placeholder values — wire data only.) Add an
   "Appendix — Data Tables" section divider (same navy divider layout as Parts 1–8) after the Fine
   Print section, followed by one slide per key chart rendering `fig_*()$table` as a real HTML
   `<table>` (proper `<th scope="col">` headers, no merged cells, banded rows, ≥24px text).
   Cover at minimum: quarterly denominators, expected substances, expectation-vs-lab, key-substance
   rates, new detections. Each chart slide's footer links to its appendix table ("Data: Appendix A2");
   tables hydrate nightly from `figs/alt.json` (`tables` key).
8. **Accessibility statement slide in Methods.** (ALREADY BUILT — slide 08b, static.) Add one slide at the end of Part One stating:
   conformance target **WCAG 2.2 Level AA** per the UNC standard and ADA Title II; what that means
   here (contrast-checked palette, alt text regenerated nightly with the figures, data tables in the
   Appendix, no color-only encoding, screen-reader reading order); and the reporting channel
   (digital_accessibility@unc.edu / DAO report form). Static slide, reviewed by lab staff.
9. When distributing, share the HTML/source (or a properly tagged export) — never Print-to-PDF,
   which strips tags, alt text, and reading order.

## Sources
- Data: https://data.streetsafe.supply/datasets/NC/analysis_dataset.csv + lab_detail.csv (linked by sampleid)
- Codebook: https://github.com/opioiddatalab/drugchecking/blob/main/datasets/unc_druchecking_codebook.txt
- Schema: https://github.com/opioiddatalab/drugchecking/blob/main/datasets/technical_details.md
