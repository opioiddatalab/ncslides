# Working notes — NC Drug Checking nightly deck

## Brand / look (results.streetsafe.supply + attached assets)
- Carolina blue #7BAFD4 (admin portal uses it; panel logo)
- Panel logo: uploads/OpioidDateLab_logo-panels_v2__01-blue-text.jpg — "Science in Service", 4 comic panels, black line art + Carolina blue; two variants (blue bg / white bg)
- Example deck style (pasted images): light gray text (#d9d9d9) on white, ORANGE bold emphasis (~#E8A33D) for key numbers; gray-blue body text (#5B6770); bullets with orange italics for caveats
- Site: white bg, panel logo header, plain nav, stat line "23,731 samples completed from 42 states. Last updated <date>."
- Attribution required: "UNC Street Drug Analysis Lab" and "OpioidData.org"

## Data
- NC files: https://data.streetsafe.supply/datasets/NC/analysis_dataset.csv (wide, 1 row/sample) + lab_detail.csv (long, 1 row/substance/sample), linked by sampleid
- Codebook: github opioiddatalab/drugchecking → datasets/unc_druchecking_codebook.txt (96 vars); schema: datasets/technical_details.md
- Naming: lab_X = primary only; lab_X_any = primary OR trace; lab_X_trace = trace only. abundance in lab_detail: blank=primary, "trace"
- analysis_dataset vars: sampleid, program, location, county, state, full_state, countyfips/state_county, date_collect, date_complete, sampletype/collection, consumed, expectedsubstance + expect_opioid/fentanyl/xylazine/stimulant/benzo/meth/cocaine/cannabis/hall, color, bright_color, texture + tar/pill/powder/plant/crystals/lustre, sensations + sen_strength/-1..1, sen_weird/hall/up/down/nice/long/burn/skin/seizure, texture_notes, sensation_notes, overdose/od/overdose_notes/fatal_od, confirmatory, primary, trace, lab_null, lab_num_substances(_any), lab_fentanyl(_any), lab_xylazine(_any/_trace), lab_meth(_any), lab_cocaine(_any), lab_caffeine, lab_gabapentin, lab_levamisole_any, lab_mdma, lab_tramadol, lab_carfentanil(_any), lab_ketamine(_any), lab_fentanyl_impurity (4-ANPP etc.), + more lab flags
- lab_detail: substance (standardized), method (GCMS), abundance, chemdict class flags (cathinone, fent impurity, etc.), PubChem CID, CAS, UNII
- Geocoding: OpenCage, county centroid; defaults to program mailing city if blank

## Notebook (Program Summary.ipynb) graph inventory
1. Monthly sample timeline (line, unique sampleid by month, zero-filled)
2. Bar charts: expected substances, colors, textures, sample types (split ';', count)
3. Substances detected table: count + chemdict 'sixwords' description
4. Co-occurrence: what else found with selected substance
5. State comparison for selected substance; earliest/latest detection dates
6. Sample links: https://streetsafe.supply/results/p/{sampleid}
Summary stats: N samples, N programs, N counties/states, latest date

## User requirements
- Deck about the VARIABLES in the NC datasets (comprehensive codebook-style tour)
- New detections in last 3 months in NC slide
- Primary vs trace specified on each graph, with variations
- Data caveats slide (post-consumption bias 70%, FTIR-first programs send complex samples, geo caveats)
- R ggplot2 code to generate every graph; handoff to Claude Code for full R deck generation
- Runs nightly (parameterized by date; data URLs refresh daily)
- Include UNC Street Drug Analysis Lab + OpioidData.org branding
