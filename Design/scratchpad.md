# Deck plan — NC Drug Checking Nightly Data Brief (35 slides, 1920×1080)

Audience: NC public health officials + researchers. Story arc. Quarterly rates per 1,000 unique samples completed that quarter. Primary vs trace = stacked bars (primary solid blue, trace light blue). Current quarter labeled "partial". New detections = first-ever-in-NC, surfaced in last 3 months. No geography slides. All chart numbers are PLACEHOLDER mockups; nightly R scripts (plain scripts → PNGs) compute real values.

Palette: Carolina blue #7BAFD4, navy ink #13294B, body #5B6770, orange accent #E8963C, trace #C7DCEC, rule #E2E8F0. Helvetica Neue. White bg; section dividers Carolina blue.

Titles (topic noun-phrases):
1. North Carolina Drug Checking — Nightly Data Brief (title)
2. About This Brief
3. How Samples Reach the Lab
4. Two Linked Datasets
5. Primary vs. Trace: How to Read the Variables
6. North Carolina at a Glance
7. Samples Completed by Quarter
8. § What the Card Tells Us
9. Expected Substances
10. Expectation vs. Lab Result
11. Reported Sensations
12. Overdose Involvement
13. Color and Texture
14. § What the Lab Detects
15. Substances per Sample
16. Top Substances Detected
17. Fentanyl
18. Fentanyl Synthesis Impurities
19. Xylazine
20. Medetomidine
21. Methamphetamine
22. Cocaine
23. Levamisole
24. Carfentanil
25. Nitazenes
26. BTMPS
27. Opioid–Stimulant Overlap
28. § New Detections
29. New to North Carolina: Last Three Months
30. Emerging Substance Watchlist
31. § Fine Print
32. Data Caveats
33. Variable Reference
34. Data Access
35. Credits

Chart-slide footer pattern: definition line (`lab_X` primary / `lab_X_any` any abundance · rate per 1,000 samples that quarter) + R tag (`r/figs.R::fig_fentanyl → figs/fentanyl_quarterly.png`).
Deliverables besides deck: r/theme_unc.R, r/data.R, r/figs.R, r/new_detections.R, HANDOFF.md, github.md.
