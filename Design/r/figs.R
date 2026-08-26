# figs.R — one function per deck figure; each ggsave_slide()s a PNG the deck loads
source("r/theme_unc.R"); source("r/data.R")
library(forcats); library(stringr)

nc <- load_nc(); card <- nc$card; lab <- nc$lab

# Inclusion window for since-inception figures — cite in every all-data caption
incl_dates <- function() sprintf("Includes samples with date_complete %s\u2013%s.",
  format(min(card$date_complete, na.rm = TRUE), "%d %b %Y"), format(nc$latest, "%d %b %Y"))

partial_lab <- function(p, df) {
  # annotate the in-progress quarter
  pq <- unique(df$quarter[df$partial]); if (!length(pq)) return(p)
  p + annotate("text", x = pq, y = Inf, label = "partial", vjust = 1.5,
               color = unc$partial, size = 4.5, family = "Helvetica")
}

# 07 — Samples completed by quarter
fig_samples_quarterly <- function() {
  d <- quarter_denoms(card)
  p <- ggplot(d, aes(factor(quarter), n_samples, alpha = !partial)) +
    geom_col(fill = unc$blue) + scale_alpha_manual(values = c(0.45, 1), guide = "none") +
    labs(title = "Samples completed by quarter", x = NULL, y = "Unique samples") + theme_unc()
  ggsave_slide("07_samples_quarterly.png", p)
}

# 09 — Expected substances (card): split ';' lists, top 12
fig_expected <- function() {
  d <- card %>% mutate(expectedsubstance = ifelse(expectedsubstance == "", "unknown", expectedsubstance)) %>%
    separate_rows(expectedsubstance, sep = ";") %>%
    mutate(expectedsubstance = str_trim(expectedsubstance)) %>%
    count(expectedsubstance, sort = TRUE) %>% slice_head(n = 12)
  p <- ggplot(d, aes(n, fct_reorder(expectedsubstance, n))) +
    geom_col(fill = unc$blue) +
    labs(title = "What donors expected the sample to be", x = "Samples", y = NULL, caption = incl_dates()) + theme_unc()
  ggsave_slide("09_expected_substances.png", p)
}

# 10 — Expectation vs lab result (fentanyl example + opioid/stimulant classes)
fig_expect_vs_lab <- function() {
  d <- bind_rows(
    card %>% filter(expect_fentanyl == 1) %>%
      summarise(group = "Expected fentanyl", confirmed = mean(lab_fentanyl_any == 1, na.rm = TRUE)),
    card %>% filter(expect_meth == 1) %>%
      summarise(group = "Expected methamphetamine", confirmed = mean(lab_meth_any == 1, na.rm = TRUE)),
    card %>% filter(expect_cocaine == 1) %>%
      summarise(group = "Expected cocaine", confirmed = mean(lab_cocaine_any == 1, na.rm = TRUE))
  )
  p <- ggplot(d, aes(confirmed, fct_reorder(group, confirmed))) +
    geom_col(fill = unc$blue) +
    scale_x_continuous(labels = scales::percent) +
    labs(title = "Share of expected substance confirmed in any abundance (lab_*_any)",
         x = "Confirmed", y = NULL, caption = incl_dates()) + theme_unc()
  ggsave_slide("10_expect_vs_lab.png", p)
}

# 11 — Sensations
fig_sensations <- function() {
  d <- card %>% filter(sensations != "") %>% separate_rows(sensations, sep = ";") %>%
    mutate(sensations = str_trim(sensations)) %>% count(sensations, sort = TRUE) %>% slice_head(n = 10)
  p <- ggplot(d, aes(n, fct_reorder(sensations, n))) + geom_col(fill = unc$blue) +
    labs(title = "Sensations reported on the card", x = "Samples", y = NULL,
         caption = paste("Post-consumption reports only; blank cards excluded (see consumed flag).", incl_dates())) + theme_unc()
  ggsave_slide("11_sensations.png", p)
}

# 13 — Overdose involvement by quarter (od: 1 involved / 0 not / NA unreported)
fig_overdose <- function() {
  d <- card %>% filter(!is.na(quarter)) %>% group_by(quarter) %>%
    summarise(rate = 1000 * sum(od == 1, na.rm = TRUE) / n_distinct(sampleid), .groups = "drop") %>%
    left_join(quarter_denoms(card), by = "quarter")
  p <- ggplot(d, aes(factor(quarter), rate, alpha = !partial)) + geom_col(fill = unc$orange) +
    scale_alpha_manual(values = c(0.45, 1), guide = "none") +
    labs(title = "Overdose-involved samples per 1,000 completed", x = NULL, y = "Rate per 1,000") + theme_unc()
  ggsave_slide("12_overdose.png", p)
}

# 13 — Color & texture
fig_color_texture <- function() {
  cx <- card %>% mutate(color = ifelse(color == "", "unknown", color)) %>%
    separate_rows(color, sep = ";") %>% count(color = str_trim(color), sort = TRUE) %>% slice_head(n = 8) %>%
    mutate(var = "Color", value = color)
  tx <- card %>% mutate(texture = ifelse(texture == "", "unknown", texture)) %>%
    separate_rows(texture, sep = ";") %>% count(value = str_trim(texture), sort = TRUE) %>% slice_head(n = 8) %>%
    mutate(var = "Texture")
  d <- bind_rows(cx %>% select(var, value, n), tx)
  p <- ggplot(d, aes(n, tidytext::reorder_within(value, n, var))) + geom_col(fill = unc$blue) +
    tidytext::scale_y_reordered() + facet_wrap(~var, scales = "free_y") +
    labs(title = "Reported color and texture", x = "Samples", y = NULL, caption = incl_dates()) + theme_unc()
  ggsave_slide("13_color_texture.png", p)
}

# 15 — Substances per sample (lab_num_substances vs lab_num_substances_any)
fig_substances_per_sample <- function(window_months = NULL) {
  cw <- win_filter(card, window_months)
  d <- bind_rows(cw %>% count(n_sub = lab_num_substances) %>% mutate(def = "Primary only"),
                 cw %>% count(n_sub = lab_num_substances_any) %>% mutate(def = "Any abundance"))
  p <- ggplot(d, aes(n_sub, n, fill = def)) + geom_col(position = "dodge") +
    scale_fill_manual(values = c("Primary only" = unc$blue, "Any abundance" = unc$trace)) +
    labs(title = "Substances detected per sample", x = "Substances in sample", y = "Samples", caption = win_caption(window_months)) + theme_unc()
  ggsave_slide(ifelse(is.null(window_months), "15_substances_per_sample.png",
                      paste0("19_substances_per_sample_", window_months, "mo.png")), p)
}

# Restrict a frame to the trailing N months of date_complete (NULL = since inception)
win_filter <- function(df, window_months, date_col = "date_complete") {
  if (is.null(window_months)) return(df)
  df %>% filter(.data[[date_col]] >= nc$latest %m-% months(window_months))
}
win_caption <- function(window_months) {
  if (is.null(window_months)) return(incl_dates())
  sprintf("Trailing %d months: date_complete %s\u2013%s (current month partial).", window_months,
          format(nc$latest %m-% months(window_months), "%d %b %Y"), format(nc$latest, "%d %b %Y"))
}

# 17 (since inception) / 18 (window_months = 12) — Top substances (lab_detail long file)
fig_top_substances <- function(window_months = NULL) {
  d <- lab %>% inner_join(card %>% select(sampleid, date_complete), by = "sampleid") %>%
    win_filter(window_months) %>% mutate(abundance = ifelse(abundance == "trace", "Trace only", "Primary")) %>%
    count(substance, abundance) %>% group_by(substance) %>% mutate(total = sum(n)) %>% ungroup() %>%
    arrange(desc(total)) %>% filter(substance %in% head(unique(substance), 15))
  p <- ggplot(d, aes(n, fct_reorder(substance, total), fill = abundance)) +
    geom_col() + scale_fill_primarytrace() +
    labs(title = paste0("Top 15 substances detected", ifelse(is.null(window_months), " (all NC samples)", " — past 12 months")),
         x = "Detections", y = NULL, caption = win_caption(window_months)) + theme_unc()
  ggsave_slide(ifelse(is.null(window_months), "16_top_substances.png",
                      paste0("18_top_substances_", window_months, "mo.png")), p)
}

# 17–26 — priority substance rate trends: LOESS line + observed points.
# Monthly when volume supports it (trend_period), quarterly otherwise.
# Partial current period: point plotted normally, starred, caveat in caption;
# LOESS is fit on COMPLETE periods only so the in-progress period can't drag it.
scale_color_primarytrace <- function()
  scale_color_manual(values = c("Primary" = unc$blue, "Trace only" = unc$trace))

fig_drug_trend <- function(slug, label, primary_var, any_var, cc = card) {
  by <- trend_period(cc)
  d  <- period_rate(cc, primary_var, any_var, by)
  x_lab <- if (by == "month") scales::label_date("%b %Y") else function(x) format(as.yearqtr(x), "%YQ%q")
  p <- ggplot(d, aes(period, rate, color = abundance, group = abundance)) +
    geom_smooth(data = dplyr::filter(d, !partial), method = "loess", se = FALSE,
                span = 0.75, linewidth = 1.4) +
    geom_point(size = 2.6) +
    geom_text(data = dplyr::filter(d, partial), aes(label = "*"),
              vjust = -0.4, size = 9, show.legend = FALSE) +
    scale_color_primarytrace() + scale_x_date(labels = x_lab) +
    labs(title = paste0(label, " per 1,000 samples completed — ",
                        ifelse(by == "month", "monthly", "quarterly"), " LOESS trend"),
         caption = paste0("Points: observed ", by, "ly rates; line: LOESS fit on complete ", by, "s only. ",
                          "* Current ", by, " is partial (data through ", format(nc$latest, "%d %b %Y"), "). ",
                          "Series: ", primary_var, " (primary) and trace-only (", any_var, " minus ", primary_var, ")."),
         x = NULL, y = "Rate per 1,000") + theme_unc()
  ggsave_slide(paste0(slug, "_trend.png"), p)
}

# Substances without a derived pair in analysis_dataset: build flags from lab_detail
flag_from_lab <- function(pattern) {
  ids_any  <- lab %>% filter(str_detect(substance, regex(pattern, ignore_case = TRUE))) %>% pull(sampleid)
  ids_prim <- lab %>% filter(str_detect(substance, regex(pattern, ignore_case = TRUE)),
                             abundance == "primary") %>% pull(sampleid)
  card %>% mutate(flag_prim = as.integer(sampleid %in% ids_prim),
                  flag_any  = as.integer(sampleid %in% ids_any))
}

fig_drug_trend_pattern <- function(slug, label, pattern) {
  fig_drug_trend(slug, paste0(label, " (lab_detail regex '", pattern, "')"),
                 "flag_prim", "flag_any", cc = flag_from_lab(pattern))
}

# 28 — Opioid–stimulant overlap: fentanyl (any) inside meth/cocaine-primary samples
fig_opioid_stimulant <- function() {
  d <- bind_rows(
    card %>% filter(lab_meth == 1) %>% group_by(quarter) %>%
      summarise(rate = 1000 * mean(lab_fentanyl_any == 1, na.rm = TRUE), .groups = "drop") %>%
      mutate(group = "Fentanyl (any) in meth-primary samples"),
    card %>% filter(lab_cocaine == 1) %>% group_by(quarter) %>%
      summarise(rate = 1000 * mean(lab_fentanyl_any == 1, na.rm = TRUE), .groups = "drop") %>%
      mutate(group = "Fentanyl (any) in cocaine-primary samples")
  ) %>% filter(!is.na(quarter))
  p <- ggplot(d, aes(factor(quarter), rate, color = group, group = group)) +
    geom_line(linewidth = 1.4) + geom_point(size = 2.5) +
    scale_color_manual(values = c(unc$blue, unc$orange)) +
    labs(title = "Fentanyl contamination of stimulant samples, per 1,000", x = NULL, y = "Rate per 1,000") +
    theme_unc()
  ggsave_slide("28_opioid_stimulant.png", p)
}

# 27 — Two-year trend dashboard: monthly points + LOESS per metric, last 24 months.
# Deltas (last complete month vs prior) printed green/red by direction of concern.
month_any_rate <- function(cc, any_var, lab_metric) {
  cc %>% filter(!is.na(date_complete)) %>%
    mutate(month = floor_date(date_complete, "month")) %>% group_by(month) %>%
    summarise(rate = 1000 * sum(.data[[any_var]] == 1, na.rm = TRUE) / n_distinct(sampleid),
              .groups = "drop") %>% mutate(metric = lab_metric)
}

fig_trend_dashboard <- function() {
  win_start <- floor_date(Sys.Date(), "month") %m-% months(23)
  cur_month <- floor_date(Sys.Date(), "month")
  d <- bind_rows(
    month_denoms(card) %>% transmute(month, rate = n_samples, metric = "Samples completed / month"),
    month_any_rate(card, "lab_fentanyl_any",  "Fentanyl (any)"),
    month_any_rate(flag_from_lab("4-ANPP|4ANPP|phenethyl"), "flag_any", "Fentanyl impurities (any)"),
    month_any_rate(card, "lab_xylazine_any",  "Xylazine (any)"),
    month_any_rate(flag_from_lab("medetomidine"), "flag_any", "Medetomidine (any)"),
    month_any_rate(card, "lab_meth_any",      "Methamphetamine (any)"),
    month_any_rate(card, "lab_cocaine_any",   "Cocaine (any)"),
    month_any_rate(flag_from_lab("levamisole"), "flag_any", "Levamisole (any)"),
    month_any_rate(card, "lab_carfentanil_any", "Carfentanil (any)"),
    month_any_rate(flag_from_lab("nitazene|etonitazene|metonitazene|protonitazene|isotonitazene"), "flag_any", "Nitazenes (any)"),
    month_any_rate(flag_from_lab("BTMPS|bis\\(2,2,6,6"), "flag_any", "BTMPS (any)"),
    month_any_rate(card %>% mutate(od_flag = as.integer(od == 1)), "od_flag", "Overdose-involved")
  ) %>% filter(month >= win_start) %>%
    mutate(metric = factor(metric, levels = unique(metric)), partial = month == cur_month)
  up_good <- c("Samples completed / month")
  deltas <- d %>% filter(!partial) %>% group_by(metric) %>% arrange(month) %>%
    summarise(delta = 100 * (last(rate) - nth(rate, -2L)) / nth(rate, -2L),
              x = max(month), y = max(rate), .groups = "drop") %>%
    mutate(good = ifelse(metric %in% up_good, delta >= 0, delta < 0),
           lab = sprintf("%s%.0f%%", ifelse(delta >= 0, "\u25b2", "\u25bc"), abs(delta)),
           col = ifelse(good, "#1E8E5A", "#C4453C"))
  p <- ggplot(d, aes(month, rate)) +
    geom_smooth(data = dplyr::filter(d, !partial), method = "loess", se = FALSE,
                span = 0.55, color = unc$blue, linewidth = 1.1) +
    geom_point(aes(color = partial), size = 1.7, show.legend = FALSE) +
    scale_color_manual(values = c("FALSE" = unc$blue, "TRUE" = unc$orange)) +
    geom_text(data = deltas, aes(x, y, label = lab), color = deltas$col,
              hjust = 1, vjust = 0, size = 4.2, fontface = "bold") +
    facet_wrap(~metric, ncol = 4, scales = "free_y") +
    labs(title = "Key metrics, trailing 24 months",
         caption = paste0("Rates per 1,000 samples/month (counts for samples completed). LOESS on complete months; ",
                          "orange point = partial month (through ", format(nc$latest, "%d %b %Y"), "). ",
                          "\u0394 = last complete month vs prior; green = favorable direction."),
         x = NULL, y = NULL) + theme_unc()
  ggsave_slide("27_dashboard.png", p)
}

# 30–33 — OCME-legacy-style state view, one call per key substance.
# Emits three PNGs per slug: monthly line (this yr vs last), year-to-year YTD bars, co-detection mix.
fig_state_view <- function(slug, any_var, co_vars) {
  cc <- card %>% filter(!is.na(date_complete), .data[[any_var]] == 1) %>%
    mutate(month = floor_date(date_complete, "month"), yr = year(date_complete))
  this_yr <- year(Sys.Date()); mcur <- floor_date(Sys.Date(), "month")
  # (1) month-to-month, current vs prior year
  d1 <- cc %>% filter(yr >= this_yr - 1) %>% count(yr, m = month(month)) %>%
    mutate(yr = factor(yr))
  p1 <- ggplot(d1, aes(m, n, color = yr, group = yr)) + geom_line(linewidth = 1.3) +
    geom_point(data = ~dplyr::filter(.x, yr == this_yr), size = 2.4) +
    scale_color_manual(values = setNames(c(unc$trace, unc$navy), c(this_yr - 1, this_yr))) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    labs(title = NULL, x = NULL, y = "Positive samples", color = NULL,
         caption = paste0("* ", format(mcur, "%b %Y"), " is partial (through ", format(nc$latest, "%d %b %Y"), ").")) + theme_unc()
  ggsave_slide(paste0("state_", slug, "_line.png"), p1)
  # (2) year-to-year: full-year totals + YTD overlay, YTD-vs-YTD % change
  ytd_cut <- yday(nc$latest)
  d2 <- cc %>% group_by(yr) %>%
    summarise(full = n(), ytd = sum(yday(date_complete) <= ytd_cut), .groups = "drop") %>%
    arrange(yr) %>% mutate(delta = 100 * (ytd - lag(ytd)) / lag(ytd))
  p2 <- ggplot(d2, aes(factor(yr))) +
    geom_col(aes(y = full), fill = unc$trace) + geom_col(aes(y = ytd), fill = unc$navy) +
    geom_text(aes(y = full, label = scales::comma(full)), vjust = -0.5, size = 4.2, fontface = "bold") +
    geom_text(aes(y = ytd, label = ifelse(is.na(delta), "", sprintf("%+.0f%%", delta)),
                  color = delta >= 0), vjust = -0.5, size = 4, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c("TRUE" = "#C4453C", "FALSE" = "#1E8E5A")) +
    labs(x = NULL, y = "Positive samples",
         caption = "Light = full year; dark = YTD through the same day-of-year as the run date.") + theme_unc()
  ggsave_slide(paste0("state_", slug, "_years.png"), p2)
  # (3) co-detection mix, trailing 12 months, share of positive samples per co-substance
  win <- mcur %m-% months(11)
  d3 <- cc %>% filter(month >= win) %>% select(sampleid, month) %>%
    left_join(card %>% select(sampleid, all_of(unname(co_vars))), by = "sampleid") %>%
    group_by(month) %>%
    summarise(total = n(), across(all_of(unname(co_vars)), ~100 * mean(.x == 1, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(-c(month, total), names_to = "co", values_to = "pct") %>%
    mutate(co = names(co_vars)[match(co, co_vars)])
  p3 <- ggplot(d3, aes(month, pct, fill = co)) + geom_col(position = "stack") +
    geom_text(data = ~dplyr::distinct(.x, month, total),
              aes(month, y = Inf, label = total), inherit.aes = FALSE, vjust = 1.4, size = 4, fontface = "bold") +
    scale_fill_manual(values = unname(c(unc$navy, unc$orange, unc$blue, unc$trace))[seq_along(co_vars)]) +
    scale_x_date(labels = scales::label_date("%b '%y")) +
    labs(x = NULL, y = "% of positive samples", fill = NULL,
         caption = "Shares exceed 100% when multiple co-substances occur in one sample (unlike OCME class shares).") + theme_unc()
  ggsave_slide(paste0("state_", slug, "_mix.png"), p3)
}

# 34–37 — regional maps: NC in 3 exploded regions (r/nc_county_regions.csv),
# choropleth by region rate (last complete quarter) + % change vs prior quarter.
# Needs: sf, tigris (counties(state = "NC", cb = TRUE)).
fig_region_maps <- function(slug, any_var) {
  regions <- readr::read_csv("r/nc_county_regions.csv", show_col_types = FALSE)
  shp <- tigris::counties(state = "NC", cb = TRUE, progress_bar = FALSE) %>%
    left_join(regions, by = c("NAME" = "county")) %>%
    group_by(region) %>% summarise()  # dissolve to 3 region polygons
  # explode: shift western/eastern outward for whitespace between blocks
  shift <- c(western = -0.25, central = 0, eastern = 0.25)
  sf::st_geometry(shp) <- sf::st_geometry(shp) + lapply(shp$region, function(r) sf::st_point(c(shift[[r]], 0)))
  sf::st_crs(shp) <- 4269
  qs <- sort(unique(card$quarter)); qcur <- qs[length(qs) - 1]; qprev <- qs[length(qs) - 2]  # last COMPLETE quarter
  d <- card %>% filter(quarter %in% c(qprev, qcur)) %>%
    left_join(regions, by = c("samplecounty" = "county")) %>%
    filter(!is.na(region)) %>% group_by(region, quarter) %>%
    summarise(rate = 1000 * sum(.data[[any_var]] == 1, na.rm = TRUE) / n_distinct(sampleid), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = quarter, values_from = rate) %>%
    rename(prev_q = 2, cur_q = 3) %>% mutate(delta = 100 * (cur_q - prev_q) / prev_q)
  m <- shp %>% left_join(d, by = "region")
  lab <- function(v) paste0(toupper(m$region), "\n", v)
  p1 <- ggplot(m) + geom_sf(aes(fill = cur_q), color = "white", linewidth = 0.3) +
    geom_sf_text(aes(label = lab(sprintf("%.0f per 1,000", cur_q))), nudge_y = -0.55, size = 4.4, fontface = "bold", color = unc$navy) +
    scale_fill_gradient(low = unc$trace, high = unc$navy, guide = "none") +
    labs(title = paste0("Rate per 1,000 samples, ", format(as.yearqtr(qcur), "%YQ%q"))) + theme_void()
  ggsave_slide(paste0("maps_", slug, "_prev.png"), p1)
  p2 <- ggplot(m) + geom_sf(aes(fill = delta), color = "white", linewidth = 0.3) +
    geom_sf_text(aes(label = lab(sprintf("%+.0f%%", delta))), nudge_y = -0.55, size = 4.6, fontface = "bold") +
    scale_fill_gradient2(low = "#1E8E5A", mid = "#F5F7FA", high = "#C4453C", midpoint = 0, guide = "none") +
    labs(title = paste0("% change vs ", format(as.yearqtr(qprev), "%YQ%q"))) + theme_void()
  ggsave_slide(paste0("maps_", slug, "_chg.png"), p2)
}

# 12 — Sensations by key substance: sen_strength Likert + aggregate atypical flag.
# Consumed samples only; denominator = consumed samples with the substance detected (any abundance).
fig_sensations_substance <- function() {
  keys <- c(Fentanyl = "lab_fentanyl_any", Xylazine = "lab_xylazine_any",
            Methamphetamine = "lab_meth_any", Cocaine = "lab_cocaine_any")
  cc <- card %>% filter(consumed == 1) %>%
    mutate(atypical = as.integer(str_detect(tolower(sensations),
                                            "unpleasant|weird smell|weird taste")))
  d <- purrr::imap_dfr(keys, function(v, nm) {
    cc %>% filter(.data[[v]] == 1) %>%
      summarise(substance = nm, n = n(),
                Weaker  = mean(sen_strength == "weaker",  na.rm = TRUE),
                Normal  = mean(sen_strength == "normal",  na.rm = TRUE),
                Stronger = mean(sen_strength == "stronger", na.rm = TRUE),
                atypical = mean(atypical == 1, na.rm = TRUE))
  })
  lik <- d %>% select(substance, Weaker, Normal, Stronger) %>%
    pivot_longer(-substance, names_to = "strength", values_to = "share") %>%
    mutate(strength = factor(strength, c("Weaker", "Normal", "Stronger")),
           # diverging: center Normal on 0
           lo = case_when(strength == "Weaker" ~ -share, TRUE ~ 0))
  p1 <- ggplot(lik, aes(y = substance)) +
    geom_col(data = ~dplyr::filter(.x, strength == "Normal"),
             aes(x = share, fill = strength), position = position_nudge(x = 0), width = 0.6) +
    geom_col(data = ~dplyr::filter(.x, strength != "Normal"),
             aes(x = ifelse(strength == "Weaker", -share, share), fill = strength), width = 0.6) +
    scale_fill_manual(values = c(Weaker = unc$trace, Normal = unc$rule, Stronger = unc$orange)) +
    scale_x_continuous(labels = ~scales::percent(abs(.x))) +
    labs(title = "Reported strength vs expected (sen_strength)", x = NULL, y = NULL, fill = NULL,
         caption = paste("Consumed samples with the substance detected in any abundance.", incl_dates())) +
    theme_unc()
  ggsave_slide("12_sen_strength.png", p1)
  p2 <- ggplot(d, aes(atypical, forcats::fct_reorder(substance, atypical))) +
    geom_col(fill = unc$orange) +
    geom_text(aes(label = scales::percent(atypical, 1)), hjust = -0.15, fontface = "bold", size = 4.6) +
    scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, .15))) +
    labs(title = "Unpleasant, weird smell, or weird taste (aggregate)", x = NULL, y = NULL,
         caption = paste("Any of the three atypical reports, parsed from semicolon sensation lists.", incl_dates())) +
    theme_unc()
  ggsave_slide("12_sen_atypical.png", p2)
}

# 13/14 — Sensation distribution shift by drug class: trailing 12 months vs all time before.
# class = "stim" (meth/cocaine any) or "opi" (fentanyl/heroin/benzo any); overlap appears in both.
fig_sensations_shift <- function(class = c("stim", "opi"), top_n = 8) {
  class <- match.arg(class)
  base <- if (class == "stim") card %>% filter(lab_meth_any == 1 | lab_cocaine_any == 1)
          else card %>% filter(lab_fentanyl_any == 1 | lab_heroin_any == 1 | lab_benzo_any == 1)
  cut <- nc$latest %m-% months(12)
  d <- base %>% filter(consumed == 1, sensations != "") %>%
    mutate(win = ifelse(date_complete >= cut, "Past 12 months", "Before")) %>%
    separate_rows(sensations, sep = ";") %>% mutate(sensations = str_trim(sensations)) %>%
    distinct(sampleid, win, sensations)
  denom <- base %>% filter(consumed == 1, sensations != "") %>%
    mutate(win = ifelse(date_complete >= cut, "Past 12 months", "Before")) %>% count(win, name = "n_win")
  top <- d %>% count(sensations, sort = TRUE) %>% slice_head(n = top_n) %>% pull(sensations)
  s <- d %>% filter(sensations %in% top) %>% count(win, sensations) %>%
    left_join(denom, by = "win") %>% mutate(share = n / n_win) %>%
    select(win, sensations, share) %>% pivot_wider(names_from = win, values_from = share) %>%
    mutate(delta = `Past 12 months` - Before,
           sensations = forcats::fct_reorder(sensations, `Past 12 months`))
  st <- base %>% filter(consumed == 1, !is.na(sen_strength)) %>%
    mutate(win = ifelse(date_complete >= cut, "Past 12 months", "Before")) %>%
    count(win, sen_strength) %>% group_by(win) %>% mutate(share = n / sum(n)) %>% ungroup() %>%
    filter(sen_strength %in% c("stronger", "weaker")) %>%
    mutate(sensations = ifelse(sen_strength == "stronger", "Stronger than expected", "Weaker than expected")) %>%
    select(win, sensations, share) %>% pivot_wider(names_from = win, values_from = share) %>%
    mutate(delta = `Past 12 months` - Before)
  s <- bind_rows(st, s)  # strength rows on top (own denominator: samples with sen_strength)
  p <- ggplot(s, aes(y = sensations)) +
    geom_segment(aes(x = Before, xend = `Past 12 months`, yend = sensations), color = "#B8C4D4", linewidth = 1.6) +
    geom_point(aes(x = Before), size = 4, color = unc$trace) +
    geom_point(aes(x = `Past 12 months`), size = 4.6, color = unc$orange) +
    geom_text(aes(x = pmax(Before, `Past 12 months`) + 0.02,
                  label = sprintf("%+.0f pts", 100 * delta), color = delta >= 0),
              hjust = 0, size = 4.4, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c("TRUE" = "#C4453C", "FALSE" = "#1E8E5A")) +
    scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, .12))) +
    labs(title = paste0("Sensation reporting, ", ifelse(class == "stim", "stimulant", "opioid/benzo"), " samples: past 12 months (orange) vs before (light)"),
         x = "% of consumed samples reporting", y = NULL,
         caption = paste0("Consumed samples only; semicolon lists parsed, one count per sample. Windows split at ",
                          format(cut, "%d %b %Y"), "; current month partial.")) + theme_unc()
  ggsave_slide(sprintf("%s_sensations_shift_%s.png", ifelse(class == "stim", "13", "14"), class), p)
}

# 08 — county coverage: any completed analysis per county, all time + trailing 12 months
fig_county_coverage <- function() {
  shp <- tigris::counties(state = "NC", cb = TRUE, progress_bar = FALSE)
  draw <- function(window_months, suffix, label) {
    cc <- win_filter(card, window_months) %>% filter(!is.na(date_complete))
    cov <- cc %>% distinct(samplecounty) %>% pull(samplecounty)
    m <- shp %>% mutate(covered = NAME %in% cov)
    p <- ggplot(m) + geom_sf(aes(fill = covered), color = "white", linewidth = 0.3) +
      scale_fill_manual(values = c("TRUE" = unc$blue, "FALSE" = "#F0F3F7"), guide = "none") +
      labs(title = sprintf("%d of %d counties %s", sum(m$covered), nrow(m), label),
           caption = win_caption(window_months)) + theme_void() +
      theme(plot.title = element_text(color = unc$navy, face = "bold", size = 22))
    ggsave_slide(paste0("08_coverage_", suffix, ".png"), p)
  }
  draw(NULL, "alltime", "with a completed analysis, all time")
  # last 12 months: count choropleth (sqrt scale), not binary
  cc <- win_filter(card, 12) %>% filter(!is.na(date_complete)) %>%
    count(samplecounty, name = "n_samples")
  m <- shp %>% left_join(cc, by = c("NAME" = "samplecounty"))
  p <- ggplot(m) + geom_sf(aes(fill = n_samples), color = "white", linewidth = 0.3) +
    scale_fill_gradient(low = "#DCEAF5", high = unc$navy, trans = "sqrt",
                        na.value = "#F0F3F7", name = "Samples") +
    labs(title = sprintf("%s samples · %d of %d counties, last 12 months",
                         scales::comma(sum(cc$n_samples)), nrow(cc), nrow(m)),
         caption = win_caption(12)) + theme_void() +
    theme(plot.title = element_text(color = unc$navy, face = "bold", size = 22))
  ggsave_slide("08_counts_12mo.png", p)
}

run_all_figs <- function() {
  fig_county_coverage()
  fig_sensations_shift("stim"); fig_sensations_shift("opi")
  fig_sensations_substance()
  fig_region_maps("fentanyl", "lab_fentanyl_any")
  fig_region_maps("xylazine", "lab_xylazine_any")
  fig_region_maps("meth",     "lab_meth_any")
  fig_region_maps("cocaine",  "lab_cocaine_any")
  fig_trend_dashboard()
  fig_state_view("fentanyl", "lab_fentanyl_any",
    c("Cocaine" = "lab_cocaine_any", "Methamphetamine" = "lab_meth_any", "Xylazine" = "lab_xylazine_any"))
  fig_state_view("xylazine", "lab_xylazine_any",
    c("Fentanyl" = "lab_fentanyl_any", "Cocaine" = "lab_cocaine_any", "Methamphetamine" = "lab_meth_any"))
  fig_state_view("meth", "lab_meth_any",
    c("Fentanyl" = "lab_fentanyl_any", "Cocaine" = "lab_cocaine_any", "Xylazine" = "lab_xylazine_any"))
  fig_state_view("cocaine", "lab_cocaine_any",
    c("Fentanyl" = "lab_fentanyl_any", "Levamisole" = "lab_levamisole_any", "Methamphetamine" = "lab_meth_any"))
  fig_samples_quarterly(); fig_expected(); fig_expect_vs_lab(); fig_sensations()
  fig_overdose(); fig_color_texture(); fig_substances_per_sample(); fig_top_substances()
  fig_top_substances(window_months = 12); fig_substances_per_sample(window_months = 12)
  fig_drug_trend("17_fentanyl", "Fentanyl", "lab_fentanyl", "lab_fentanyl_any")
  fig_drug_trend_pattern("18_fent_impurities", "Fentanyl synthesis impurities (4-ANPP, phenethyl-4-ANPP)", "4-ANPP|4ANPP|phenethyl")
  fig_drug_trend("19_xylazine", "Xylazine", "lab_xylazine", "lab_xylazine_any")
  fig_drug_trend_pattern("20_medetomidine", "Medetomidine", "medetomidine")
  fig_drug_trend("21_meth", "Methamphetamine", "lab_meth", "lab_meth_any")
  fig_drug_trend("22_cocaine", "Cocaine", "lab_cocaine", "lab_cocaine_any")
  fig_drug_trend_pattern("23_levamisole", "Levamisole", "levamisole")
  fig_drug_trend("24_carfentanil", "Carfentanil", "lab_carfentanil", "lab_carfentanil_any")
  fig_drug_trend_pattern("25_nitazenes", "Nitazenes", "nitazene|etonitazene|metonitazene|protonitazene|isotonitazene")
  fig_drug_trend_pattern("26_btmps", "BTMPS", "BTMPS|bis\\(2,2,6,6")
  fig_opioid_stimulant()
}
