# figs.R -- one function per deck figure.
#
# Every fig_*() returns list(plot, alt, table, stats):
#   plot  -- the ggplot
#   alt   -- programmatic alt text, built by sprintf() over the frame that was
#            just plotted, naming chart type, variable, range and takeaways.
#            Never "chart of data" (HANDOFF task 6).
#   table -- the tidy frame behind the chart, for the Appendix slides (task 7).
#   stats -- named scalars/sentences the deck slides interpolate.
#
# Captions must name the exact variables used; see HANDOFF "Analytic contract".

library(forcats); library(stringr); library(scales); library(purrr)

# --------------------------------------------------------------- alt-text bits
rng_txt <- function(v, unit = "") {
  v <- v[is.finite(v)]
  if (!length(v)) return("no data")
  sprintf("range %s to %s%s", comma(round(min(v))), comma(round(max(v))), unit)
}

top_txt <- function(labels, values, n = 3) {
  o <- order(values, decreasing = TRUE)[seq_len(min(n, length(values)))]
  paste(sprintf("%s %s", labels[o], comma(round(values[o]))), collapse = "; ")
}

# Diagonal stripes for the in-progress period: a distinct texture, not a lower
# alpha, which reads as "faded" rather than "incomplete".
partial_fill <- function(partial, base = unc$blue)
  ifelse(partial, "partial", "complete")

scale_partial <- function(base = unc$blue)
  scale_fill_manual(values = c(complete = base, partial = unc$trace),
                    guide = "none")

annotate_partial <- function(p, d, x = "period", y) {
  pr <- d[d$partial, , drop = FALSE]
  if (!nrow(pr)) return(p)
  p + geom_text(data = pr, aes(x = .data[[x]], y = .data[[y]], label = PARTIAL_LABEL),
                vjust = -0.6, size = 4.4, color = unc$partial,
                fontface = "italic", inherit.aes = FALSE, show.legend = FALSE)
}

# ============================================================ coverage / volume

# 11 -- samples completed by quarter (the denominator for every later rate)
fig_samples_quarterly <- function(nc) {
  d <- quarter_denoms(nc$card, nc$latest)
  if (!nrow(d)) return(NULL)
  d$state <- partial_fill(d$partial)
  p <- ggplot(d, aes(factor(fmt_q(.data$period)), .data$n_samples, fill = .data$state)) +
    geom_col(color = unc$body, linewidth = 0.3) +
    geom_text(aes(label = comma(.data$n_samples)), vjust = -0.5,
              size = 4.6, color = unc$navy, fontface = "bold") +
    scale_partial() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12)), labels = comma) +
    labs(x = NULL, y = "Unique samples",
         caption = paste("Unique sampleid by calendar quarter of date_complete.",
                         incl_caption(nc))) +
    theme_unc()
  cmp <- d[!d$partial, , drop = FALSE]
  last_c <- tail(cmp, 1)
  alt <- sprintf(paste("Bar chart, unique samples completed per quarter, %s to %s.",
                       "%s; latest complete quarter %s: %s.%s"),
                 fmt_q(min(d$period)), fmt_q(max(d$period)), rng_txt(d$n_samples),
                 fmt_q(last_c$period), comma(last_c$n_samples),
                 if (any(d$partial)) sprintf(" %s partial: %s.",
                   fmt_q(d$period[d$partial][1]),
                   comma(d$n_samples[d$partial][1])) else "")
  list(plot = p, alt = alt,
       table = d %>% transmute(Quarter = fmt_q(.data$period),
                               `Unique samples` = comma(.data$n_samples),
                               Status = ifelse(.data$partial, "partial", "complete")),
       stats = setNames(list(
         comma(last_c$n_samples),
         sprintf("unique samples completed last quarter (%s)", fmt_q(last_c$period)),
         sprintf("These quarterly counts are the denominator for every per-1,000 rate that follows. Median complete quarter: %s samples.",
                 comma(round(median(cmp$n_samples))))),
         c("07_samples_quarterly", "07_samples_quarterly_label",
           "07_samples_quarterly_label2")))
}

# 12 -- county coverage: ever, and unique samples in the trailing 12 months
fig_county_coverage <- function(nc, counties) {
  if (!has_cols(nc$card, "county_clean", "county coverage")) return(NULL)
  # build_nightly.R passes NULL when r/nc_counties.rds is absent and promises
  # the maps are skipped -- so skip, rather than dying on counties$county.
  if (is.null(counties)) {
    warning("skipping county coverage: no counties table", call. = FALSE)
    return(NULL)
  }
  total <- length(unique(counties$county))

  draw <- function(window_months, slug) {
    cc <- win_filter(nc$card, window_months, nc$latest) %>%
      filter(!is.na(.data$date_complete), !is.na(.data$county_clean))
    per <- cc %>% count(county = .data$county_clean, name = "n_samples")
    m <- counties %>% left_join(per, by = "county") %>%
      mutate(n_samples = tidyr::replace_na(.data$n_samples, 0))
    covered <- length(unique(m$county[m$n_samples > 0]))
    if (is.null(window_months)) {
      p <- ggplot(m, aes(.data$long, .data$lat, group = .data$group,
                         fill = .data$n_samples > 0)) +
        geom_polygon(color = "white", linewidth = 0.3) +
        scale_fill_manual(values = c("TRUE" = unc$blue, "FALSE" = "#F0F3F7"),
                          labels = c("TRUE" = "≥1 completed analysis",
                                     "FALSE" = "No samples yet"), name = NULL) +
        coord_fixed(1.3) + theme_void(base_size = 16) +
        theme(legend.position = "bottom", legend.text = element_text(color = unc$body))
      alt <- sprintf(paste("Map of North Carolina counties shaded where at least one",
                           "sample has a completed analysis: %d of %d counties, all",
                           "time (through %s)."),
                     covered, total, format(nc$latest, "%d %b %Y"))
    } else {
      p <- ggplot(m, aes(.data$long, .data$lat, group = .data$group,
                         fill = .data$n_samples)) +
        geom_polygon(color = "white", linewidth = 0.3) +
        scale_fill_gradient(low = "#DCEAF5", high = unc$navy, trans = "sqrt",
                            name = "Samples", labels = comma) +
        coord_fixed(1.3) + theme_void(base_size = 16) +
        theme(legend.position = "bottom", legend.title = element_text(color = unc$body))
      top <- per %>% arrange(desc(.data$n_samples)) %>% head(3)
      alt <- sprintf(paste("Choropleth map of North Carolina, unique samples per county",
                           "in the trailing 12 months: %s samples across %d of %d",
                           "counties; darker is higher. Highest: %s."),
                     comma(sum(per$n_samples)), covered, total,
                     top_txt(top$county, top$n_samples))
    }
    list(slug = slug, plot = p, alt = alt, covered = covered,
         table = per %>% arrange(desc(.data$n_samples)) %>%
           transmute(County = .data$county, `Unique samples` = comma(.data$n_samples)))
  }

  a <- draw(NULL, "08_coverage_alltime")
  b <- draw(12,   "08_coverage_12mo")
  list(multi = list(a, b),
       stats = setNames(list(sprintf("%d of %d counties", a$covered, total),
                             sprintf("%d of %d counties", b$covered, total)),
                        c("08_coverage_alltime_n", "08_coverage_12mo_n")))
}

# ================================================================== the card

# 14 -- expected substances (verbatim card write-in, semicolon lists split)
fig_expected <- function(nc, top_n = 12) {
  if (!has_cols(nc$card, "expectedsubstance", "expected substances")) return(NULL)
  d <- nc$card %>%
    mutate(expectedsubstance = ifelse(is.na(.data$expectedsubstance) |
                                        .data$expectedsubstance == "",
                                      "unknown", .data$expectedsubstance)) %>%
    separate_rows("expectedsubstance", sep = ";") %>%
    mutate(expectedsubstance = str_trim(.data$expectedsubstance)) %>%
    filter(.data$expectedsubstance != "") %>%
    count(.data$expectedsubstance, sort = TRUE, name = "n")
  top <- head(d, top_n)
  p <- ggplot(top, aes(.data$n, fct_reorder(.data$expectedsubstance, .data$n))) +
    geom_col(fill = unc$blue) +
    geom_text(aes(label = comma(.data$n)), hjust = -0.15, size = 4.4, color = unc$navy) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12)), labels = comma) +
    labs(x = "Samples", y = NULL,
         caption = paste("expectedsubstance, semicolon lists split.", incl_caption(nc))) +
    theme_unc()
  list(plot = p,
       alt = sprintf(paste("Horizontal bar chart, what donors expected the sample to",
                           "be, top %d of %d distinct write-ins. %s. Most common: %s."),
                     nrow(top), nrow(d), rng_txt(top$n), top_txt(top$expectedsubstance, top$n)),
       table = d %>% transmute(`Expected substance` = .data$expectedsubstance,
                               Samples = comma(.data$n)),
       stats = list())
}

# 15 -- expectation vs lab result, for the classes with expect_* flags
fig_expect_vs_lab <- function(nc) {
  pairs <- list(
    list(exp = "expect_fentanyl", lab = "lab_fentanyl_any", label = "Expected fentanyl"),
    list(exp = "expect_meth",     lab = "lab_meth_any",     label = "Expected methamphetamine"),
    list(exp = "expect_cocaine",  lab = "lab_cocaine_any",  label = "Expected cocaine"),
    list(exp = "expect_benzo",    lab = "lab_benzodiazepine_any", label = "Expected benzodiazepine"),
    list(exp = "expect_xylazine", lab = "lab_xylazine_any", label = "Expected xylazine"))
  card <- nc$card
  rows <- purrr::map_dfr(pairs, function(pp) {
    if (!all(c(pp$exp, pp$lab) %in% names(card))) {
      warning("skipping ", pp$label, ": missing ", pp$exp, " or ", pp$lab, call. = FALSE)
      return(NULL)
    }
    sub <- card %>% filter(as.integer(.data[[pp$exp]]) == 1)
    if (!nrow(sub)) return(NULL)
    tibble::tibble(group = pp$label, n = nrow(sub),
                   confirmed = mean(sub[[pp$lab]] == 1, na.rm = TRUE),
                   lab_var = pp$lab)
  })
  if (!nrow(rows)) return(NULL)
  p <- ggplot(rows, aes(.data$confirmed, fct_reorder(.data$group, .data$confirmed))) +
    geom_col(fill = unc$blue) +
    geom_text(aes(label = percent(.data$confirmed, 1)), hjust = -0.15,
              size = 4.6, color = unc$navy, fontface = "bold") +
    scale_x_continuous(labels = percent, limits = c(0, 1),
                       expand = expansion(mult = c(0, 0.14))) +
    labs(x = "Confirmed in any abundance", y = NULL,
         caption = paste("Numerator lab_*_any (primary or trace) among samples with",
                         "expect_* = 1.", incl_caption(nc))) +
    theme_unc()
  list(plot = p,
       alt = sprintf(paste("Horizontal bar chart, share of each expected substance",
                           "confirmed by the lab in any abundance. %s."),
                     paste(sprintf("%s %s (n=%s)", rows$group, percent(rows$confirmed, 1),
                                   comma(rows$n)), collapse = "; ")),
       table = rows %>% transmute(Expectation = .data$group,
                                  Samples = comma(.data$n),
                                  `Confirmed (any abundance)` = percent(.data$confirmed, 1),
                                  Variable = .data$lab_var),
       stats = list())
}

# 16 -- sensations reported on the card
fig_sensations <- function(nc, top_n = 10) {
  if (!has_cols(nc$card, c("sensations", "consumed_flag"), "sensations")) return(NULL)
  base <- nc$card %>% filter(.data$consumed_flag == 1, !is.na(.data$sensations),
                             .data$sensations != "")
  if (!nrow(base)) {
    warning("skipping sensations: no consumed samples with sensations", call. = FALSE)
    return(NULL)
  }
  d <- base %>% separate_rows("sensations", sep = ";") %>%
    mutate(sensations = str_trim(.data$sensations)) %>%
    filter(.data$sensations != "") %>%
    distinct(.data$sampleid, .data$sensations) %>%
    count(.data$sensations, sort = TRUE, name = "n") %>%
    mutate(share = .data$n / nrow(base))
  top <- head(d, top_n)
  p <- ggplot(top, aes(.data$n, fct_reorder(.data$sensations, .data$n))) +
    geom_col(fill = unc$blue) +
    geom_text(aes(label = sprintf("%s (%s)", comma(.data$n), percent(.data$share, 1))),
              hjust = -0.1, size = 4.2, color = unc$navy) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
    labs(x = "Samples", y = NULL,
         caption = paste("sensations (circled on card), semicolon lists split, one count",
                         "per sample; consumed samples only (consumed = \"taken\").",
                         incl_caption(nc))) +
    theme_unc()
  list(plot = p,
       alt = sprintf(paste("Horizontal bar chart, sensations reported on the card among",
                           "%s consumed samples, top %d of %d. Most common: %s."),
                     comma(nrow(base)), nrow(top), nrow(d),
                     top_txt(top$sensations, top$n)),
       table = d %>% transmute(Sensation = .data$sensations, Samples = comma(.data$n),
                               `Share of consumed` = percent(.data$share, 1)),
       stats = setNames(list(
         sprintf("Reported on %s consumed samples (consumed = \"taken\"); %s distinct sensation terms.",
                 comma(nrow(base)), comma(nrow(d))),
         sprintf("Also derived: sen_strength (weaker / normal / stronger), sen_up, sen_down, sen_skin, sen_seizure, sen_burn. Top term: %s.",
                 top$sensations[1])),
         c("11_sensations_label", "11_sensations_label2")))
}

# 20 -- overdose involvement. od is missing for 30% of samples (the question
# arrived in card v2), so the denominator is samples where involvement was
# actually reported; including the silent ones would dilute the rate.
fig_overdose <- function(nc) {
  if (!has_cols(nc$card, c("od_flag", "date_complete"), "overdose")) return(NULL)
  base <- nc$card %>% filter(!is.na(.data$od_flag), !is.na(.data$date_complete))
  if (!nrow(base)) return(NULL)
  d <- base %>% mutate(period = period_of(.data$date_complete, "quarter")) %>%
    group_by(.data$period) %>%
    summarise(reported = n_distinct(.data$sampleid),
              involved = sum(.data$od_flag == 1, na.rm = TRUE), .groups = "drop") %>%
    mutate(partial = is_partial(.data$period, "quarter", nc$latest),
           rate = 1000 * .data$involved / .data$reported,
           state = partial_fill(.data$partial))
  p <- ggplot(d, aes(factor(fmt_q(.data$period)), .data$rate, fill = .data$state)) +
    geom_col(color = unc$body, linewidth = 0.3) +
    geom_text(aes(label = round(.data$rate)), vjust = -0.5, size = 4.4,
              color = unc$navy, fontface = "bold") +
    scale_fill_manual(values = c(complete = unc$orange, partial = "#E8C9A8"),
                      guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
    labs(x = NULL, y = "Per 1,000 reported",
         caption = paste("od = 1 per 1,000 samples whose overdose involvement was",
                         "REPORTED that quarter (od not missing); the question was",
                         "added in card v2, so silent samples are excluded from the",
                         "denominator rather than counted as not-involved.",
                         incl_caption(nc))) +
    theme_unc()
  cmp <- d[!d$partial, , drop = FALSE]; last_c <- tail(cmp, 1)
  excluded <- nrow(nc$card) - nrow(base)
  list(plot = p,
       alt = sprintf(paste("Bar chart, overdose-involved samples per 1,000 samples with",
                           "reported involvement, by quarter, %s to %s. %s; latest",
                           "complete quarter %s: %s per 1,000."),
                     fmt_q(min(d$period)), fmt_q(max(d$period)), rng_txt(d$rate),
                     fmt_q(last_c$period), round(last_c$rate)),
       table = d %>% transmute(Quarter = fmt_q(.data$period),
                               `Reported on` = comma(.data$reported),
                               `OD-involved` = comma(.data$involved),
                               `Per 1,000` = round(.data$rate),
                               Status = ifelse(.data$partial, "partial", "complete")),
       stats = setNames(list(
         round(last_c$rate),
         sprintf("overdose-involved samples per 1,000 with reported involvement, %s — %s samples (%s) did not report, and are excluded from the denominator",
                 fmt_q(last_c$period), comma(excluded),
                 percent(excluded / nrow(nc$card), 1))),
         c("12_overdose", "12_overdose_label")))
}

# 17 -- sensations by key substance: strength Likert + atypical-report share
fig_sensations_substance <- function(nc) {
  keys <- list(Fentanyl = "lab_fentanyl_any", Xylazine = "lab_xylazine_any",
               Methamphetamine = "lab_meth_any", Cocaine = "lab_cocaine_any")
  card <- nc$card
  if (!has_cols(card, c("consumed_flag", "strength"), "sensations by substance"))
    return(NULL)
  cc <- card %>% filter(.data$consumed_flag == 1)
  if (!nrow(cc)) {
    warning("skipping sensations by substance: no consumed samples", call. = FALSE)
    return(NULL)
  }
  cc <- cc %>% mutate(atypical = as.integer(str_detect(
    tolower(dplyr::coalesce(.data$sensations, "")),
    "unpleasant|weird smell|weird taste|bad taste|bad smell")))
  d <- purrr::imap_dfr(keys, function(v, nm) {
    if (!v %in% names(cc)) return(NULL)
    s <- cc %>% filter(.data[[v]] == 1)
    if (!nrow(s)) return(NULL)
    tibble::tibble(substance = nm, n = nrow(s),
                   weaker   = mean(s$strength == "weaker",   na.rm = TRUE),
                   normal   = mean(s$strength == "normal",   na.rm = TRUE),
                   stronger = mean(s$strength == "stronger", na.rm = TRUE),
                   atypical = mean(s$atypical == 1, na.rm = TRUE))
  })
  if (!nrow(d)) return(NULL)

  lik <- d %>% select("substance", "weaker", "normal", "stronger") %>%
    pivot_longer(-"substance", names_to = "strength", values_to = "share") %>%
    mutate(strength = factor(.data$strength, c("weaker", "normal", "stronger"),
                             c("Weaker", "Normal", "Stronger")))
  p1 <- ggplot(lik, aes(.data$share, .data$substance, fill = .data$strength)) +
    geom_col(color = "white", linewidth = 0.4) +
    geom_text(aes(label = ifelse(.data$share > 0.06, percent(.data$share, 1), "")),
              position = position_stack(vjust = 0.5), size = 4.2, color = unc$navy) +
    scale_fill_manual(values = c(Weaker = unc$trace, Normal = unc$rule,
                                 Stronger = unc$orange), name = NULL) +
    scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.02))) +
    labs(x = NULL, y = NULL,
         caption = paste("sen_strength among consumed samples with the substance in any",
                         "abundance. Segments labelled directly, so colour is not the",
                         "only cue.", incl_caption(nc))) +
    theme_unc()
  p2 <- ggplot(d, aes(.data$atypical, fct_reorder(.data$substance, .data$atypical))) +
    geom_col(fill = unc$orange) +
    geom_text(aes(label = percent(.data$atypical, 1)), hjust = -0.15,
              size = 4.6, color = unc$navy, fontface = "bold") +
    scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.16))) +
    labs(x = NULL, y = NULL,
         caption = "Any of: unpleasant, weird smell, weird taste (parsed from the semicolon sensation list).") +
    theme_unc()
  list(multi = list(
         list(slug = "12_sen_strength", plot = p1, kind = "half",
              alt = sprintf(paste("Stacked bar chart, reported strength versus expected",
                                  "for consumed samples by substance. %s."),
                            paste(sprintf("%s: %s stronger, %s normal, %s weaker (n=%s)",
                                          d$substance, percent(d$stronger, 1),
                                          percent(d$normal, 1), percent(d$weaker, 1),
                                          comma(d$n)), collapse = "; ")),
              table = d %>% transmute(Substance = .data$substance,
                                      `Consumed samples` = comma(.data$n),
                                      Weaker = percent(.data$weaker, 1),
                                      Normal = percent(.data$normal, 1),
                                      Stronger = percent(.data$stronger, 1))),
         list(slug = "12_sen_atypical", plot = p2, kind = "half",
              alt = sprintf(paste("Horizontal bar chart, share of consumed samples",
                                  "reporting an unpleasant or unusual smell or taste. %s."),
                            paste(sprintf("%s %s", d$substance, percent(d$atypical, 1)),
                                  collapse = "; ")),
              table = d %>% transmute(Substance = .data$substance,
                                      `Atypical report` = percent(.data$atypical, 1)))),
       stats = list())
}

# 21 -- colour and texture
fig_color_texture <- function(nc, top_n = 8) {
  if (!has_cols(nc$card, c("color", "texture"), "colour and texture")) return(NULL)
  grab <- function(col, lab) {
    nc$card %>%
      mutate(v = ifelse(is.na(.data[[col]]) | .data[[col]] == "", "unknown", .data[[col]])) %>%
      separate_rows("v", sep = ";") %>% mutate(v = str_trim(.data$v)) %>%
      filter(.data$v != "") %>% count(.data$v, sort = TRUE, name = "n") %>%
      head(top_n) %>% mutate(var = lab)
  }
  d <- bind_rows(grab("color", "Colour"), grab("texture", "Texture"))
  p <- ggplot(d, aes(.data$n, tidytext::reorder_within(.data$v, .data$n, .data$var))) +
    geom_col(fill = unc$blue) +
    tidytext::scale_y_reordered() +
    facet_wrap(~ .data$var, scales = "free_y") +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Samples", y = NULL,
         caption = paste("color and texture (card v3+), semicolon lists split.",
                         incl_caption(nc))) +
    theme_unc()
  list(plot = p,
       alt = sprintf(paste("Two-panel horizontal bar chart of reported colour and",
                           "texture, top %d each. Colours: %s. Textures: %s."),
                     top_n,
                     top_txt(d$v[d$var == "Colour"], d$n[d$var == "Colour"]),
                     top_txt(d$v[d$var == "Texture"], d$n[d$var == "Texture"])),
       table = d %>% transmute(Variable = .data$var, Value = .data$v,
                               Samples = comma(.data$n)),
       stats = list())
}

# 18 / 19 -- sensation reporting shift, trailing 12 months vs everything before
fig_sensations_shift <- function(nc, class = c("stim", "opi"), top_n = 8) {
  class <- match.arg(class)
  card <- nc$card
  cols <- if (class == "stim") c("lab_meth_any", "lab_cocaine_any")
          else c("lab_fentanyl_any", "lab_heroin_any", "lab_benzodiazepine_any")
  cols <- intersect(cols, names(card))
  if (!length(cols)) {
    warning("skipping sensation shift (", class, "): no class columns", call. = FALSE)
    return(NULL)
  }
  base <- card %>% filter(.data$consumed_flag == 1, !is.na(.data$sensations),
                          .data$sensations != "",
                          if_any(all_of(cols), ~ .x == 1))
  if (!nrow(base)) {
    warning("skipping sensation shift (", class, "): no matching consumed samples",
            call. = FALSE)
    return(NULL)
  }
  cut <- nc$latest %m-% months(12)
  base <- base %>% mutate(win = ifelse(.data$date_complete >= cut,
                                       "Past 12 months", "Before"))
  denom <- base %>% count(.data$win, name = "n_win")
  d <- base %>% separate_rows("sensations", sep = ";") %>%
    mutate(sensations = str_trim(.data$sensations)) %>%
    filter(.data$sensations != "") %>%
    distinct(.data$sampleid, .data$win, .data$sensations)
  top <- d %>% count(.data$sensations, sort = TRUE) %>% head(top_n) %>% pull("sensations")
  s <- d %>% filter(.data$sensations %in% top) %>%
    count(.data$win, .data$sensations) %>%
    left_join(denom, by = "win") %>%
    mutate(share = .data$n / .data$n_win) %>%
    select("win", "sensations", "share") %>%
    pivot_wider(names_from = "win", values_from = "share")
  if (!all(c("Before", "Past 12 months") %in% names(s))) {
    warning("skipping sensation shift (", class, "): only one time window present",
            call. = FALSE)
    return(NULL)
  }
  s <- s %>% mutate(across(c("Before", "Past 12 months"), ~tidyr::replace_na(.x, 0)),
                    delta = .data$`Past 12 months` - .data$Before,
                    sensations = fct_reorder(.data$sensations, .data$`Past 12 months`))
  p <- ggplot(s, aes(y = .data$sensations)) +
    geom_segment(aes(x = .data$Before, xend = .data$`Past 12 months`,
                     yend = .data$sensations), color = "#B8C4D4", linewidth = 1.6) +
    geom_point(aes(x = .data$Before), size = 4, color = unc$trace) +
    geom_point(aes(x = .data$`Past 12 months`), size = 4.6, color = unc$orange) +
    geom_text(aes(x = pmax(.data$Before, .data$`Past 12 months`) + 0.02,
                  label = sprintf("%+.0f pts", 100 * .data$delta)),
              hjust = 0, size = 4.2, color = unc$navy, fontface = "bold") +
    scale_x_continuous(labels = percent, expand = expansion(mult = c(0.02, 0.16))) +
    labs(x = "% of consumed samples reporting", y = NULL,
         caption = sprintf(paste("Light = before, orange = past 12 months. Consumed",
                                 "samples with %s detected in any abundance; windows",
                                 "split at %s. Variables: %s."),
                           if (class == "stim") "methamphetamine or cocaine"
                           else "an opioid or benzodiazepine",
                           format(cut, "%d %b %Y"), paste(cols, collapse = ", "))) +
    theme_unc()
  slug <- if (class == "stim") "13_sensations_shift_stim" else "14_sensations_shift_opi"
  moved <- s %>% arrange(desc(abs(.data$delta))) %>% head(3)
  list(slug = slug, plot = p,
       alt = sprintf(paste("Dumbbell chart, sensation reporting among %s consumed %s",
                           "samples: past 12 months versus before, %d terms. Largest",
                           "shifts: %s."),
                     comma(nrow(base)), if (class == "stim") "stimulant" else "opioid or benzodiazepine",
                     nrow(s),
                     paste(sprintf("%s %+.0f points", moved$sensations,
                                   100 * moved$delta), collapse = "; ")),
       table = s %>% transmute(Sensation = as.character(.data$sensations),
                               Before = percent(.data$Before, 1),
                               `Past 12 months` = percent(.data$`Past 12 months`, 1),
                               Change = sprintf("%+.0f pts", 100 * .data$delta)),
       stats = list())
}

# 23 / 26 -- substances detected per sample, both definitions
fig_substances_per_sample <- function(nc, window_months = NULL) {
  need <- c("lab_num_substances", "lab_num_substances_any")
  if (!has_cols(nc$card, need, "substances per sample")) return(NULL)
  cw <- win_filter(nc$card, window_months, nc$latest)
  d <- bind_rows(
    cw %>% count(n_sub = .data$lab_num_substances,     name = "n") %>% mutate(def = "Primary only"),
    cw %>% count(n_sub = .data$lab_num_substances_any, name = "n") %>% mutate(def = "Any abundance")) %>%
    filter(!is.na(.data$n_sub))
  med_p <- median(cw$lab_num_substances,     na.rm = TRUE)
  med_a <- median(cw$lab_num_substances_any, na.rm = TRUE)
  p <- ggplot(d, aes(.data$n_sub, .data$n, fill = .data$def)) +
    geom_col(position = "dodge", color = unc$body, linewidth = 0.25) +
    scale_fill_manual(values = c("Primary only" = unc$blue,
                                 "Any abundance" = unc$trace), name = NULL) +
    scale_x_continuous(breaks = scales::breaks_width(1)) +
    scale_y_continuous(labels = comma) +
    labs(x = "Substances in sample", y = "Samples",
         caption = paste("lab_num_substances vs lab_num_substances_any.",
                         incl_caption(nc, window_months))) +
    theme_unc()
  slug <- if (is.null(window_months)) "15_substances_per_sample"
          else "19_substances_per_sample_12mo"
  scope <- if (is.null(window_months)) "NC sample" else "sample, past 12 months"
  list(slug = slug, plot = p,
       alt = sprintf(paste("Grouped bar chart, distribution of substances detected per",
                           "sample under two definitions, %s. Median %s primary and %s",
                           "in any abundance; primary ranges %s to %s."),
                     if (is.null(window_months)) "all samples" else "trailing 12 months",
                     med_p, med_a,
                     min(cw$lab_num_substances, na.rm = TRUE),
                     max(cw$lab_num_substances, na.rm = TRUE)),
       table = d %>% arrange(.data$def, .data$n_sub) %>%
         transmute(Definition = .data$def, `Substances in sample` = .data$n_sub,
                   Samples = comma(.data$n)),
       stats = setNames(list(sprintf(paste0("Median %s: <strong style=\"color:%s\">%s primary</strong>",
                                            " substances, <strong style=\"color:%s\">%s in any abundance</strong>",
                                            " — trace detections carry most of the complexity."),
                                     scope, unc$orange, med_p, unc$orange, med_a)),
                        paste0(slug, "_label")))
}

# 24 / 25 -- top substances from the long lab_detail file
fig_top_substances <- function(nc, window_months = NULL, top_n = 15) {
  if (!has_cols(nc$lab, c("substance", "abundance"), "top substances")) return(NULL)
  d0 <- nc$lab %>% filter(!is.na(.data$date_complete), .data$substance != "",
                          .data$substance != "pending identification")
  d0 <- win_filter(d0, window_months, nc$latest)
  d <- d0 %>%
    mutate(abundance = ifelse(.data$abundance == "trace", "Trace only", "Primary")) %>%
    count(.data$substance, .data$abundance, name = "n") %>%
    group_by(.data$substance) %>% mutate(total = sum(.data$n)) %>% ungroup()
  keep <- d %>% distinct(.data$substance, .data$total) %>%
    arrange(desc(.data$total)) %>% head(top_n) %>% pull("substance")
  dd <- d %>% filter(.data$substance %in% keep) %>%
    mutate(abundance = factor(.data$abundance, c("Primary", "Trace only")))
  p <- ggplot(dd, aes(.data$n, fct_reorder(.data$substance, .data$total),
                      fill = .data$abundance)) +
    geom_col(color = unc$body, linewidth = 0.25) +
    geom_text(aes(label = ifelse(.data$n > max(dd$total) * 0.04, comma(.data$n), "")),
              position = position_stack(vjust = 0.5), size = 3.9, color = unc$navy) +
    scale_fill_primarytrace() +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.06))) +
    labs(x = "Detections", y = NULL,
         caption = paste("lab_detail.substance; solid = primary abundance, light =",
                         "trace, segments labelled directly.",
                         incl_caption(nc, window_months))) +
    theme_unc()
  slug <- if (is.null(window_months)) "16_top_substances" else "18_top_substances_12mo"
  tot <- dd %>% group_by(.data$substance) %>% summarise(t = sum(.data$n), .groups = "drop")
  list(slug = slug, plot = p,
       alt = sprintf(paste("Stacked horizontal bar chart, top %d substances detected%s,",
                           "split primary versus trace-only. Most detected: %s."),
                     length(keep),
                     if (is.null(window_months)) " across all NC samples" else " in the past 12 months",
                     top_txt(tot$substance, tot$t)),
       table = d %>% filter(.data$substance %in% keep) %>%
         select("substance", "abundance", "n") %>%
         pivot_wider(names_from = "abundance", values_from = "n", values_fill = 0) %>%
         mutate(total = .data$Primary + .data$`Trace only`) %>%
         arrange(desc(.data$total)) %>%
         transmute(Substance = .data$substance, Primary = comma(.data$Primary),
                   `Trace only` = comma(.data$`Trace only`),
                   Total = comma(.data$total)),
       stats = list())
}

# ==================================================== priority substance trends
# 27-36. Monthly when volume supports it, quarterly otherwise. The LOESS is fit
# on COMPLETE periods only so the in-progress period cannot drag the trend; the
# partial point is still plotted, starred, and named in the caption.
fig_drug_trend <- function(nc, slug, metric, by = NULL) {
  fp <- flag_pair(nc, metric)
  if (is.null(fp)) return(NULL)
  by <- by %||% trend_period(nc$card, nc$latest)
  d <- period_rate(fp$card, by, nc$latest)
  if (!nrow(d)) return(NULL)
  x_lab <- if (by == "month") scales::label_date("%b %Y") else function(x) fmt_q(x)
  complete <- d %>% filter(!.data$partial)
  p <- ggplot(d, aes(.data$period, .data$rate, color = .data$abundance,
                     group = .data$abundance)) +
    {if (nrow(complete) >= 5)
       geom_smooth(data = complete, method = "loess", formula = y ~ x, se = FALSE,
                   span = 0.75, linewidth = 1.4)
     else geom_line(data = complete, linewidth = 1.2)} +
    geom_point(size = 2.6) +
    geom_text(data = d %>% filter(.data$partial), aes(label = "*"), vjust = -0.35,
              size = 8, show.legend = FALSE) +
    scale_color_primarytrace() +
    scale_x_date(labels = x_lab) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Rate per 1,000",
         caption = sprintf(paste("Points: observed %sly rates; line: LOESS on complete",
                                 "%ss only. * current %s is partial (data through %s).",
                                 "Series: %s."),
                           by, by, by, format(nc$latest, "%d %b %Y"), fp$provenance)) +
    theme_unc()

  prim <- d %>% filter(.data$abundance == "Primary", !.data$partial)
  last_p <- tail(prim, 1)
  anyv <- fp$card %>% filter(!is.na(.data$date_complete)) %>%
    mutate(period = period_of(.data$date_complete, by)) %>%
    filter(.data$period == last_p$period) %>%
    summarise(r = 1000 * sum(.data$f_any == 1, na.rm = TRUE) / n_distinct(.data$sampleid)) %>%
    pull("r")
  plab <- if (by == "month") format(last_p$period, "%b %Y") else fmt_q(last_p$period)
  dir <- if (nrow(prim) >= 2) {
    d2 <- last_p$rate - prim$rate[nrow(prim) - 1]
    if (d2 > 0) "rising" else if (d2 < 0) "falling" else "flat"
  } else "insufficient history"
  list(plot = p,
       alt = sprintf(paste("Line chart with points, %s per 1,000 samples completed per",
                           "%s, %s to %s, split primary and trace-only. Primary %s;",
                           "latest complete %s %s: %s per 1,000 primary, %s any",
                           "abundance. Direction: %s."),
                     fp$label, by, plab, format(max(d$period), if (by == "month") "%b %Y" else "%Y"),
                     rng_txt(prim$rate), by, plab, round(last_p$rate), round(anyv), dir),
       table = d %>% transmute(Period = if (by == "month") format(.data$period, "%b %Y")
                                        else fmt_q(.data$period),
                               Abundance = as.character(.data$abundance),
                               `Per 1,000` = round(.data$rate, 1),
                               Samples = comma(.data$n_samples),
                               Status = ifelse(.data$partial, "partial", "complete")),
       stats = setNames(list(
         round(last_p$rate),
         sprintf(paste0("per 1,000 samples had %s as a <strong>primary</strong> substance,",
                        " %s — <strong style=\"color:%s\">%s</strong> in any abundance"),
                 tolower(fp$label), plab, unc$navy, round(anyv)),
         sprintf("Primary column: %s. Any-abundance column: %s. Trace-only is the difference.",
                 fp$primary_col, fp$any_col)),
         c(slug, paste0(slug, "_label"), paste0(slug, "_label2"))))
}

# 38 -- fentanyl contamination of stimulant-primary samples
fig_opioid_stimulant <- function(nc) {
  need <- c("lab_meth", "lab_cocaine", "lab_fentanyl_any")
  if (!has_cols(nc$card, need, "opioid-stimulant overlap")) return(NULL)
  build <- function(col, label) {
    nc$card %>% filter(.data[[col]] == 1, !is.na(.data$quarter)) %>%
      mutate(period = period_of(.data$date_complete, "quarter")) %>%
      group_by(.data$period) %>%
      summarise(n = n_distinct(.data$sampleid),
                rate = 1000 * sum(.data$lab_fentanyl_any == 1, na.rm = TRUE) /
                       n_distinct(.data$sampleid), .groups = "drop") %>%
      mutate(group = label)
  }
  d <- bind_rows(build("lab_meth", "Fentanyl (any) in meth-primary samples"),
                 build("lab_cocaine", "Fentanyl (any) in cocaine-primary samples")) %>%
    mutate(partial = is_partial(.data$period, "quarter", nc$latest))
  if (!nrow(d)) return(NULL)
  p <- ggplot(d, aes(.data$period, .data$rate, color = .data$group, group = .data$group)) +
    geom_line(data = d %>% filter(!.data$partial), linewidth = 1.4) +
    geom_point(size = 2.6) +
    geom_text(data = d %>% filter(.data$partial), aes(label = "*"), vjust = -0.35,
              size = 8, show.legend = FALSE) +
    scale_color_manual(values = c(unc$orange, unc$blue), name = NULL) +
    scale_x_date(labels = function(x) fmt_q(x)) +
    labs(x = NULL, y = "Rate per 1,000",
         caption = sprintf(paste("Denominator: samples with that stimulant as PRIMARY",
                                 "(lab_meth / lab_cocaine); numerator lab_fentanyl_any.",
                                 "* partial quarter (data through %s)."),
                           format(nc$latest, "%d %b %Y"))) +
    theme_unc()
  cmp <- d %>% filter(!.data$partial)
  last_q <- cmp %>% filter(.data$period == max(.data$period))
  list(plot = p,
       alt = sprintf(paste("Line chart, fentanyl detected in any abundance per 1,000",
                           "stimulant-primary samples, by quarter. Latest complete",
                           "quarter %s: %s."),
                     fmt_q(max(cmp$period)),
                     paste(sprintf("%s %s per 1,000", last_q$group, round(last_q$rate)),
                           collapse = "; ")),
       table = d %>% transmute(Quarter = fmt_q(.data$period), Group = .data$group,
                               `Stimulant-primary samples` = comma(.data$n),
                               `Fentanyl per 1,000` = round(.data$rate),
                               Status = ifelse(.data$partial, "partial", "complete")),
       stats = list())
}

# ================================================== 37: twelve-tile KPI strip
# The design's dashboard slide is twelve metric tiles, each with a value, a
# direction badge and a sparkline -- not one faceted image. Direction colour
# means FAVOURABLE, not merely up: rising sample volume is good, a rising
# xylazine rate is not.
DASH_METRICS <- list(
  list(key = "samples",         label = "Samples completed",        col = NULL,                          up_good = TRUE),
  list(key = "fentanyl",        label = "Fentanyl (any)",           col = "lab_fentanyl_any",            up_good = FALSE),
  list(key = "fent_impurities", label = "Fentanyl impurities (any)", col = "lab_fentanyl_impurities_any", up_good = FALSE),
  list(key = "xylazine",        label = "Xylazine (any)",           col = "lab_xylazine_any",            up_good = FALSE),
  list(key = "medetomidine",    label = "Medetomidine (any)",       col = "lab_medetomidine_any",        up_good = FALSE),
  list(key = "meth",            label = "Methamphetamine (any)",    col = "lab_meth_any",                up_good = FALSE),
  list(key = "cocaine",         label = "Cocaine (any)",            col = "lab_cocaine_any",             up_good = FALSE),
  list(key = "levamisole",      label = "Levamisole (any)",         col = "lab_levamisole_any",          up_good = FALSE),
  list(key = "carfentanil",     label = "Carfentanil (any)",        col = "lab_carfentanil_any",         up_good = FALSE),
  list(key = "nitazenes",       label = "Nitazenes (any)",          col = "lab_nitazene_any",            up_good = FALSE),
  list(key = "btmps",           label = "BTMPS (any)",              col = "lab_btmps_any",               up_good = FALSE),
  list(key = "overdose",        label = "Overdose-involved",        col = "od_flag",                     up_good = FALSE)
)

dash_series <- function(nc, m, months_back = 24) {
  card <- nc$card %>% filter(!is.na(.data$date_complete))
  start <- period_of(nc$latest, "month") %m-% months(months_back - 1)
  if (is.null(m$col)) {
    d <- card %>% mutate(period = period_of(.data$date_complete, "month")) %>%
      group_by(.data$period) %>%
      summarise(value = n_distinct(.data$sampleid), .groups = "drop")
  } else if (!m$col %in% names(card)) {
    warning("skipping KPI ", m$key, ": missing ", m$col, call. = FALSE); return(NULL)
  } else if (m$col == "od_flag") {
    d <- card %>% filter(!is.na(.data$od_flag)) %>%
      mutate(period = period_of(.data$date_complete, "month")) %>%
      group_by(.data$period) %>%
      summarise(value = 1000 * sum(.data$od_flag == 1, na.rm = TRUE) /
                        n_distinct(.data$sampleid), .groups = "drop")
  } else {
    d <- card %>% mutate(period = period_of(.data$date_complete, "month")) %>%
      group_by(.data$period) %>%
      summarise(value = 1000 * sum(.data[[m$col]] == 1, na.rm = TRUE) /
                        n_distinct(.data$sampleid), .groups = "drop")
  }
  d %>% filter(.data$period >= start) %>%
    mutate(partial = is_partial(.data$period, "month", nc$latest)) %>%
    arrange(.data$period)
}

fig_dashboard <- function(nc) {
  out <- list(); stats <- list(); tbl <- list()
  for (m in DASH_METRICS) {
    d <- dash_series(nc, m)
    if (is.null(d) || nrow(d) < 3) next
    cmp <- d %>% filter(!.data$partial)
    if (nrow(cmp) < 2) next
    last_v <- cmp$value[nrow(cmp)]
    prev_v <- cmp$value[nrow(cmp) - 1]
    delta <- if (is.finite(prev_v) && prev_v != 0) 100 * (last_v - prev_v) / prev_v else NA_real_
    favorable <- if (!is.finite(delta)) TRUE else if (m$up_good) delta >= 0 else delta < 0
    p <- ggplot(d, aes(.data$period, .data$value)) +
      geom_line(data = cmp, color = unc$blue, linewidth = 0.9) +
      geom_point(data = d %>% filter(.data$partial), color = unc$orange, size = 1.6) +
      theme_void() + theme(plot.margin = margin(2, 2, 2, 2))
    out[[length(out) + 1]] <- list(
      slug = paste0("dash_", m$key), plot = p, kind = "spark",
      alt = sprintf(paste("Sparkline, %s by month over the trailing 24 months, %s to %s.",
                          "%s. Last complete month %s: %s, %+.0f%% versus the prior month."),
                    m$label, format(min(d$period), "%b %Y"), format(max(d$period), "%b %Y"),
                    rng_txt(d$value), format(cmp$period[nrow(cmp)], "%b %Y"),
                    comma(round(last_v)), delta),
      table = NULL)
    stats[[paste0("dash_", m$key, "_val")]] <- comma(round(last_v))
    stats[[paste0("dash_", m$key, "_badge")]] <- badge_html(delta, favorable)
    tbl[[length(tbl) + 1]] <- tibble::tibble(
      Metric = m$label,
      `Per 1,000 samples` = if (is.null(m$col)) paste0(comma(round(last_v)), " / month")
                            else comma(round(last_v)),
      Change = if (is.finite(delta)) sprintf("%+.0f%%", delta) else "n/a")
  }
  rates <- bind_rows(tbl)
  half <- ceiling(nrow(rates) / 2)
  list(multi = out, stats = stats,
       tables = list(a4  = rates[seq_len(half), ],
                     a4b = rates[seq(half + 1, nrow(rates)), ]))
}

# ============================================ 40-43: per-substance state view
fig_state_view <- function(nc, key, metric) {
  fp <- flag_pair(nc, metric)
  if (is.null(fp)) return(NULL)
  pos <- fp$card %>% filter(!is.na(.data$date_complete), .data$f_any == 1)
  if (!nrow(pos)) return(NULL)
  this_yr <- year(nc$latest)
  ytd_cut <- yday(nc$latest)

  # (1) year over year: full-year totals with a YTD overlay
  d2 <- pos %>% mutate(yr = year(.data$date_complete)) %>%
    group_by(.data$yr) %>%
    summarise(full = n_distinct(.data$sampleid),
              ytd = n_distinct(.data$sampleid[yday(.data$date_complete) <= ytd_cut]),
              .groups = "drop") %>%
    arrange(.data$yr) %>%
    mutate(delta = 100 * (.data$ytd - lag(.data$ytd)) / lag(.data$ytd))
  p2 <- ggplot(d2, aes(factor(.data$yr))) +
    geom_col(aes(y = .data$full), fill = unc$trace) +
    geom_col(aes(y = .data$ytd), fill = unc$navy, width = 0.55) +
    geom_text(aes(y = .data$full, label = comma(.data$full)), vjust = -0.5,
              size = 4.2, color = unc$navy, fontface = "bold") +
    geom_text(aes(y = .data$ytd, label = ifelse(is.na(.data$delta), "",
                                                sprintf("%+.0f%%", .data$delta))),
              vjust = 1.4, size = 4, color = "white", fontface = "bold") +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
    labs(x = NULL, y = "Positive samples",
         caption = sprintf(paste("Light = full year; dark = year to date through the same",
                                 "day-of-year as the data (%s). %% = YTD vs prior-year YTD.",
                                 "Positive = %s."),
                           format(nc$latest, "%d %b"), fp$any_col)) +
    theme_unc()

  # (2) month to month, current year against last
  d1 <- pos %>% filter(year(.data$date_complete) >= this_yr - 1) %>%
    mutate(yr = factor(year(.data$date_complete)), m = month(.data$date_complete)) %>%
    count(.data$yr, .data$m, name = "n")
  p1 <- ggplot(d1, aes(.data$m, .data$n, color = .data$yr, group = .data$yr)) +
    geom_line(linewidth = 1.3) + geom_point(size = 2.2) +
    scale_color_manual(values = setNames(c(unc$trace, unc$navy),
                                         c(this_yr - 1, this_yr)), name = NULL) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Positive samples",
         caption = sprintf("Counts, not rates. Final month partial (data through %s).",
                           format(nc$latest, "%d %b %Y"))) +
    theme_unc()

  cur <- d2 %>% filter(.data$yr == this_yr)
  list(multi = list(
         list(slug = paste0("state_", key, "_years"), plot = p2, kind = "half",
              alt = sprintf(paste("Bar chart, %s-positive samples per year with a",
                                  "year-to-date overlay, %d to %d. %s YTD: %s, %s versus",
                                  "the prior year to the same date."),
                            tolower(fp$label), min(d2$yr), max(d2$yr), this_yr,
                            comma(cur$ytd),
                            if (nrow(cur) && is.finite(cur$delta)) sprintf("%+.0f%%", cur$delta) else "no comparison"),
              table = d2 %>% transmute(Year = .data$yr, `Full year` = comma(.data$full),
                                       YTD = comma(.data$ytd),
                                       `YTD change` = ifelse(is.na(.data$delta), "",
                                                             sprintf("%+.0f%%", .data$delta)))),
         list(slug = paste0("state_", key, "_line"), plot = p1, kind = "half",
              alt = sprintf(paste("Line chart, %s-positive samples by month, %d versus",
                                  "%d. %s."),
                            tolower(fp$label), this_yr - 1, this_yr, rng_txt(d1$n)),
              table = NULL)),
       stats = list())
}

# ================================================= 45-48: three-region maps
# Counties are pre-projected and vendored by r/prep_counties.R, so the nightly
# run needs neither sf/tigris nor the Census API.
fig_region_maps <- function(nc, key, metric, counties) {
  fp <- flag_pair(nc, metric)
  if (is.null(fp)) return(NULL)
  if (is.null(counties)) {
    warning("skipping maps: no counties table", call. = FALSE)
    return(NULL)
  }
  if (!"region" %in% names(counties)) {
    warning("skipping maps: counties table has no region column", call. = FALSE)
    return(NULL)
  }
  qs <- counties  # keep name short below
  card <- fp$card %>% filter(!is.na(.data$date_complete), !is.na(.data$county_clean))
  reg <- qs %>% distinct(.data$county, .data$region)

  # last COMPLETE quarter, derived from dates rather than from whatever
  # quarters happen to appear in the data
  cur_q <- period_of(nc$latest, "quarter")
  last_complete <- if (is_partial(cur_q, "quarter", nc$latest))
    cur_q %m-% months(3) else cur_q
  prev_q <- last_complete %m-% months(3)

  d <- card %>% mutate(period = period_of(.data$date_complete, "quarter")) %>%
    filter(.data$period %in% c(prev_q, last_complete)) %>%
    inner_join(reg, by = c("county_clean" = "county")) %>%
    group_by(.data$region, .data$period) %>%
    summarise(n = n_distinct(.data$sampleid),
              rate = 1000 * sum(.data$f_any == 1, na.rm = TRUE) / n_distinct(.data$sampleid),
              .groups = "drop")
  if (!nrow(d)) return(NULL)
  wide <- d %>%
    mutate(slot = ifelse(.data$period == last_complete, "cur", "prev")) %>%
    select("region", "slot", "rate") %>%
    pivot_wider(names_from = "slot", values_from = "rate")
  for (col in c("cur", "prev")) if (!col %in% names(wide)) wide[[col]] <- NA_real_
  wide <- wide %>% mutate(delta = 100 * (.data$cur - .data$prev) / .data$prev)

  # explode the three regions apart for readability
  shift <- c(western = -0.9, central = 0, eastern = 0.9)
  m <- qs %>% mutate(long = .data$long + unname(shift[.data$region])) %>%
    left_join(wide, by = "region")
  centers <- m %>% group_by(.data$region) %>%
    summarise(x = mean(range(.data$long)), y = min(.data$lat), .groups = "drop") %>%
    left_join(wide, by = "region")

  base_map <- function(col) {
    ggplot(m, aes(.data$long, .data$lat, group = .data$group)) +
      geom_polygon(aes(fill = .data[[col]]), color = "white", linewidth = 0.25) +
      coord_fixed(1.3) + theme_void(base_size = 15)
  }
  p1 <- base_map("cur") +
    scale_fill_gradient(low = unc$trace, high = unc$navy, guide = "none",
                        na.value = "#F0F3F7") +
    geom_text(data = centers, aes(.data$x, .data$y, group = NULL,
                                  label = sprintf("%s\n%s per 1,000", toupper(.data$region),
                                                  round(.data$cur))),
              vjust = 1.1, size = 4.4, color = unc$navy, fontface = "bold")
  p2 <- base_map("delta") +
    scale_fill_gradient2(low = unc$good, mid = "#F5F7FA", high = unc$bad,
                         midpoint = 0, guide = "none", na.value = "#F0F3F7") +
    geom_text(data = centers, aes(.data$x, .data$y, group = NULL,
                                  label = sprintf("%s\n%+.0f%%", toupper(.data$region),
                                                  .data$delta)),
              vjust = 1.1, size = 4.6, color = unc$navy, fontface = "bold")
  desc <- paste(sprintf("%s %s per 1,000 (%+.0f%%)", wide$region, round(wide$cur),
                        wide$delta), collapse = "; ")
  list(multi = list(
         list(slug = paste0("maps_", key, "_prev"), plot = p1, kind = "map",
              alt = sprintf(paste("Map of North Carolina in three exploded regions, %s",
                                  "per 1,000 samples completed in %s; darker is higher.",
                                  "%s."),
                            tolower(fp$label), fmt_q(last_complete), desc),
              table = wide %>% transmute(Region = .data$region,
                                         `Per 1,000` = round(.data$cur),
                                         `Prior quarter` = round(.data$prev),
                                         Change = sprintf("%+.0f%%", .data$delta))),
         list(slug = paste0("maps_", key, "_chg"), plot = p2, kind = "map",
              alt = sprintf(paste("Map of North Carolina in three exploded regions,",
                                  "percent change in the %s rate, %s versus %s; red is",
                                  "an increase, green a decrease. %s."),
                            tolower(fp$label), fmt_q(last_complete), fmt_q(prev_q), desc),
              table = NULL)),
       stats = list())
}

# =================================================================== the runner
# Collects every figure, writes SVG + PNG, and gathers the alt text, appendix
# tables and slide stats. A figure that returns NULL (missing columns, no rows)
# is recorded as skipped and the run continues -- HANDOFF task 2.
run_all_figs <- function(nc, counties) {
  acc <- list(alt = list(), tables = list(), stats = list(),
              made = character(), skipped = character())

  emit <- function(acc, slug, item, kind = "slide") {
    if (is.null(item) || is.null(item$plot)) {
      acc$skipped <- c(acc$skipped, slug); return(acc)
    }
    ggsave_fig(slug, item$plot, kind = item$kind %||% kind)
    acc$alt[[slug]] <- item$alt
    if (!is.null(item$table)) acc$tables[[slug]] <- item$table
    acc$made <- c(acc$made, slug)
    acc
  }

  # one figure, slug supplied by the caller
  one <- function(acc, slug, item, kind = "slide") {
    if (is.null(item)) { acc$skipped <- c(acc$skipped, slug); return(acc) }
    acc$stats <- utils::modifyList(acc$stats, item$stats %||% list())
    emit(acc, item$slug %||% slug, item, kind)
  }

  # several figures from one call (item$multi)
  many <- function(acc, slugs, item, kind = "slide") {
    if (is.null(item)) { acc$skipped <- c(acc$skipped, slugs); return(acc) }
    acc$stats <- utils::modifyList(acc$stats, item$stats %||% list())
    for (nm in names(item$tables %||% list()))
      acc$tables[[nm]] <- item$tables[[nm]]
    for (sub in item$multi %||% list())
      acc <- emit(acc, sub$slug, sub, kind)
    acc
  }

  message("-- coverage and volume")
  acc <- one(acc, "07_samples_quarterly", fig_samples_quarterly(nc))
  acc <- many(acc, c("08_coverage_alltime", "08_coverage_12mo"),
              fig_county_coverage(nc, counties), kind = "half")

  message("-- the card")
  acc <- one(acc, "09_expected_substances", fig_expected(nc))
  acc <- one(acc, "10_expect_vs_lab",       fig_expect_vs_lab(nc))
  acc <- one(acc, "11_sensations",          fig_sensations(nc))
  acc <- many(acc, c("12_sen_strength", "12_sen_atypical"),
              fig_sensations_substance(nc), kind = "half")
  acc <- one(acc, "12_overdose",            fig_overdose(nc))
  acc <- one(acc, "13_color_texture",       fig_color_texture(nc))
  acc <- one(acc, "13_sensations_shift_stim", fig_sensations_shift(nc, "stim"))
  acc <- one(acc, "14_sensations_shift_opi",  fig_sensations_shift(nc, "opi"))

  message("-- the lab")
  acc <- one(acc, "15_substances_per_sample",      fig_substances_per_sample(nc))
  acc <- one(acc, "19_substances_per_sample_12mo", fig_substances_per_sample(nc, 12))
  acc <- one(acc, "16_top_substances",             fig_top_substances(nc))
  acc <- one(acc, "18_top_substances_12mo",        fig_top_substances(nc, 12))

  message("-- substance trends")
  trends <- list(
    c("17_fentanyl_trend",        "fentanyl"),
    c("18_fent_impurities_trend", "fent_impurities"),
    c("19_xylazine_trend",        "xylazine"),
    c("20_medetomidine_trend",    "medetomidine"),
    c("21_meth_trend",            "meth"),
    c("22_cocaine_trend",         "cocaine"),
    c("23_levamisole_trend",      "levamisole"),
    c("24_carfentanil_trend",     "carfentanil"),
    c("25_nitazenes_trend",       "nitazenes"),
    c("26_btmps_trend",           "btmps"))
  for (t in trends) acc <- one(acc, t[1], fig_drug_trend(nc, t[1], t[2]))
  acc <- one(acc, "28_opioid_stimulant", fig_opioid_stimulant(nc))

  message("-- KPI tiles")
  acc <- many(acc, paste0("dash_", vapply(DASH_METRICS, function(m) m$key, "")),
              fig_dashboard(nc), kind = "spark")

  message("-- state views and maps")
  for (s in list(c("fentanyl", "fentanyl"), c("xylazine", "xylazine"),
                 c("meth", "meth"), c("cocaine", "cocaine"))) {
    acc <- many(acc, paste0("state_", s[1], c("_years", "_line")),
                fig_state_view(nc, s[1], s[2]), kind = "half")
    acc <- many(acc, paste0("maps_", s[1], c("_prev", "_chg")),
                fig_region_maps(nc, s[1], s[2], counties), kind = "map")
  }

  message(sprintf("figures: %d made, %d skipped", length(acc$made), length(acc$skipped)))
  if (length(acc$skipped))
    warning("skipped figures: ", paste(unique(acc$skipped), collapse = ", "), call. = FALSE)
  acc
}
