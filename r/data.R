# data.R -- load the nightly NC extracts and derive periods, denominators, rates.
#
# Inputs (refreshed daily upstream):
#   https://data.streetsafe.supply/datasets/NC/analysis_dataset.csv  (wide, 1 row/sample)
#   https://data.streetsafe.supply/datasets/NC/lab_detail.csv        (long, 1 row/substance/sample)
# Codebook: github.com/opioiddatalab/drugchecking/blob/main/datasets/unc_druchecking_codebook.txt
# Schema:   github.com/opioiddatalab/drugchecking/blob/main/datasets/technical_details.md
#
# Convention: lab_X = PRIMARY substance; lab_X_any = primary OR trace;
# trace-only = lab_X_any & !lab_X.
#
# IMPORTANT -- the CSV is an export of a Stata dataset, and value-LABELLED
# variables arrive as label TEXT, not the numeric codes the codebook documents.
# `consumed` is "taken"/"unused", not 1/0; `sen_strength` is
# "weaker"/"normal"/"stronger", not -1/0/1. Comparing those to 1 matches
# nothing, yields an empty chart, and exits 0. Every such column goes through a
# normaliser below that accepts both forms.

library(dplyr); library(readr); library(lubridate); library(tidyr); library(zoo)

BASE_URL <- "https://data.streetsafe.supply/datasets/NC"
MIN_ROWS <- 500L   # a healthy NC extract is thousands of rows; below this, fail

# --------------------------------------------------------------- small helpers
`%||%` <- function(a, b) if (is.null(a)) b else a

# Warn-and-skip contract (HANDOFF task 2): a figure whose columns are missing
# is skipped with a warning; it never aborts the nightly run.
has_cols <- function(df, cols, what) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    warning(sprintf("skipping %s: missing column(s) %s",
                    what, paste(missing, collapse = ", ")), call. = FALSE)
    return(FALSE)
  }
  TRUE
}

# 1/0 from either a numeric code or a Stata value label.
as_flag <- function(x, true_labels = character()) {
  if (is.numeric(x)) return(as.integer(x == 1))
  s <- trimws(tolower(as.character(x)))
  as.integer(s %in% c("1", "yes", "true", tolower(true_labels)))
}

norm_consumed    <- function(x) as_flag(x, c("taken", "consumed"))
norm_sen_strength <- function(x) {
  s <- trimws(tolower(as.character(x)))
  # numeric -1/0/1 per the codebook, or the labels the CSV actually ships
  out <- dplyr::case_when(
    s %in% c("-1", "weaker")   ~ "weaker",
    s %in% c("0", "normal")    ~ "normal",
    s %in% c("1", "stronger")  ~ "stronger",
    TRUE                       ~ NA_character_)
  factor(out, levels = c("weaker", "normal", "stronger"))
}

# County names arrive with a " County" suffix -- except Buncombe and
# Northampton, which do not. The region table and the vendored polygons are
# both suffix-free, so every join goes through this.
norm_county <- function(x) {
  s <- trimws(as.character(x))
  s <- sub("\\s+County$", "", s)
  ifelse(s == "", NA_character_, s)
}

# Stata-style dates: 04aug2022.
parse_dc_date <- function(x) suppressWarnings(dmy(x))

# ------------------------------------------------------------------- the loader
fetch_csv <- function(name) {
  url <- file.path(BASE_URL, paste0(name, ".csv"))
  message("fetching ", url)
  # Everything as character, coerced explicitly below: readr's type guessing
  # would otherwise shift as the data grows (e.g. an all-blank column becoming
  # logical, then character), changing behaviour with no code change.
  df <- readr::read_csv(url, col_types = cols(.default = col_character()),
                        progress = FALSE)
  if (nrow(df) < MIN_ROWS)
    stop(sprintf("%s returned only %d rows (expected >= %d) -- refusing to build",
                 name, nrow(df), MIN_ROWS), call. = FALSE)
  df
}

load_nc <- function() {
  card <- fetch_csv("analysis_dataset")
  lab  <- fetch_csv("lab_detail")

  flag_cols <- grep("^lab_", names(card), value = TRUE)
  card <- card %>%
    mutate(
      date_complete = parse_dc_date(.data$date_complete),
      date_collect  = parse_dc_date(.data$date_collect),
      across(all_of(flag_cols), ~as.integer(.x == "1")),
      od_flag       = suppressWarnings(as.integer(.data$od)),
      consumed_flag = norm_consumed(.data$consumed),
      strength      = norm_sen_strength(.data$sen_strength),
      county_clean  = norm_county(.data$county),
      quarter       = as.yearqtr(.data$date_complete),
      month         = floor_date(.data$date_complete, "month"))

  lab_flag_cols <- grep("^lab_", names(lab), value = TRUE)
  lab <- lab %>%
    mutate(
      date_complete = parse_dc_date(.data$date_complete),
      across(all_of(lab_flag_cols), ~as.integer(.x == "1")),
      abundance = ifelse(is.na(.data$abundance) | trimws(.data$abundance) == "",
                         "primary", trimws(.data$abundance)),
      quarter   = as.yearqtr(.data$date_complete),
      substance = trimws(.data$substance))

  latest <- max(card$date_complete, na.rm = TRUE)
  first  <- min(card$date_complete, na.rm = TRUE)
  message(sprintf("loaded %d samples / %d lab rows; date_complete %s to %s",
                  nrow(card), nrow(lab), format(first), format(latest)))

  list(card = card, lab = lab, latest = latest, first = first,
       run_date = Sys.Date())
}

# ------------------------------------------------------- periods and partiality
# Everything is anchored to the DATA, not the run date. The extract lags the
# calendar (e.g. built 26 Aug with data through 11 Aug), so anchoring to
# Sys.Date() flags an empty period as "partial" and quietly shrinks rolling
# windows by the lag.
period_of <- function(dates, by) {
  if (by == "month") floor_date(dates, "month") else as.Date(as.yearqtr(dates))
}

period_end <- function(period_start, by) {
  if (by == "month") ceiling_date(period_start, "month") - 1
  else ceiling_date(period_start, "quarter") - 1
}

# A period is partial when the data stops inside it.
is_partial <- function(period_start, by, latest) {
  period_start == period_of(latest, by) & latest < period_end(period_start, by)
}

fmt_q <- function(x) format(as.yearqtr(x), "%YQ%q")

denoms <- function(card, by, latest) {
  card %>%
    filter(!is.na(.data$date_complete)) %>%
    mutate(period = period_of(.data$date_complete, by)) %>%
    group_by(.data$period) %>%
    summarise(n_samples = n_distinct(.data$sampleid), .groups = "drop") %>%
    mutate(partial = is_partial(.data$period, by, latest)) %>%
    arrange(.data$period)
}

quarter_denoms <- function(card, latest) denoms(card, "quarter", latest)
month_denoms   <- function(card, latest) denoms(card, "month",   latest)

# Monthly if volume supports it, quarterly otherwise.
trend_period <- function(card, latest, min_monthly_n = 30) {
  m <- month_denoms(card, latest) %>% filter(!.data$partial)
  if (nrow(m) >= 6 && median(m$n_samples) >= min_monthly_n) "month" else "quarter"
}

# ------------------------------------------------------------- substance flags
# The extract carries the lab's own derived flags, traceable to the chemical
# dictionary. Prefer them over regexes on substance names: the skeleton's BTMPS
# pattern ("BTMPS|bis\\(2,2,6,6") matches none of the 243 substance strings,
# while lab_btmps_any is populated -- an empty chart with a clean exit.
#
# Some flags ship WITHOUT a primary counterpart (levamisole, fentanyl
# impurities, pf fentanyl impurities). For those the primary side is derived
# from lab_detail, which repeats the same flag on each substance row and adds
# `abundance` -- so "primary" means the sample has a row with the flag set at
# primary abundance. No regex, same provenance.
LAB_FLAGS <- list(
  fentanyl        = list(any = "lab_fentanyl_any",            primary = "lab_fentanyl",       label = "Fentanyl"),
  fent_impurities = list(any = "lab_fentanyl_impurities_any", primary = NULL,                 label = "Fentanyl synthesis impurities"),
  xylazine        = list(any = "lab_xylazine_any",            primary = "lab_xylazine",       label = "Xylazine"),
  medetomidine    = list(any = "lab_medetomidine_any",        primary = "lab_medetomidine",   label = "Medetomidine"),
  meth            = list(any = "lab_meth_any",                primary = "lab_meth",           label = "Methamphetamine"),
  cocaine         = list(any = "lab_cocaine_any",             primary = "lab_cocaine",        label = "Cocaine"),
  levamisole      = list(any = "lab_levamisole_any",          primary = NULL,                 label = "Levamisole"),
  carfentanil     = list(any = "lab_carfentanil_any",         primary = "lab_carfentanil",    label = "Carfentanil"),
  nitazenes       = list(any = "lab_nitazene_any",            primary = "lab_nitazene",       label = "Nitazenes"),
  btmps           = list(any = "lab_btmps_any",               primary = "lab_btmps",          label = "BTMPS")
)

# Returns the card with `f_any` / `f_primary` columns for one metric, plus the
# provenance string every caption must carry.
flag_pair <- function(nc, metric) {
  spec <- LAB_FLAGS[[metric]]
  if (is.null(spec)) stop("unknown metric: ", metric)
  card <- nc$card
  if (!has_cols(card, spec$any, paste0(metric, " (any)"))) return(NULL)

  any_vec <- card[[spec$any]]
  if (!is.null(spec$primary) && spec$primary %in% names(card)) {
    prim_vec <- card[[spec$primary]]
    prov <- sprintf("%s (primary) and trace-only (%s minus %s)",
                    spec$primary, spec$any, spec$primary)
  } else {
    if (!has_cols(nc$lab, c(spec$any, "abundance", "sampleid"),
                  paste0(metric, " primary split"))) return(NULL)
    prim_ids <- nc$lab %>%
      filter(.data[[spec$any]] == 1, .data$abundance == "primary") %>%
      pull(.data$sampleid) %>% unique()
    prim_vec <- as.integer(card$sampleid %in% prim_ids)
    prov <- sprintf(paste("%s has no primary counterpart in analysis_dataset;",
                          "primary derived from lab_detail rows with %s = 1 at",
                          "primary abundance"), spec$any, spec$any)
  }
  card$f_any <- any_vec
  card$f_primary <- prim_vec
  list(card = card, label = spec$label, provenance = prov,
       any_col = spec$any, primary_col = spec$primary %||% "(derived)")
}

# Rate per 1,000 unique samples completed in each period, split primary vs
# trace-only, long format. `partial` marks the period the data stops inside.
period_rate <- function(card, by, latest) {
  d <- card %>%
    filter(!is.na(.data$date_complete)) %>%
    mutate(period = period_of(.data$date_complete, by)) %>%
    group_by(.data$period) %>%
    summarise(n_samples = n_distinct(.data$sampleid),
              primary   = sum(.data$f_primary == 1, na.rm = TRUE),
              any       = sum(.data$f_any == 1,     na.rm = TRUE),
              .groups   = "drop") %>%
    mutate(partial    = is_partial(.data$period, by, latest),
           trace_only = pmax(.data$any - .data$primary, 0),
           `Primary`    = 1000 * .data$primary    / .data$n_samples,
           `Trace only` = 1000 * .data$trace_only / .data$n_samples)
  d %>%
    select("period", "partial", "n_samples", "Primary", "Trace only") %>%
    pivot_longer(c("Primary", "Trace only"),
                 names_to = "abundance", values_to = "rate") %>%
    mutate(abundance = factor(.data$abundance, c("Primary", "Trace only")))
}

# Restrict to the trailing N months of date_complete, measured from the data's
# own end date. NULL = since inception.
win_filter <- function(df, window_months, latest, date_col = "date_complete") {
  if (is.null(window_months)) return(df)
  df %>% filter(.data[[date_col]] >= latest %m-% months(window_months))
}

incl_caption <- function(nc, window_months = NULL) {
  if (is.null(window_months))
    sprintf("Includes samples with date_complete %s–%s.",
            format(nc$first, "%d %b %Y"), format(nc$latest, "%d %b %Y"))
  else
    sprintf("Trailing %d months: date_complete %s–%s; final period partial.",
            window_months, format(nc$latest %m-% months(window_months), "%d %b %Y"),
            format(nc$latest, "%d %b %Y"))
}
