# data.R — load nightly NC extracts and derive quarterly frames
# Nightly inputs (refreshed daily upstream):
#   https://data.streetsafe.supply/datasets/NC/analysis_dataset.csv  (wide, 1 row/sample)
#   https://data.streetsafe.supply/datasets/NC/lab_detail.csv        (long, 1 row/substance/sample)
# Codebook: github.com/opioiddatalab/drugchecking/blob/main/datasets/unc_druchecking_codebook.txt
# Schema:   github.com/opioiddatalab/drugchecking/blob/main/datasets/technical_details.md
# Convention: lab_X = PRIMARY substance; lab_X_any = primary OR trace; trace-only = lab_X_any & !lab_X

library(dplyr); library(readr); library(lubridate); library(tidyr); library(zoo)

load_nc <- function() {
  card <- read_csv("https://data.streetsafe.supply/datasets/NC/analysis_dataset.csv",
                   guess_max = 100000, show_col_types = FALSE)
  lab  <- read_csv("https://data.streetsafe.supply/datasets/NC/lab_detail.csv",
                   guess_max = 100000, show_col_types = FALSE)
  # Stata-style dates like 04aug2022
  card <- card %>% mutate(
    date_complete = dmy(date_complete),
    date_collect  = dmy(date_collect),
    quarter = as.yearqtr(date_complete)
  )
  lab <- lab %>% mutate(date_complete = dmy(date_complete),
                        quarter = as.yearqtr(date_complete),
                        abundance = ifelse(is.na(abundance) | abundance == "", "primary", abundance))
  list(card = card, lab = lab,
       current_q = as.yearqtr(Sys.Date()),
       latest = max(card$date_complete, na.rm = TRUE))
}

# Denominator: unique samples COMPLETED in each calendar quarter
quarter_denoms <- function(card) {
  card %>% filter(!is.na(quarter)) %>%
    group_by(quarter) %>% summarise(n_samples = n_distinct(sampleid), .groups = "drop") %>%
    mutate(partial = quarter == as.yearqtr(Sys.Date()))
}

# Monthly denominators (calendar month of date_complete)
month_denoms <- function(card) {
  card %>% filter(!is.na(date_complete)) %>%
    mutate(month = floor_date(date_complete, "month")) %>%
    group_by(month) %>% summarise(n_samples = n_distinct(sampleid), .groups = "drop") %>%
    mutate(partial = month == floor_date(Sys.Date(), "month"))
}

MIN_MONTHLY_N <- 30  # median samples per COMPLETE month required to plot monthly

# Monthly if volume supports it, quarterly otherwise (slides 17-26 trend charts)
trend_period <- function(card) {
  m <- month_denoms(card) %>% filter(!partial)
  if (nrow(m) >= 6 && median(m$n_samples) >= MIN_MONTHLY_N) "month" else "quarter"
}

# Rate per 1,000 unique samples completed per period ("month"/"quarter"),
# split primary vs trace-only, long format. Current period flagged partial.
period_rate <- function(card, primary_var, any_var, by = trend_period(card)) {
  cc <- card %>% filter(!is.na(date_complete)) %>%
    mutate(period = if (by == "month") floor_date(date_complete, "month")
                    else as.Date(as.yearqtr(date_complete)))
  cur <- if (by == "month") floor_date(Sys.Date(), "month") else as.Date(as.yearqtr(Sys.Date()))
  cc %>% group_by(period) %>%
    summarise(n_samples = n_distinct(sampleid),
              primary = sum(.data[[primary_var]] == 1, na.rm = TRUE),
              any     = sum(.data[[any_var]]  == 1, na.rm = TRUE), .groups = "drop") %>%
    mutate(partial = period == cur, trace_only = any - primary,
           `Primary`    = 1000 * primary    / n_samples,
           `Trace only` = 1000 * trace_only / n_samples) %>%
    select(period, partial, `Primary`, `Trace only`) %>%
    pivot_longer(c(`Primary`, `Trace only`), names_to = "abundance", values_to = "rate")
}

# Rate per 1,000 unique samples completed that quarter, split primary vs trace-only.
# primary_var / any_var are the 1/0 columns from analysis_dataset (e.g. lab_fentanyl, lab_fentanyl_any).
quarterly_rate <- function(card, primary_var, any_var) {
  d <- quarter_denoms(card)
  card %>% filter(!is.na(quarter)) %>% group_by(quarter) %>%
    summarise(primary = sum(.data[[primary_var]] == 1, na.rm = TRUE),
              any     = sum(.data[[any_var]]  == 1, na.rm = TRUE), .groups = "drop") %>%
    mutate(trace_only = any - primary) %>%
    left_join(d, by = "quarter") %>%
    transmute(quarter, partial,
              `Primary`    = 1000 * primary   / n_samples,
              `Trace only` = 1000 * trace_only / n_samples) %>%
    pivot_longer(c(`Primary`, `Trace only`), names_to = "abundance", values_to = "rate")
}
