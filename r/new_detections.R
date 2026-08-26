# new_detections.R -- substances first seen EVER in NC inside the recent window.
#
# The window is measured from the DATA's last date, not the run date: the
# extract lags the calendar, so anchoring to Sys.Date() silently shortens the
# window by the lag (e.g. a 91-day window becomes 76 days when the data is 15
# days behind).

library(dplyr); library(lubridate)

# Rows that are not identified substances. "pending identification" is a real
# lab_detail value and would otherwise be reported as a brand-new NC detection
# every night until it resolves.
NON_SUBSTANCES <- c("pending identification", "none", "no substances detected",
                    "not identified", "unidentified", "")

SAMPLE_URL <- "https://www.streetsafe.supply/results/p/%s"

new_detections_nc <- function(nc, window_days = 91) {
  lab <- nc$lab %>%
    filter(!is.na(.data$date_complete),
           !tolower(trimws(.data$substance)) %in% NON_SUBSTANCES)
  if (!nrow(lab)) return(lab[0, ])
  first_seen <- lab %>%
    group_by(.data$substance) %>%
    summarise(first_date      = min(.data$date_complete),
              first_sample    = .data$sampleid[which.min(.data$date_complete)],
              abundance_first = .data$abundance[which.min(.data$date_complete)],
              n_detections    = dplyr::n(),
              .groups = "drop")
  first_seen %>%
    filter(.data$first_date >= nc$latest - window_days) %>%
    arrange(desc(.data$first_date)) %>%
    mutate(result_url = sprintf(SAMPLE_URL, .data$first_sample))
}

# The deck table plus its alt text; `table` matches the design's five columns.
new_detections_bundle <- function(nc, window_days = 91) {
  d <- new_detections_nc(nc, window_days)
  tbl <- d %>% transmute(
    Substance    = .data$substance,
    `First seen` = format(.data$first_date, "%d %b %Y"),
    Abundance    = .data$abundance_first,
    Detections   = .data$n_detections,
    Sample       = .data$first_sample)
  alt <- if (!nrow(d))
    sprintf(paste("Table, substances new to North Carolina in the %d days to %s:",
                  "none in this window."),
            window_days, format(nc$latest, "%d %b %Y"))
  else
    sprintf(paste("Table of %d substances whose first-ever North Carolina detection",
                  "falls in the %d days to %s. Most recent: %s."),
            nrow(d), window_days, format(nc$latest, "%d %b %Y"),
            paste(sprintf("%s on %s", head(d$substance, 3),
                          format(head(d$first_date, 3), "%d %b")), collapse = "; "))
  list(table = tbl, alt = alt, data = d)
}
