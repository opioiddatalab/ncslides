# new_detections.R — substances first seen EVER in NC within the last 3 months
source("r/theme_unc.R"); source("r/data.R")
library(dplyr); library(lubridate)

new_detections_nc <- function(window_days = 91) {
  nc <- load_nc(); lab <- nc$lab
  first_seen <- lab %>% filter(!is.na(date_complete)) %>%
    group_by(substance) %>%
    summarise(first_date = min(date_complete),
              first_sample = sampleid[which.min(date_complete)],
              abundance_first = abundance[which.min(date_complete)],
              n_detections = n(), .groups = "drop")
  first_seen %>% filter(first_date >= Sys.Date() - window_days) %>%
    arrange(desc(first_date)) %>%
    mutate(result_url = paste0("https://results.streetsafe.supply/sample/", first_sample))
}

write_new_detections <- function() {
  d <- new_detections_nc()
  dir.create("figs", showWarnings = FALSE)
  write.csv(d, "figs/29_new_detections.csv", row.names = FALSE)
  # Also emit a simple JSON for the HTML deck table
  jsonlite::write_json(d, "figs/29_new_detections.json", pretty = TRUE)
}
