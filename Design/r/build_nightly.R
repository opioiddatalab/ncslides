# build_nightly.R — cron entrypoint: regenerate every figure + stats for the deck
# Suggested crontab (2:30am ET, after upstream daily refresh):
#   30 2 * * * cd /path/to/deck && Rscript r/build_nightly.R >> logs/nightly.log 2>&1
source("r/figs.R"); source("r/new_detections.R")
library(jsonlite)

run_all_figs()
write_new_detections()

# Headline stats consumed by the deck (slide 6 and title slide)
nc <- load_nc(); card <- nc$card
stats <- list(
  updated      = format(Sys.time(), "%A, %B %d, %Y"),
  n_samples    = dplyr::n_distinct(card$sampleid),
  n_programs   = dplyr::n_distinct(card$program),
  n_counties   = dplyr::n_distinct(card$county[card$county != ""]),
  first_date   = format(min(card$date_complete, na.rm = TRUE), "%B %Y"),
  latest_date  = format(max(card$date_complete, na.rm = TRUE), "%B %d, %Y"),
  n_substances = dplyr::n_distinct(nc$lab$substance),
  pct_consumed = round(100 * mean(card$consumed == 1, na.rm = TRUE))
)
write_json(stats, "figs/stats.json", auto_unbox = TRUE, pretty = TRUE)
message("Nightly build complete: ", stats$updated)
