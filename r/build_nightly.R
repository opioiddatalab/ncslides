#!/usr/bin/env Rscript
# build_nightly.R -- the entry point. Rebuilds every figure, the HTML deck, the
# PPTX, the month-end archive and the library page.
#
#   Rscript r/build_nightly.R
#
# Runs in GitHub Actions (.github/workflows/nightly.yml) on a schedule. Exits
# non-zero on a bad or short download so the workflow fails loudly instead of
# publishing a half-built deck: a nightly job that quietly stops updating is
# worse than one that breaks.

options(warn = 1)                    # surface warnings as they happen, in order
Sys.setenv(TZ = "America/New_York")  # the lab reads these dates as Eastern

t0 <- Sys.time()
source("r/theme_unc.R")
source("r/data.R")
source("r/figs.R")
source("r/new_detections.R")
source("r/deck.R")
source("r/pptx.R")
source("r/library.R")

cfg <- yaml::read_yaml("r/config.yml")

counties_path <- "r/nc_counties.rds"
counties <- if (file.exists(counties_path)) readRDS(counties_path) else {
  warning("no ", counties_path, " -- maps will be skipped. ",
          "Run Rscript r/prep_counties.R locally and commit the result.",
          call. = FALSE)
  NULL
}

nc <- load_nc()

figs <- run_all_figs(nc, counties)

nd <- new_detections_bundle(nc, cfg$new_detection_window_days %||% 91)

# Appendix wiring: chart slug -> appendix table id.
tables <- figs$tables
tables$a1 <- figs$tables[["07_samples_quarterly"]]
tables$a2 <- figs$tables[["09_expected_substances"]]
tables$a3 <- figs$tables[["10_expect_vs_lab"]]
tables$a5 <- nd$table
tables$new_detections <- nd$table

alt <- figs$alt
alt$new_detections <- nd$alt

stats <- utils::modifyList(
  global_stats(nc, cfg, dplyr::n_distinct(nc$lab$substance)),
  figs$stats)

build_deck(nc, cfg, figs, tables, stats, alt)
build_pptx(nc, cfg, tables, stats, alt)

# stats.json is not consumed by the deck (values are baked in) but costs
# nothing and gives anything downstream a machine-readable copy.
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
jsonlite::write_json(c(stats, list(
  data_through = format(nc$latest), built = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  figures_made = length(figs$made), figures_skipped = as.list(unique(figs$skipped)))),
  file.path(FIG_DIR, "stats.json"), auto_unbox = TRUE, pretty = TRUE)

archive_snapshot(nc$run_date)
build_library(nc, cfg)

message(sprintf("done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
