# library.R -- month-end archive plus the public file library page.
#
# The archive keeps one dated snapshot per calendar month, forever. The check is
# done in R rather than in the workflow's cron so there is a single code path,
# and so a MISSED run still gets caught: if last month has no archive and we are
# past its end, it is written on the next run. A schedule that silently skips a
# month would otherwise leave a permanent hole.

library(dplyr); library(lubridate)

ARCHIVE_DIR <- file.path(OUT_DIR, "archive")
LIB_HTML    <- file.path(ARCHIVE_DIR, "index.html")

month_end <- function(d) ceiling_date(d, "month") - 1

# Months that should have an archive but do not (most recent first).
pending_months <- function(run_date, dir = ARCHIVE_DIR) {
  have <- basename(Sys.glob(file.path(dir, "*.pptx")))
  have <- substr(have, 1, 10)
  candidates <- month_end(seq(floor_date(run_date, "month") %m-% months(1),
                              by = "month", length.out = 2))
  candidates <- candidates[candidates <= run_date]
  if (month_end(run_date) == run_date) candidates <- c(candidates, run_date)
  candidates <- sort(unique(candidates))
  candidates[!as.character(candidates) %in% have]
}

archive_snapshot <- function(run_date, html = OUT_HTML, pptx = OUT_PPTX,
                             dir = ARCHIVE_DIR) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  wrote <- character()
  for (d in as.character(pending_months(run_date, dir))) {
    for (src in c(html, pptx)) {
      if (!file.exists(src)) next
      ext <- tools::file_ext(src)
      dest <- file.path(dir, paste0(d, ".", ext))
      file.copy(src, dest, overwrite = TRUE)
      # An archived copy sits one level down, so the deck's relative references
      # (logos, deck-stage.js, the PPTX download) would resolve inside
      # archive/. Repoint them at the parent, and at this month's own PPTX
      # rather than whatever "latest" happens to be years from now.
      if (identical(ext, "html")) {
        h <- paste(readLines(dest, warn = FALSE), collapse = "\n")
        h <- gsub('src="assets/', 'src="../assets/', h, fixed = TRUE)
        h <- gsub('src="deck-stage.js"', 'src="../deck-stage.js"', h, fixed = TRUE)
        h <- gsub(sprintf('href="%s"', basename(OUT_PPTX)),
                  sprintf('href="%s.pptx"', d), h, fixed = TRUE)
        writeLines(h, dest, useBytes = TRUE)
      }
      wrote <- c(wrote, dest)
    }
  }
  if (length(wrote)) message("archived: ", paste(basename(wrote), collapse = ", "))
  else message("no month-end archive due today")
  invisible(wrote)
}

# format.object_size() is scalar-only under units = "auto", so map over the
# vector -- the archive always holds at least an .html and a .pptx.
fmt_size <- function(bytes)
  vapply(bytes, function(b)
    format(structure(b, class = "object_size"), units = "auto", digits = 1),
    character(1), USE.NAMES = FALSE)

build_library <- function(nc, cfg, dir = ARCHIVE_DIR, out = LIB_HTML) {
  files <- Sys.glob(file.path(dir, "*.*"))
  files <- files[!basename(files) %in% "index.html"]
  arch <- if (length(files)) {
    tibble::tibble(file = basename(files), path = files,
                   date = substr(basename(files), 1, 10),
                   ext = toupper(tools::file_ext(files)),
                   size = file.size(files)) %>%
      arrange(desc(.data$date), .data$ext)
  } else tibble::tibble()

  tbl <- if (nrow(arch))
    render_table(arch %>% transmute(
      Month = format(as.Date(.data$date), "%B %Y"),
      `As of` = format(as.Date(.data$date), "%d %b %Y"),
      Format = .data$ext,
      Size = fmt_size(.data$size),
      File = .data$file), max_rows = 240)
  else sprintf('<p style="font-size:28px;color:%s">No month-end snapshots yet — the first lands at the end of this month.</p>', unc$muted)

  # link the file names in the rendered table
  for (f in arch$file)
    tbl <- gsub(paste0(">", f, "<"),
                sprintf('><a href="%s">%s</a><', f, f), tbl, fixed = TRUE)

  doc <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>NC Drug Checking — Deck Library</title>
<style>
body{margin:0;background:#FFFFFF;color:%s;font-family:"Helvetica Neue",Helvetica,Arial,sans-serif;padding:64px 8vw 96px}
a{color:%s;text-decoration:underline;text-underline-offset:4px}
a:hover{color:%s}
a:focus-visible{outline:3px solid %s;outline-offset:3px}
h1{font-size:clamp(34px,5vw,64px);color:%s;margin:14px 0 0;line-height:1.1}
h2{font-size:clamp(24px,3vw,34px);color:%s;margin:56px 0 18px}
.eyebrow{font-size:20px;font-weight:700;letter-spacing:4px;text-transform:uppercase;color:%s;margin:0}
.lede{font-size:22px;line-height:1.55;max-width:60ch}
.latest{border:3px solid %s;padding:26px 32px;margin:32px 0 0;max-width:60ch}
.latest p{margin:0 0 6px}
.big{font-size:26px;font-weight:700;color:%s}
footer{margin-top:64px;font-size:18px;color:%s;line-height:1.6}
table{max-width:100%%;overflow-x:auto;display:block}
</style>
</head>
<body>
<p class="eyebrow">UNC Street Drug Analysis Lab</p>
<h1>North Carolina Drug Checking<br>Deck Library</h1>
<p class="lede">The current brief rebuilds every night from the day\'s data. One dated
snapshot is kept for each calendar month.</p>

<div class="latest">
<p class="big"><a href="../index.html">Current deck (HTML)</a></p>
<p><a href="../%s">Current deck (PowerPoint)</a></p>
<p style="font-size:19px;color:%s;margin-top:10px">Data through %s · rebuilt %s</p>
</div>

<h2>Month-end snapshots</h2>
%s

<footer>
The HTML deck is the conforming version: real text, alt text regenerated nightly
with the figures, and data tables in the Appendix (WCAG 2.2 Level AA, per the UNC
Accessibility of Digital Content and Materials Standard). The PowerPoint is a
convenience export — its charts are images.<br><br>
%s. Attribution is not required, but is requested: %s.
</footer>
</body>
</html>
',
    unc$body, unc$blue, unc$navy, unc$navy, unc$navy, unc$navy, unc$blue,
    unc$navy, unc$navy, unc$muted,
    basename(OUT_PPTX), unc$muted,
    format(nc$latest, "%d %B %Y"), format(nc$run_date, "%d %B %Y"),
    tbl, cfg$license_label, cfg$attribution)

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  writeLines(doc, out, useBytes = TRUE)
  message(sprintf("wrote %s (%d archived file(s))", out, nrow(arch)))
  invisible(out)
}
