# deck.R -- assemble the HTML deck from the slide templates in r/slides/.
#
# The templates ARE the design, exported from the Claude Design canvas and
# annotated once by tools/annotate_slides.py. This file only supplies values:
#
#   {{CHART:<slug>}}     inline SVG (real text, so it stays selectable/searchable)
#   {{STAT:<key>}}       a scalar or a generated sentence
#   {{TABLE:<id>}}       a full <table> from render_table()
#   {{ noteDisplay }}    the design's own mustache vars, from r/config.yml
#   {{ rTagDisplay }}
#
# Values are baked in at build time -- there is no client-side fetch, so the
# deck cannot flash placeholder numbers or fail on a missing JSON file, and its
# alt text and data tables are correct by construction rather than by a
# browser-side join.

library(glue); library(htmltools); library(dplyr)

SLIDE_DIR <- "r/slides"
OUT_HTML  <- "docs/index.html"

esc <- function(x) htmltools::htmlEscape(as.character(x))

# ------------------------------------------------------------------- tables
# Reproduces the appendix table markup exactly: per-cell inline styles, real
# <th scope="col">, banded even rows, 28px body / 24px uppercase headers, no
# merged cells.
TH <- paste0("text-align:%s;font-size:24px;font-weight:700;letter-spacing:2px;",
             "text-transform:uppercase;color:", unc$navy,
             ";border-bottom:3px solid ", unc$navy, ";padding:0 24px 14px 0")
TD <- paste0("padding:16px 24px 16px 0;border-bottom:1px solid ", unc$rule, "%s")

render_table <- function(df, max_rows = 12, note = NULL) {
  if (is.null(df) || !nrow(df))
    return(sprintf('<p style="font-size:28px;color:%s">No rows for this period.</p>',
                   unc$muted))
  total <- nrow(df)
  shown <- head(df, max_rows)
  # right-align columns whose values read as numbers
  numeric_col <- vapply(shown, function(v)
    all(grepl("^[+-]?[0-9][0-9,.]*%?$", trimws(as.character(v))) |
        trimws(as.character(v)) == ""), logical(1))

  head_cells <- paste0(sprintf('<th scope="col" style="%s">%s</th>',
                               sprintf(TH, ifelse(numeric_col, "right", "left")),
                               esc(names(shown))), collapse = "")
  rows <- vapply(seq_len(nrow(shown)), function(i) {
    cells <- vapply(seq_along(shown), function(j) {
      extra <- paste0(if (j == 1) sprintf(";font-weight:700;color:%s", unc$navy) else "",
                      if (numeric_col[j]) ";text-align:right" else "")
      sprintf('<td style="%s">%s</td>', sprintf(TD, extra), esc(shown[[j]][i]))
    }, character(1))
    sprintf('<tr%s>%s</tr>',
            if (i %% 2 == 0) sprintf(' style="background:%s"', unc$band) else "",
            paste(cells, collapse = ""))
  }, character(1))

  cap <- c(note,
           if (total > max_rows)
             sprintf("Showing the first %d of %d rows; the full table ships with the data.",
                     max_rows, total))
  paste0('<table style="width:100%;border-collapse:collapse;font-size:28px;color:',
         unc$body, '"><thead><tr>', head_cells, '</tr></thead><tbody>',
         paste(rows, collapse = ""), '</tbody></table>',
         if (length(cap)) sprintf('<p style="font-size:24px;color:%s;margin:18px 0 0">%s</p>',
                                  unc$muted, esc(paste(cap, collapse = " "))) else "")
}

# -------------------------------------------------------------------- charts
# Inline SVG, labelled for assistive tech and pointed at its appendix table.
chart_html <- function(slug, alt, appendix = NULL) {
  svg <- read_svg_inline(slug)
  if (is.null(svg))
    return(sprintf('<p style="font-size:28px;color:%s">Figure %s unavailable for this run.</p>',
                   unc$muted, esc(slug)))
  desc <- paste(c(alt, if (!is.null(appendix)) paste0("Data: Appendix ", appendix)),
                collapse = " ")
  sprintf('<div role="img" aria-label="%s" style="width:100%%;height:100%%">%s</div>',
          esc(desc), svg)
}

# --------------------------------------------------------------- global stats
global_stats <- function(nc, cfg, lab_substances) {
  cur_q <- period_of(nc$latest, "quarter")
  last_complete <- if (is_partial(cur_q, "quarter", nc$latest)) cur_q %m-% months(3) else cur_q
  counties <- dplyr::n_distinct(stats::na.omit(nc$card$county_clean))
  list(
    date_range   = sprintf("%s to %s", format(nc$first, "%B %Y"),
                           format(nc$latest, "%A, %B %d, %Y")),
    n_samples    = scales::comma(dplyr::n_distinct(nc$card$sampleid)),
    # Not derivable: the codebook documents `program`, but the public NC
    # extract omits it. Maintained in r/config.yml by the lab.
    n_programs   = scales::comma(cfg$n_programs),
    n_counties   = scales::comma(counties),
    n_substances = scales::comma(lab_substances),
    first_date   = format(nc$first,  "%d %b %Y"),
    latest_date  = format(nc$latest, "%d %b %Y"),
    latest_date_prior_year = format(nc$latest %m-% years(1), "%d %b %Y"),
    run_date     = format(nc$run_date, "%d %b %Y"),
    run_date_prior_year = format(nc$run_date %m-% years(1), "%d %b %Y"),
    latest_q     = fmt_q(last_complete),
    prev_q       = fmt_q(last_complete %m-% months(3)),
    latest_month = format(period_of(nc$latest, "month"), "%b %Y"),
    latest_month_prior_year = format(period_of(nc$latest, "month") %m-% years(1), "%b %Y"),
    # CC0-1.0 waives attribution, so the deck REQUESTS it; saying "required"
    # here would contradict the repo's own LICENSE.
    license_line = sprintf(paste("Data and code released under %s. Attribution is not",
                                 "required, but is requested: %s."),
                           cfg$license_label, cfg$attribution))
}

# Which appendix table backs which chart, for the "Data: Appendix A_" pointers.
APPENDIX_OF <- c("07_samples_quarterly" = "A1", "09_expected_substances" = "A2",
                 "10_expect_vs_lab" = "A3", "17_fentanyl_trend" = "A4",
                 "19_xylazine_trend" = "A4", "21_meth_trend" = "A4",
                 "22_cocaine_trend" = "A4")

# ------------------------------------------------------------------- assembly
substitute_markers <- function(html, file, stats, alt, tables, cfg, used) {
  # the design's own two vars first
  html <- gsub("{{ noteDisplay }}",
               if (isTRUE(cfg$show_placeholder_notes)) "block" else "none", html, fixed = TRUE)
  html <- gsub("{{ rTagDisplay }}",
               if (isTRUE(cfg$show_r_tags)) "flex" else "none", html, fixed = TRUE)

  repl <- function(html, pattern, resolve) {
    m <- gregexpr(pattern, html, perl = TRUE)[[1]]
    if (m[1] == -1) return(html)
    keys <- regmatches(html, gregexpr(pattern, html, perl = TRUE))[[1]]
    for (k in unique(keys)) {
      key <- sub("^\\{\\{[A-Z]+:", "", sub("\\}\\}$", "", k))
      val <- resolve(key)
      if (is.null(val))
        stop(sprintf("%s: no value for %s", file, k), call. = FALSE)
      used$keys <- c(used$keys, key)
      html <- gsub(k, val, html, fixed = TRUE)
    }
    html
  }

  html <- repl(html, "\\{\\{CHART:[^}]+\\}\\}",
               function(k) chart_html(k, alt[[k]], unname(APPENDIX_OF[k])))
  html <- repl(html, "\\{\\{TABLE:[^}]+\\}\\}",
               function(k) if (is.null(tables[[k]])) NULL else render_table(tables[[k]]))
  html <- repl(html, "\\{\\{STAT:[^}]+\\}\\}",
               function(k) if (is.null(stats[[k]])) NULL else as.character(stats[[k]]))
  html
}

build_deck <- function(nc, cfg, figs, tables, stats, alt) {
  spec <- readr::read_csv(file.path(SLIDE_DIR, "spec.csv"), show_col_types = FALSE)
  css  <- paste(readLines(file.path(SLIDE_DIR, "_helmet.css"), warn = FALSE), collapse = "\n")
  used <- new.env(); used$keys <- character()

  sections <- vapply(spec$file, function(f) {
    html <- paste(readLines(file.path(SLIDE_DIR, f), warn = FALSE), collapse = "\n")
    substitute_markers(html, f, stats, alt, tables, cfg, used)
  }, character(1), USE.NAMES = FALSE)

  # every value we computed but never placed: not fatal, but worth knowing
  unused <- setdiff(names(stats), used$keys)
  if (length(unused))
    message("unplaced stats (harmless): ", paste(unused, collapse = ", "))

  doc <- glue('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>North Carolina Drug Checking — Daily Brief, {format(nc$run_date, "%d %b %Y")}</title>
<meta name="description" content="Nightly brief on North Carolina drug checking results from the UNC Street Drug Analysis Lab. Data through {format(nc$latest, "%d %b %Y")}.">
<style>
{css}
</style>
</head>
<body>
<deck-stage width="1920" height="1080">
{paste(sections, collapse = "\n\n")}
</deck-stage>
<script src="deck-stage.js"></script>
</body>
</html>
', .open = "{", .close = "}")

  dir.create(dirname(OUT_HTML), showWarnings = FALSE, recursive = TRUE)
  writeLines(doc, OUT_HTML, useBytes = TRUE)
  message(sprintf("wrote %s (%d slides, %s)", OUT_HTML, nrow(spec),
                  format(structure(nchar(doc, type = "bytes"), class = "object_size"),
                         units = "auto")))
  invisible(OUT_HTML)
}
