# pptx.R -- hybrid PowerPoint export.
#
# The HTML deck is the conforming deliverable (HANDOFF task 9, slide 08b: WCAG
# 2.2 AA). This is a convenience export, built HYBRID so it degrades as little
# as possible: chart areas are images, but every title, eyebrow, caption,
# footer and appendix table is a REAL text box, so the text stays selectable,
# searchable and reachable by a screen reader. Screenshotting whole slides
# would have been easier and would have destroyed all of that.
#
# Geometry: the design is 1920x1080, exported as 13.333 x 7.5 in 16:9, so
# exactly 144 px per inch. Titles and footers are read out of the same slide
# templates the HTML uses, so the two outputs cannot drift.

library(officer); library(flextable); library(dplyr)

PPTX_W <- 13.333; PPTX_H <- 7.5
PX <- 144                        # px per inch at this slide size
px2in <- function(px) px / PX

OUT_PPTX <- "docs/nc-drug-checking-latest.pptx"

# --------------------------------------------------------- template text mining
strip_tags <- function(x) {
  x <- gsub("<br\\s*/?>", " ", x)
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&#8202;|&thinsp;", " ", x)
  trimws(gsub("[ \t\n]+", " ", x))
}

first_match <- function(html, pattern) {
  m <- regmatches(html, regexpr(pattern, html, perl = TRUE))
  if (!length(m)) return(NA_character_)
  strip_tags(sub(pattern, "\\1", m, perl = TRUE))
}

slide_text <- function(html) {
  list(
    eyebrow = first_match(html, "letter-spacing:[45]px;text-transform:uppercase;[^\"]*\"[^>]*>(.*?)</p>"),
    title   = first_match(html, "<h[12][^>]*>([\\s\\S]*?)</h[12]>"),
    footer  = first_match(html, sprintf("<p style=\"font-size:24px;color:%s;margin:0\">([\\s\\S]*?)</p>", unc$muted)))
}

# ------------------------------------------------------------------- elements
txt <- function(x, size, color = unc$body, bold = FALSE, font = "Helvetica Neue") {
  fp <- fp_text(font.size = size, color = color, bold = bold, font.family = font)
  ftext(x, prop = fp)
}

add_text <- function(doc, x, left, top, width, height, align = "left") {
  ph <- ph_location(left = left, top = top, width = width, height = height)
  ph_with(doc, block_list(fpar(x, fp_p = fp_par(text.align = align))), location = ph)
}

# Appendix tables as real PowerPoint tables, styled like the HTML.
add_table <- function(doc, df, left, top, width, height, max_rows = 12) {
  if (is.null(df) || !nrow(df)) return(doc)
  ft <- flextable(head(as.data.frame(df), max_rows)) %>%
    theme_booktabs() %>%
    bg(part = "header", bg = "white") %>%
    color(part = "header", color = unc$navy) %>%
    bold(part = "header") %>%
    fontsize(size = 12, part = "all") %>%
    font(fontname = "Helvetica Neue", part = "all") %>%
    color(part = "body", color = unc$body) %>%
    bg(i = seq(2, max(2, min(nrow(df), max_rows)), by = 2), bg = unc$band, part = "body") %>%
    autofit()
  ph_with(doc, ft, location = ph_location(left = left, top = top,
                                          width = width, height = height))
}

# ---------------------------------------------------------------------- build
build_pptx <- function(nc, cfg, tables, stats, alt, out = OUT_PPTX) {
  spec <- readr::read_csv(file.path(SLIDE_DIR, "spec.csv"), show_col_types = FALSE)
  doc <- read_pptx()
  layout <- "Blank"; master <- officer::layout_summary(doc)$master[1]
  if (!"Blank" %in% officer::layout_summary(doc)$layout)
    layout <- officer::layout_summary(doc)$layout[1]

  pad <- px2in(110); top_pad <- px2in(88)
  body_top <- top_pad + px2in(150)
  body_h <- PPTX_H - body_top - px2in(120)
  body_w <- PPTX_W - 2 * pad

  for (i in seq_len(nrow(spec))) {
    row <- spec[i, ]
    html <- paste(readLines(file.path(SLIDE_DIR, row$file), warn = FALSE), collapse = "\n")
    st <- slide_text(html)
    charts <- strsplit(row$charts, ";")[[1]]
    charts <- charts[nzchar(charts)]

    doc <- add_slide(doc, layout = layout, master = master)

    if (!is.na(st$eyebrow) && nzchar(st$eyebrow))
      doc <- add_text(doc, txt(st$eyebrow, 13, unc$blue, TRUE),
                      pad, top_pad - px2in(34), body_w, px2in(34))
    if (!is.na(st$title) && nzchar(st$title))
      doc <- add_text(doc, txt(st$title, 30, unc$navy, TRUE),
                      pad, top_pad, body_w, px2in(96))

    if (row$archetype == "table") {
      ids <- c(row$table_id, paste0(row$table_id, "b"))
      ids <- ids[ids %in% names(tables)]
      if (length(ids) == 1) {
        doc <- add_table(doc, tables[[ids[1]]], pad, body_top, body_w, body_h)
      } else if (length(ids) == 2) {
        half <- (body_w - px2in(56)) / 2
        doc <- add_table(doc, tables[[ids[1]]], pad, body_top, half, body_h)
        doc <- add_table(doc, tables[[ids[2]]], pad + half + px2in(56), body_top,
                         half, body_h)
      }
    } else if (length(charts)) {
      pngs <- file.path(FIG_DIR, paste0(charts, ".png"))
      keep <- file.exists(pngs)
      pngs <- pngs[keep]; slugs <- charts[keep]
      if (length(pngs) == 1) {
        doc <- ph_with(doc, external_img(pngs[1], width = body_w, height = body_h),
                       location = ph_location(left = pad, top = body_top,
                                              width = body_w, height = body_h),
                       alt_text = alt[[slugs[1]]] %||% slugs[1])
      } else if (length(pngs) > 1) {
        n <- length(pngs); gap <- px2in(40)
        w <- (body_w - gap * (n - 1)) / n
        for (k in seq_len(n))
          doc <- ph_with(doc, external_img(pngs[k], width = w, height = body_h),
                         location = ph_location(left = pad + (k - 1) * (w + gap),
                                                top = body_top, width = w,
                                                height = body_h),
                         alt_text = alt[[slugs[k]]] %||% slugs[k])
      }
    } else {
      # static slide: carry its prose as text so it is not a blank page
      body <- strip_tags(sub("^[\\s\\S]*?</h[12]>", "", html, perl = TRUE))
      if (nzchar(body))
        doc <- add_text(doc, txt(substr(body, 1, 1200), 14, unc$body),
                        pad, body_top, body_w, body_h)
    }

    if (!is.na(st$footer) && nzchar(st$footer))
      doc <- add_text(doc, txt(st$footer, 10, unc$muted),
                      pad, PPTX_H - px2in(96), body_w, px2in(60))
  }

  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  print(doc, target = out)
  message(sprintf("wrote %s (%d slides)", out, nrow(spec)))
  invisible(out)
}
