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

PPTX_W <- 40 / 3; PPTX_H <- 7.5  # 13.33333 x 7.5 in = 16:9; 1920/144 exactly
PX <- 144                        # px per inch at this slide size
EMU <- 914400                    # EMU per inch, per the OOXML spec
px2in <- function(px) px / PX

OUT_PPTX <- file.path(OUT_DIR, "nc-drug-checking-latest.pptx")

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
  # The footer sits below the last border-top rule. Searching the whole slide
  # and taking the first hit grabs any earlier muted <p> instead -- on the KPI
  # dashboard that is a tile's "/ month" unit label, not the caption.
  tail_html <- html
  cut <- gregexpr("border-top:2px solid", html, fixed = TRUE)[[1]]
  if (cut[1] != -1) tail_html <- substring(html, cut[length(cut)])
  list(
    eyebrow = first_match(html, "letter-spacing:[45]px;text-transform:uppercase;[^\"]*\"[^>]*>(.*?)</p>"),
    title   = first_match(html, "<h[12][^>]*>([\\s\\S]*?)</h[12]>"),
    footer  = first_match(tail_html, sprintf("<p style=\"font-size:24px;color:%s;margin:0\">([\\s\\S]*?)</p>", unc$muted)))
}

# KPI dashboard tiles, read straight out of the template so the two outputs
# cannot disagree about which metric sits in which cell.
kpi_tiles <- function(html) {
  grab <- function(pat) regmatches(html, gregexpr(pat, html, perl = TRUE))[[1]]
  labs  <- sub(".*>", "", sub("</p>$", "", grab("text-overflow:ellipsis\">[^<]*</p>")))
  slugs <- gsub("\\{\\{CHART:|\\}\\}", "", grab("\\{\\{CHART:[a-z_]+\\}\\}"))
  units <- sub(".*>", "", sub("</p>$", "",
                grab("<p style=\"font-size:24px;color:#667180;margin:0\">(?:/ month|per 1,000)</p>")))
  n <- min(length(labs), length(slugs))
  if (!n) return(NULL)
  data.frame(label = labs[seq_len(n)], slug = slugs[seq_len(n)],
             unit = if (length(units) >= n) units[seq_len(n)] else "",
             stringsAsFactors = FALSE)
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

# ------------------------------------------------------------------ template
# officer ships ONE template and it is 4:3 (10 x 7.5in); slide_size() is a
# getter with no setter, so a bare read_pptx() silently produces 4:3 slides
# while the geometry above places content out to 13.333in -- a quarter of every
# slide off the right edge. There is no officer API for this, so mint a 16:9
# template per run: rewrite <p:sldSz> in a copy of officer's own.
pptx_template_16x9 <- function() {
  src <- system.file("template", "template.pptx", package = "officer")
  if (!nzchar(src)) stop("officer template.pptx not found", call. = FALSE)
  dir <- file.path(tempdir(), "pptx16x9")
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  utils::unzip(src, exdir = dir)

  pres <- file.path(dir, "ppt", "presentation.xml")
  xml  <- paste(readLines(pres, warn = FALSE), collapse = "\n")
  sldSz <- sprintf('<p:sldSz cx="%.0f" cy="%.0f"/>', PPTX_W * EMU, PPTX_H * EMU)
  xml2 <- sub("<p:sldSz[^>]*/>", sldSz, xml)
  if (identical(xml2, xml))
    stop("no <p:sldSz> in officer's template -- cannot set 16:9", call. = FALSE)
  writeLines(xml2, pres)

  # [Content_Types].xml must be the first entry in an OPC package.
  parts <- list.files(dir, all.files = TRUE, no.. = TRUE)
  parts <- c("[Content_Types].xml", setdiff(parts, "[Content_Types].xml"))
  out <- file.path(tempdir(), "template-16x9.pptx")
  unlink(out)
  zip::zip(out, files = parts, root = dir, mode = "cherry-pick")
  out
}

# ---------------------------------------------------------------------- build
build_pptx <- function(nc, cfg, tables, stats, alt, out = OUT_PPTX) {
  spec <- readr::read_csv(file.path(SLIDE_DIR, "spec.csv"), show_col_types = FALSE)
  doc <- read_pptx(pptx_template_16x9())
  sz <- officer::slide_size(doc)
  if (abs(sz$width / sz$height - 16 / 9) > 0.01)
    stop(sprintf("slide size is %.3f x %.3f in (%.3f:1), not 16:9",
                 sz$width, sz$height, sz$width / sz$height), call. = FALSE)
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
    } else if (row$archetype == "kpi12") {
      # A 4x3 grid, matching the HTML. The generic multi-chart branch below
      # lays every chart in ONE row, which for 12 sparklines means 12 tall
      # slivers with no labels and no numbers.
      tl <- kpi_tiles(html)
      if (!is.null(tl)) {
        ncol <- 4; nrow_g <- ceiling(nrow(tl) / ncol); gap <- px2in(20)
        cw <- (body_w - gap * (ncol - 1)) / ncol
        ch <- (body_h - gap * (nrow_g - 1)) / nrow_g
        lab_h <- px2in(30); val_h <- px2in(46)
        for (k in seq_len(nrow(tl))) {
          cx <- pad + ((k - 1) %% ncol) * (cw + gap)
          cy <- body_top + ((k - 1) %/% ncol) * (ch + gap)
          doc <- add_text(doc, txt(toupper(tl$label[k]), 9, unc$muted, TRUE),
                          cx, cy, cw, lab_h)
          val <- stats[[paste0(tl$slug[k], "_val")]]
          if (!is.null(val)) {
            lbl <- if (nzchar(tl$unit[k])) paste(val, tl$unit[k]) else as.character(val)
            doc <- add_text(doc, txt(lbl, 16, unc$navy, TRUE), cx, cy + lab_h, cw, val_h)
          }
          png <- file.path(FIG_DIR, paste0(tl$slug[k], ".png"))
          if (file.exists(png))
            doc <- ph_with(doc, external_img(png, width = cw,
                                             height = ch - lab_h - val_h),
                           location = ph_location(left = cx, top = cy + lab_h + val_h,
                                                  width = cw,
                                                  height = ch - lab_h - val_h),
                           alt_text = alt[[tl$slug[k]]] %||% tl$slug[k])
        }
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
