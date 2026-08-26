# theme_unc.R -- UNC Street Drug Analysis Lab ggplot theme, palette, and export.
library(ggplot2)

# WCAG 2.2 AA palette (UNC Digital Accessibility Standard).
# Text colours >= 4.5:1 on white; graphical fills >= 3:1. #7BAFD4 is reserved
# for large fills on navy only -- never text or marks on white.
unc <- list(
  blue    = "#3A6D99",  # accessible Carolina-adjacent blue -- PRIMARY abundance (5.5:1)
  trace   = "#C9DEEE",  # TRACE-only increment -- always outlined + directly labelled
  navy    = "#13294B",  # titles
  body    = "#5B6770",  # body text (5.8:1)
  muted   = "#667180",  # captions / axis text (5.0:1)
  orange  = "#B45D08",  # emphasis / callouts (4.7:1)
  rule    = "#E2E8F0",
  partial = "#667180",  # in-progress period hatch / label
  good    = "#1E8E5A",  # favourable direction
  bad     = "#C4453C",  # unfavourable direction
  good_bg = "#E9F5EF",
  bad_bg  = "#FAEBE9",
  band    = "#F4F8FB"   # banded table rows
)

theme_unc <- function(base_size = 16) {
  theme_minimal(base_size = base_size, base_family = "Helvetica") +
    theme(
      plot.title         = element_text(color = unc$navy, face = "bold", size = base_size * 1.4),
      plot.subtitle      = element_text(color = unc$body),
      plot.caption       = element_text(color = unc$body, size = base_size * 0.7, hjust = 0),
      plot.caption.position = "plot",
      axis.text          = element_text(color = unc$muted),
      axis.title         = element_text(color = unc$body),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "top",
      legend.title       = element_blank()
    )
}

# Trace segments also carry an outline and a direct label, so hue alone never
# distinguishes primary from trace (WCAG 1.4.1).
scale_fill_primarytrace <- function()
  scale_fill_manual(values = c("Primary" = unc$blue, "Trace only" = unc$trace))

scale_color_primarytrace <- function()
  scale_color_manual(values = c("Primary" = unc$blue, "Trace only" = unc$trace))

# Diagonal-stripe fill marking an in-progress period. Low alpha is not used:
# it reads as "faded" rather than "incomplete" and fails at low contrast.
PARTIAL_LABEL <- "partial"

# ------------------------------------------------------------------ exporting
# Every figure is written twice: SVG for the HTML deck (text stays real text)
# and PNG for the PPTX (PowerPoint's SVG support is unreliable across versions).
# Output root. docs/ is TRACKED -- the nightly job commits it and GitHub Pages
# serves it -- so a local build writing there dirties ~90 tracked files and
# invites committing a stale local build over the bot's newer one. Point
# DECK_OUT elsewhere for a throwaway build:
#
#   DECK_OUT=build Rscript r/build_nightly.R
#
# CI sets nothing and keeps writing docs/.
OUT_DIR <- Sys.getenv("DECK_OUT", "docs")
FIG_DIR <- file.path(OUT_DIR, "figs")

fig_size <- function(kind = c("slide", "half", "third", "spark", "map")) {
  kind <- match.arg(kind)
  switch(kind,
    slide = c(14.5, 6.7),   # full chart area of a 1920x1080 slide
    half  = c(7.1,  6.4),   # side-by-side columns
    third = c(4.6,  5.6),
    spark = c(3.0,  1.15),  # KPI tile sparkline
    map   = c(7.0,  4.6)
  )
}

ggsave_fig <- function(slug, plot, kind = "slide", dir = FIG_DIR) {
  dim <- fig_size(kind)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  svglite::svglite(file.path(dir, paste0(slug, ".svg")),
                   width = dim[1], height = dim[2], bg = "white")
  print(plot); grDevices::dev.off()
  ggsave(file.path(dir, paste0(slug, ".png")), plot,
         width = dim[1], height = dim[2], dpi = 200, bg = "white",
         limitsize = FALSE)
  invisible(slug)
}

# Read an SVG back for inline embedding, stripping the XML prolog and forcing
# it to scale to its container. role/title are set by r/deck.R.
read_svg_inline <- function(slug, dir = FIG_DIR) {
  p <- file.path(dir, paste0(slug, ".svg"))
  if (!file.exists(p)) return(NULL)
  svg <- paste(readLines(p, warn = FALSE), collapse = "\n")
  svg <- sub("^<\\?xml[^>]*\\?>\\s*", "", svg)
  svg <- sub("<!DOCTYPE[^>]*>\\s*", "", svg)
  # drop the fixed pt width/height so the SVG fills the slide's chart area
  svg <- sub("<svg([^>]*?)\\s+width='[^']*'", "<svg\\1", svg)
  svg <- sub("<svg([^>]*?)\\s+height='[^']*'", "<svg\\1", svg)
  sub("<svg", "<svg preserveAspectRatio='xMidYMid meet' style='width:100%;height:100%'", svg)
}

# Delta badge shared by the HTML deck and the PPTX: arrow, magnitude, and a
# colour that means *favourable*, not merely *up*. Rising sample volume is
# good; a rising xylazine rate is not.
badge_html <- function(delta_pct, favorable) {
  if (!is.finite(delta_pct)) return("")
  arrow <- if (delta_pct >= 0) "▲" else "▼"
  fg <- if (favorable) unc$good else unc$bad
  bg <- if (favorable) unc$good_bg else unc$bad_bg
  sprintf(paste0('<p style="font-size:24px;font-weight:700;color:%s;',
                 'background:%s;padding:2px 12px;margin:0 0 0 auto">%s %d%%</p>'),
          fg, bg, arrow, round(abs(delta_pct)))
}
