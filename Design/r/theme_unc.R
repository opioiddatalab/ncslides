# theme_unc.R — UNC Street Drug Analysis Lab ggplot theme + palette
library(ggplot2)

# WCAG 2.2 AA palette (UNC Digital Accessibility Standard, Apr 2026).
# Text colors >= 4.5:1 on white; graphical fills >= 3:1. #7BAFD4 reserved
# for large fills on navy only — never for text or marks on white.
unc <- list(
  blue      = "#3A6D99",  # accessible Carolina-adjacent blue — PRIMARY abundance (5.5:1)
  trace     = "#C9DEEE",  # TRACE-only increment — always outline with unc$body + direct label
  navy      = "#13294B",  # titles
  body      = "#5B6770",  # body text (5.8:1)
  muted     = "#667180",  # captions/axis text (5.0:1)
  orange    = "#B45D08",  # emphasis / callouts (4.7:1)
  rule      = "#E2E8F0",
  partial   = "#667180"   # in-progress quarter hatch/label
)

theme_unc <- function(base_size = 16) {
  theme_minimal(base_size = base_size, base_family = "Helvetica") +
    theme(
      plot.title    = element_text(color = unc$navy, face = "bold", size = base_size * 1.4),
      plot.subtitle = element_text(color = unc$body),
      plot.caption  = element_text(color = unc$body, size = base_size * 0.7, hjust = 0),
      axis.text     = element_text(color = unc$muted),
      axis.title    = element_text(color = unc$body),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "top",
      legend.title = element_blank()
    )
}

scale_fill_primarytrace <- function() {
  scale_fill_manual(values = c("Primary" = unc$blue, "Trace only" = unc$trace)) # trace segments need geom outline (color = unc$body) + direct labels: hue alone may not carry primary/trace (WCAG 1.4.1)
}

# Standard export size: chart area on a 1920x1080 slide
ggsave_slide <- function(filename, plot, path = "figs") {
  dir.create(path, showWarnings = FALSE)
  ggsave(file.path(path, filename), plot, width = 14.5, height = 6.7, dpi = 120, bg = "white")
}
