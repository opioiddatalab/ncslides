# prep_counties.R -- ONE-TIME. Not part of the nightly build.
#
# Projects North Carolina's 100 counties once and vendors the result as
# r/nc_counties.rds, a plain data frame of polygon vertices. The nightly run
# then draws maps with ggplot2::geom_polygon and needs neither sf/tigris nor
# GDAL/PROJ/GEOS, and never calls the Census API. County boundaries do not
# change; a nightly job should not depend on an external geo service to render.
#
# Run locally (where the geo stack is installed), commit the .rds:
#   Rscript r/prep_counties.R
#
# Requires: sf, tigris  (dev-only; deliberately absent from the nightly deps)

library(dplyr)

REGIONS <- "r/nc_county_regions.csv"
OUT     <- "r/nc_counties.rds"

main <- function() {
  for (p in c("sf", "tigris")) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("prep_counties.R needs the dev-only package '", p, "'. ",
           "It is intentionally not in the nightly dependency set.", call. = FALSE)
  }
  regions <- readr::read_csv(REGIONS, show_col_types = FALSE)

  shp <- tigris::counties(state = "NC", cb = TRUE, progress_bar = FALSE)
  shp <- sf::st_transform(shp, 4269)

  # to a flat vertex table: one row per polygon vertex
  coords <- sf::st_coordinates(sf::st_geometry(shp))
  # L2/L3 identify ring and polygon; combine with the feature index for `group`
  idx <- coords[, ncol(coords)]
  df <- tibble::tibble(
    long  = coords[, "X"],
    lat   = coords[, "Y"],
    group = paste(idx, coords[, "L1"], sep = "."),
    county = shp$NAME[idx])

  missing <- setdiff(df$county, regions$county)
  extra   <- setdiff(regions$county, df$county)
  if (length(missing)) warning("counties with no region: ",
                              paste(unique(missing), collapse = ", "), call. = FALSE)
  if (length(extra))   warning("region rows not in the shapefile: ",
                              paste(extra, collapse = ", "), call. = FALSE)

  out <- df %>% left_join(regions, by = "county")
  saveRDS(out, OUT, compress = "xz")
  message(sprintf("wrote %s: %d vertices, %d counties, regions %s",
                  OUT, nrow(out), dplyr::n_distinct(out$county),
                  paste(sort(unique(na.omit(out$region))), collapse = "/")))
}

if (identical(environment(), globalenv())) main()
