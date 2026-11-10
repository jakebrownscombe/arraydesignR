# ---- Study areas ----
# Every module reads a study area as one list: the depth raster packed by
# terra::wrap(), so it can be cached and sent to a worker process, and the
# facts the summary line and the cache keys need.

# The largest number of water cells a study area may hold. Peak memory grows
# with the raster as well as with the pairs.
default_max_water_cells <- 250000

new_study_area <- function(grid, name, synthetic = FALSE,
                           max_water_cells = default_max_water_cells) {
  grid <- as_projected_grid(grid)
  vals <- terra::values(grid, mat = FALSE)
  wet <- !is.na(vals)
  if (!any(wet)) stop_app("The raster has no water cells: every cell is NA.")
  if (sum(wet) > max_water_cells) {
    stop_app(cells_refusal(grid, sum(wet), max_water_cells))
  }
  if (any(vals[wet] < 0) && any(vals[wet] > 0)) {
    stop_app(sprintf(paste("The raster mixes depths below and above 0 (%s and %s).",
                           "Give depth with one sign and land as NA."),
                     fmt_count(sum(vals[wet] < 0), "water cell"),
                     fmt_count(sum(vals[wet] > 0), "water cell")))
  }
  if (all(wet)) {
    warn_app("The raster has no NA cells, so every cell counts as water. Set land to NA.")
  }
  structure(list(
    name = name, synthetic = synthetic, grid = terra::wrap(grid), n_water = sum(wet),
    n_cells = length(vals), water_xy = terra::xyFromCell(grid, which(wet)),
    water_depth = abs(vals[wet]),
    res = terra::res(grid)[1], epsg = grid_epsg(grid), depth_range = range(abs(vals[wet])),
    hash = rlang::hash(list(vals, as.vector(terra::ext(grid)), terra::crs(grid)))
  ), class = "study_area")
}

cells_refusal <- function(grid, n_water, max_water_cells) {
  factor <- ceiling(sqrt(n_water / max_water_cells))
  sprintf(paste("The raster has %s, above this app's limit of %s. Aggregate it by",
                "%d (cells of %s m) before upload, for example with terra::aggregate()."),
          fmt_count(n_water, "water cell"), fmt_int(max_water_cells), factor,
          format(signif(factor * terra::res(grid)[1], 3)))
}

# A raster in a projected CRS in metres with square cells. A raster in
# longitude and latitude is projected to the standard six-degree UTM zone of
# its centre; the exceptions round Norway and Svalbard are not applied.
as_projected_grid <- function(grid) {
  if (inherits(grid, "PackedSpatRaster")) grid <- terra::unwrap(grid)
  if (!inherits(grid, "SpatRaster")) grid <- terra::rast(grid)
  grid <- grid[[1]]
  if (!nzchar(terra::crs(grid))) {
    stop_app("The raster has no coordinate reference system. Save it with one, in metres.")
  }
  # is.lonlat() of the CRS text reads no coordinate, so it never warns that
  # they are out of range; the extent checks come before any call that does.
  if (terra::is.lonlat(terra::crs(grid))) {
    # One statement each: a refusal raised while an S4 method of terra
    # evaluates its argument reaches the caller as a plain simpleError.
    check_lonlat_extent(grid)
    grid <- project_to_utm(rotate_lon(grid))
  }
  if (!isTRUE(all.equal(terra::linearUnits(grid), 1))) {
    stop_app("The raster's coordinate reference system is not in metres.")
  }
  check_grid_scale(grid)
  r <- terra::res(grid)
  if (abs(r[1] - r[2]) > 1e-6 * r[1]) {
    grid <- terra::project(grid, terra::crs(grid), res = min(r), method = "bilinear")
  }
  grid
}

# The widest and tallest a longitude-latitude raster may be, in degrees. The
# app projects it to the UTM zone of its centre. check_grid_scale() measures
# the scale at the projected raster's centre. That point can lie more than 3
# degrees of longitude from the zone's central meridian, most near the poles,
# but it stays within about 350 km of it, where the UTM scale is at most 1.0011.
max_lonlat_span <- 12

# A span typed to one decimal, such as 22.1 - 10.1, can land a few units in
# the 15th digit above 12. Spans within this many degrees of the bound read.
lonlat_span_tolerance <- 1e-9

# A longitude-latitude raster is checked on its extent alone, before any terra
# call that reads its coordinates. Longitudes lie from -180 to 180 or from 0
# to 360. A lake across 180 degrees written from -180 to 180 reaches -180 or
# 180 and spans far more than 12 degrees, so the span refusal names the 0 to
# 360 form for a raster that reaches either edge.
check_lonlat_extent <- function(grid) {
  e <- as.vector(terra::ext(grid))
  if (e[["xmin"]] < -180 || e[["xmax"]] > 360 || e[["ymin"]] < -90 || e[["ymax"]] > 90) {
    stop_app(sprintf(paste("The raster runs from %s to %s in longitude and from %s to %s in",
                           "latitude. Longitudes must lie from -180 to 180 or from 0 to 360,",
                           "and latitudes from -90 to 90."),
                     lon_text(e[["xmin"]]), lon_text(e[["xmax"]]),
                     lon_text(e[["ymin"]]), lon_text(e[["ymax"]])))
  }
  span <- c(e[["xmax"]] - e[["xmin"]], e[["ymax"]] - e[["ymin"]])
  over <- span > max_lonlat_span + lonlat_span_tolerance
  if (any(over)) stop_app(span_refusal(e, span, over))
  invisible(grid)
}

# The span refusal. A raster in the -180 to 180 form that reaches either edge
# also gets the 0 to 360 sentence.
span_refusal <- function(e, span, over) {
  reaches_180 <- e[["xmin"]] < 0 &&
    (e[["xmin"]] <= -180 + lonlat_span_tolerance || e[["xmax"]] >= 180 - lonlat_span_tolerance)
  lon_shown <- span_degrees_text(span[1], over[1])
  paste0(sprintf(paste("The raster spans %s %s of longitude and %s of latitude. The app projects",
                       "a study area to one UTM zone, so a raster in longitude and latitude may",
                       "span at most %s degrees of longitude and %s degrees of latitude. Give a",
                       "larger area as a raster in a projected coordinate reference system in",
                       "metres; the app takes one."),
                 lon_shown, if (lon_shown == "1") "degree" else "degrees",
                 span_degrees_text(span[2], over[2]),
                 format(max_lonlat_span), format(max_lonlat_span)),
         if (reaches_180) {
           paste(" The raster reaches 180 degrees: give a lake across 180 degrees in the 0 to",
                 "360 form.")
         })
}

# A span in degrees, rounded to three decimals or to as many more as it
# takes for a span over the bound to print above it: 12.0001 prints as
# 12.0001.
span_degrees_text <- function(span, over) {
  digits <- 3
  while (over && digits < 12 && round(span, digits) <= max_lonlat_span) digits <- digits + 1
  format(round(span, digits), digits = 15)
}

# A lon-lat raster whose longitudes all lie from 180 to 360 is moved 360
# degrees west, with a message, so it reads from -180 to 0. terra::rotate()
# is for a raster that spans the globe: a regional raster keeps its
# longitudes above 180 under it. A raster that crosses 180 degrees is kept as
# it is; project_to_utm() wraps its centre into -180 to 180.
rotate_lon <- function(grid) {
  e <- as.vector(terra::ext(grid))
  if (e[["xmin"]] < 180) return(grid)
  inform_app(sprintf(paste("The raster's longitudes run from %s to %s. They are read as %s to",
                           "%s, 360 degrees less."),
                     lon_text(e[["xmin"]]), lon_text(e[["xmax"]]),
                     lon_text(e[["xmin"]] - 360), lon_text(e[["xmax"]] - 360)))
  terra::shift(grid, dx = -360)
}

lon_text <- function(x) format(round(x, 3), nsmall = 3)

project_to_utm <- function(grid) {
  e <- as.vector(terra::ext(grid))
  lon <- (mean(e[c("xmin", "xmax")]) + 180) %% 360 - 180
  lat <- mean(e[c("ymin", "ymax")])
  epsg <- (if (lat >= 0) 32600L else 32700L) + utm_zone(lon)
  # 111.32 km is a degree of longitude at the equator on WGS 84; times the
  # cosine of latitude it gives a degree of longitude there. A degree of
  # latitude is 110.57 to 111.69 km, within 0.7 percent of 111.32 km. The
  # smaller side sets the cell size.
  cell_m <- terra::res(grid) * 111320 * c(cos(lat * pi / 180), 1)
  terra::project(grid, paste0("EPSG:", epsg), res = max(1, round(min(cell_m))),
                 method = "bilinear")
}

# The UTM zone, 1 to 60, of a longitude from -180 to 180. project_to_utm()
# wraps the centre into -180 to 180 first, so a centre at exactly 180 reads as
# -180 and takes zone 1. The cap keeps utm_zone(180) itself in zone 60.
utm_zone <- function(lon) min(60L, as.integer(floor((lon + 180) / 6) + 1))

# The largest departure from 1 that the scale of a projected CRS may have at
# the raster centre. arraydesign and detfield refuse a grid beyond it, so the
# app refuses it at upload, in its own words, before a receiver is placed.
max_scale_error <- 0.01

# The scale of a grid's CRS at the raster centre: the grid length of four 1 km
# segments through the centre (east-west, north-south and the two diagonals),
# each over its geodesic length on WGS 84. Also returns the centre in
# longitude and latitude. NA when the points cannot be projected to WGS 84,
# as for an engineering CRS with no geographic datum. terra warns before it
# fails there, so a warning also gives NA and never reaches the user. It
# measures as detfield's crs_scale() does; a change edits both.
grid_scale <- function(grid, half = 500) {
  e <- as.vector(terra::ext(grid))
  cx <- (e[["xmin"]] + e[["xmax"]]) / 2
  cy <- (e[["ymin"]] + e[["ymax"]]) / 2
  d <- half / sqrt(2)
  from <- rbind(c(cx - half, cy), c(cx, cy - half), c(cx - d, cy - d), c(cx - d, cy + d))
  to <- rbind(c(cx + half, cy), c(cx, cy + half), c(cx + d, cy + d), c(cx + d, cy - d))
  unmeasured <- function(cnd) matrix(NA_real_, 9, 2)
  ll <- tryCatch(terra::project(rbind(c(cx, cy), from, to), from = terra::crs(grid),
                                to = "EPSG:4326"),
                 warning = unmeasured, error = unmeasured)
  if (anyNA(ll)) return(list(scale = rep(NA_real_, 4), lon = NA_real_, lat = NA_real_))
  geo <- terra::distance(ll[2:5, , drop = FALSE], ll[6:9, , drop = FALSE], lonlat = TRUE,
                         pairwise = TRUE)
  list(scale = 2 * half / geo, lon = ll[1, 1], lat = ll[1, 2])
}

# Refuses a projected raster whose scale at its centre differs from 1 by more
# than max_scale_error on any of the four segments, such as Web Mercator,
# whose scale is about 1 / cos(latitude). A scale that cannot be measured is
# refused too: leaflet cannot draw such a raster, and the session would end
# at the first draw.
check_grid_scale <- function(grid) {
  s <- grid_scale(grid)
  if (anyNA(s$scale)) {
    stop_app(paste("The scale of this CRS at the raster centre cannot be measured, so give the",
                   "raster in a projected CRS with a geographic datum, such as its UTM zone."))
  }
  if (all(abs(s$scale - 1) <= max_scale_error)) return(invisible(grid))
  shown <- sprintf("%.3f", s$scale)
  diagonals <- if (shown[3] == shown[4]) {
    paste(shown[3], "on both diagonals")
  } else {
    paste(shown[3], "and", shown[4], "on the diagonals")
  }
  lon <- (s$lon + 180) %% 360 - 180
  zone <- utm_zone(lon)
  epsg <- (if (s$lat >= 0) 32600L else 32700L) + zone
  stop_app(sprintf(paste("At the raster's centre, its coordinate reference system has a scale of",
                         "%s east-west, %s north-south and %s. That is more than 1 percent",
                         "from 1, so its metres are not metres on the ground. Project the",
                         "raster to the UTM zone of its centre, %d%s (EPSG:%d), and upload it",
                         "again."),
                   shown[1], shown[2], diagonals, zone, if (s$lat >= 0) "N" else "S", epsg))
}

# The EPSG code of a raster; NA when there is no raster or terra names no
# numeric code. The same reader as detfield's, so the two packages report the
# same code for the same input.
grid_epsg <- function(grid) {
  if (!inherits(grid, "SpatRaster")) return(NA_integer_)
  code <- terra::crs(grid, describe = TRUE)$code
  if (length(code) && !is.na(code) && grepl("^[0-9]+$", code)) as.integer(code) else NA_integer_
}

# A raster of one depth prints it once, as "depth 5 m".
area_summary <- function(area) {
  sprintf("%s: %s of %s m (EPSG %s); depth %s m.", area$name,
          fmt_count(area$n_water, "water cell"), format(signif(area$res, 3)),
          if (is.na(area$epsg)) "unknown" else area$epsg,
          span_text(area$depth_range, function(v) format(signif(v, 3))))
}

# The bundled Stoney Lake raster, read once per process: the depth raster the
# Array DesignR 1.x app opened on, 100 m cells in UTM zone 17N, 4,818 water
# cells. inst/extdata/README.md gives its source. Its name carries the cell
# size only. The app opens on it when no other area is given.
stoney_area <- function() {
  if (is.null(pkg_env$stoney)) {
    path <- system.file("extdata", "stoney_depth.tif", package = "arraydesignR", mustWork = TRUE)
    pkg_env$stoney <- new_study_area(path, "Stoney Lake (100 m)")
  }
  pkg_env$stoney
}

# The study areas a session can choose from: any given to run_app(), each
# read once when the app starts, then the bundled Stoney Lake. The app opens
# on the first of them: the first lake given to run_app(), or Stoney Lake
# when none is given.
study_areas <- function(extra = NULL, max_water_cells = default_max_water_cells) {
  bundled <- list(`Stoney Lake (100 m)` = stoney_area())
  if (is.null(extra)) return(bundled)
  if (!is.list(extra) || is.null(names(extra)) || any(!nzchar(names(extra)))) {
    stop_app("`areas` must be a named list of rasters or raster file paths.")
  }
  out <- list()
  for (nm in names(extra)) {
    out[[nm]] <- new_study_area(extra[[nm]], nm, max_water_cells = max_water_cells)
  }
  c(out, bundled[setdiff(names(bundled), names(out))])
}
