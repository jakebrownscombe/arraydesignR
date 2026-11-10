# ---- Receiver layouts ----
# A layout is the receiver_layout data frame of arraydesign::place_receivers():
# the table contract's receiver columns, with x and y in the CRS of the area.

layout_columns <- c("receiver_serial", "station_id", "x", "y", "deploy_lat", "deploy_lon",
                    "deploy_z")

# The receiver depth below which the grid, random draws and map clicks place
# no receiver. 1 m is a starting value the user can change in the sidebar,
# and no field data set it; the help of arraydesign::place_receivers() uses the
# same 1 m in its example of a depth floor.
default_min_depth <- 1

is_one_number <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)

check_min_depth <- function(min_depth) {
  if (!is_one_number(min_depth) || min_depth < 0) {
    stop_app("The minimum receiver depth must be a number of 0 m or more.")
  }
  invisible(min_depth)
}

# Water cells a receiver may sit in: at least `min_depth` deep, and never 0 m.
deployable <- function(depth, min_depth) depth >= min_depth & depth > 0

# arraydesign::place_receivers() has no depth floor; its help says to set the
# shallow cells to NA first, which this does. Coverage is still computed over
# every water cell.
deployable_grid <- function(area, min_depth) {
  grid <- terra::unwrap(area$grid)
  terra::ifel(deployable(abs(grid), min_depth), grid, NA)
}

generate_layout <- function(area, method, n, seed, min_depth = default_min_depth) {
  if (!is_one_number(n) || n < 1 || n != round(n)) {
    stop_app("The number of receivers must be a whole number of at least 1.")
  }
  check_min_depth(min_depth)
  n_ok <- sum(deployable(area$water_depth, min_depth))
  if (n > n_ok) {
    stop_app(sprintf(paste("Only %s %s m deep or more. Ask for at most %s, or lower the",
                           "minimum receiver depth."),
                     fmt_count(n_ok, "water cell is", "water cells are"), format(min_depth),
                     fmt_count(n_ok, "receiver")))
  }
  if (!is_one_number(seed)) seed <- NULL
  arraydesign::place_receivers(deployable_grid(area, min_depth), n = n, method = method,
                               seed = seed)
}

# Receiver column names in common exports, and the contract name each maps to.
# The rows follow detqc's deployment map (inst/column_maps/deployments.csv):
# GLATOS receiver files use deploy_long, OTN files latitude and longitude, OTN
# short forms the same names in upper case. A bare `depth` column is not read:
# in some files it holds the bottom depth. Names are matched in lower case; the
# first present one is used.
receiver_aliases <- list(
  receiver_serial = c("receiver_serial", "ins_serial_no", "receiver_serial_number"),
  station_id = c("station_id", "station", "station_name", "station_no"),
  deploy_lon = c("deploy_lon", "deploy_long", "longitude", "stn_long"),
  deploy_lat = c("deploy_lat", "latitude", "stn_lat"),
  deploy_z = c("deploy_z", "instrument_depth", "ins_depth"),
  x = "x", y = "y"
)

# The file's columns renamed to the contract; `from` records each source name.
map_receiver_columns <- function(d) {
  low <- tolower(trimws(names(d)))
  out <- list()
  from <- character()
  for (target in names(receiver_aliases)) {
    hit <- match(receiver_aliases[[target]], low)
    hit <- hit[!is.na(hit)]
    if (!length(hit)) next
    out[[target]] <- d[[hit[1]]]
    from[[target]] <- names(d)[hit[1]]
  }
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  attr(out, "from") <- from
  out
}

coords_refusal <- function(d) {
  sprintf(paste("The file needs longitude and latitude columns (deploy_lon, deploy_long or",
                "longitude; deploy_lat or latitude) in WGS 84, or x and y in the coordinate",
                "system of the raster. It has: %s."), paste(names(d), collapse = ", "))
}

read_layout_csv <- function(path, area, min_depth = default_min_depth) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(raw)) stop_app("The CSV file has no rows.")
  d <- map_receiver_columns(raw)
  has <- function(cols) all(cols %in% names(d))
  if (!has(c("x", "y")) && !has(c("deploy_lon", "deploy_lat"))) {
    stop_app(coords_refusal(raw))
  }
  keep <- intersect(c("receiver_serial", "station_id", "x", "y", "deploy_lon", "deploy_lat"),
                    names(d))
  if (has(c("x", "y"))) keep <- setdiff(keep, c("deploy_lon", "deploy_lat"))
  for (nm in intersect(c("receiver_serial", "station_id"), keep)) d[[nm]] <- as.character(d[[nm]])
  for (nm in intersect(c("station_id", "receiver_serial"), keep)) check_unique(d, nm)
  out <- arraydesign::place_receivers(terra::unwrap(area$grid), method = "manual",
                                      coords = d[, keep, drop = FALSE])
  out <- keep_file_depth(out, d)
  warn_shallow(out, min_depth)
  out
}

# A deployment history has one row per deployment, so its stations and
# serials repeat; a layout needs one row per receiver.
check_unique <- function(d, nm) {
  rep <- unique(d[[nm]][duplicated(d[[nm]])])
  if (!length(rep)) return(invisible())
  shown <- paste(utils::head(rep, 5), collapse = ", ")
  if (length(rep) > 5) shown <- sprintf("%s and %d more", shown, length(rep) - 5)
  stop_app(sprintf(paste("`%s` repeats in the CSV file (%s): one row per receiver.",
                         "Keep the rows of one deployment date."),
                   attr(d, "from")[[nm]], shown))
}

# A receiver depth in the file is where the receiver is, so it replaces the
# depth of the raster cell, which on a coarse raster can be far off. The
# attribute "depth_from_file" names the stations it was kept for.
keep_file_depth <- function(layout, d) {
  if (!"deploy_z" %in% names(d)) return(layout)
  z <- suppressWarnings(as.numeric(d$deploy_z))
  ok <- !is.na(z)
  if (!any(ok)) return(layout)
  col <- attr(d, "from")[["deploy_z"]]
  inform_app(sprintf(paste("Receiver depth from the file's `%s` column for %d of %s",
                           "(%s m); the raster gives %s m at %s."),
                     col, sum(ok), fmt_count(nrow(layout), "receiver"),
                     span_text(abs(z[ok])), span_text(layout$deploy_z[ok]),
                     noun_for(sum(ok), "its cell", "their cells")))
  layout$deploy_z[ok] <- abs(z[ok])
  attr(layout, "depth_from_file") <- layout$station_id[ok]
  layout
}

# An uploaded layout is where the receivers are, or will be: it is kept as
# given, and receivers shallower than the minimum are named.
warn_shallow <- function(layout, min_depth) {
  low <- !deployable(layout$deploy_z, min_depth)
  if (any(low)) {
    warn_app(sprintf(paste("%d of %s in the file %s too shallow to deploy in",
                           "(0 m, or under the %s m minimum): %s. %s kept."),
                     sum(low), fmt_count(nrow(layout), "receiver"),
                     noun_for(sum(low), "sits", "sit"), format(min_depth),
                     paste(layout$station_id[low], collapse = ", "),
                     noun_for(sum(low), "It is", "They are")))
  }
  invisible(low)
}

# A receiver at (x, y), numbered after the highest number in use. A click on
# water shallower than the minimum moves to the nearest cell deep enough.
add_receiver <- function(layout, area, x, y, min_depth = default_min_depth) {
  check_min_depth(min_depth)
  grid <- terra::unwrap(area$grid)
  new <- arraydesign::place_receivers(grid, method = "manual", coords = data.frame(x = x, y = y))
  if (!deployable(new$deploy_z, min_depth)) {
    to <- nearest_deployable(area, new$x, new$y, min_depth)
    inform_app(sprintf(paste("The click fell on water %s m deep, below the %s m minimum. The",
                             "receiver moved %s m to the nearest cell %s m deep or more."),
                       format(round(new$deploy_z, 1)), format(min_depth),
                       fmt_int(round(to[["moved"]])), format(min_depth)))
    new <- arraydesign::place_receivers(grid, method = "manual",
                                        coords = data.frame(x = to[["x"]], y = to[["y"]]))
  }
  if (is.null(layout) || !nrow(layout)) return(new)
  k <- next_number(layout)
  new$receiver_serial <- sprintf("R%03d", k)
  new$station_id <- sprintf("S%03d", k)
  out <- rbind(as.data.frame(layout)[, layout_columns], as.data.frame(new)[, layout_columns])
  restore_layout(out, layout)
}

nearest_deployable <- function(area, x, y, min_depth) {
  ok <- which(deployable(area$water_depth, min_depth))
  if (!length(ok)) {
    stop_app(sprintf("No water cell is %s m deep or more.", format(min_depth)))
  }
  xy <- area$water_xy[ok, , drop = FALSE]
  d <- sqrt((xy[, 1] - x)^2 + (xy[, 2] - y)^2)
  i <- which.min(d)
  c(x = unname(xy[i, 1]), y = unname(xy[i, 2]), moved = unname(d[i]))
}

remove_receiver <- function(layout, station_id) {
  if (is.null(layout) || is.null(station_id)) return(layout)
  keep <- layout$station_id != station_id
  if (all(keep)) return(layout)
  restore_layout(as.data.frame(layout)[keep, layout_columns, drop = FALSE], layout)
}

restore_layout <- function(rows, like) {
  rownames(rows) <- NULL
  attr(rows, "epsg") <- attr(like, "epsg")
  attr(rows, "method") <- "edited"
  from_file <- intersect(attr(like, "depth_from_file"), rows$station_id)
  if (length(from_file)) attr(rows, "depth_from_file") <- from_file
  class(rows) <- c("receiver_layout", "data.frame")
  rows
}

# The smallest whole number above every number used in a station or serial,
# so a new receiver never repeats an ID however the layout was made.
next_number <- function(layout) {
  ids <- c(layout$station_id, layout$receiver_serial)
  used <- suppressWarnings(as.integer(sub("^\\D*", "", ids)))
  max(c(nrow(layout), used), na.rm = TRUE) + 1L
}

write_layout <- function(layout, file) {
  utils::write.csv(as.data.frame(layout)[, layout_columns], file, row.names = FALSE)
}

layout_filename <- function(area, layout) {
  slug <- gsub("[^a-z0-9]+", "_", tolower(area$name))
  sprintf("receivers_%s_n%d_epsg%s.csv", gsub("^_|_$", "", slug), nrow(layout),
          if (is.na(area$epsg)) "unknown" else area$epsg)
}

# The table on screen: readable headers with units. The download keeps the
# contract's column names.
layout_table_names <- c("Receiver", "Station", "x (m)", "y (m)", "Latitude", "Longitude",
                        "Depth (m)")

layout_table <- function(layout) {
  if (is.null(layout) || !nrow(layout)) return(NULL)
  d <- as.data.frame(layout)[, layout_columns]
  d$x <- sprintf("%.0f", d$x)
  d$y <- sprintf("%.0f", d$y)
  d$deploy_lat <- sprintf("%.5f", d$deploy_lat)
  d$deploy_lon <- sprintf("%.5f", d$deploy_lon)
  d$deploy_z <- sprintf("%.1f", d$deploy_z)
  names(d) <- layout_table_names
  d
}

layout_table_note <- function(area, layout = NULL) {
  n_file <- length(attr(layout, "depth_from_file"))
  depth <- if (n_file == 0) {
    "from the depth raster"
  } else if (n_file == nrow(layout)) {
    "from the uploaded file"
  } else {
    sprintf("from the uploaded file for %s and from the depth raster for the rest",
            fmt_count(n_file, "receiver"))
  }
  sprintf(paste("x and y: metres in EPSG %s. Latitude and longitude: decimal degrees, WGS 84.",
                "Depth: metres below the surface, %s."),
          if (is.na(area$epsg)) "unknown" else area$epsg, depth)
}
