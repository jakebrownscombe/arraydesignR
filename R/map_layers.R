# ---- Map layers ----
# One raster layer at a time over the study area, drawn cell by cell
# (nearest-neighbour projection), so no value is smoothed or invented at the
# shore. The suite's colour rules: depth on the blue ramp
# of the positionR 1.x depth maps, cornflowerblue in the shallows to blue4 at
# the deepest cell; every probability on magma over the fixed limits 0 to 1,
# as detfield::scale_fill_probability() draws it. Distance uses viridis
# reversed, so bright means close to a receiver. Receivers are yellow rings,
# the positionR 1.x receiver colour, over a dark ring one step wider: the dark
# edge keeps a ring legible on the pale-yellow end of magma and on the blues.

layer_choices <- c("Depth" = "depth", "Distance through water to nearest receiver" = "distance",
                   "Array detection probability" = "p_array",
                   "Detection within an hour" = "p_hour")

# The ring colours are detfield's, so the app's receivers match the package
# maps. They are read when a map is drawn. A call at the top level of R/ runs
# once at install, so a new detfield would not reach the app.
ring_colours <- function() detfield::positionr_ring_colours()

# On the simulation track map, heard transmissions are green so that no
# mark shares a colour with the red crosses or the yellow receivers.
detected_green <- "#2ECC40"

depth_colours <- c("cornflowerblue", "blue4")

# The raster, palette and legend for one layer, or NULL when the layer needs a
# coverage result for this study area and there is none, or, for the hour
# layer, a usable transmission delay. `legend` carries the legend's own
# palette and values; for probability it runs from 1 at the top to 0 at the
# bottom.
map_layer <- function(area, cov, layer, delay = default_delay) {
  grid <- terra::unwrap(area$grid)
  ring <- ring_colours()[["heard"]]
  if (layer == "depth") {
    r <- abs(grid)
    pal <- leaflet::colorNumeric(depth_colours, outward(terra::minmax(r)[, 1]), na.color = NA)
    return(list(raster = r, title = "Depth (m)", ring = ring, pal = pal,
                legend = list(pal = pal, values = terra::minmax(r)[, 1], decreasing = FALSE)))
  }
  if (is.null(cov) || !identical(cov$area_hash, area$hash)) return(NULL)
  if (layer == "distance") {
    ok <- !is.na(cov$nearest_m)
    r <- cells_raster(grid, cov$cells[ok], cov$nearest_m[ok])
    lim <- c(0, ceiling(max(cov$nearest_m[ok]) / 100) * 100)
    pal <- leaflet::colorNumeric("viridis", lim, reverse = TRUE, na.color = NA)
    return(list(raster = r, title = "Distance through water (m)", ring = ring,
                pal = pal, legend = list(pal = pal, values = lim, decreasing = FALSE)))
  }
  if (layer == "p_hour") {
    if (length(delay_problems(delay))) return(NULL)
    values <- p_hour(cov$p_array, delay)
    title <- "Detection within an hour"
  } else {
    values <- cov$p_array
    title <- "Array detection probability"
  }
  r <- cells_raster(grid, cov$cells, values)
  down <- leaflet::colorNumeric("magma", c(0, 1), reverse = TRUE, na.color = NA)
  list(raster = r, title = title, ring = ring,
       pal = leaflet::colorNumeric("magma", c(0, 1), na.color = NA),
       legend = list(pal = down, values = c(0, 1), decreasing = TRUE))
}

# Palette limits rounded outward to one decimal: terra stores the projected
# raster in single precision, which can move an extreme by up to 6e-8 of its
# value.
outward <- function(lim) c(floor(lim[1] * 10) / 10, ceiling(lim[2] * 10) / 10)

# Bounds of the study area in WGS 84, for the first view of the map.
area_bounds <- function(area) {
  grid <- terra::unwrap(area$grid)
  e <- terra::ext(terra::project(terra::as.polygons(terra::ext(grid), crs = terra::crs(grid)),
                                 "EPSG:4326"))
  as.vector(e)
}

# A simulated lake has no true place, so it gets no base map: real roads and
# forest under it would read as water drawn over land.
base_map <- function(bounds, tiles = TRUE) {
  m <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 3))
  # OpenStreetMap tiles need no API key, so they show no key watermark.
  if (tiles) m <- leaflet::addTiles(m, group = "Base map")
  m <- leaflet::addScaleBar(m, position = "bottomleft",
                            options = leaflet::scaleBarOptions(imperial = FALSE))
  leaflet::fitBounds(m, bounds[["xmin"]], bounds[["ymin"]], bounds[["xmax"]], bounds[["ymax"]])
}

# `map` is a leaflet map or a leafletProxy(); both take the same calls. The
# colour key is not a leaflet control: it is drawn beside the map by
# map_key(), outside the map canvas.
draw_layer <- function(map, layer) {
  map <- leaflet::clearGroup(map, "layer")
  if (is.null(layer)) return(invisible(map))
  invisible(leaflet::addRasterImage(map, layer$raster, colors = layer$pal, opacity = 0.85,
                                    group = "layer", project = TRUE, method = "ngb"))
}

# The map key, as HTML drawn beside the map: a key drawn inside a map covers
# part of the map, and any corner can hold water. It holds the colour bar of
# the layer and, once a receiver is placed, the receiver ring, so every mark
# on the map is keyed. The bar is the layer's
# own palette at the raster's opacity, with round values beside it;
# probability runs from 1 at the top to 0 at the bottom, depth and distance
# from low to high. NULL when the map shows neither a layer nor a receiver.
map_key <- function(layer, n_rx = 0L, ring = ring_colours()[["heard"]], n = 5) {
  if (is.null(layer) && n_rx == 0L) return(NULL)
  parts <- list()
  label <- character()
  if (!is.null(layer)) {
    bar <- key_bar(layer, n)
    parts <- list(bar$html)
    label <- sprintf("%s, %s to %s", layer$title, bar$from, bar$to)
  }
  if (n_rx > 0L) {
    parts <- c(parts, list(key_ring(ring)))
    label <- c(label, "receiver ring")
  }
  shiny::div(class = "map-key-box", role = "img",
             `aria-label` = paste("Map key:", paste(label, collapse = "; ")), parts)
}

key_bar <- function(layer, n) {
  lg <- layer$legend
  rng <- range(lg$values)
  ends <- if (isTRUE(lg$decreasing)) rev(rng) else rng
  at <- pretty(rng, n)
  at <- at[at >= rng[1] - 1e-9 & at <= rng[2] + 1e-9]
  span <- if (diff(rng) > 0) diff(ends) else 1
  pct <- abs(100 * (at - ends[1]) / span)
  stops <- seq(ends[1], ends[2], length.out = 11)
  grad <- sprintf("linear-gradient(to bottom, %s)",
                  paste(sprintf("%s %d%%", substr(layer$pal(stops), 1, 7), seq(0, 100, by = 10)),
                        collapse = ", "))
  labels <- format(at, big.mark = ",", trim = TRUE)
  html <- shiny::tagList(
    shiny::div(class = "map-key-title", layer$title),
    shiny::div(
      class = "map-key-scale",
      shiny::div(class = "map-key-bar", style = sprintf("background: %s;", grad)),
      lapply(seq_along(at), function(i) {
        shiny::span(class = "map-key-label", style = sprintf("top: %.1f%%;", pct[i]), labels[i])
      })
    )
  )
  list(html = html, from = labels[1], to = labels[length(at)])
}

# The receiver mark as draw_receivers() draws it: the ring over a wider dark
# ring, at the radius and stroke widths of the map.
key_ring <- function(ring) {
  circle <- function(colour, width, opacity) {
    shiny::tag("circle", list(cx = 10, cy = 10, r = 6, fill = "none", stroke = colour,
                              `stroke-width` = width, `stroke-opacity` = opacity))
  }
  shiny::div(
    class = "map-key-mark",
    shiny::tag("svg", list(width = 20, height = 20, viewBox = "0 0 20 20", `aria-hidden` = "true",
                           circle(ring_colours()[["edge"]], 4.5, 0.9), circle(ring, 2, 1))),
    shiny::span("Receiver")
  )
}

# Each receiver is two circles: a dark ring underneath, wider, that takes no
# clicks, and the yellow ring on top that carries the station ID.
draw_receivers <- function(map, layout, ring) {
  map <- leaflet::clearGroup(map, "receivers")
  if (is.null(layout) || !nrow(layout)) return(invisible(map))
  map <- leaflet::addCircleMarkers(
    map, lng = layout$deploy_lon, lat = layout$deploy_lat, group = "receivers", radius = 6,
    color = ring_colours()[["edge"]], weight = 4.5, opacity = 0.9, fill = FALSE,
    options = leaflet::pathOptions(interactive = FALSE)
  )
  invisible(leaflet::addCircleMarkers(
    map, lng = layout$deploy_lon, lat = layout$deploy_lat, layerId = layout$station_id,
    group = "receivers", radius = 6, color = ring, weight = 2, opacity = 1, fill = TRUE,
    fillOpacity = 0, label = layout$station_id,
    options = leaflet::pathOptions(bubblingMouseEvents = FALSE)
  ))
}

ring_colour <- function(layer) if (is.null(layer)) ring_colours()[["heard"]] else layer$ring

# A map click in WGS 84, moved into the CRS of the study area.
click_xy <- function(click, area) {
  if (is.null(click)) return(NULL)
  xy <- terra::project(cbind(click$lng, click$lat), from = "EPSG:4326",
                       to = terra::crs(terra::unwrap(area$grid)))
  list(x = xy[1, 1], y = xy[1, 2])
}

map_caption <- function(layer_name, layer, layout, cov, synthetic = FALSE,
                        delay = default_delay) {
  n_rx <- if (is.null(layout)) 0L else nrow(layout)
  rings <- if (n_rx == 0L) {
    "No receivers placed yet."
  } else {
    sprintf("%s placed.", fmt_count(n_rx, "receiver"))
  }
  if (is.null(layer)) return(no_layer_caption(layer_name, cov, delay, rings))
  what <- switch(layer_name,
    depth = "Water depth per cell.",
    distance = sprintf(paste("Distance through water from each cell to the nearest receiver.",
                             "Blank water lies more than %s m from every receiver."),
                       fmt_int(cov$max_distance)),
    p_array = paste("Probability that at least one receiver detects one transmission sent from",
                    "the cell (detfield::system_de); fixed scale 0 to 1."),
    p_hour = sprintf(paste("Probability of at least one detection in an hour from the cell,",
                           "1 - (1 - p)^n with n = %s %s at a %s s mean delay",
                           "(Performance tab). Transmissions are treated as independent, which",
                           "overstates it where detection is clustered in time.",
                           "Fixed scale 0 to 1."),
                     format(signif(transmissions_per_hour(delay), 3)),
                     noun_for(signif(transmissions_per_hour(delay), 3), "transmission"),
                     format(delay))
  )
  cells <- ""
  if (!is.null(cov) && layer_name != "depth") {
    cells <- sprintf(" %s. Range model: %s.", fmt_count(length(cov$cells), "water cell"),
                     cov$range_label)
  }
  place <- if (synthetic) {
    " The lake is simulated and its place on the map is nominal, so no base map is drawn."
  } else {
    ""
  }
  paste0(what, cells, " ", rings, place)
}

# The caption while the layer cannot be drawn: no coverage yet, or no valid
# delay for the hour layer.
no_layer_caption <- function(layer_name, cov, delay, rings) {
  if (layer_name == "p_hour" && !is.null(cov) && length(delay_problems(delay))) {
    return(paste("Set a mean transmission delay above 0 s on the Performance tab to draw",
                 "this layer.", rings))
  }
  paste("Compute coverage to draw this layer.", rings)
}
