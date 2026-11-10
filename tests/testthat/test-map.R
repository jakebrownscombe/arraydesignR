test_that("receiver rings take detfield's three ring colours", {
  # The values are pinned here, so a change in detfield shows as a failure.
  pinned <- c(heard = "#FDE725", silent = "#FFFFFF", edge = "#1A1A1A")
  expect_identical(detfield::positionr_ring_colours(), pinned)
  expect_identical(ring_colours(), pinned)
})

test_that("map layers come from the area and the coverage of that area only", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 3, 1)
  cov <- run_coverage(coverage_request(a, lay, new_range_model(toy_spec())))
  depth <- map_layer(a, NULL, "depth")
  expect_equal(unname(terra::minmax(depth$raster)[, 1]), a$depth_range)
  expect_identical(depth$title, "Depth (m)")
  expect_null(map_layer(a, NULL, "distance"))
  expect_null(map_layer(a, NULL, "p_array"))
  p <- map_layer(a, cov, "p_array")
  vals <- terra::values(p$raster, mat = FALSE)
  expect_equal(vals[cov$cells], cov$p_array)
  expect_equal(sum(!is.na(vals)), 2152)
  dist <- map_layer(a, cov, "distance")
  expect_equal(terra::values(dist$raster, mat = FALSE)[cov$cells], cov$nearest_m)
  expect_match(map_caption("distance", dist, lay, cov), "more than 1,071 m from every receiver")
  other <- new_study_area(-terra::unwrap(a$grid), "neg")
  expect_null(map_layer(other, cov, "p_array"))
})

test_that("probability is magma on 0 to 1 and depth the 1.x blues", {
  a <- small_area()
  cov <- run_coverage(coverage_request(a, generate_layout(a, "grid", 3, 1),
                                       new_range_model(toy_spec())))
  p <- map_layer(a, cov, "p_array")
  # The magma ends, as detfield::scale_fill_probability() draws them.
  expect_identical(p$pal(c(0, 1)), c("#000004", "#FCFDBF"))
  expect_identical(p$pal(0.5), "#B6367A")
  expect_identical(p$legend$values, c(0, 1))
  expect_true(p$legend$decreasing)
  expect_identical(p$legend$pal(0), p$pal(1))
  key <- as.character(map_key(p))
  expect_match(key, "top: 0.0%;\">1.0<", fixed = TRUE)
  expect_match(key, "top: 100.0%;\">0.0<", fixed = TRUE)
  expect_identical(p$ring, "#FDE725")
  # Depth: cornflowerblue in the shallows to blue4 at the deepest cell.
  d <- map_layer(a, NULL, "depth")
  lim <- outward(a$depth_range)
  expect_identical(d$pal(lim), toupper(c("#6495ED", "#00008B")))
  dist <- map_layer(a, cov, "distance")
  expect_identical(dist$legend$values[1], 0)
  expect_identical(dist$pal(0), toupper("#FDE725"))
  expect_identical(names(layer_choices)[2], "Distance through water to nearest receiver")
})

test_that("a raster that stores depth as negative numbers draws its deepest cell blue4", {
  # The bundled Stoney raster stores depth as -32.5 to 0 m.
  expect_lt(min(terra::values(terra::unwrap(stoney_area()$grid)), na.rm = TRUE), 0)
  d <- map_layer(stoney_area(), NULL, "depth")
  vals <- terra::values(d$raster, mat = FALSE)
  expect_gte(min(vals, na.rm = TRUE), 0)
  expect_identical(d$pal(outward(range(vals, na.rm = TRUE))), c("#6495ED", "#00008B"))
  neg <- new_study_area(-arraydesign::example_lake(), "neg")
  expect_identical(map_layer(neg, NULL, "depth")$pal(outward(neg$depth_range)),
                   c("#6495ED", "#00008B"))
})

test_that("the hour layer is 1 - (1 - p)^n on magma, n from the transmission delay", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 3, 1)
  cov <- run_coverage(coverage_request(a, lay, new_range_model(default_spec())))
  expect_identical(layer_choices[["Detection within an hour"]], "p_hour")
  h <- map_layer(a, cov, "p_hour", delay = 120)
  vals <- terra::values(h$raster, mat = FALSE)[cov$cells]
  expect_equal(vals, 1 - (1 - cov$p_array)^30, tolerance = 1e-6)
  expect_true(all(vals >= cov$p_array - 1e-6))
  expect_identical(h$pal(c(0, 1)), c("#000004", "#FCFDBF"))
  expect_identical(h$title, "Detection within an hour")
  slow <- map_layer(a, cov, "p_hour", delay = 600)
  expect_true(all(terra::values(slow$raster, mat = FALSE)[cov$cells] <= vals + 1e-6))
  expect_null(map_layer(a, cov, "p_hour", delay = NA))
  expect_null(map_layer(a, cov, "p_hour", delay = 0))
  expect_null(map_layer(a, NULL, "p_hour"))
  cap <- map_caption("p_hour", h, lay, cov, delay = 120)
  expect_match(cap, "n = 30 transmissions at a 120 s mean delay", fixed = TRUE)
  expect_match(cap, "treated as independent")
  expect_match(map_caption("p_hour", NULL, lay, cov, delay = NA),
               "^Set a mean transmission delay above 0 s on the Performance tab")
})

test_that("a click in WGS 84 lands on the receiver position it came from", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 2, 1)
  xy <- click_xy(list(lng = lay$deploy_lon[1], lat = lay$deploy_lat[1]), a)
  expect_equal(c(xy$x, xy$y), c(lay$x[1], lay$y[1]), tolerance = 1e-6)
  expect_null(click_xy(NULL, a))
  b <- area_bounds(a)
  expect_true(b[["xmin"]] < lay$deploy_lon[1] && lay$deploy_lon[1] < b[["xmax"]])
  expect_true(b[["ymin"]] < lay$deploy_lat[1] && lay$deploy_lat[1] < b[["ymax"]])
})

test_that("the map carries its layer, legend, scale bar and hollow rings", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 3, 1)
  m <- draw_receivers(draw_layer(base_map(area_bounds(a)), map_layer(a, NULL, "depth")), lay,
                      ring_colours()[["heard"]])
  calls <- vapply(m$x$calls, `[[`, "", "method")
  expect_true(all(c("addTiles", "addScaleBar", "addRasterImage", "addCircleMarkers")
                  %in% sub("addProviderTiles", "addTiles", calls)))
  # No leaflet legend inside the map; the key is drawn beside it.
  expect_false("addLegend" %in% calls)
  # Two circles per receiver: a dark edge that takes no clicks, then the
  # yellow ring that carries the station ID, so a ring reads on any magma.
  markers <- which(calls == "addCircleMarkers")
  expect_length(markers, 2)
  edge <- m$x$calls[[markers[1]]]$args
  expect_null(edge[[4]])
  expect_identical(edge[[6]]$color, "#1A1A1A")
  expect_false(edge[[6]]$interactive)
  expect_gt(edge[[6]]$weight, 2)
  rings <- m$x$calls[[markers[2]]]$args
  expect_identical(rings[[4]], lay$station_id)
  expect_identical(rings[[6]]$color, "#FDE725")
  expect_equal(rings[[6]]$fillOpacity, 0)
  expect_false(rings[[6]]$bubblingMouseEvents)
  empty <- draw_receivers(draw_layer(base_map(area_bounds(a)), NULL), NULL, "#FDE725")
  expect_false(any(vapply(empty$x$calls, `[[`, "", "method") %in%
                     c("addRasterImage", "addCircleMarkers")))
})

test_that("the colour key is drawn outside the map, in the layer's colours", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 3, 1)
  cov <- run_coverage(coverage_request(a, lay, new_range_model(default_spec())))
  p <- map_layer(a, cov, "p_array")
  html <- as.character(map_key(p))
  labels <- regmatches(html, gregexpr("top: [0-9.]+%;\">[^<]+", html))[[1]]
  top <- as.numeric(sub("top: ([0-9.]+)%.*", "\\1", labels))
  text <- sub(".*>", "", labels)
  # Probability runs from 1.0 at the top to 0.0 at the bottom.
  expect_identical(text[which.min(top)], "1.0")
  expect_identical(text[which.max(top)], "0.0")
  expect_equal(range(top), c(0, 100))
  stops <- regmatches(html, gregexpr("#[0-9A-F]{6} [0-9]+%", html))[[1]]
  expect_length(stops, 11L)
  expect_identical(substr(stops[1], 1, 7), substr(p$pal(1), 1, 7))
  expect_identical(substr(stops[11], 1, 7), substr(p$pal(0), 1, 7))
  expect_match(html, "Array detection probability", fixed = TRUE)
  # Depth runs from shallow at the top to deep at the bottom.
  d <- map_layer(a, NULL, "depth")
  dh <- as.character(map_key(d))
  ds <- regmatches(dh, gregexpr("#[0-9A-F]{6} [0-9]+%", dh))[[1]]
  rng <- range(d$legend$values)
  expect_identical(substr(ds[1], 1, 7), substr(d$pal(rng[1]), 1, 7))
  expect_null(map_key(NULL))
  expect_no_match(dh, "Receiver", fixed = TRUE)
  # Once a receiver is placed, the key draws its ring as the map does: the
  # dark ring under the receiver ring.
  rh <- as.character(map_key(d, 3L, "#FDE725"))
  expect_match(rh, "<span>Receiver</span>", fixed = TRUE)
  circles <- regmatches(rh, gregexpr("<circle[^>]*>", rh))[[1]]
  expect_length(circles, 2L)
  expect_match(circles[1], "stroke=\"#1A1A1A\" stroke-width=\"4.5\"")
  expect_match(circles[2], "stroke=\"#FDE725\" stroke-width=\"2\"")
  expect_match(rh, "aria-label=\"Map key: Depth \\(m\\), .*; receiver ring\"")
  # Before coverage the map shows rings and no layer, so the key holds the ring only.
  only <- as.character(map_key(NULL, 3L))
  expect_match(only, "Receiver", fixed = TRUE)
  expect_no_match(only, "map-key-bar", fixed = TRUE)
  # The UI holds the key in its own element beside the map.
  ui <- as.character(mod_map_ui("map"))
  expect_match(ui, "class=\"map-row\"", fixed = TRUE)
  expect_match(ui, "id=\"map-key\"", fixed = TRUE)
})

test_that("map captions name what is drawn, the range model and the receivers", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 3, 1)
  cov <- run_coverage(coverage_request(a, lay, new_range_model(default_spec())))
  expect_match(map_caption("p_array", NULL, lay, NULL), "^Compute coverage to draw this layer")
  empty <- map_caption("depth", map_layer(a, NULL, "depth"), NULL, NULL)
  expect_match(empty, "No receivers placed yet")
  one <- map_caption("depth", map_layer(a, NULL, "depth"), lay[1, , drop = FALSE], NULL)
  expect_match(one, "1 receiver placed\\.")
  cap <- map_caption("p_array", map_layer(a, cov, "p_array"), lay, cov)
  expect_match(cap, "at least one receiver detects one transmission")
  expect_match(cap, "fixed scale 0 to 1")
  expect_match(cap, "2,152 water cells. Range model: App defaults, unsourced")
  expect_match(cap, "3 receivers placed")
  dcap <- map_caption("distance", map_layer(a, cov, "distance"), lay, cov)
  expect_match(dcap, "Range model: App defaults, unsourced")
  expect_false(grepl("Range model", map_caption("depth", map_layer(a, cov, "depth"), lay, cov)))
  # The map key names the marks, so no caption names a colour.
  for (nm in c("depth", "distance", "p_array", "p_hour")) {
    expect_no_match(map_caption(nm, map_layer(a, cov, nm), lay, cov), colour_words, perl = TRUE)
    expect_no_match(map_caption(nm, NULL, lay, NULL), colour_words, perl = TRUE)
  }
})

test_that("the map module emits clicks in the area's CRS and marker clicks as station IDs", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 3, 1)
  args <- list(area = shiny::reactiveVal(a), layout = shiny::reactiveVal(lay),
               coverage = shiny::reactiveVal(NULL))
  shiny::testServer(mod_map_server, args = args, {
    session$setInputs(layer = "depth")
    expect_s3_class(output$map, "json")
    expect_match(output$caption, "Water depth per cell")
    session$setInputs(map_click = list(lng = lay$deploy_lon[2], lat = lay$deploy_lat[2]))
    got <- session$returned$click()
    expect_equal(c(got$x, got$y), c(lay$x[2], lay$y[2]), tolerance = 1e-6)
    session$setInputs(map_marker_click = list(id = "S003"))
    expect_identical(session$returned$remove(), "S003")
    session$setInputs(layer = "p_array")
    expect_match(output$caption, "Compute coverage")
    expect_identical(ring_colour(NULL), "#FDE725")
  })
})

test_that("a new study area does not replay the last click", {
  a <- small_area()
  area <- shiny::reactiveVal(a)
  lay <- generate_layout(a, "grid", 3, 1)
  args <- list(area = area, layout = shiny::reactiveVal(lay), coverage = shiny::reactiveVal(NULL))
  shiny::testServer(mod_map_server, args = args, {
    session$setInputs(layer = "depth")
    clicks <- 0
    shiny::observeEvent(session$returned$click(), clicks <<- clicks + 1)
    session$setInputs(map_click = list(lng = lay$deploy_lon[1], lat = lay$deploy_lat[1]))
    expect_equal(clicks, 1)
    area(stoney_area())
    session$flushReact()
    expect_equal(clicks, 1)
  })
})

test_that("a simulated lake, such as the toy lake, is drawn with no base map and says why", {
  expect_true(toy_area()$synthetic)
  expect_false(small_area()$synthetic)
  calls <- function(m) vapply(m$x$calls, `[[`, "", "method")
  expect_false("addTiles" %in% calls(base_map(area_bounds(toy_area()), tiles = FALSE)))
  expect_true("addScaleBar" %in% calls(base_map(area_bounds(toy_area()), tiles = FALSE)))
  toy_args <- list(area = shiny::reactiveVal(toy_area()), layout = shiny::reactiveVal(NULL),
                   coverage = shiny::reactiveVal(NULL))
  shiny::testServer(mod_map_server, args = toy_args, {
    session$setInputs(layer = "depth")
    expect_false(grepl("addTiles", output$map))
    expect_match(output$caption, "The lake is simulated and its place on the map is nominal")
  })
  real_args <- list(area = shiny::reactiveVal(small_area()), layout = shiny::reactiveVal(NULL),
                    coverage = shiny::reactiveVal(NULL))
  shiny::testServer(mod_map_server, args = real_args, {
    session$setInputs(layer = "depth")
    expect_match(output$map, "addTiles")
    expect_false(grepl("simulated", output$caption))
  })
})

test_that("the map draws once a study area that was not ready arrives", {
  chosen <- shiny::reactiveVal(NULL)
  area <- shiny::reactive(shiny::req(chosen()))
  args <- list(area = area, layout = shiny::reactiveVal(NULL), coverage = shiny::reactiveVal(NULL))
  shiny::testServer(mod_map_server, args = args, {
    session$setInputs(layer = "depth")
    expect_error(output$map, class = "shiny.silent.error")
    chosen(toy_area())
    session$flushReact()
    expect_s3_class(output$map, "json")
  })
})

test_that("the first coverage result selects the probability layer, and only the first", {
  calls <- character()
  testthat::local_mocked_bindings(
    updateRadioButtons = function(session, input_id, selected = NULL, ...) {
      calls <<- c(calls, paste(input_id, selected))
    },
    .package = "shiny"
  )
  cov <- shiny::reactiveVal(NULL)
  args <- list(area = shiny::reactiveVal(toy_area()), layout = shiny::reactiveVal(NULL),
               coverage = cov)
  shiny::testServer(mod_map_server, args = args, {
    session$setInputs(layer = "depth")
    expect_length(calls, 0)
    cov(perf_cov())
    session$flushReact()
    expect_identical(calls, "layer p_array")
    session$setInputs(layer = "depth")
    cov(NULL)
    session$flushReact()
    cov(perf_cov())
    session$flushReact()
    expect_identical(calls, "layer p_array")
  })
})

test_that("the map is short enough for its scale bar to show at 1280 x 720", {
  expect_identical(map_height, "500px")
  expect_match(as.character(mod_map_ui("map")), "height:500px")
})
