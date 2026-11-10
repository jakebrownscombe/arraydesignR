# ---- Test helpers ----

# Lets an ExtendedTask finish inside testServer(): its promise resolves on the
# later loop, and the session must flush for observers to see the result.
wait_for_task <- function(session, rounds = 100) {
  for (i in seq_len(rounds)) {
    later::run_now(0.01)
    session$flushReact()
  }
  invisible(NULL)
}

# The app's default inputs, as setInputs() takes them, with module prefixes:
# Stoney Lake, the one bundled lake, the 20 receivers Generate places by
# default and the app-default curve it opens on.
default_inputs <- function() {
  list(
    `area-choice` = "Stoney Lake (100 m)", `array-method` = "grid", `array-n` = 20,
    `array-seed` = 1, `array-min_depth` = 1,
    `range-depth_1` = 1, `range-d50_1` = 250, `range-d05_1` = 500,
    `range-depth_2` = 35, `range-d50_2` = 750, `range-d05_2` = 1500, `map-layer` = "depth"
  )
}

# ---- The toy lake ----
# A small simulated study area with a known curve. The app does not offer
# it; the tests use it where a lake must be synthetic or small.
toy_cache <- new.env(parent = emptyenv())

# new_study_area() reads and hashes every cell, so the tests share one copy.
toy_area <- function() {
  if (is.null(toy_cache$area)) {
    toy_cache$area <- new_study_area(toy_grid(), "Toy lake", synthetic = TRUE)
  }
  toy_cache$area
}

# A 60 by 40 grid of 30 m cells in UTM zone 17N: an oval lake 1 to 14 m deep
# with a peninsula from the north shore, 1,690 water cells. It uses no
# random numbers, so every run builds the same lake.
toy_grid <- function() {
  x0 <- 720000
  y0 <- 4935000
  grid <- terra::rast(nrows = 40, ncols = 60, xmin = x0, xmax = x0 + 1800, ymin = y0,
                      ymax = y0 + 1200, crs = "EPSG:32617")
  xy <- sweep(terra::xyFromCell(grid, seq_len(terra::ncell(grid))), 2, c(x0, y0))
  oval <- ((xy[, 1] - 900) / 870)^2 + ((xy[, 2] - 600) / 570)^2
  depth <- round(pmax(0, 14 * (1 - oval)), 1)
  depth[oval >= 1] <- NA
  depth[abs(xy[, 1] - 900) < 45 & xy[, 2] > 480] <- NA
  depth[!is.na(depth)] <- pmax(depth[!is.na(depth)], 1)
  terra::values(grid) <- depth
  names(grid) <- "depth_m"
  grid
}

# The toy lake's simulation curve: d50 250 m and d05 600 m at every depth.
toy_spec <- function() range_spec(1, 250, 600, 14, 250, 600)

# The array's detection probability per cell of a coverage result with every
# receiver deaf to cells behind land (blocked = -Inf). The app does not
# compute it; the tests use it as a lower bound on the app's numbers.
deaf_p <- function(cov) {
  grid <- terra::unwrap(cov$grid)
  rx <- cov$receivers[, c("station_id", "x", "y", "deploy_z")]
  d <- detfield::water_distances(grid, rx, max_distance = cov$max_distance)
  model <- detfield::de_logistic(cov$spec$d50, cov$spec$d05, cov$spec$depth)
  deaf <- detfield::de_field(d, detfield::de_model(model, blocked = -Inf))
  listening <- data.frame(station_id = deaf$receivers, date = as.Date("2000-01-01"))
  p <- detfield::system_de(deaf, listening)$p
  stopifnot(identical(deaf$cells, cov$cells))
  p
}

# The app-default curve every real lake opens on, written out here so a change
# to range_presets shows as a failing test.
default_spec <- function() range_spec(1, 250, 500, 35, 750, 1500)

# Colour words a caption may not hold: the figure or the map key names each
# mark. Case-insensitive, whole words.
colour_words <- paste0("(?i)\\b(red|orange|yellow|green|blue|purple|magenta|pink|grey|gray|",
                       "black|white|pale|dark|darker|light|lighter)\\b")

# A study area with a peninsula, small enough to keep tests fast.
small_area <- function() new_study_area(arraydesign::example_lake(), "Example lake")

# The example lake with its shallow water set to 0 m, as Stoney Lake stores
# 56 water cells at 0 m: every cell 3 m deep or less reads 0.
shallow_area <- function() {
  g <- arraydesign::example_lake()
  g <- terra::ifel(!is.na(g) & g <= 3, 0, g)
  new_study_area(g, "Shallow lake")
}

# A small layout on the example lake, scored under a depth-varying curve.
perf_cov <- function() {
  a <- small_area()
  lay <- generate_layout(a, "grid", 4, 1)
  run_coverage(coverage_request(a, lay, new_range_model(range_spec(1, 150, 450, 20, 300, 700))))
}

coverage_session_args <- function(a, cache = cachem::cache_mem()) {
  list(area = shiny::reactiveVal(a), layout = shiny::reactiveVal(generate_layout(a, "grid", 4, 1)),
       range_model = shiny::reactiveVal(new_range_model(toy_spec())), cache = cache)
}

# The distance at which a logistic through 0.5 at d50 and 0.05 at d05 falls
# to 0.001, worked out by hand: logit(p) = logit(0.05) (d - d50) / (d05 - d50).
hand_cutoff <- function(spec) {
  ceiling(max(spec$d50 + (spec$d05 - spec$d50) * log(0.001 / 0.999) / log(0.05 / 0.95)))
}

# The package path, written without any app code: place, measure, field,
# array probability. The regression tests compare the app with this.
package_coverage <- function(grid, layout, spec, max_distance = hand_cutoff(spec)) {
  rx <- as.data.frame(layout)[, c("station_id", "x", "y", "deploy_z")]
  d <- detfield::water_distances(grid, rx, max_distance = max_distance)
  field <- detfield::de_field(d, detfield::de_logistic(spec$d50, spec$d05, spec$depth))
  listening <- data.frame(station_id = field$receivers, date = as.Date("2024-06-01"))
  list(p = detfield::system_de(field, listening)$p, field = field)
}

# A map module stand-in: reactiveVals the test sets to play clicks.
fake_map <- function() {
  click <- shiny::reactiveVal(NULL)
  remove <- shiny::reactiveVal(NULL)
  list(click = click, remove = remove)
}

# ---- Regression helpers ----
# testServer() evaluates its block inside the server, where `session` and
# `app` exist; the linter cannot see that.
# nolint start: object_usage_linter.
app_coverage_for <- function(areas, area_name, n, spec) {
  out <- new.env()
  # testServer() takes no arguments for an app's server, so a wrapper passes
  # the areas, as run_app() does.
  server <- function(input, output, session) {
    app <- app_server(input, output, session, areas = areas)
  }
  shiny::testServer(server, {
    inputs <- default_inputs()
    inputs$`area-choice` <- area_name
    inputs$`array-n` <- n
    # The area first: a new area resets a named starting curve, as a user
    # choosing a lake and then typing a curve would see.
    do.call(session$setInputs, inputs[!grepl("^range-", names(inputs))])
    curve <- list(spec$depth[1], spec$d50[1], spec$d05[1], spec$depth[2], spec$d50[2],
                  spec$d05[2])
    names(curve) <- paste0("range-", range_inputs)
    do.call(session$setInputs, curve)
    session$setInputs(`array-generate` = 1)
    session$setInputs(`coverage-compute` = 1)
    wait_for_task(session)
    out$cov <- app$coverage$coverage()
    out$layout <- app$layout()
  })
  as.list(out)
}
# nolint end

# The check is per cell: the same cells in the same order,
# the same probability in each, and the same shares.
expect_app_matches_package <- function(app, grid, spec) {
  pkg <- package_coverage(grid, app$layout, spec)
  testthat::expect_identical(app$cov$cells, pkg$field$cells)
  max_diff <- max(abs(app$cov$p_array - pkg$p))
  testthat::expect_lte(max_diff, 1e-9)
  single <- arraydesign::coverage_summary(pkg$field, thresholds = 0.5)
  tab <- coverage_table(app$cov, 0.5)
  diff_pp <- 100 * abs(tab$array_share - mean(pkg$p >= 0.5))
  testthat::expect_lte(diff_pp, 0.01)
  testthat::expect_lte(100 * abs(tab$single_share - single$share), 0.01)
  invisible(list(app = tab$array_share, package = mean(pkg$p >= 0.5), diff_pp = diff_pp,
                 max_diff = max_diff, single_app = tab$single_share,
                 single_package = single$share))
}

# A hand oracle with no detfield code: straight-line distance, a logistic
# through d50 and d05 interpolated linearly in receiver depth and held
# outside the two depths, 0 beyond the cutoff, and 1 - prod(1 - p).
oracle_p <- function(xy, layout, spec, cutoff) {
  p <- vapply(seq_len(nrow(layout)), function(i) {
    d <- sqrt((xy[, 1] - layout$x[i])^2 + (xy[, 2] - layout$y[i])^2)
    z <- min(max(layout$deploy_z[i], spec$depth[1]), spec$depth[2])
    w <- (z - spec$depth[1]) / (spec$depth[2] - spec$depth[1])
    a <- spec$d50[1] + w * (spec$d50[2] - spec$d50[1])
    b <- spec$d05[1] + w * (spec$d05[2] - spec$d05[1])
    ifelse(d <= cutoff, 1 / (1 + exp(-log(0.05 / 0.95) * (d - a) / (b - a))), 0)
  }, numeric(nrow(xy)))
  1 - apply(1 - matrix(p, nrow(xy)), 1, prod)
}

# Cells whose straight line to every receiver stays on water with a margin:
# points a twentieth of a cell apart, on the line and a quarter cell to each
# side, all fall in water cells. The margin keeps out lines that graze a land
# corner, which detfield routes around. On the cells kept, the distance
# through water is the straight-line distance.
clear_sight_cells <- function(grid, cells, layout) {
  vals <- terra::values(grid, mat = FALSE)
  xy <- terra::xyFromCell(grid, cells)
  res <- terra::res(grid)[1]
  clear <- rep(TRUE, length(cells))
  for (i in seq_len(nrow(layout))) {
    dx <- xy[, 1] - layout$x[i]
    dy <- xy[, 2] - layout$y[i]
    len <- pmax(sqrt(dx^2 + dy^2), 1e-9)
    for (side in c(-0.25, 0, 0.25)) {
      ox <- -dy / len * side * res
      oy <- dx / len * side * res
      for (f in seq(0, 1, length.out = ceiling(max(len) / (res / 20)) + 1)) {
        pt <- cbind(layout$x[i] + f * dx + ox, layout$y[i] + f * dy + oy)
        clear <- clear & !is.na(vals[terra::cellFromXY(grid, pt)])
      }
    }
  }
  clear
}

# ---- Browser helpers ----
# Shiny draws a port from 3000 to 8000 when none is given. The tests keep
# their apps above 4000, clear of the local services that listen below it.
# Browsers refuse some ports, so the candidates leave out the 11 of them from
# 4001 to 8000 that shiny::runApp() also skips.
# The draw leaves the random seed of the tests as it was, so the first free
# port of the same 50 candidates is taken each time.
browser_refused_ports <- c(4045, 5060, 5061, 6000, 6566, 6665:6669, 6697)

app_port <- function() {
  candidates <- setdiff(4001:8000, browser_refused_ports)
  for (port in withr::with_preserve_seed(sample(candidates, 50))) {
    sock <- tryCatch(serverSocket(port), error = function(e) NULL)
    if (!is.null(sock)) {
      close(sock)
      return(port)
    }
  }
  stop("no free port from 4001 to 8000 in 50 tries")
}

start_app <- function(name, app = run_app(), width = 1400, height = 900) {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  testthat::skip_if(is.null(chromote::find_chrome()), "no Chrome")
  testthat::skip_if(!nzchar(system.file(package = "arraydesignR")), "arraydesignR not installed")
  shinytest2::AppDriver$new(app, name = name, height = height, width = width,
                            load_timeout = 60000, timeout = 30000,
                            shiny_args = list(port = app_port()))
}

# Compute coverage is a bslib task button. app$click(input = ) sets an input
# through its binding, and shinytest2 before 0.4.0 turns that into a click only
# for shiny's actionButton binding: on the task button it did nothing. A click
# on the element is what a user does, and works in every shinytest2 version.
click_task_button <- function(app, id) {
  app$click(selector = paste0("#", id))
}

compute_now <- function(app) {
  click_task_button(app, "coverage-compute")
  wait_current(app)
}

# The number of error outputs on the Simulation tab's map card. A silent
# req() leaves the card without the class; a raw R error or a validation
# message adds it.
map_errors <- function(app) {
  app$get_js("document.querySelectorAll('#score-map.shiny-output-error').length")
}

# Waits for the green banner of a current coverage result.
wait_current <- function(app) {
  app$wait_for_js("$('#coverage-banner .alert-success').length > 0", timeout = 30000)
}

# The app opens with no receivers. Generate places the default 20 on the
# grid; Compute coverage then scores them.
generate_and_compute <- function(app) {
  app$wait_for_value(output = "array-count", ignore = list(NULL, ""))
  empty <- app$get_value(output = "array-count")
  app$click("array-generate")
  app$wait_for_value(output = "array-count", ignore = list(NULL, "", empty))
  compute_now(app)
}

# The first screen after Compute coverage, at the window the app was started
# with: Stoney Lake at zoom 12 (a 2 km scale bar) and wholly inside the map,
# the scale bar's bottom edge at least 8 px above the window's, no page
# scroll, and the colour key to the right of the map. The 500 px map fits the
# lake at zoom 12 with 12 px to spare.
expect_first_screen <- function(app) {
  lake <- area_bounds(stoney_area())
  js <- paste(
    "(() => { const m = HTMLWidgets.find('#map-map').getMap();",
    "const lake = L.latLngBounds([%f, %f], [%f, %f]);",
    "const s = document.querySelector('#map-map .leaflet-control-scale').getBoundingClientRect();",
    "const k = document.querySelector('#map-key .map-key-box').getBoundingClientRect();",
    "const r = document.getElementById('map-map').getBoundingClientRect();",
    "return {zoom: m.getZoom(), contains: m.getBounds().contains(lake),",
    "scale: document.querySelector('#map-map .leaflet-control-scale-line').textContent,",
    "margin: window.innerHeight - s.bottom, scroll: [window.scrollY, ",
    "document.documentElement.scrollHeight - window.innerHeight],",
    "key: [k.left - r.right, k.top - r.top, r.bottom - k.bottom]}; })()"
  )
  got <- app$get_js(sprintf(js, lake[3], lake[1], lake[4], lake[2]))
  testthat::expect_equal(got$zoom, 12)
  testthat::expect_identical(got$scale, "2 km")
  testthat::expect_true(got$contains)
  testthat::expect_gte(got$margin, 8)
  testthat::expect_equal(unlist(got$scroll), c(0, 0))
  # Beside the map: right of it, its top and bottom within the map's.
  key <- unlist(got$key)
  testthat::expect_gte(key[1], 0)
  testthat::expect_gte(key[2], 0)
  testthat::expect_gte(key[3], 0)
  invisible(got)
}
