test_that("the toy lake the tests use reads as 1,690 water cells of 30 m in UTM zone 17N", {
  a <- toy_area()
  expect_s3_class(a, "study_area")
  expect_equal(a$n_water, 1690)
  expect_equal(a$n_cells, 2400)
  expect_equal(a$res, 30)
  expect_identical(a$epsg, 32617L)
  expect_equal(a$depth_range, c(1, 14))
  expect_identical(area_summary(a),
                   "Toy lake: 1,690 water cells of 30 m (EPSG 32617); depth 1 to 14 m.")
  expect_identical(toy_area(), a)
  expect_true(a$synthetic)
  expect_equal(nrow(a$water_xy), 1690)
})

test_that("negative depths read as depth below the surface and the hash follows the values", {
  g <- arraydesign::example_lake()
  pos <- new_study_area(g, "pos")
  neg <- new_study_area(-g, "neg")
  expect_equal(neg$depth_range, pos$depth_range)
  expect_false(identical(neg$hash, pos$hash))
})

test_that("a raster in longitude and latitude is projected to the UTM zone of its centre", {
  g <- terra::rast(nrows = 20, ncols = 30, xmin = -78.3, xmax = -78.27, ymin = 44.48,
                   ymax = 44.50, crs = "EPSG:4326", vals = 5)
  g[1:30] <- NA
  a <- new_study_area(g, "lonlat")
  expect_identical(a$epsg, 32617L)
  expect_gt(a$n_water, 0)
  # 0.001 degrees of longitude at 44.49 N is about 79 m; of latitude, 111 m.
  expect_equal(a$res, 79)
  south <- terra::rast(nrows = 5, ncols = 5, xmin = 150, xmax = 150.05, ymin = -34.05,
                       ymax = -34, crs = "EPSG:4326", vals = 2)
  south[1] <- NA
  expect_identical(new_study_area(south, "south")$epsg, 32756L)
})

test_that("a lon-lat raster lands in the UTM zone of its centre in every form", {
  ll <- terra::project(arraydesign::example_lake(), "EPSG:4326", method = "near")
  e <- as.vector(terra::ext(ll))
  # The same raster moved so that its centre sits at `lon`, `lat`.
  at <- function(lon, lat) {
    g <- ll
    terra::ext(g) <- e + c(rep(lon - mean(e[1:2]), 2), rep(lat - mean(e[3:4]), 2))
    g
  }
  centre <- c(mean(e[1:2]), mean(e[3:4]))
  cases <- list(
    `as given` = list(ll, 32617L), `150 E, 34 S` = list(at(150, -34), 32756L),
    `179.99 E` = list(at(179.99, centre[2]), 32660L), `85 N` = list(at(centre[1], 85), 32617L),
    `5.5 E, 60 N` = list(at(5.5, 60), 32631L), `0 to 360` = list(at(centre[1] + 360, centre[2]),
                                                                 32617L)
  )
  for (nm in names(cases)) {
    a <- suppressMessages(new_study_area(cases[[nm]][[1]], nm))
    expect_identical(a$epsg, cases[[nm]][[2]], label = nm)
    expect_gt(a$n_water, 0)
  }
  # The 0 to 360 form says what it did, by class, and gives the same study
  # area as the -180 to 180 form.
  g360 <- cases[["0 to 360"]][[1]]
  expect_message(new_study_area(g360, "x"), class = "arraydesignR_message")
  expect_message(new_study_area(g360, "x"), "360 degrees less", fixed = TRUE)
  expect_no_message(new_study_area(ll, "x"))
  a180 <- new_study_area(ll, "x")
  a360 <- suppressMessages(new_study_area(g360, "x"))
  expect_identical(a360$n_water, a180$n_water)
  expect_equal(a360$water_xy, a180$water_xy)
  # Depths agree to the float precision of the raster's values.
  expect_equal(a360$water_depth, a180$water_depth, tolerance = 1e-6)
  # A raster across 180 degrees, given from 0 to 360, keeps its longitudes and
  # goes to the zone of its centre: zone 60 west of 180, zone 1 east of it.
  expect_identical(new_study_area(at(179.995, 10), "x")$epsg, 32660L)
  expect_identical(new_study_area(at(180.005, 10), "x")$epsg, 32601L)
  expect_identical(vapply(c(-180, -0.001, 0, 179.999, 180), utm_zone, 1L),
                   c(1L, 30L, 31L, 60L, 60L))
  # A centre at exactly 180 is wrapped to -180 before the zone is taken.
  c180 <- terra::rast(nrows = 4, ncols = 4, xmin = 179.99, xmax = 180.01, ymin = 10,
                      ymax = 10.02, crs = "EPSG:4326", vals = 5)
  c180[1] <- NA
  expect_identical(mean(as.vector(terra::ext(c180))[1:2]), 180)
  expect_identical(new_study_area(c180, "x")$epsg, 32601L)
})

test_that("a lon-lat raster over 12 degrees either way, or out of range, is refused by class", {
  # A raster of `w` by `h` degrees centred at `lon`, `lat`, with one NA cell.
  box <- function(lon, lat, w, h = 1) {
    g <- terra::rast(nrows = 4, ncols = 44, xmin = lon - w / 2, xmax = lon + w / 2,
                     ymin = lat - h / 2, ymax = lat + h / 2, crs = "EPSG:4326", vals = 5)
    g[1] <- NA
    g
  }
  # The refusal and every warning raised before it.
  refusal <- function(g) {
    warned <- 0L
    err <- tryCatch(withCallingHandlers(new_study_area(g, "x"), warning = function(w) {
      warned <<- warned + 1L
      invokeRestart("muffleWarning")
    }), error = identity)
    list(error = err, warned = warned)
  }
  span_msg <- paste("The app projects a study area to one UTM zone, so a raster in longitude",
                    "and latitude may span at most 12 degrees of longitude and 12 degrees of",
                    "latitude. Give a larger area as a raster in a projected coordinate",
                    "reference system in metres; the app takes one.")
  for (w in c(13, 179.9)) {
    r <- refusal(box(-80.5, 45, w))
    expect_s3_class(r$error, "arraydesignR_error")
    expect_match(conditionMessage(r$error), span_msg, fixed = TRUE)
    expect_no_match(conditionMessage(r$error), "0 to 360 form", fixed = TRUE)
    expect_identical(r$warned, 0L)
  }
  expect_s3_class(refusal(box(-80.5, 45, 1, 13))$error, "arraydesignR_error")
  # 11 degrees wide at 45 N reads, in the zone of its centre, 17N.
  eleven <- new_study_area(box(-80.5, 45, 11), "eleven")
  expect_identical(eleven$epsg, 32617L)
  # Out of range on either side: refused before terra reads the coordinates.
  for (lon in c(-200, 400)) {
    r <- refusal(box(lon, 10, 0.2))
    expect_s3_class(r$error, "arraydesignR_error")
    expect_match(conditionMessage(r$error),
                 "Longitudes must lie from -180 to 180 or from 0 to 360", fixed = TRUE)
    expect_identical(r$warned, 0L, label = lon)
  }
  expect_s3_class(refusal(box(-179.95, 10, 0.2))$error, "arraydesignR_error")
  # A lake across 180 degrees at 17 S: 20 by 20 cells of 0.01 degrees.
  w360 <- terra::rast(nrows = 20, ncols = 20, xmin = 179.9, xmax = 180.1, ymin = -17,
                      ymax = -16.8, crs = "EPSG:4326", vals = 5)
  a <- new_study_area(w360, "0 to 360 form")
  expect_identical(a$epsg, 32701L)
  expect_identical(a$n_water, 411L)
  # The same lake from -180 to 180 spans the whole range.
  w180 <- terra::rast(nrows = 20, ncols = 36000, xmin = -180, xmax = 180, ymin = -17,
                      ymax = -16.8, crs = "EPSG:4326")
  m <- matrix(NA_real_, 20, 36000)
  m[, c(1:10, 35991:36000)] <- 5
  terra::values(w180) <- as.vector(t(m))
  err <- expect_error(new_study_area(w180, "-180 to 180 form"),
                      "^The raster spans 360 degrees of longitude and 0.2 of latitude\\.",
                      class = "arraydesignR_error")
  expect_match(conditionMessage(err), paste("The raster reaches 180 degrees: give a lake across",
                                            "180 degrees in the 0 to 360 form."), fixed = TRUE)
  # The bundled raster, in UTM already, passes the check untouched.
  expect_identical(stoney_area()$n_water, 4818L)
  expect_identical(stoney_area()$epsg, 32617L)
})

test_that("a lon-lat span of 12 degrees typed to one decimal reads, and one above it is refused", {
  box <- function(xmin, xmax, ymin = 40, ymax = 41) {
    terra::rast(nrows = 2, ncols = 2, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                crs = "EPSG:4326")
  }
  # 22.1 - 10.1 is 12.000000000000002 in floating point.
  expect_gt(22.1 - 10.1, 12)
  expect_no_error(check_lonlat_extent(box(10.1, 22.1)))
  expect_no_error(check_lonlat_extent(box(10, 11, 40.3, 52.3)))
  # A span over the bound prints with the digits that put it above 12.
  expect_error(check_lonlat_extent(box(10, 22.0001)),
               "^The raster spans 12.0001 degrees of longitude and 1 of latitude\\.",
               class = "arraydesignR_error")
  expect_error(check_lonlat_extent(box(10, 22 + 2e-9)), "spans 12.000000002 degrees",
               fixed = TRUE, class = "arraydesignR_error")
  expect_error(check_lonlat_extent(box(10, 22.1)), "spans 12.1 degrees", fixed = TRUE,
               class = "arraydesignR_error")
  expect_error(check_lonlat_extent(box(10, 11, 40, 52.1)), "and 12.1 of latitude", fixed = TRUE,
               class = "arraydesignR_error")
})

test_that("a projected raster whose scale at its centre is more than 1 percent from 1 is refused", {
  g <- arraydesign::example_lake()
  mercator <- terra::project(g, "EPSG:3857", method = "near")
  err <- expect_error(new_study_area(mercator, "x"), class = "arraydesignR_error")
  expect_match(conditionMessage(err),
               "scale of 1.401 east-west, 1.405 north-south and 1.403 on both diagonals",
               fixed = TRUE)
  expect_match(conditionMessage(err), "UTM zone of its centre, 17N (EPSG:32617)", fixed = TRUE)
  # The unit refusal comes first for a CRS in feet.
  feet <- terra::project(g, "EPSG:2263", method = "near")
  expect_error(new_study_area(feet, "x"), "not in metres", class = "arraydesignR_error")
  # Sinusoidal off its central meridian: 1.000 east-west, 0.991 north-south,
  # sheared on the diagonals.
  square <- function(crs, lon, lat) {
    ctr <- terra::project(matrix(c(lon, lat), 1), from = "EPSG:4326", to = crs)
    r <- terra::rast(terra::ext(ctr[1] - 5000, ctr[1] + 5000, ctr[2] - 5000, ctr[2] + 5000),
                     resolution = 500, crs = crs, vals = 10)
    r[1] <- NA
    r
  }
  sinu <- square("ESRI:54008", 11, 45.6)
  expect_equal(grid_scale(sinu)$scale[1:2], c(1, 0.991), tolerance = 1e-3)
  expect_error(new_study_area(sinu, "x"), "0.934 and 1.071 on the diagonals", fixed = TRUE,
               class = "arraydesignR_error")
  # UTM and a conformal or equal-area CRS within 1 percent read.
  expect_identical(new_study_area(square("EPSG:32632", 11, 45.6), "x")$epsg, 32632L)
  expect_identical(new_study_area(square("EPSG:3035", 11, 45.6), "x")$epsg, 3035L)
  expect_identical(new_study_area(g, "x")$epsg, 32617L)
})

test_that("a CRS whose scale cannot be measured is refused with no terra warning", {
  # An engineering CRS in metres has no geographic datum, so no point of it
  # projects to longitude and latitude. Written to a GeoTIFF it reads back as
  # ENGCRS["Cartesian (Meter)"].
  local <- terra::rast(xmin = 0, xmax = 1000, ymin = 0, ymax = 1000, resolution = 100,
                       crs = "local", vals = 10)
  local[1] <- NA
  path <- file.path(withr::local_tempdir(), "local.tif")
  terra::writeRaster(local, path)
  expect_match(terra::crs(terra::rast(path)), "^ENGCRS")
  expect_true(all(is.na(grid_scale(terra::rast(path))$scale)))
  unmeasured <- paste("The scale of this CRS at the raster centre cannot be measured, so give the",
                      "raster in a projected CRS with a geographic datum, such as its UTM zone.")
  for (g in list(local, path)) {
    expect_no_warning(err <- expect_error(new_study_area(g, "x"), class = "arraydesignR_error"))
    expect_identical(conditionMessage(err), unmeasured)
  }
  # The bundled raster in UTM zone 17N reads; in Web Mercator it is refused with
  # its scale.
  stoney <- terra::rast(system.file("extdata", "stoney_depth.tif", package = "arraydesignR"))
  expect_identical(new_study_area(stoney, "x")$n_water, 4818L)
  expect_error(new_study_area(terra::project(stoney, "EPSG:3857", method = "near"), "x"),
               "scale of 1.401 east-west", fixed = TRUE, class = "arraydesignR_error")
})

test_that("the summary prints one depth once", {
  g <- terra::rast(nrows = 4, ncols = 4, xmin = 700000, xmax = 700400, ymin = 4900000,
                   ymax = 4900400, crs = "EPSG:32617", vals = 5)
  g[1] <- NA
  expect_identical(area_summary(new_study_area(g, "Flat")),
                   "Flat: 15 water cells of 100 m (EPSG 32617); depth 5 m.")
})

test_that("an upload's message becomes a notification", {
  expect_silent(warn_notify(inform_app("moved"), session = NULL))
})

test_that("rectangular cells are resampled to square ones", {
  g <- terra::rast(nrows = 10, ncols = 20, xmin = 0, xmax = 600, ymin = 0, ymax = 400,
                   crs = "EPSG:32617", vals = 3)
  g[1] <- NA
  a <- new_study_area(g, "rect")
  expect_equal(a$res, 30)
  expect_equal(diff(terra::res(terra::unwrap(a$grid))), 0)
})

test_that("bad rasters are refused with a reason", {
  g <- arraydesign::example_lake()
  mixed <- g
  mixed[100:200] <- -mixed[100:200]
  expect_error(new_study_area(mixed, "m"), "mixes depths below and above 0")
  nocrs <- g
  terra::crs(nocrs) <- ""
  expect_error(new_study_area(nocrs, "n"), "no coordinate reference system")
  dry <- g
  dry[] <- NA
  expect_error(new_study_area(dry, "d"), "no water cells")
  ft <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 500, ymin = 0, ymax = 500,
                    crs = "EPSG:2227", vals = 1)
  ft[1] <- NA
  expect_error(new_study_area(ft, "feet"), "not in metres")
  expect_warning(new_study_area(terra::ifel(is.na(g), 1, g), "all"), "no NA cells")
  expect_identical(grid_epsg(terra::rast(nrows = 2, ncols = 2, crs = "")), NA_integer_)
  # Anything other than a raster gives NA, as detfield's reader does.
  expect_identical(grid_epsg(NULL), NA_integer_)
  expect_identical(grid_epsg(data.frame(x = 1)), NA_integer_)
})

test_that("a raster above the water-cell limit is refused with the factor that fits it", {
  g <- arraydesign::example_lake()
  expect_error(new_study_area(g, "big", max_water_cells = 2151),
               "2,152 water cells, above this app's limit of 2,151. Aggregate it by 2")
  expect_error(new_study_area(g, "big", max_water_cells = 500), "Aggregate it by 3 \\(cells of")
  expect_equal(new_study_area(g, "ok", max_water_cells = 2152)$n_water, 2152)
  path <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(g, path)
  expect_error(study_areas(list(Example = path), max_water_cells = 100), "limit of 100")
})

test_that("study_areas() puts the given areas first and reads them once", {
  path <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(arraydesign::example_lake(), path)
  areas <- study_areas(list(Example = path))
  # The app opens on the first area, so a lake given to run_app() is the base.
  expect_named(areas, c("Example", "Stoney Lake (100 m)"))
  expect_equal(areas$Example$n_water, 2152)
  expect_error(study_areas(list(path)), "named list")
  # Stoney Lake is the only bundled raster, so it is the whole list by default.
  expect_named(study_areas(), "Stoney Lake (100 m)")
  # A given area with the bundled lake's name replaces it.
  same <- study_areas(list(`Stoney Lake (100 m)` = path))
  expect_named(same, "Stoney Lake (100 m)")
  expect_equal(same[[1]]$n_water, 2152)
})

test_that("the bundled Stoney Lake raster is the one the 1.x app shipped", {
  a <- stoney_area()
  expect_s3_class(a, "study_area")
  expect_identical(a$name, "Stoney Lake (100 m)")
  expect_false(a$synthetic)
  expect_equal(a$n_water, 4818)
  expect_equal(a$res, 100)
  expect_identical(a$epsg, 32617L)
  expect_match(area_summary(a),
               "^Stoney Lake \\(100 m\\): 4,818 water cells of 100 m \\(EPSG 32617\\)")
  expect_identical(stoney_area(), a)
  # Byte-identical to array_designR/depth_raster.tif, the file the 1.x app
  # loaded on startup (server.R, "Load default raster on startup").
  expect_identical(unname(tools::md5sum(system.file("extdata", "stoney_depth.tif",
                                                    package = "arraydesignR"))),
                   "21ba8e62d4f1fe7050b155a8fa6c59b3")
})

test_that("the bundled raster holds the counts inst/extdata/README.md gives", {
  path <- system.file("extdata", "stoney_depth.tif", package = "arraydesignR")
  v <- terra::values(terra::rast(path))
  expect_equal(sum(!is.na(v)), 4818)
  expect_equal(sum(v == 0, na.rm = TRUE), 56)
  expect_equal(sum(v > 0, na.rm = TRUE), 0)
  readme <- system.file("extdata", "README.md", package = "arraydesignR", mustWork = TRUE)
  text <- paste(readLines(readme), collapse = " ")
  expect_match(text, "Depth is zero or negative metres", fixed = TRUE)
  expect_match(text, "4,818 cells are water, and 56 of them hold 0.", fixed = TRUE)
})

test_that("the study area module serves the chosen area and an upload", {
  areas <- study_areas()
  shiny::testServer(mod_study_area_server, args = list(areas = areas), {
    session$setInputs(choice = "Stoney Lake (100 m)")
    expect_identical(session$returned(), areas[["Stoney Lake (100 m)"]])
    expect_match(output$summary, "4,818 water cells")
    path <- withr::local_tempfile(fileext = ".tif")
    terra::writeRaster(arraydesign::example_lake(), path)
    session$setInputs(upload = list(name = "lake.tif", datapath = path))
    session$setInputs(choice = "Upload: lake.tif")
    expect_equal(session$returned()$n_water, 2152)
    bad <- withr::local_tempfile(fileext = ".tif")
    writeLines("not a raster", bad)
    session$setInputs(upload = list(name = "bad.tif", datapath = bad))
    expect_equal(session$returned()$n_water, 2152)
    expect_match(output$refused$html, "Upload refused. </strong>")
    expect_match(output$refused$html, "The app stays on Upload: lake.tif.")
    session$setInputs(choice = "Stoney Lake (100 m)")
    expect_equal(session$returned()$n_water, 4818)
    expect_null(output$refused)
  })
})

test_that("an ASCII grid upload is refused, named, and shown beside the area", {
  asc <- withr::local_tempfile(fileext = ".asc")
  terra::writeRaster(arraydesign::example_lake(), asc)
  shiny::testServer(mod_study_area_server, args = list(areas = study_areas()), {
    session$setInputs(choice = "Stoney Lake (100 m)")
    session$setInputs(upload = list(name = "lake.asc", datapath = asc))
    expect_identical(session$returned()$name, "Stoney Lake (100 m)")
    expect_match(output$refused$html, "lake.asc is not a GeoTIFF")
    expect_match(output$refused$html, "The app stays on Stoney Lake (100 m).", fixed = TRUE)
  })
  expect_match(as.character(mod_study_area_upload_ui("area")), "accept=\".tif,.tiff\"")
})

test_that("an upload above the water-cell limit is refused in the module", {
  path <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(arraydesign::example_lake(), path)
  args <- list(areas = study_areas(), max_water_cells = 1000)
  shiny::testServer(mod_study_area_server, args = args, {
    session$setInputs(choice = "Stoney Lake (100 m)")
    session$setInputs(upload = list(name = "lake.tif", datapath = path))
    expect_identical(session$returned()$name, "Stoney Lake (100 m)")
    expect_match(output$refused$html, "2,152 water cells, above this app's limit of 1,000")
  })
})

test_that("warnings become notifications and stop there", {
  expect_silent(try_notify(warning("a raster warning"), session = NULL))
  expect_silent(warn_notify(warning("a raster warning"), session = NULL))
  expect_error(warn_notify(stop("refused"), session = NULL), "refused")
})
