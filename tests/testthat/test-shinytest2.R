# Browser flows: what testServer() cannot reach (the task button, uploads in a
# real browser, the rendered banner, layout and colour key). They need Chrome and
# an installed arraydesignR, so they run in R CMD check and are skipped on CRAN.

test_that("the app opens with no receivers; Generate and Compute give the package's number", {
  app <- start_app("default-array")
  withr::defer(app$stop())
  # The app opens on the bundled Stoney Lake raster with no receivers and no
  # coverage, on the app-default range curve.
  app$wait_for_value(output = "array-count", ignore = list(NULL, ""))
  expect_identical(app$get_value(output = "array-count"), "0 receivers on Stoney Lake (100 m).")
  expect_match(app$get_text("#coverage-banner_map"), "No coverage yet", fixed = TRUE)
  expect_identical(app$get_value(input = "map-layer"), "depth")
  generate_and_compute(app)
  expect_identical(app$get_value(output = "array-count"), "20 receivers on Stoney Lake (100 m).")
  shown <- app$get_text("#coverage-summary")
  lay <- generate_layout(stoney_area(), "grid", 20, 1)
  pkg <- package_coverage(terra::unwrap(stoney_area()$grid), lay, default_spec())
  expect_match(shown, sprintf("%s of the water covered", fmt_pct(mean(pkg$p >= 0.5))),
               fixed = TRUE)
  expect_match(shown, sprintf("%s of 4,818 water cells", fmt_int(sum(pkg$p >= 0.5))), fixed = TRUE)
  expect_match(shown, "Range model: App defaults, unsourced")
  expect_false(grepl("behind land", shown))
})

test_that("an uploaded CSV layout replaces the array", {
  app <- start_app("csv-upload")
  withr::defer(app$stop())
  app$wait_for_value(output = "array-count", ignore = list(NULL, ""))
  path <- file.path(withr::local_tempdir(), "three.csv")
  write_layout(generate_layout(stoney_area(), "random", 3, 7), path)
  app$upload_file(`array-csv` = path)
  app$wait_for_value(output = "array-count",
                     ignore = list(NULL, "", "0 receivers on Stoney Lake (100 m)."))
  expect_identical(app$get_value(output = "array-count"), "3 receivers on Stoney Lake (100 m).")
})

test_that("changing the range model after computing turns the banner amber", {
  app <- start_app("banner")
  withr::defer(app$stop())
  generate_and_compute(app)
  app$set_inputs(`range-d50_1` = 300)
  app$wait_for_js("$('#coverage-banner .alert-warning').length > 0", timeout = 10000)
  expect_match(app$get_text("#coverage-banner"), "the range model changed")
  compute_now(app)
  expect_match(app$get_text("#coverage-banner"),
               "Coverage is from the specified receiver arrangement and detection range model",
               fixed = TRUE)
  expect_match(app$get_text("#coverage-summary"), "Range model: Your values")
})

test_that("at 1280 x 720 Compute coverage is in view without scrolling the sidebar", {
  app <- start_app("small-screen", width = 1280, height = 720)
  withr::defer(app$stop())
  generate_and_compute(app)
  box <- app$get_js(paste(
    "(() => { const s = document.querySelector('.bslib-sidebar-layout > .sidebar');",
    "const b = document.getElementById('coverage-compute').getBoundingClientRect();",
    "const r = s.getBoundingClientRect();",
    "return [b.top, b.bottom, r.top, Math.min(r.bottom, window.innerHeight), s.scrollTop]; })()"
  ))
  box <- unlist(box)
  expect_gte(box[1], box[3])
  expect_lte(box[2], box[4])
  expect_equal(box[5], 0)
})

test_that("at 1280 x 720, after Compute coverage, one screen shows layer, number and scale bar", {
  app <- start_app("first-screen", width = 1280, height = 720)
  withr::defer(app$stop())
  generate_and_compute(app)
  app$wait_for_value(input = "map-layer", ignore = list(NULL, "depth"))
  expect_identical(app$get_value(input = "map-layer"), "p_array")
  key <- "document.querySelector('#map-key .map-key-box')"
  app$wait_for_js(sprintf("%s !== null && %s.innerText.includes('probability')", key, key),
                  timeout = 10000)
  # 13.4% and 648 of 4,818 cells: the package path for the default layout
  # (test-regression.R).
  expect_match(app$get_text("#coverage-banner_map"), "^\\s*13.4% of the water covered")
  expect_match(app$get_text("#coverage-banner_map"), "648 of 4,818 water cells", fixed = TRUE)
  expect_false(grepl("Range model", app$get_text("#coverage-banner_map")))
  got <- unlist(app$get_js(paste(
    "(() => { const b = document.querySelector('#coverage-banner_map .alert')",
    ".getBoundingClientRect();",
    "const c = document.getElementById('map-map').closest('.card').getBoundingClientRect();",
    "return [b.top, b.bottom, c.top]; })()"
  )))
  expect_gte(got[1], 0)
  expect_gt(got[2], got[1])
  # The banner and the map card do not touch.
  expect_gte(got[3] - got[2], 8)
  expect_first_screen(app)
})

test_that("the probability key runs from 1 at the top to 0 at the bottom, beside the map", {
  app <- start_app("legend")
  withr::defer(app$stop())
  generate_and_compute(app)
  app$wait_for_value(input = "map-layer", ignore = list(NULL, "depth"))
  key <- "document.querySelector('#map-key .map-key-box')"
  app$wait_for_js(sprintf("%s !== null && %s.innerText.includes('probability')", key, key),
                  timeout = 10000)
  labels <- app$get_js(paste(
    "Array.from(document.querySelectorAll('#map-key .map-key-label'))",
    ".map(t => [t.textContent, t.getBoundingClientRect().top])"
  ))
  text <- vapply(labels, `[[`, "", 1)
  top <- vapply(labels, function(l) as.numeric(l[[2]]), 1)
  expect_identical(text[which.min(top)], "1.0")
  expect_identical(text[which.max(top)], "0.0")
  # The key lies wholly to the right of the map, so it covers none of it.
  got <- unlist(app$get_js(paste(
    "(() => { const k = document.querySelector('#map-key .map-key-box').getBoundingClientRect();",
    "const m = document.getElementById('map-map').getBoundingClientRect();",
    "return [k.left, m.right, document.querySelectorAll('#map-map .info.legend').length]; })()"
  )))
  expect_gte(got[1], got[2])
  expect_equal(got[3], 0)
  # The same first screen as at 1280 x 720, at the 1400 x 900 window.
  expect_first_screen(app)
})

test_that("on a phone the key sits below the map; the table scrolls in its card", {
  app <- start_app("phone", width = 390, height = 844)
  withr::defer(app$stop())
  generate_and_compute(app)
  app$wait_for_value(input = "map-layer", ignore = list(NULL, "depth"))
  app$wait_for_js("document.querySelector('#map-key .map-key-box') !== null", timeout = 10000)
  got <- unlist(app$get_js(paste(
    "(() => { const k = document.querySelector('#map-key .map-key-box').getBoundingClientRect();",
    "const m = document.getElementById('map-map').getBoundingClientRect();",
    "const t = document.querySelector('.table-responsive');",
    "return [k.top - m.bottom, document.documentElement.scrollWidth,",
    "window.innerWidth, t.scrollWidth, t.clientWidth]; })()"
  )))
  expect_gte(got[1], 0)
  expect_lte(got[2], got[3])
  expect_gte(got[4], got[5])
})

test_that("a second lake keeps the app defaults in the browser, named as unsourced", {
  path <- file.path(withr::local_tempdir(), "example.tif")
  terra::writeRaster(arraydesign::example_lake(), path)
  app <- start_app("second-lake", app = run_app(areas = list(Example = path)))
  withr::defer(app$stop())
  app$wait_for_idle()
  # The app opens on the first area given, so the test reads the area. Setting
  # an input to its current value changes no output.
  expect_identical(app$get_value(input = "area-choice"), "Example")
  app$wait_for_value(output = "array-count", ignore = list(NULL, ""))
  expect_identical(app$get_value(output = "array-count"), "0 receivers on Example.")
  expect_equal(app$get_value(input = "range-d50_1"), 250)
  expect_equal(app$get_value(input = "range-d05_2"), 1500)
  app$run_js("document.querySelector(\"a[data-value='Range model']\").click()")
  app$wait_for_value(output = "range-name", ignore = list(NULL, ""))
  expect_match(app$get_value(output = "range-name"), "^App defaults, unsourced")
})

test_that("the Simulation tab simulates on the default layout and names its movement model", {
  app <- start_app("simulation")
  withr::defer(app$stop())
  generate_and_compute(app)
  # Outputs on a hidden tab are suspended, so open the tab first.
  app$click(selector = "a[data-value='Simulation']")
  app$wait_for_js("$('#score-movement_line').text().length > 0", timeout = 10000)
  expect_match(app$get_text("#score-movement_line"),
               "Correlated random walk: step 30 \u00b1 15 m per 60 s, turn 0 \u00b1 40\u00b0",
               fixed = TRUE)
  expect_match(app$get_text("#score-banner"), "No simulation yet")
  expect_identical(map_errors(app), 0L)
  app$set_inputs(`score-n_paths` = 5, `score-hours` = 2, `score-seed` = 1)
  click_task_button(app, "score-score")
  app$wait_for_js("$('#score-banner .alert-success').length > 0", timeout = 60000)
  expect_match(app$get_text("#score-banner"),
               "Simulation is from the specified receiver arrangement and detection range model",
               fixed = TRUE)
  first <- names(study_areas())[1]
  expect_true(startsWith(trimws(app$get_text("#score-line")),
                         sprintf("%s: 5 animals for 2 hours each, seed 1;", first)))
  boxes <- app$get_text("#score-boxes")
  for (title in c("Mean detection rate", "Median detection rate",
                  "Animal-hours with a detection")) {
    expect_match(boxes, title, fixed = TRUE)
  }
  expect_match(boxes, "of 10 animal-hours", fixed = TRUE)
  expect_false(grepl("seeds", boxes, fixed = TRUE))
  for (id in c("score-distribution", "score-map")) {
    app$wait_for_js(sprintf("$('#%s img').length > 0", id), timeout = 10000)
  }
  expect_match(app$get_text("#score-table"), "All animals", fixed = TRUE)
  app$set_inputs(`score-step_mean` = 25)
  expect_match(app$get_text("#score-movement_line"), "step 25 \u00b1 15 m per 60 s", fixed = TRUE)
  app$wait_for_js("$('#score-banner .alert-warning').length > 0", timeout = 10000)
  expect_match(app$get_text("#score-banner"), "the simulation settings changed")
})

test_that("the Simulation map shows no error before a run, and none after a switch of lake", {
  path <- file.path(withr::local_tempdir(), "example.tif")
  terra::writeRaster(arraydesign::example_lake(), path)
  app <- start_app("simulation-map", app = run_app(areas = list(Example = path)))
  withr::defer(app$stop())
  app$wait_for_value(output = "array-count", ignore = list(NULL, ""))
  app$click(selector = "a[data-value='Simulation']")
  app$wait_for_js("$('#score-banner').text().length > 0", timeout = 10000)
  expect_identical(map_errors(app), 0L)
  app$click("array-generate")
  app$wait_for_value(output = "array-count", ignore = list(NULL, "", "0 receivers on Example."))
  expect_identical(map_errors(app), 0L)
  app$set_inputs(`score-n_paths` = 3, `score-hours` = 1)
  click_task_button(app, "score-score")
  app$wait_for_js("$('#score-banner .alert-success').length > 0", timeout = 60000)
  app$wait_for_js("$('#score-map img').length > 0", timeout = 10000)
  expect_identical(map_errors(app), 0L)
  # After a switch the last result belongs to the other lake: the map card is
  # empty and shows no error.
  app$set_inputs(`area-choice` = "Stoney Lake (100 m)")
  app$wait_for_value(output = "array-count", ignore = list(NULL, "", "3 receivers on Example.",
                                                           "20 receivers on Example."))
  expect_identical(map_errors(app), 0L)
  expect_identical(app$get_js("$('#score-map img').length"), 0L)
  # The banner does not claim a result that the card does not show.
  expect_false(grepl("results on screen", app$get_text("#score-banner")))
})

test_that("an upload whose CRS scale cannot be measured is refused, and the session stays", {
  app <- start_app("engcrs-upload")
  withr::defer(app$stop())
  app$wait_for_value(output = "array-count", ignore = list(NULL, ""))
  local <- terra::rast(xmin = 0, xmax = 1000, ymin = 0, ymax = 1000, resolution = 100,
                       crs = "local", vals = 10)
  local[1] <- NA
  path <- file.path(withr::local_tempdir(), "local.tif")
  terra::writeRaster(local, path)
  app$upload_file(`area-upload` = path)
  app$wait_for_js("$('#area-refused .alert-danger').length > 0", timeout = 30000)
  shown <- app$get_text("#area-refused")
  expect_match(shown, "Upload refused.", fixed = TRUE)
  expect_match(shown, paste("The scale of this CRS at the raster centre cannot be measured, so",
                            "give the raster in a projected CRS with a geographic datum, such as",
                            "its UTM zone."), fixed = TRUE)
  expect_match(shown, "The app stays on Stoney Lake (100 m).", fixed = TRUE)
  # No terra warning reaches a notification, and the session is still live:
  # the map and the receiver count still answer.
  notes <- app$get_js(paste("Array.from(document.querySelectorAll('.shiny-notification'))",
                            ".map(e => e.innerText).join(' | ')"))
  expect_false(grepl("coordinate operations", notes, fixed = TRUE))
  expect_false(app$get_js("document.getElementById('shiny-disconnected-overlay') !== null"))
  expect_identical(app$get_value(input = "area-choice"), "Stoney Lake (100 m)")
  expect_identical(app$get_value(output = "array-count"), "0 receivers on Stoney Lake (100 m).")
})

test_that("a Web Mercator upload is refused at upload with its scale, and the app stays", {
  app <- start_app("mercator-upload")
  withr::defer(app$stop())
  app$wait_for_value(output = "array-count", ignore = list(NULL, ""))
  path <- file.path(withr::local_tempdir(), "mercator.tif")
  terra::writeRaster(terra::project(arraydesign::example_lake(), "EPSG:3857", method = "near"),
                     path)
  app$upload_file(`area-upload` = path)
  app$wait_for_js("$('#area-refused .alert-danger').length > 0", timeout = 30000)
  shown <- app$get_text("#area-refused")
  expect_match(shown, "Upload refused.", fixed = TRUE)
  expect_match(shown, "scale of 1.401 east-west, 1.405 north-south and 1.403 on both diagonals",
               fixed = TRUE)
  expect_match(shown, "17N (EPSG:32617)", fixed = TRUE)
  expect_match(shown, "The app stays on Stoney Lake (100 m).", fixed = TRUE)
  expect_identical(app$get_value(input = "area-choice"), "Stoney Lake (100 m)")
})
