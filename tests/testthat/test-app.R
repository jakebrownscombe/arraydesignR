test_that("the app frame builds and run_app() returns an app with the extra areas", {
  ui <- app_ui(NULL)
  html <- as.character(ui)
  for (tab in c("Map", "Range model", "Performance", "Simulation", "Guide")) {
    expect_match(html, tab, fixed = TRUE)
  }
  for (id in c("area-choice", "array-generate", "array-min_depth", "coverage-compute", "map-map",
               "range-d05_2", "performance-table", "score-score")) {
    expect_match(html, sprintf("id=\"%s\"", id), fixed = TRUE)
  }
  path <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(arraydesign::example_lake(), path)
  app <- run_app(areas = list(Example = path))
  expect_s3_class(app, "shiny.appobj")
  expect_named(environment(app$serverFuncSource())$loaded, c("Example", "Stoney Lake (100 m)"))
  expect_error(run_app(areas = list(path)), "named list")
  expect_error(run_app(areas = list(Example = path), max_water_cells = 100), "limit of 100")
  expect_error(run_app(max_pairs = NA), "at least 1")
  # One receiver on Stoney Lake takes 4,818 distances, the floor of max_pairs.
  expect_s3_class(run_app(max_pairs = 4818), "shiny.appobj")
  err <- tryCatch(run_app(max_pairs = 4817), error = identity)
  expect_s3_class(err, "arraydesignR_error")
  expect_match(conditionMessage(err), paste("`max_pairs` must be at least 4,818: one receiver on",
                                            "Stoney Lake (100 m) takes 4,818"), fixed = TRUE)
  expect_s3_class(run_app(areas = list(Example = path), max_pairs = 4818), "shiny.appobj")
  expect_s3_class(run_app(), "shiny.appobj")
  expect_identical(check_pairs_floor(1e6, study_areas()), 4818)
  expect_identical(app_limits(), list(water_cells = 250000, pairs = 1e6))
})

test_that("on a phone the controls sit above the map, and the key below it", {
  html <- as.character(app_ui(NULL))
  expect_match(html, "data-open-mobile=\"always-above\"", fixed = TRUE)
  expect_match(html, "data-open-desktop=\"open\"", fixed = TRUE)
  css <- as.character(sass::as_sass(app_theme()))
  expect_match(css, ".map-key { flex: 0 0 130px;", fixed = TRUE)
  expect_match(css, "@media (max-width: 576px) { .map-row { flex-direction: column; }",
               fixed = TRUE)
})

test_that("the app opens on Stoney Lake with no receivers; Generate and Compute give 13.4%", {
  shiny::testServer(app_server, {
    session$setInputs(`area-choice` = "Stoney Lake (100 m)", `map-layer` = "depth")
    wait_for_task(session)
    expect_null(layout())
    expect_identical(coverage$state()$kind, "none")
    expect_match(output$`array-count`, "^0 receivers on Stoney Lake \\(100 m\\).")
    expect_match(output$`coverage-banner_map`$html,
                 "No coverage yet. Place receivers and press Compute coverage.", fixed = TRUE)
    session$setInputs(`array-generate` = 1)
    wait_for_task(session)
    expect_equal(nrow(layout()), 20)
    expect_null(coverage$coverage())
    session$setInputs(`coverage-compute` = 1, `map-layer` = "p_array")
    wait_for_task(session)
    expect_identical(coverage$state()$kind, "current")
    # 13.4% and 648 of 4,818 cells come from the package path (package_coverage()
    # in helper-app.R, no app code) for 20 receivers on the regular grid and the
    # app-default curve; test-regression.R checks the same layout cell by cell.
    # The numbers rest on receivers at cell centres and on detfield's exact
    # path through water.
    pkg <- package_coverage(terra::unwrap(stoney_area()$grid), layout(), default_spec())
    expect_identical(fmt_pct(mean(pkg$p >= 0.5)), "13.4%")
    expect_equal(sum(pkg$p >= 0.5), 648)
    summary <- output$`coverage-summary`$html
    expect_match(summary, "<strong>13.4% of the water covered</strong>", fixed = TRUE)
    for (point in c("648 of 4,818 water cells", "20 receivers",
                    "Range model: App defaults, unsourced",
                    "d50 250 m, d05 500 m at 1 m; d50 750 m, d05 1,500 m at 35 m")) {
      expect_match(summary, sprintf("<li>%s</li>", point), fixed = TRUE)
    }
    expect_false(grepl("behind land", summary))
    expect_match(output$`map-caption`, "Range model: App defaults, unsourced")
    expect_match(output$`map-caption`, "20 receivers placed")
    expect_no_match(output$`map-caption`, colour_words, perl = TRUE)
    expect_match(output$`map-key`$html, ">Receiver</span>", fixed = TRUE)
    banner <- output$`coverage-banner_map`$html
    expect_match(banner, "<strong>13.4% of the water covered</strong>", fixed = TRUE)
    expect_match(banner, "648 of 4,818 water cells", fixed = TRUE)
    expect_false(grepl("Range model|behind land", banner))
    expect_false(grepl("behind land", output$`performance-boxes`$html))
    expect_identical(score$state()$kind, "none")
  })
})

test_that("the whole app runs headless from a default array to a current coverage", {
  shiny::testServer(app_server, {
    do.call(session$setInputs, default_inputs())
    session$setInputs(`array-generate` = 1)
    expect_equal(nrow(layout()), 20)
    session$setInputs(`coverage-compute` = 1)
    wait_for_task(session)
    cov <- coverage$coverage()
    expect_identical(coverage$state()$kind, "current")
    expect_equal(length(cov$cells), 4818)
    click <- list(lng = layout()$deploy_lon[1] + 0.001, lat = layout()$deploy_lat[1])
    session$setInputs(`map-map_click` = click)
    expect_equal(nrow(layout()), 21)
    expect_match(paste(coverage$state()$reasons), "for 20 receivers and the layout now has 21")
    session$setInputs(`map-map_marker_click` = list(id = "S021"))
    expect_identical(coverage$state()$kind, "current")
    session$setInputs(`range-d50_1` = 300)
    expect_identical(coverage$state()$reasons, "the range model changed")
  })
})

test_that("every lake the app offers starts on the app defaults, named as unsourced", {
  path <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(arraydesign::example_lake(), path)
  areas <- study_areas(list(Example = path))
  server <- function(input, output, session) {
    app <- app_server(input, output, session, areas = areas)
  }
  shiny::testServer(server, {
    session$setInputs(`area-choice` = "Stoney Lake (100 m)")
    wait_for_task(session)
    expect_identical(app$range_model()$spec, default_spec())
    session$setInputs(`area-choice` = "Example")
    session$setInputs(`array-generate` = 1)
    session$setInputs(`coverage-compute` = 1)
    wait_for_task(session)
    expect_identical(app$range_model()$spec, default_spec())
    expect_match(output$`coverage-summary`$html,
                 "Range model: App defaults, unsourced")
    expect_match(output$`performance-range`, "^App defaults, unsourced")
  })
})

test_that("app.R opens on the bundled Stoney Lake raster, the only lake it offers", {
  entry <- testthat::test_path("..", "..", "app.R")
  skip_if(!file.exists(entry), "app.R is not in the built package")
  # app.R sets a multisession plan. The test needs only the areas, so the
  # plan is mocked and no worker starts.
  local_mocked_bindings(plan = function(...) invisible(NULL), .package = "future")
  dir <- withr::local_tempdir()
  file.copy(entry, file.path(dir, "app.R"))
  app <- withr::with_dir(dir, source("app.R", local = new.env())$value)
  expect_named(environment(app$serverFuncSource())$loaded, "Stoney Lake (100 m)")
  # The raster ships inside the package, so nothing is ignored at build.
  ignore <- readLines(testthat::test_path("..", "..", ".Rbuildignore"))
  expect_false(any(grepl("stoney_depth", ignore)))
})

test_that("the run-time Imports that are never called by name are installed", {
  expect_true(all(vapply(needed_at_run_time(), is.function, logical(1))))
})

test_that("the app object run_app() returns serves Stoney Lake", {
  shiny::testServer(run_app(), {
    session$setInputs(`area-choice` = "Stoney Lake (100 m)")
    wait_for_task(session)
    expect_match(output$`array-count`, "^0 receivers on Stoney Lake \\(100 m\\).")
    session$setInputs(`array-generate` = 1)
    expect_match(output$`array-count`, "^20 receivers on Stoney Lake \\(100 m\\).")
  })
})

test_that("after a switch of study area no tab shows the last lake's numbers", {
  path <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(arraydesign::example_lake(), path)
  areas <- study_areas(list(Example = path))
  server <- function(input, output, session) {
    app <- app_server(input, output, session, areas = areas)
  }
  shiny::testServer(server, {
    session$setInputs(`area-choice` = "Stoney Lake (100 m)", `score-n_paths` = 3,
                      `score-hours` = 2, `score-seed` = 1)
    session$setInputs(`array-generate` = 1)
    session$setInputs(`coverage-compute` = 1)
    wait_for_task(session)
    expect_identical(output$`performance-context`, "Stoney Lake (100 m), 20 receivers.")
    session$setInputs(`score-score` = 1)
    wait_for_task(session)
    # The Simulation tab's own wording belongs to its module; here it only has to
    # show a result, and then withhold it after the switch.
    expect_match(output$`score-boxes`$html, "bslib-value-box", fixed = TRUE)
    session$setInputs(`area-choice` = "Example")
    # The switch clears the layout: a layout belongs to the raster it was made on.
    expect_null(app$layout())
    expect_match(output$`array-count`, "^0 receivers on Example")
    note <- paste("No coverage for Example yet: the last result is for Stoney Lake (100 m).",
                  "Press Compute coverage.")
    expect_identical(as.character(output$`coverage-summary`$html), note)
    expect_error(output$`performance-boxes`, note, fixed = TRUE)
    expect_error(output$`performance-table`, note, fixed = TRUE)
    expect_error(output$`performance-histogram`, note, fixed = TRUE)
    expect_error(output$`score-boxes`,
                 "for Example yet: the last result is for Stoney Lake (100 m).", fixed = TRUE)
    # The map draws nothing until there is a simulation for this lake: its
    # req() is silent, so no error shows in the card.
    expect_error(output$`score-map`, class = "shiny.silent.error")
    # Nothing of the last lake is on screen, so no banner says so.
    for (b in c("score-banner", "coverage-banner", "coverage-banner_map")) {
      expect_false(grepl("results on screen", output[[b]]$html), info = b)
    }
    line <- output$`score-line`
    expect_false(grepl("Stoney", if (is.list(line)) as.character(line$html) else line))
    session$setInputs(`array-generate` = 1)
    session$setInputs(`coverage-compute` = 1)
    wait_for_task(session)
    expect_identical(output$`performance-context`, "Example, 20 receivers.")
    expect_match(output$`coverage-summary`$html, "of the water covered")
    session$setInputs(`area-choice` = "Stoney Lake (100 m)")
    expect_error(output$`performance-boxes`, "the last result is for Example")
  })
})

test_that("app_port() draws above 4000 and leaves out the ports browsers refuse", {
  expect_length(browser_refused_ports, 11)
  expect_true(all(browser_refused_ports > 4000 & browser_refused_ports <= 8000))
  port <- app_port()
  expect_gt(port, 4000)
  expect_false(port %in% browser_refused_ports)
})

test_that("the app opens on the first area given", {
  path <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(arraydesign::example_lake(), path)
  areas <- study_areas(list(Example = path))
  expect_named(areas, c("Example", "Stoney Lake (100 m)"))
  # The module sets the select through updateSelectInput(), which the test
  # session does not apply, so the test records what the module asks for.
  asked <- list()
  local_mocked_bindings(
    updateSelectInput = function(session, ..., choices = NULL, selected = NULL) {
      asked[[length(asked) + 1]] <<- list(choices = choices, selected = selected)
    },
    .package = "shiny"
  )
  shiny::testServer(mod_study_area_server, args = list(areas = areas), {
    session$flushReact()
  })
  expect_length(asked, 1)
  expect_identical(asked[[1]]$choices, c("Example", "Stoney Lake (100 m)"))
  expect_identical(asked[[1]]$selected, "Example")
})

test_that("the app's own errors and warnings carry its classes", {
  expect_error(app_limits(NA, 1), class = "arraydesignR_error")
  expect_error(parse_thresholds("a, 0.5"), class = "arraydesignR_error")
  open <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 400, ymin = 0, ymax = 400,
                      crs = "EPSG:32617", vals = 5)
  expect_warning(new_study_area(open, "open"), class = "arraydesignR_warning")
  err <- tryCatch(app_limits(NA, 1), error = identity)
  expect_null(conditionCall(err))
})

test_that("collect_warnings() returns the value and the warnings, and drops messages", {
  got <- expect_silent(collect_warnings({
    warning("first")
    message("dropped")
    warning("second")
    42
  }))
  expect_identical(got, list(value = 42, warnings = c("first", "second")))
})

test_that("at 1 receiver, 1 animal and 1 hour every text the app prints is singular", {
  path <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(arraydesign::example_lake(), path)
  areas <- study_areas(list(Example = path))
  notes <- character()
  local_mocked_bindings(showNotification = function(ui, ...) {
    notes <<- c(notes, paste(as.character(ui), collapse = " "))
    invisible(NULL)
  }, .package = "shiny")
  # Every text and HTML output the page holds, read from the page itself.
  html <- as.character(app_ui(NULL))
  ids <- regmatches(html, gregexpr("id=\"[^\"]+\" class=\"shiny-(text|html)-output", html))[[1]]
  ids <- unique(sub("id=\"([^\"]+)\".*", "\\1", ids))
  expect_gt(length(ids), 20)
  server <- function(input, output, session) {
    app <- app_server(input, output, session, areas = areas)
  }
  texts <- character()
  shiny::testServer(server, {
    read_all <- function() {
      for (id in ids) {
        v <- tryCatch(output[[id]], error = function(e) NULL)
        if (is.list(v)) v <- v$html
        if (is.character(v)) texts <<- c(texts, v)
      }
    }
    inputs <- default_inputs()
    inputs$`area-choice` <- "Example"
    inputs$`array-n` <- 1
    do.call(session$setInputs, inputs)
    session$setInputs(`array-generate` = 1)
    expect_equal(nrow(app$layout()), 1)
    session$setInputs(`coverage-compute` = 1)
    wait_for_task(session)
    expect_identical(app$coverage$state()$kind, "current")
    for (layer in c("depth", "distance", "p_array", "p_hour")) {
      session$setInputs(`map-layer` = layer)
      read_all()
    }
    session$setInputs(`score-n_paths` = 1, `score-hours` = 1, `score-seed` = 1,
                      `score-delay_min` = 120, `score-delay_max` = 180, `score-turn_sd` = 40,
                      `score-score` = 1)
    wait_for_task(session)
    expect_identical(app$score$state()$kind, "current")
    read_all()
    # A second receiver makes the coverage result stale for the one before it.
    click <- list(lng = app$layout()$deploy_lon[1] + 0.001, lat = app$layout()$deploy_lat[1])
    session$setInputs(`map-map_click` = click)
    expect_equal(nrow(app$layout()), 2)
    read_all()
  })
  all <- paste(c(texts, notes), collapse = "\n")
  for (bad in c("1 receivers", "1 hours", "1 animals", "animals 1 to 1", "1 water cells",
                "1 transmissions", "1 distances", "1 animal-hours")) {
    expect_false(grepl(paste0("(^|[^0-9,.])", bad, "\\b"), all), label = bad)
  }
  # One value printed as a range of itself.
  expect_false(grepl("Tracks of the 1 animal", all, fixed = TRUE))
  expect_false(grepl("(?<![0-9.])([0-9.]+%?) to \\1(?![0-9])", all, perl = TRUE))
  for (good in c("Track of the one animal over water depth, with its", "One animal: ")) {
    expect_true(grepl(good, all, fixed = TRUE), label = good)
  }
  for (good in c("1 receiver placed.", "for 1 receiver;",
                 "1 animal for 1 hour, seed 1; 1 receiver;",
                 "The point is the one simulated animal", "over 1 hour.", "1 animal x 1 hour",
                 "the results are for 1 receiver and the layout now has 2")) {
    expect_true(grepl(good, all, fixed = TRUE), label = good)
  }
})
