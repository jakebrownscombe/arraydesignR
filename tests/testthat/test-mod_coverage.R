test_that("the banner goes stale the moment the layout changes and clears on recompute", {
  a <- small_area()
  args <- coverage_session_args(a)
  shiny::testServer(mod_coverage_server, args = args, {
    session$flushReact()
    expect_identical(session$returned$state()$kind, "none")
    expect_match(output$banner$html, "No coverage yet")
    session$setInputs(compute = 1)
    wait_for_task(session)
    expect_identical(session$returned$state()$kind, "current")
    expect_equal(nrow(session$returned$coverage()$receivers), 4)
    expect_match(output$banner$html, "alert-success")
    expect_match(output$summary$html, "<li>4 receivers</li>", fixed = TRUE)
    layout(add_receiver(layout(), a, 725300, 4935600))
    session$flushReact()
    expect_identical(session$returned$state()$kind, "stale")
    expect_match(output$banner$html, "for 4 receivers and the layout now has 5")
    session$setInputs(compute = 2)
    wait_for_task(session)
    expect_identical(session$returned$state()$kind, "current")
    expect_equal(nrow(session$returned$coverage()$receivers), 5)
    range_model(new_range_model(default_spec()))
    session$flushReact()
    expect_match(output$banner$html, "the range model changed")
    layout(NULL)
    session$flushReact()
    expect_match(output$banner$html,
                 "cannot be computed. </strong>\\s*Place at least one receiver.")
  })
})

test_that("a layout already scored comes from the cache without a new task", {
  a <- small_area()
  cache <- cachem::cache_mem()
  kept <- new.env()
  args <- coverage_session_args(a, cache)
  shiny::testServer(mod_coverage_server, args = args, {
    session$setInputs(compute = 1)
    wait_for_task(session)
    kept$key <- session$returned$coverage()$key
    expect_equal(cache$size(), 1)
  })
  shiny::testServer(mod_coverage_server, args = coverage_session_args(a, cache), {
    session$setInputs(compute = 1)
    expect_identical(job$task$status(), "initial")
    expect_identical(session$returned$state()$kind, "current")
    expect_identical(session$returned$coverage()$key, kept$key)
    expect_match(output$summary$html, "Taken from the cache")
    expect_false(grepl("Computed in", output$summary$html))
  })
  expect_null(cache$get(kept$key)$from_cache)
})

test_that("with auto on, the first valid inputs are computed without a click, once", {
  a <- small_area()
  args <- coverage_session_args(a)
  args$auto <- TRUE
  shiny::testServer(mod_coverage_server, args = args, {
    wait_for_task(session)
    expect_identical(session$returned$state()$kind, "current")
    expect_equal(nrow(session$returned$coverage()$receivers), 4)
    layout(add_receiver(layout(), a, 725300, 4935600))
    wait_for_task(session)
    expect_identical(session$returned$state()$kind, "stale")
  })
})

test_that("with auto on, an empty start waits for the first layout", {
  a <- small_area()
  args <- coverage_session_args(a)
  args$auto <- TRUE
  args$layout <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_coverage_server, args = args, {
    wait_for_task(session)
    expect_identical(session$returned$state()$kind, "none")
    layout(generate_layout(a, "grid", 2, 1))
    wait_for_task(session)
    expect_identical(session$returned$state()$kind, "current")
  })
})

test_that("the work line sits below the button and the summary", {
  html <- as.character(app_ui(NULL))
  at <- vapply(c("coverage-compute", "coverage-summary", "coverage-work"),
               function(id) regexpr(sprintf("id=\"%s\"", id), html)[1], 1)
  expect_true(all(at > 0))
  expect_identical(order(at), 1:3)
})

test_that("compute with no receivers or a failing task leaves the session running", {
  a <- small_area()
  args <- coverage_session_args(a)
  args$layout <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_coverage_server, args = args, {
    session$setInputs(compute = 1)
    expect_null(session$returned$coverage())
    expect_identical(session$returned$state()$kind, "none")
  })
  args <- coverage_session_args(a)
  lay <- shiny::isolate(args$layout())
  lay$station_id[2] <- lay$station_id[1]
  lay$x[2] <- -1e9
  args$layout <- shiny::reactiveVal(lay)
  shiny::testServer(mod_coverage_server, args = args, {
    session$setInputs(compute = 1)
    expect_warning(wait_for_task(session), "error occurred when invoking the ExtendedTask")
    expect_identical(job$task$status(), "error")
    expect_null(session$returned$coverage())
  })
})

test_that("a package warning reaches the session and the result is still kept", {
  a <- small_area()
  args <- coverage_session_args(a)
  lay <- shiny::isolate(args$layout())
  lay$station_id[2] <- lay$station_id[1]
  args$layout <- shiny::reactiveVal(lay)
  shiny::testServer(mod_coverage_server, args = args, {
    session$setInputs(compute = 1)
    wait_for_task(session)
    expect_identical(job$task$status(), "success")
    expect_true(any(grepl("more than one cell", session$returned$coverage()$warnings)))
    expect_identical(session$returned$state()$kind, "current")
  })
})

test_that("the banner shows above the map and the performance tab as well", {
  shiny::testServer(mod_coverage_server, args = coverage_session_args(small_area()), {
    session$flushReact()
    expect_identical(output$banner_map, output$banner)
    expect_identical(output$banner_performance, output$banner)
    session$setInputs(compute = 1)
    wait_for_task(session)
    # While current, the map banner carries the headline and the cell count
    # only; the green banner says it is current.
    cov <- session$returned$coverage()
    lead <- sprintf("<strong>%s of the water covered</strong>", fmt_pct(mean(cov$p_array >= 0.5)))
    expect_match(output$banner_map$html, lead, fixed = TRUE)
    expect_match(output$banner_map$html,
                 sprintf("%s of %s water cells", fmt_int(sum(cov$p_array >= 0.5)),
                         fmt_int(length(cov$cells))), fixed = TRUE)
    expect_match(output$banner_map$html, "alert-success", fixed = TRUE)
    expect_false(grepl("Range model", output$banner_map$html))
    expect_false(grepl("of the water", output$banner$html))
    expect_identical(output$banner_performance, output$banner)
    # Stale: no number, because it is for earlier inputs.
    layout(add_receiver(layout(), small_area(), 725300, 4935600))
    session$flushReact()
    expect_false(grepl("of the water", output$banner_map$html))
    expect_identical(output$banner_map, output$banner)
  })
  expect_match(as.character(mod_coverage_banner_ui("coverage", "map")), "coverage-banner_map")
})

test_that("an invalid range model names itself in the banner and in the notification", {
  args <- coverage_session_args(small_area())
  model <- shiny::reactiveVal(TRUE)
  args$range_model <- shiny::reactive({
    shiny::validate(shiny::need(model(), "At 1 m, d05 (250 m) must be larger than d50 (250 m)."))
    new_range_model(toy_spec())
  })
  shiny::testServer(mod_coverage_server, args = args, {
    session$setInputs(compute = 1)
    wait_for_task(session)
    expect_identical(session$returned$state()$kind, "current")
    model(FALSE)
    session$flushReact()
    st <- session$returned$state()
    expect_identical(st$kind, "invalid")
    expect_identical(st$reasons, paste("The range model is invalid: At 1 m, d05 (250 m) must be",
                                       "larger than d50 (250 m)."))
    expect_match(output$banner$html, "The range model is invalid: At 1 m")
    expect_match(output$banner$html, "The results on screen are for earlier inputs.")
    session$setInputs(compute = 2)
    expect_identical(job$task$status(), "success")
  })
})

test_that("a request check carries validation and reaches the user as an error notification", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 4, 1)
  rm <- new_range_model(toy_spec())
  settings <- list(n_paths = 6, hours = 2, seed = 1, delay_min = 120, delay_max = 180)
  movement <- movement_settings(30, 15, 40, 60)
  refusals <- list(
    coverage = function() coverage_request(a, NULL, rm),
    simulation = function() simulation_request(a, lay[0, ], rm, settings, movement)
  )
  for (nm in names(refusals)) {
    e <- tryCatch(refusals[[nm]](), error = identity)
    expect_s3_class(e, "validation")
    expect_false(inherits(e, "arraydesignR_error"))
    sent <- list()
    session <- list(sendNotification = function(type, message) sent[[length(sent) + 1]] <<- message)
    expect_null(try_notify(refusals[[nm]](), session))
    expect_length(sent, 1)
    expect_identical(sent[[1]]$type, "error")
    expect_match(as.character(sent[[1]]$html), "Place at least one receiver.", fixed = TRUE)
  }
})

test_that("an input that is not ready sends no notification when a button is pressed", {
  sent <- list()
  session <- list(sendNotification = function(type, message) sent[[length(sent) + 1]] <<- message)
  expect_null(try_notify(shiny::req(NULL), session))
  expect_length(sent, 0)
  args <- coverage_session_args(small_area())
  ready <- shiny::reactiveVal(FALSE)
  rm <- new_range_model(toy_spec())
  args$range_model <- shiny::reactive({
    shiny::req(ready())
    rm
  })
  shown <- list()
  local_mocked_bindings(showNotification = function(ui, ...) shown[[length(shown) + 1]] <<- ui,
                        .package = "shiny")
  shiny::testServer(mod_coverage_server, args = args, {
    session$setInputs(compute = 1)
    session$flushReact()
    expect_identical(session$returned$state()$kind, "none")
  })
  expect_length(shown, 0)
  # The same press with a missing layout does notify, so the mock is reached.
  args$layout(NULL)
  ready(TRUE)
  shiny::testServer(mod_coverage_server, args = args, {
    session$setInputs(compute = 1)
    session$flushReact()
  })
  expect_identical(unlist(shown), "Place at least one receiver.")
})

test_that("the three start-up checks leave the banner at 'No coverage yet' until a result", {
  none <- "No coverage yet. Place receivers and press Compute coverage."
  a <- small_area()
  args <- coverage_session_args(a)
  ready <- shiny::reactiveVal(TRUE)
  rm <- new_range_model(toy_spec())
  args$range_model <- shiny::reactive({
    shiny::req(ready())
    rm
  })
  checks <- list(
    list(set = function() args$area(NULL), reset = function() args$area(a),
         msg = "Choose a study area."),
    list(set = function() args$layout(NULL),
         reset = function() args$layout(generate_layout(a, "grid", 4, 1)),
         msg = "Place at least one receiver."),
    list(set = function() ready(FALSE), reset = function() ready(TRUE),
         msg = "An input is not ready yet.")
  )
  shiny::testServer(mod_coverage_server, args = args, {
    for (ch in checks) {
      ch$set()
      session$flushReact()
      expect_identical(session$returned$state()$kind, "none")
      expect_match(output$banner$html, none, fixed = TRUE)
      expect_false(grepl(ch$msg, output$banner$html, fixed = TRUE))
      ch$reset()
    }
    session$setInputs(compute = 1)
    wait_for_task(session)
    expect_identical(session$returned$state()$kind, "current")
    for (ch in checks) {
      ch$set()
      session$flushReact()
      expect_identical(session$returned$state()$kind, "invalid")
      expect_match(output$banner$html, ch$msg, fixed = TRUE)
      ch$reset()
    }
  })
  a2 <- small_area()
  sim_args <- list(area = shiny::reactiveVal(a2), layout = shiny::reactiveVal(NULL),
                   range_model = shiny::reactiveVal(rm), cache = cachem::cache_mem())
  shiny::testServer(mod_score_server, args = sim_args, {
    session$setInputs(n_paths = 4, hours = 2, seed = 1, delay_min = 120,
                      delay_max = 180, turn_sd = 40, step_mean = 30, step_sd = 15,
                      time_step = 60)
    expect_identical(session$returned$state()$kind, "none")
    expect_match(output$banner$html,
                 "No simulation yet. Place receivers and press Run simulation.", fixed = TRUE)
  })
})

test_that("a computation over the pair limit is refused before any task runs", {
  args <- coverage_session_args(small_area())
  args$max_pairs <- 10
  shiny::testServer(mod_coverage_server, args = args, {
    session$flushReact()
    expect_match(output$work, "\\(limit 10\\)")
    st <- session$returned$state()
    expect_identical(st$kind, "invalid")
    expect_match(st$reasons, "above this app's limit of 10")
    session$setInputs(compute = 1)
    expect_identical(job$task$status(), "initial")
    expect_null(session$returned$coverage())
  })
})

test_that("checked_model() names the range model, and lets a silent req() pass as it is", {
  bad <- function() shiny::validate(shiny::need(FALSE, "At 1 m, d05 is too small."))
  expect_error(checked_model(bad), "^The range model is invalid: At 1 m, d05 is too small.$")
  silent <- tryCatch(checked_model(function() shiny::req(FALSE)), error = identity)
  expect_s3_class(silent, "shiny.silent.error")
  expect_identical(conditionMessage(silent), "")
})
