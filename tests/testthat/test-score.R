# The Simulation tab (module id "score"): detection rates of simulated animals,
# the animal the unit. Every test runs on small_area(), the example lake of
# arraydesign, so none depends on which lakes the app bundles.

sim_inputs <- function() {
  a <- small_area()
  list(area = a, layout = generate_layout(a, "grid", 4, 1),
       range = new_range_model(toy_spec()),
       settings = list(n_paths = 6, hours = 2, seed = 1, delay_min = 120, delay_max = 180),
       movement = movement_settings(30, 15, 40, 60))
}

sim_request <- function(i = sim_inputs(), ...) {
  simulation_request(i$area, i$layout, i$range, i$settings, i$movement, ...)
}

test_that("a simulation request is refused on bad settings and above the limits", {
  i <- sim_inputs()
  req <- sim_request(i)
  expect_identical(req$range_label, range_model_label(toy_spec()))
  expect_identical(sim_request(i)$key, req$key)
  expect_null(req$n_seeds)
  expect_identical(req$tag_delay, c(120, 180))
  # Every setting of the walk, the delays and the seed are in the key.
  changed <- function(what, field, value) {
    j <- i
    j[[what]][[field]] <- value
    !identical(sim_request(j)$key, req$key)
  }
  expect_true(changed("settings", "seed", 2))
  expect_true(changed("movement", "turn_sd", 60))
  expect_true(changed("movement", "step_mean", 25))
  expect_true(changed("settings", "delay_min", 150))
  expect_true(changed("settings", "delay_max", 150))
  # Every receiver-to-cell pair: 4 receivers x 2,152 water cells.
  err <- tryCatch(sim_request(i, max_pairs = 8607), error = identity)
  expect_match(conditionMessage(err), "every receiver-to-cell distance: 8,608 here")
  expect_match(conditionMessage(err), "Keep any 3 of these 4 receivers", fixed = TRUE)
  expect_silent(sim_request(i, max_pairs = 8608))
  bad <- function(field, value, what = "settings") {
    j <- i
    j[[what]][[field]] <- value
    sim_request(j)
  }
  expect_error(bad("n_paths", 51), "1 to 50")
  expect_error(bad("n_paths", 2.5), "1 to 50")
  expect_error(bad("hours", 0), "1 to 24")
  expect_error(bad("seed", NA), "seed")
  expect_error(bad("delay_min", 0), "from 5 to 3,600")
  expect_error(bad("delay_max", NA), "from 5 to 3,600")
  expect_error(bad("delay_max", 3601), "from 5 to 3,600")
  expect_error(bad("delay_min", 181), "shortest delay must not be above the longest")
  # A fixed delay: the longest equal to the shortest.
  expect_silent(bad("delay_min", 180))
  expect_error(bad("turn_sd", -1, "movement"), "0 to 360")
  expect_error(bad("step_mean", 0, "movement"), "above 0")
  expect_error(bad("step_sd", NA, "movement"), "above 0")
  expect_error(bad("time_step", 7, "movement"), "divides an hour")
  expect_silent(bad("time_step", 120, "movement"))
  # 50 animals x 24 hours at 10 s is 432,000 steps.
  i$settings <- list(n_paths = 50, hours = 24, seed = 1, delay_min = 120, delay_max = 180)
  i$movement$time_step <- 10
  expect_error(sim_request(i), "432,000 steps, above this app's limit of 100,000")
  # At 60 s, 72,000 steps; at a 5 s delay, 864,000 transmissions.
  i$movement$time_step <- 60
  expect_silent(sim_request(i))
  i$settings$delay_min <- i$settings$delay_max <- 5
  expect_error(sim_request(i), "about 864,000 transmissions, above this app's limit of 400,000")
  i$layout <- i$layout[0, ]
  expect_error(sim_request(i), "at least one receiver")
})

test_that("the movement line reports the model in use, in words", {
  crw <- movement_settings(25, 15, 40, 60)
  # The signs are HTML entities, which read the same under any locale.
  expect_identical(as.character(movement_line(crw)),
                   paste("Correlated random walk: step 25 &plusmn; 15 m per 60 s,",
                         "turn 0 &plusmn; 40&deg;"))
  m <- build_movement(crw)
  expect_identical(m$time_step, 60)
  expect_match(m$source, "mean 25 m and sd 15 m per 60 s;.*sd 40 degrees")
  expect_identical(delay_text(c(120, 180)), "random, 120 to 180 s")
  expect_identical(delay_text(c(120, 120)), "fixed, 120 s")
})

test_that("one run simulates exactly the animals set; a seed repeats it, another redraws", {
  req <- sim_request()
  res <- run_simulation(req)
  expect_s3_class(res, "detection_simulation")
  a <- res$animals
  tx <- res$transmissions
  expect_identical(nrow(a), 6L)
  expect_identical(a$path_id, 1:6)
  expect_null(res$seeds)
  expect_true(all(a$detected <= a$sent))
  expect_true(all(a$rate >= 0 & a$rate <= 1))
  expect_true(all(a$hours_detected <= a$hours) && all(a$hours <= 2))
  expect_identical(sum(a$sent), nrow(tx))
  expect_identical(sum(a$detected), sum(tx$detected))
  expect_identical(length(unique(res$tracks$path_id)), 6L)
  # About 3600 / 150 = 24 transmissions an hour per animal at 120 to 180 s.
  expect_true(all(abs(a$sent / 2 - 24) < 6))
  # The same seed gives the same animals and detections; another seed does not.
  again <- run_simulation(req)
  expect_identical(again$transmissions, tx)
  expect_identical(again$animals, a)
  req2 <- req
  req2$seed <- 2
  other <- run_simulation(req2)
  expect_identical(nrow(other$animals), 6L)
  expect_false(identical(other$transmissions[c("x", "y")], tx[c("x", "y")]))
})

test_that("the run's detections are the package's, called directly", {
  req <- sim_request()
  res <- run_simulation(req)
  grid <- terra::unwrap(req$grid)
  model <- new_range_model(req$spec)$model
  dist <- detfield::water_distances(grid, req$receivers[c("receiver_serial", "x", "y",
                                                          "deploy_z")])
  field <- suppressMessages(detfield::de_field(dist, model))
  movement <- arraydesign::crw_model(30, 15, 40, time_step_s = 60)
  tr <- arraydesign::simulate_tracks(grid, movement, n_animals = 6, n_steps = 120,
                                     control = arraydesign::track_control(), seed = 1)
  tx <- arraydesign::simulate_transmissions(tr, tag_delay = c(120, 180), seed = 1)
  det <- arraydesign::simulate_detections(tx, field, overdispersion = "none", seed = 1)
  expect_identical(res$animals$sent, as.integer(table(det$transmissions$path_id)))
  expect_identical(res$animals$detected,
                   as.integer(tapply(det$transmissions$n_detections > 0,
                                     det$transmissions$path_id, sum)))
  expect_equal(res$tracks$x, tr$x)
})

test_that("a layout with no receivers detects nothing, and the summary says so", {
  req <- sim_request()
  req$receivers <- req$receivers[0, ]
  res <- run_simulation(req)
  expect_true(all(res$animals$rate == 0))
  s <- simulation_summary(res)
  expect_identical(c(s$mean, s$median, s$hour_share), c(0, 0, 0))
  expect_identical(s$never, 6L)
  expect_s3_class(plot_animal_rates(res), "ggplot")
  expect_match(sim_animal_caption(res), "Hollow points: the 6 animals never detected.",
               fixed = TRUE)
})

test_that("the table, tiles, line and captions agree with the animals", {
  res <- run_simulation(sim_request())
  a <- res$animals
  s <- simulation_summary(res)
  expect_equal(s$mean, mean(a$rate))
  expect_equal(s$se, stats::sd(a$rate) / sqrt(6))
  expect_equal(s$median, stats::median(a$rate))
  expect_identical(s$never, sum(a$detected == 0))
  expect_identical(s$animal_hours, 12)
  expect_equal(s$hour_share, sum(a$hours_detected) / 12)
  tab <- simulation_table(res)
  expect_named(tab, c("Animal", "Transmissions sent", "Heard", "Detection rate",
                      "Hours with a detection"))
  # One row per animal, highest rate first, then the row over all animals.
  expect_identical(nrow(tab), 7L)
  ord <- a[order(-a$rate, a$path_id), ]
  expect_identical(tab$Animal[1:6], format(ord$path_id))
  expect_identical(tab$`Detection rate`[1:6], fmt_pct(ord$rate))
  expect_identical(tab$`Hours with a detection`[1:6], sprintf("%d of 2", ord$hours_detected))
  expect_identical(tab$Animal[7], "All animals")
  expect_identical(tab$`Transmissions sent`[7], fmt_int(nrow(res$transmissions)))
  expect_identical(tab$Heard[7], fmt_int(sum(res$transmissions$detected)))
  expect_identical(tab$`Detection rate`[7], sprintf("mean %s (SE %s); median %s", fmt_pct(s$mean),
                                                    fmt_pct(s$se), fmt_pct(s$median)))
  expect_match(tab$`Hours with a detection`[7], sprintf("of 12; %d animal", s$never))
  html <- as.character(simulation_boxes_ui(s))
  for (title in c("Mean detection rate", "Median detection rate",
                  "Animal-hours with a detection")) {
    expect_match(html, title, fixed = TRUE)
  }
  expect_match(html, sprintf("&plusmn; %s SE over 6 animals", fmt_pct(s$se)), fixed = TRUE)
  expect_match(html, sprintf("%d of 6 animals never detected", s$never), fixed = TRUE)
  expect_match(html, sprintf("%d of 12 animal-hours", s$hours_detected), fixed = TRUE)
  expect_false(grepl("seeds", html, fixed = TRUE))
  line <- as.character(simulation_line(res))
  expect_match(line, paste("^Example lake: 6 animals for 2 hours each, seed 1; 4 receivers;",
                           "transmission delay random, 120 to 180 s."))
  expect_match(line, "Movement: Correlated random walk: step 30 &plusmn; 15 m per 60 s")
  expect_match(line, "Range model: Toy lake simulation curve")
  expect_match(line, "Computed in [0-9.]+ s.$")
  expect_match(sim_animal_caption(res), "^Each point is one simulated animal, animals 1 to 6")
  expect_match(simulation_map_caption(res), "Tracks of the 6 animals")
  expect_match(simulation_map_caption(res), "(delay random, 120 to 180 s)", fixed = TRUE)
  res$from_cache <- TRUE
  expect_match(as.character(simulation_line(res)), "Taken from the cache.$")
  # Every figure is a ggplot at the size of the other panels' text, and draws.
  curve <- plot_range_curve(curve_table(new_range_model(toy_spec())), toy_spec())
  size <- function(plot, el) ggplot2::calc_element(el, plot$theme)$size
  for (p in list(plot_animal_rates(res), plot_simulation_map(res))) {
    expect_s3_class(p, "ggplot")
    expect_equal(size(p, "axis.title.x"), size(curve, "axis.title.x"))
    path <- withr::local_tempfile(fileext = ".png")
    ggplot2::ggsave(path, p, width = 6, height = 4, dpi = 96)
    expect_gt(file.size(path), 5000)
  }
})

test_that("the rate figure has one x entry per animal, in id order, with the mean dashed", {
  layers <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], ""))
  req <- sim_request()
  for (n in c(1, 4, 6)) {
    req$n_paths <- n
    res <- run_simulation(req)
    p <- plot_animal_rates(res)
    b <- ggplot2::ggplot_build(p)
    # One level, one point and one bar per animal set, in id order.
    expect_identical(levels(p$data$animal), as.character(seq_len(n)))
    expect_identical(nrow(b$data[[3]]), as.integer(n))
    expect_identical(layers(p), c("GeomHline", "GeomSegment", "GeomPoint"))
    expect_equal(b$data[[1]]$yintercept, mean(res$animals$rate))
    expect_equal(b$data[[3]]$y, res$animals$rate[order(res$animals$path_id)])
    expect_identical(p$labels$x, "Animal")
    expect_identical(p$labels$y, "Share of transmissions heard")
    expect_identical(b$layout$panel_params[[1]]$y.range[1] <= 0, TRUE)
    dashed <- if (n == 1) {
      sprintf("Dashed line: its share, %s.", fmt_pct(res$animals$rate))
    } else {
      sprintf("Dashed line: the mean across the %s, %s.", fmt_animals(n),
              fmt_pct(mean(res$animals$rate)))
    }
    expect_match(sim_animal_caption(res), dashed, fixed = TRUE)
  }
  expect_false(grepl("violin", sim_animal_caption(res), ignore.case = TRUE))
  # Never-detected animals are hollow (shape 21); detected ones are filled (16).
  res$animals$detected[1] <- 0L
  res$animals$rate[1] <- 0
  shapes <- ggplot2::ggplot_build(plot_animal_rates(res))$data[[3]]$shape
  expect_identical(shapes, c(21, rep(16, 5)))
  expect_match(sim_animal_caption(res), "Hollow point: the 1 animal never detected.", fixed = TRUE)
  # Every id is labelled up to 25 animals; above, animal 1 and every fifth.
  expect_identical(animal_axis_breaks(1:25), as.character(1:25))
  expect_identical(animal_axis_breaks(1:50), as.character(c(1, seq(5, 50, by = 5))))
  fifty <- data.frame(path_id = 1:50, sent = 10L, detected = rep(c(0L, 5L), 25),
                      hours = 1L, hours_detected = 1L)
  fifty$rate <- fifty$detected / fifty$sent
  big <- plot_animal_rates(list(animals = fifty, hours = 1))
  bb <- ggplot2::ggplot_build(big)
  expect_identical(nlevels(big$data$animal), 50L)
  expect_identical(nrow(bb$data[[3]]), 50L)
  expect_identical(bb$layout$panel_params[[1]]$x$get_labels(),
                   as.character(c(1, seq(5, 50, by = 5))))
  req$n_paths <- 1
  one <- run_simulation(req)
  one_html <- as.character(simulation_boxes_ui(simulation_summary(one)))
  expect_match(one_html, "No SE from one animal")
  # One animal has one rate, printed once; so has a group whose rates print alike.
  expect_match(one_html, sprintf("One animal: %s", fmt_pct(one$animals$rate)), fixed = TRUE)
  expect_false(grepl("Range ", one_html, fixed = TRUE))
  expect_match(simulation_map_caption(one), "^Track of the one animal over water depth, with its ")
  alike <- list(n_animals = 3, min = 0.5, max = 0.5)
  expect_identical(rate_range_line(alike), "All 3 animals at 50.0%")
  expect_identical(rate_range_line(modifyList(alike, list(max = 0.8))), "Range 50.0% to 80.0%")
  # A fixed delay sends every 120 s: 60 transmissions in 2 hours.
  req$tag_delay <- c(120, 120)
  fixed <- run_simulation(req)
  expect_true(all(fixed$animals$sent %in% 59:60))
  expect_match(simulation_map_caption(fixed), "(delay fixed, 120 s)", fixed = TRUE)
})

test_that("the track map keys its marks, with no land fill and no start circles", {
  res <- run_simulation(sim_request())
  p <- plot_simulation_map(res)
  b <- ggplot2::ggplot_build(p)
  geoms <- unname(vapply(p$layers, function(l) class(l$geom)[1], ""))
  # Crosses under the tracks, so the tracks show where the crosses are dense.
  expect_identical(geoms[1:6], c("GeomRaster", "GeomPoint", "GeomPath", "GeomPoint", "GeomPoint",
                                 "GeomPoint"))
  # Water cells only: no land layer, and the panel is white.
  expect_identical(nrow(b$data[[1]]), small_area()$n_water)
  expect_identical(ggplot2::calc_element("panel.background", p$theme)$fill, "white")
  # Track lines are grey; no point layer sits on the track positions.
  expect_identical(unique(b$data[[3]]$colour), "grey30")
  tx <- res$transmissions
  # Not heard: red crosses (shape 4); heard: green points.
  expect_identical(nrow(b$data[[2]]), sum(!tx$detected))
  expect_identical(unique(b$data[[2]]$shape), 4)
  expect_identical(unique(b$data[[2]]$colour), "red3")
  expect_identical(nrow(b$data[[4]]), sum(tx$detected))
  expect_identical(unique(b$data[[4]]$fill), detected_green)
  # Receivers: a yellow ring over a dark one, as on the Map tab.
  expect_identical(c(unique(b$data[[5]]$colour), unique(b$data[[6]]$colour)),
                   c("#1A1A1A", "#FDE725"))
  expect_identical(nrow(b$data[[6]]), nrow(res$receivers))
  expect_gt(b$data[[5]]$stroke[1], b$data[[6]]$stroke[1])
  # The key names the four marks: the track line, and the three point marks
  # in the colours the layers draw.
  g <- ggplot2::get_guide_data(b, "shape")
  expect_identical(g$.label, c("Heard", "Not heard", "Receiver"))
  expect_identical(g$shape, c(21, 4, 1))
  expect_identical(ggplot2::get_guide_data(b, "linetype")$.label, "Track")
  # Key and layers read one mark table: each glyph's colour, fill, size and
  # stroke equal its layer's, and the receiver glyph draws the edge ring.
  marks <- sim_map_marks()
  key <- p$scales$get_scales("shape")$guide$params$override.aes
  for (aes in c("colour", "size", "stroke")) {
    layers <- c(unique(b$data[[4]][[aes]]), unique(b$data[[2]][[aes]]),
                unique(b$data[[6]][[aes]]))
    expect_identical(key[[aes]], layers, label = aes)
  }
  expect_identical(key$fill[1], unique(b$data[[4]]$fill))
  expect_identical(unique(b$data[[5]]$colour), marks["Receiver", "edge_colour"])
  expect_identical(unique(b$data[[5]]$stroke), marks["Receiver", "edge_stroke"])
  glyph <- draw_key_mark(marks)(data.frame(shape = 1, colour = marks["Receiver", "colour"],
                                           fill = NA, size = 4.2, stroke = 0.8, alpha = 1),
                                list(), c(1, 1))
  expect_s3_class(glyph, "gTree")
  edge_gp <- glyph$children[[1]]$gp
  expect_identical(edge_gp$col, ggplot2::alpha(marks["Receiver", "edge_colour"], 1))
  expect_identical(class(draw_key_mark(marks)(data.frame(shape = 4, colour = "red3", fill = NA,
                                                         size = 0.6, stroke = 0.5, alpha = 1),
                                              list(), c(1, 1)))[1], "points")
  cap <- simulation_map_caption(res)
  expect_no_match(cap, colour_words, perl = TRUE)
  expect_match(cap, sprintf("%d receivers\\.$", nrow(res$receivers)))
  expect_match(cap, sprintf("%s not heard", fmt_int(sum(!tx$detected))), fixed = TRUE)
  # A run that detects every transmission, or none, draws one point mark fewer,
  # and the key holds only the marks drawn.
  for (heard in c(TRUE, FALSE)) {
    one <- res
    one$transmissions <- tx[tx$detected == heard, ]
    g <- ggplot2::get_guide_data(ggplot2::ggplot_build(plot_simulation_map(one)), "shape")
    expect_identical(g$.label, c(if (heard) "Heard" else "Not heard", "Receiver"))
    png_file <- withr::local_tempfile(fileext = ".png")
    grDevices::png(png_file)
    expect_no_error(print(plot_simulation_map(one)))
    grDevices::dev.off()
  }
})

test_that("the simulation module runs on request, goes stale on a change, and reports", {
  a <- small_area()
  args <- list(area = shiny::reactiveVal(a),
               layout = shiny::reactiveVal(generate_layout(a, "grid", 4, 1)),
               range_model = shiny::reactiveVal(new_range_model(toy_spec())),
               cache = cachem::cache_mem())
  shiny::testServer(mod_score_server, args = args, {
    session$setInputs(n_paths = 4, hours = 2, seed = 1, delay_min = 120,
                      delay_max = 180, turn_sd = 40, step_mean = 30, step_sd = 15,
                      time_step = 60)
    expect_identical(session$returned$state()$kind, "none")
    expect_match(output$banner$html, "No simulation yet")
    expect_match(output$movement_line$html, "^Correlated random walk: step 30 &plusmn; 15 m")
    session$setInputs(score = 1)
    wait_for_task(session)
    expect_identical(session$returned$state()$kind, "current")
    res <- session$returned$score()
    # The module's numbers are run_simulation()'s for the same request.
    direct <- run_simulation(simulation_request(a, layout(), range_model(),
                                                list(n_paths = 4, hours = 2, seed = 1,
                                                     delay_min = 120, delay_max = 180),
                                                movement_settings(30, 15, 40, 60)))
    expect_identical(res$animals, direct$animals)
    expect_match(output$line$html, "Example lake: 4 animals for 2 hours each, seed 1;")
    expect_match(output$boxes$html, "Mean detection rate")
    expect_match(output$table, "Transmissions sent")
    for (id in c("distribution", "map")) expect_true(nzchar(output[[id]]$src))
    expect_match(output$table, "All animals")
    expect_match(output$map_caption, "Tracks of the 4 animals")
    session$setInputs(step_mean = 25)
    expect_match(output$movement_line$html,
                 "Correlated random walk: step 25 &plusmn; 15 m per 60 s", fixed = TRUE)
    expect_identical(session$returned$state()$reasons, "the simulation settings changed")
    session$setInputs(time_step = 7)
    expect_match(output$movement_line$html, "divides an hour")
    expect_match(output$banner$html, "Simulation cannot be computed")
    session$setInputs(time_step = 60, step_mean = 30, seed = 2)
    range_model(new_range_model(default_spec()))
    session$flushReact()
    expect_match(output$banner$html,
                 "Simulation is out of date: </strong>\\s*the range model changed")
    session$setInputs(n_paths = 99)
    expect_match(output$banner$html, "Simulation cannot be computed")
    session$setInputs(n_paths = 4, delay_min = 200)
    expect_match(output$banner$html, "shortest delay must not be above the longest")
    session$setInputs(delay_min = 120, hours = 3)
    expect_identical(session$returned$state()$kind, "stale")
    session$setInputs(score = 2)
    expect_identical(session$returned$score(), res)
    area(shallow_area())
    session$flushReact()
    expect_error(output$boxes, paste("No simulation for Shallow lake yet: the last result is",
                                     "for Example lake. Press Run simulation."), fixed = TRUE)
    expect_identical(as.character(output$line$html), "")
  })
})

test_that("simulation warnings travel back, a failed task leaves the session up, a repeat is cached", { # nolint: line_length_linter.
  a <- small_area()
  rm <- new_range_model(toy_spec())
  lay <- generate_layout(a, "grid", 3, 1)
  # Steps of 1 km a minute on a lake of 30 m cells: the walker warns that
  # most steps meet the shore.
  long <- movement_settings(1000, 50, 40, 60)
  res <- run_simulation(simulation_request(a, lay, rm,
                                           list(n_paths = 2, hours = 1, seed = 1,
                                                delay_min = 120, delay_max = 180), long))
  expect_true(any(grepl("reflected at the shore", res$warnings)))
  cache <- cachem::cache_mem()
  args <- list(area = shiny::reactiveVal(a), layout = shiny::reactiveVal(lay),
               range_model = shiny::reactiveVal(rm), cache = cache)
  set <- list(n_paths = 2, hours = 1, seed = 1, delay_min = 120, delay_max = 180,
              turn_sd = 40, score = 1)
  shiny::testServer(mod_score_server, args = args, {
    do.call(session$setInputs, set)
    wait_for_task(session)
    expect_identical(job$task$status(), "success")
  })
  shiny::testServer(mod_score_server, args = args, {
    do.call(session$setInputs, set)
    expect_identical(job$task$status(), "initial")
    expect_match(output$line$html, "Taken from the cache.$")
  })
  off <- lay
  off$x[1] <- -1e9
  args$layout <- shiny::reactiveVal(off)
  shiny::testServer(mod_score_server, args = args, {
    do.call(session$setInputs, set)
    expect_warning(wait_for_task(session), "error occurred when invoking the ExtendedTask")
    expect_identical(job$task$status(), "error")
    expect_null(session$returned$score())
  })
})
