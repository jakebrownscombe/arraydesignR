# ---- Detection rates by simulation ----
# The Simulation tab (module id "score"). Animals walk on the lake with
# arraydesign's walker, their tags transmit, and every receiver hears each
# transmission, or not, under the range model of the Range model tab. The tab
# counts detections per animal and per animal-hour; the animal is the unit.
# It computes no position: positioning needs more than a detection count, and
# detfield does it.

# The pair limit counts every receiver-to-cell distance: the simulation needs a
# probability wherever an animal can be, so the distances have no cutoff.
# A delay of at most an hour puts at least one transmission in every track,
# since a track lasts at least an hour. The app's floor of 5 s keeps the count
# of transmissions in reach.
simulation_limits <- list(n_paths = c(1, 50), hours = c(1, 24), delay = c(5, 3600),
                          step = c(0.01, 5000), turn_sd = c(0, 360))

# The walker is an R loop: 72,000 steps (50 animals x 24 hours at 60 s) take
# about 1.3 s on Stoney Lake on an arm64 Mac, the median of 5 runs of
# validation/01_walker_time.R (log in validation/outputs/).
max_steps_per_run <- 1e5

# Transmissions expected in a run, at the mean delay. 50 animals x 24 hours
# at a 120 s mean delay is 36,000; at the shortest delay, 5 s, 864,000.
max_transmissions_per_run <- 4e5

# The simulation clock starts on the hour, so the hours counted are UTC hours.
sim_start <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")

# The settings at open. They are starting values the user changes. The seed
# only makes a run repeatable.
simulation_defaults <- list(n_paths = 20, hours = 6, seed = 1, delay_min = 120,
                            delay_max = 180)

# The correlated random walk of the movement-models vignette of arraydesign
# (crw_model(30, 15, ..., time_step_s = 60)) with turns of SD 40 degrees, as
# arraydesign::evaluate_array() walks by default. They are starting values for
# the user to change; no species was measured to set them.
movement_defaults <- list(step_mean = 30, step_sd = 15, turn_sd = 40, time_step = 60)

# ---- Movement model ----
# The settings that define the walk: a correlated random walk, the only model.
movement_settings <- function(step_mean, step_sd, turn_sd, time_step) {
  list(step_mean = step_mean, step_sd = step_sd, turn_sd = turn_sd, time_step = time_step)
}

check_movement <- function(m) {
  in_range <- function(v, lim) is_one_number(v) && v >= lim[1] && v <= lim[2]
  ts <- m$time_step
  shiny::validate(
    shiny::need(in_range(m$turn_sd, simulation_limits$turn_sd),
                "The turn angle SD must be a number of degrees from 0 to 360."),
    shiny::need(in_range(m$step_mean, simulation_limits$step) &&
                  in_range(m$step_sd, simulation_limits$step),
                "The step length mean and SD must be numbers of metres above 0, at most 5,000."),
    shiny::need(is_one_number(ts) && ts >= 1 && ts == round(ts) && 3600 %% ts == 0,
                paste("The time step must be a whole number of seconds that divides an hour",
                      "(60, 120, ...)."))
  )
  invisible(m)
}

# arraydesign's model for the settings.
build_movement <- function(m) {
  arraydesign::crw_model(m$step_mean, m$step_sd, m$turn_sd, time_step_s = m$time_step)
}

sim_fmt_num <- function(x) format(signif(x, 3), big.mark = ",", trim = TRUE)

# Escapes &, < and > for HTML.
sim_html_text <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# The model in use, in words, with its parameters, as one HTML string. The
# plus-minus and degree signs are HTML entities: under a C locale, as in an
# Rscript run, R writes the characters themselves as <U+00B1>.
movement_line <- function(m) {
  shiny::HTML(sprintf(paste("Correlated random walk: step %s &plusmn; %s m per %s s,",
                            "turn 0 &plusmn; %s&deg;"),
                      sim_fmt_num(m$step_mean), sim_fmt_num(m$step_sd),
                      sim_fmt_num(m$time_step), sim_fmt_num(m$turn_sd)))
}

# The delay in words: "random, 120 to 180 s", or "fixed, 120 s".
delay_text <- function(d) {
  if (d[1] == d[2]) return(sprintf("fixed, %s s", sim_fmt_num(d[1])))
  sprintf("random, %s to %s s", sim_fmt_num(d[1]), sim_fmt_num(d[2]))
}

# ---- Request ----
simulation_request <- function(area, layout, range_model, settings, movement,
                               max_pairs = default_max_pairs) {
  rx <- request_receivers(area, layout)
  check_simulation_settings(settings)
  check_movement(movement)
  check_simulation_size(settings, movement)
  pairs <- nrow(layout) * area$n_water
  shiny::validate(shiny::need(pairs <= max_pairs, simulation_refusal(area$n_water, rx$station_id,
                                                                     max_pairs)))
  args <- list(grid = area$grid, area_hash = area$hash, area_name = area$name, receivers = rx,
               spec = range_model$spec, range_label = range_model$label, n_paths = settings$n_paths,
               hours = settings$hours, seed = settings$seed,
               tag_delay = c(settings$delay_min, settings$delay_max), movement = movement)
  args$key <- rlang::hash(c(list(what = "detection simulation"),
                            args[c("area_hash", "receivers", "spec", "n_paths", "hours", "seed",
                                   "tag_delay", "movement")]))
  args
}

check_simulation_settings <- function(s) {
  whole <- function(v, lim) is_one_number(v) && v == round(v) && v >= lim[1] && v <= lim[2]
  delay <- function(v) {
    is_one_number(v) && v >= simulation_limits$delay[1] && v <= simulation_limits$delay[2]
  }
  shiny::validate(
    shiny::need(whole(s$n_paths, simulation_limits$n_paths),
                "Simulated animals must be a whole number from 1 to 50."),
    shiny::need(whole(s$hours, simulation_limits$hours),
                "Hours per animal must be a whole number from 1 to 24."),
    shiny::need(is_one_number(s$seed), "The seed must be a number."),
    shiny::need(delay(s$delay_min) && delay(s$delay_max),
                "Each delay must be a number of seconds from 5 to 3,600."),
    shiny::need(!delay(s$delay_min) || !delay(s$delay_max) || s$delay_min <= s$delay_max,
                "The shortest delay must not be above the longest.")
  )
}

# Refuses a run whose walker steps or transmissions would take too long.
check_simulation_size <- function(s, m) {
  steps <- s$n_paths * s$hours * 3600 / m$time_step
  sent <- s$n_paths * s$hours * 3600 / mean(c(s$delay_min, s$delay_max))
  shiny::validate(
    shiny::need(steps <= max_steps_per_run, steps_refusal(steps)),
    shiny::need(sent <= max_transmissions_per_run, transmissions_refusal(sent))
  )
}

steps_refusal <- function(steps) {
  sprintf(paste("This run walks %s, above this app's limit of %s. Use fewer animals or",
                "hours, or a longer time step."), fmt_count(steps, "step"),
          fmt_int(max_steps_per_run))
}

transmissions_refusal <- function(sent) {
  sprintf(paste("This run sends about %s, above this app's limit of %s. Use",
                "fewer animals or hours, or longer delays."),
          fmt_count(round(sent), "transmission"), fmt_int(max_transmissions_per_run))
}

simulation_refusal <- function(n_water, ids, max_pairs) {
  each <- rep(n_water, length(ids))
  paste(sprintf(paste("The simulation measures every receiver-to-cell distance: %s here, above",
                      "this app's limit of %s."), fmt_int(sum(each)), fmt_int(max_pairs)),
        limit_advice(each, max_pairs, ids))
}

# ---- Run ----
# Runs in a worker process: tracks, transmissions and detections of the
# animals set, each drawn from the seed, so a seed always gives the same
# animals. Warnings travel back with the result.
run_simulation <- function(args) {
  t0 <- proc.time()[["elapsed"]]
  sim <- collect_warnings({
    grid <- terra::unwrap(args$grid)
    model <- new_range_model(args$spec)$model
    field <- sim_detection_field(grid, args$receivers, model)
    simulate_animals(grid, field, build_movement(args$movement), args)
  })
  out <- c(sim$value,
           args[c("key", "area_hash", "area_name", "receivers", "spec", "range_label", "n_paths",
                  "hours", "seed", "tag_delay", "movement")],
           list(seconds = proc.time()[["elapsed"]] - t0, warnings = unique(sim$warnings)))
  structure(out, class = "detection_simulation")
}

# The field of every receiver over every water cell, as evaluate_array()
# builds it; NULL with no receivers, where nothing can be detected.
sim_detection_field <- function(grid, receivers, model) {
  if (!NROW(receivers)) return(NULL)
  dist <- detfield::water_distances(grid, receivers[c("receiver_serial", "x", "y", "deploy_z")])
  detfield::de_field(dist, model)
}

simulate_animals <- function(grid, field, movement, args) {
  n_steps <- round(args$hours * 3600 / movement$time_step)
  control <- arraydesign::track_control(start_time = sim_start)
  tracks <- arraydesign::simulate_tracks(grid, movement, n_animals = args$n_paths,
                                         n_steps = n_steps, control = control, seed = args$seed)
  tx <- arraydesign::simulate_transmissions(tracks, tag_delay = args$tag_delay, seed = args$seed)
  heard <- if (is.null(field)) {
    integer(nrow(tx))
  } else {
    arraydesign::simulate_detections(tx, field, overdispersion = "none",
                                     seed = args$seed)$transmissions$n_detections
  }
  since <- as.numeric(tx$datetime) - as.numeric(sim_start)
  per_tx <- data.frame(path_id = tx$path_id, hour = pmin(args$hours - 1, floor(since / 3600)),
                       detected = heard > 0, x = tx$x, y = tx$y)
  list(animals = sim_animal_rates(per_tx), transmissions = per_tx, tracks = tracks)
}

# One row per animal: transmissions sent and heard by at least one
# receiver, and the hours holding a transmission and those with a detection.
sim_animal_rates <- function(tx) {
  by <- split(tx, tx$path_id)
  rows <- lapply(by, function(d) {
    data.frame(path_id = d$path_id[1], sent = nrow(d), detected = sum(d$detected),
               hours = length(unique(d$hour)),
               hours_detected = length(unique(d$hour[d$detected])))
  })
  out <- do.call(rbind, rows)
  out$rate <- out$detected / out$sent
  rownames(out) <- NULL
  out
}

# ---- Summaries ----
# The numbers of the tiles and of the table's last row, over the animals.
simulation_summary <- function(res) {
  a <- res$animals
  rate <- a$rate
  list(n_animals = nrow(a), hours = res$hours, mean = mean(rate),
       se = if (nrow(a) > 1) stats::sd(rate) / sqrt(nrow(a)) else NA_real_,
       median = stats::median(rate), min = min(rate), max = max(rate),
       never = sum(a$detected == 0), sent = sum(a$sent), detected = sum(a$detected),
       animal_hours = res$n_paths * res$hours, hours_detected = sum(a$hours_detected),
       hour_share = sum(a$hours_detected) / (res$n_paths * res$hours))
}

# One row per animal, highest rate first, then a row over all animals: the
# sums of sent and detected, the mean rate with its SE across animals, the
# median, and the animals never detected.
simulation_table <- function(res) {
  a <- res$animals
  a <- a[order(-a$rate, a$path_id), ]
  s <- simulation_summary(res)
  rows <- data.frame(
    Animal = format(a$path_id), `Transmissions sent` = fmt_int(a$sent),
    Heard = fmt_int(a$detected), `Detection rate` = fmt_pct(a$rate),
    `Hours with a detection` = sprintf("%d of %d", a$hours_detected, res$hours),
    check.names = FALSE
  )
  se <- if (is.na(s$se)) "" else sprintf(" (SE %s)", fmt_pct(s$se))
  all <- data.frame(
    Animal = "All animals", `Transmissions sent` = fmt_int(s$sent),
    Heard = fmt_int(s$detected),
    `Detection rate` = sprintf("mean %s%s; median %s", fmt_pct(s$mean), se, fmt_pct(s$median)),
    `Hours with a detection` = sprintf("%s of %s; %s never detected", fmt_int(s$hours_detected),
                                       fmt_int(s$animal_hours), fmt_animals(s$never)),
    check.names = FALSE
  )
  rbind(rows, all)
}

fmt_animals <- function(n) fmt_count(n, "animal")

# Each box: a short title, one number and two short lines, as on the
# Performance tab.
simulation_boxes_ui <- function(s) {
  box <- function(title, value, line1, line2) {
    bslib::value_box(title, value, shiny::p(line1), shiny::p(line2))
  }
  over <- fmt_animals(s$n_animals)
  se <- if (is.na(s$se)) "No SE from one animal" else sprintf("&plusmn; %s SE", fmt_pct(s$se))
  bslib::layout_column_wrap(
    width = 1 / 3, fill = FALSE,
    box("Mean detection rate", fmt_pct(s$mean),
        shiny::HTML(sprintf("%s over %s", se, over)),
        "Transmissions heard / sent, per animal"),
    box("Median detection rate", fmt_pct(s$median),
        rate_range_line(s),
        sprintf("%s of %s never detected", fmt_int(s$never), over)),
    box("Animal-hours with a detection", fmt_pct(s$hour_share),
        sprintf("%s of %s", fmt_int(s$hours_detected), fmt_count(s$animal_hours, "animal-hour")),
        sprintf("%s x %s", over, fmt_count(s$hours, "hour")))
  )
}

# The spread of the animals' rates, as one value where there is one.
rate_range_line <- function(s) {
  if (s$n_animals == 1) return(sprintf("One animal: %s", fmt_pct(s$min)))
  spread <- span_text(c(s$min, s$max), fmt_pct)
  if (!grepl(" to ", spread, fixed = TRUE)) {
    return(sprintf("All %s at %s", fmt_animals(s$n_animals), spread))
  }
  paste("Range", spread)
}

simulation_line <- function(res) {
  if (is.null(res)) return("")
  when <- if (isTRUE(res$from_cache)) {
    "Taken from the cache."
  } else {
    sprintf("Computed in %.1f s.", res$seconds)
  }
  shiny::HTML(paste0(
    sim_html_text(sprintf("%s: %s for %s%s, seed %s; %s; transmission delay %s.",
                          res$area_name, fmt_animals(res$n_paths), fmt_count(res$hours, "hour"),
                          if (res$n_paths == 1) "" else " each", format(res$seed),
                          fmt_count(nrow(res$receivers), "receiver"),
                          delay_text(res$tag_delay))),
    " Movement: ", movement_line(res$movement),
    sim_html_text(sprintf(". Range model: %s. %s", res$range_label, when))
  ))
}

# What the tab shows in place of another lake's numbers.
other_area_simulation <- function(area, res) {
  sprintf("No simulation for %s yet: the last result is for %s. Press Run simulation.",
          area$name, res$area_name)
}

# ---- Figures ----
# Drawn with ggplot2 at the app's base size, in the suite theme, in one
# colour, the middle of viridis.
sim_colour <- "#21908C"

sim_percent_axis <- function(x) sprintf("%g%%", 100 * x)

# Up to this many animals every id is labelled on the x axis; above it, animal
# 1 and every fifth, so 50 labels do not overlap.
max_animals_all_labels <- 25

# The ids labelled on the x axis of the per-animal figure.
animal_axis_breaks <- function(ids) {
  if (length(ids) <= max_animals_all_labels) return(as.character(ids))
  as.character(ids[ids == 1 | ids %% 5 == 0])
}

# Detection rate per animal: one point per animal at x = animal id, in id
# order, so each animal has its own place on the axis. Animals never
# detected are hollow, so a point at 0 still reads. The dashed line is the
# mean across animals.
plot_animal_rates <- function(res) {
  a <- res$animals[order(res$animals$path_id), ]
  a$animal <- factor(a$path_id, levels = a$path_id)
  a$mark <- factor(ifelse(a$detected > 0, "Detected", "Never detected"),
                   levels = c("Detected", "Never detected"))
  ggplot2::ggplot(a, ggplot2::aes(x = .data$animal, y = .data$rate)) +
    ggplot2::geom_hline(yintercept = mean(a$rate), linetype = "dashed", colour = "grey40") +
    ggplot2::geom_segment(ggplot2::aes(xend = .data$animal, y = 0, yend = .data$rate),
                          colour = sim_colour, alpha = 0.4, linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$mark), colour = sim_colour, fill = "white",
                        size = 2.4, stroke = 1) +
    ggplot2::scale_shape_manual(values = c(Detected = 16, `Never detected` = 21), drop = TRUE,
                                guide = "none") +
    ggplot2::scale_x_discrete(breaks = animal_axis_breaks(a$path_id)) +
    ggplot2::scale_y_continuous(labels = sim_percent_axis, limits = c(0, 1),
                                expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "Animal", y = "Share of transmissions heard") +
    detfield::theme_positionr(base_size = app_base_size) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# The tracks over the water depth. arraydesign's plot method for tracks draws
# land grey and marks each start with a circle, which reads as a receiver, so
# this function draws its own map. Land is left blank on a white panel; water
# is on the suite's depth ramp, as on the Map tab. Transmissions no receiver
# heard are small red crosses, drawn first and half transparent so that
# they do not hide the lake: the default run (20 animals for 6 hours) draws
# 2,423 of its 2,884 transmissions as crosses. The track lines are grey30,
# drawn over the crosses, so dense crosses do not hide the tracks. Heard
# transmissions are green points over them. Receivers are yellow rings over a
# wider dark ring, in detfield's ring colours, as on the Map tab. The key
# beside the depth bar names each mark. Each key glyph sits on a square of
# the shallowest depth colour.
plot_simulation_map <- function(res) {
  grid <- terra::unwrap(attr(res$tracks, "grid"))
  d <- terra::as.data.frame(grid, xy = TRUE, na.rm = TRUE)
  names(d)[3] <- "depth"
  d$depth <- abs(d$depth)
  tx <- res$transmissions
  tracks <- as.data.frame(unclass(res$tracks))[c("path_id", "x", "y")]
  marks <- sim_map_marks()
  drawn <- c(any(tx$detected), any(!tx$detected), nrow(res$receivers) > 0)
  key <- marks[drawn, ]
  ggplot2::ggplot() +
    ggplot2::geom_raster(data = d, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$depth)) +
    ggplot2::scale_fill_gradient(name = "Depth (m)", low = depth_colours[1],
                                 high = depth_colours[2],
                                 guide = ggplot2::guide_colourbar(order = 1)) +
    mark_layer(tx[!tx$detected, ], marks, "Not heard") +
    ggplot2::geom_path(data = tracks, ggplot2::aes(x = .data$x, y = .data$y,
                                                   group = .data$path_id, linetype = "Track"),
                       colour = "grey30", linewidth = 0.3) +
    mark_layer(tx[tx$detected, ], marks, "Heard") +
    mark_layer(res$receivers, marks, "Receiver", edge = TRUE) +
    mark_layer(res$receivers, marks, "Receiver") +
    ggplot2::scale_linetype_manual(
      name = NULL, values = c(Track = "solid"),
      guide = ggplot2::guide_legend(order = 2,
                                    override.aes = list(colour = "grey30", linewidth = 0.6))
    ) +
    ggplot2::scale_shape_manual(
      name = NULL, values = stats::setNames(key$shape, key$mark), breaks = key$mark,
      guide = ggplot2::guide_legend(order = 3, override.aes = list(
        colour = key$colour, fill = key$fill, size = key$size, stroke = key$stroke, alpha = 1
      ))
    ) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(x = "Easting (m)", y = "Northing (m)") +
    # Bottom right, clear of the south-west arm of Stoney Lake.
    detfield::annotation_scale_bar(location = "br") +
    detfield::theme_positionr(base_size = app_base_size) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   legend.key = ggplot2::element_rect(fill = depth_colours[1], colour = NA))
}

# The point marks of the track map, one row per mark. The layers and the key
# both read this table, so each key glyph has its mark's shape, colour, fill,
# size, stroke and edge ring. The key draws at full opacity, where one faint
# cross would not read; `alpha` applies to the map only.
sim_map_marks <- function(ring = ring_colours()) {
  data.frame(
    mark = c("Heard", "Not heard", "Receiver"), shape = c(21, 4, 1),
    colour = c("black", "red3", ring[["heard"]]), fill = c(detected_green, NA, NA),
    size = c(1.6, 0.6, 4.2), stroke = c(0.3, 0.5, 0.8), alpha = c(1, 0.5, 1),
    edge_colour = c(NA, NA, ring[["edge"]]), edge_stroke = c(NA, NA, 2),
    row.names = c("Heard", "Not heard", "Receiver")
  )
}

# The points of one mark from the mark table, keyed under the mark's name.
# `edge = TRUE` gives the mark's edge ring instead: the same points in the
# edge colour and stroke, drawn under the mark and left out of the key.
mark_layer <- function(data, marks, mark, edge = FALSE) {
  m <- marks[mark, ]
  xy <- ggplot2::aes(x = .data$x, y = .data$y)
  if (edge) {
    return(ggplot2::geom_point(data = data, xy, shape = m$shape, colour = m$edge_colour,
                               size = m$size, stroke = m$edge_stroke))
  }
  ggplot2::geom_point(data = data, ggplot2::aes(x = .data$x, y = .data$y, shape = mark),
                      colour = m$colour, fill = m$fill, size = m$size, stroke = m$stroke,
                      alpha = m$alpha, key_glyph = draw_key_mark(marks))
}

# The key glyph of the point marks: the point as ggplot2 draws it, over the
# mark's edge ring when the mark has one. It finds the mark by its shape.
draw_key_mark <- function(marks) {
  function(data, params, size) {
    m <- marks[match(data$shape, marks$shape), ]
    point <- ggplot2::draw_key_point(data, params, size)
    if (is.na(m$edge_colour)) return(point)
    edge <- data
    edge$colour <- m$edge_colour
    edge$stroke <- m$edge_stroke
    grid::grobTree(ggplot2::draw_key_point(edge, params, size), point)
  }
}

# ---- Captions ----
sim_animal_caption <- function(res) {
  a <- res$animals
  n <- nrow(a)
  never <- sum(a$detected == 0)
  share <- sprintf("the share of its transmissions heard by at least one receiver over %s.",
                   fmt_count(res$hours, "hour"))
  if (n == 1) {
    hollow <- if (never) " The point is hollow: the animal was never detected." else ""
    return(sprintf("The point is the one simulated animal: %s%s Dashed line: its share, %s.",
                   share, hollow, fmt_pct(a$rate)))
  }
  hollow <- if (never == 1) {
    " Hollow point: the 1 animal never detected."
  } else if (never > 1) {
    sprintf(" Hollow points: the %s never detected.", fmt_animals(never))
  } else {
    ""
  }
  sprintf(paste("Each point is one simulated animal, animals 1 to %d: %s%s Dashed line: the",
                "mean across the %s, %s."),
          n, share, hollow, fmt_animals(n), fmt_pct(mean(a$rate)))
}

simulation_map_caption <- function(res) {
  tx <- res$transmissions
  tracks <- if (res$n_paths == 1) {
    "Track of the one animal over water depth, with its"
  } else {
    sprintf("Tracks of the %s over water depth, with their", fmt_animals(res$n_paths))
  }
  sprintf(paste("%s %s",
                "(delay %s): %s heard by at least one receiver and %s not heard.",
                "%s."),
          tracks, fmt_count(nrow(tx), "transmission"),
          delay_text(res$tag_delay), fmt_int(sum(tx$detected)), fmt_int(sum(!tx$detected)),
          fmt_count(nrow(res$receivers), "receiver"))
}
