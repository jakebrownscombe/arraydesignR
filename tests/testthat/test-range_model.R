test_that("the curve passes 0.5 at d50 and 0.05 at d05 at both depths", {
  rm <- new_range_model(default_spec())
  p <- stats::plogis(rm$model$predict(c(250, 500, 750, 1500), depth_m = c(1, 1, 35, 35)))
  expect_equal(p, c(0.5, 0.05, 0.5, 0.05), tolerance = 1e-12)
  # Halfway in depth both distances are halfway too: d50 500 m at 18 m.
  expect_equal(stats::plogis(rm$model$predict(500, depth_m = 18)), 0.5, tolerance = 1e-12)
  expect_identical(rm$key, new_range_model(default_spec())$key)
  tab <- curve_table(rm)
  expect_equal(sort(unique(tab$depth_m)), c(1, 18, 35))
  expect_equal(max(tab$distance_m), 1.25 * 1500)
  expect_true(all(diff(tab$p[tab$depth_m == 1]) < 0))
})

test_that("every curve carries its name: toy, app defaults marked unsourced, or the user's", {
  expect_identical(range_model_name(toy_spec()), "Toy lake simulation curve")
  expect_identical(range_model_name(default_spec()), "App defaults, unsourced")
  expect_identical(range_model_name(range_spec(1, 300, 600, 14, 250, 600)), "Your values")
  expect_identical(range_model_label(toy_spec()),
                   "Toy lake simulation curve (d50 250 m and d05 600 m at every depth)")
  want <- paste("App defaults, unsourced (d50 250 m and d05 500 m at 1 m;",
                "d50 750 m and d05 1,500 m at 35 m)")
  expect_identical(range_model_label(default_spec()), want)
  # Shiny sends whole numbers as integers; the spec, its name and its key do not change.
  ints <- range_spec(1L, 250L, 600L, 14L, 250L, 600L)
  expect_identical(ints, toy_spec())
  expect_identical(new_range_model(ints)$key, new_range_model(toy_spec())$key)
  expect_identical(new_range_model(default_spec())$label, range_model_label(default_spec()))
})

test_that("the toy lake starts on its own curve and every other lake on the app defaults", {
  expect_identical(area_preset(toy_area())$spec, toy_spec())
  expect_identical(area_preset(small_area())$spec, default_spec())
  expect_identical(area_preset(small_area())$name, "App defaults, unsourced")
})

test_that("spec_problems() names every reason a curve cannot be made", {
  expect_length(spec_problems(toy_spec()), 0)
  expect_identical(spec_problems(range_spec(1, 250, NA, 14, 250, 600)), "Fill in all six boxes.")
  expect_identical(spec_problems(range_spec(1, 250, 600, NULL, 250, 600)), "Fill in all six boxes.")
  expect_match(spec_problems(range_spec(10, 250, 600, 5, 250, 600)), "deep receiver depth")
  p <- spec_problems(range_spec(1, 400, 400, 14, 700, 600))
  expect_length(p, 2)
  expect_match(p[1], "At 1 m, d05 \\(400 m\\) must be larger than d50 \\(400 m\\)")
  expect_match(p[2], "At 14 m")
  expect_match(spec_problems(range_spec(-1, 250, 600, 14, 250, 600)), "0 or more")
})

test_that("the range model module returns a model, refuses d05 <= d50 and draws the curve", {
  shiny::testServer(mod_range_model_server, args = list(area = shiny::reactiveVal(toy_area())), {
    session$setInputs(depth_1 = 1, d50_1 = 250, d05_1 = 600, depth_2 = 14, d50_2 = 250,
                      d05_2 = 600)
    m <- session$returned()
    expect_equal(stats::plogis(m$model$predict(600, depth_m = 5)), 0.05, tolerance = 1e-12)
    expect_null(output$problems)
    expect_match(output$caption, "at any receiver depth")
    expect_match(output$name, "^Toy lake simulation curve")
    expect_true(nzchar(output$curve$src))
    session$setInputs(d05_2 = 250)
    expect_error(session$returned(), "must be larger than d50")
    expect_match(output$problems$html, "At 14 m, d05 \\(250 m\\)")
    expect_error(output$curve, "must be larger than d50")
    session$setInputs(d05_2 = 900)
    expect_null(output$problems)
    expect_match(output$caption, "at 1 m, 7.5 m and 14 m receiver depth")
    expect_match(output$name, "^Your values")
  })
})

test_that("a new lake takes its own starting curve, and the user's values stay", {
  area <- shiny::reactiveVal(toy_area())
  shiny::testServer(mod_range_model_server, args = list(area = area), {
    session$flushReact()
    expect_identical(session$returned()$spec, toy_spec())
    area(small_area())
    session$flushReact()
    expect_identical(session$returned()$spec, default_spec())
    expect_match(output$name, "^App defaults, unsourced")
    area(toy_area())
    session$flushReact()
    expect_identical(session$returned()$spec, toy_spec())
    session$setInputs(depth_1 = 2, d50_1 = 300, d05_1 = 700, depth_2 = 20, d50_2 = 500,
                      d05_2 = 900)
    area(small_area())
    session$flushReact()
    expect_identical(session$returned()$spec, range_spec(2, 300, 700, 20, 500, 900))
    expect_match(output$name, "^Your values")
  })
})

test_that("the curve plot draws one line when the depths agree and three when not", {
  rm <- new_range_model(default_spec())
  p <- plot_range_curve(curve_table(rm), rm$spec)
  expect_s3_class(p, "ggplot")
  expect_identical(levels(p$data$depth), c("1 m", "18 m", "35 m"))
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], "", USE.NAMES = FALSE)
  expect_identical(geoms, c("GeomHline", "GeomLine", "GeomPoint"))
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$data[[1]]$yintercept, c(0.5, 0.05))
  expect_identical(unique(b$data[[1]]$linetype), "dotted")
  expect_length(unique(b$data[[2]]$colour), 3)
  # The open circles: d50 and d05 at the shallow and the deep depth, each in
  # the colour of its line, kept out of the legend.
  pts <- b$data[[3]]
  expect_equal(pts$x, c(250, 750, 500, 1500))
  expect_equal(pts$y, c(0.5, 0.5, 0.05, 0.05))
  line_col <- stats::setNames(unique(b$data[[2]]$colour), levels(p$data$depth))
  expect_identical(pts$colour, unname(line_col[c("1 m", "35 m", "1 m", "35 m")]))
  expect_identical(unique(pts$fill), "white")
  expect_false(p$layers[[3]]$show.legend)
  expect_identical(p$labels$x, "Distance from receiver (m)")
  expect_identical(p$labels$y, "Detection probability")
  expect_identical(b$plot$scales$get_scales("colour")$name, "Receiver depth")
  toy <- new_range_model(toy_spec())
  p1 <- plot_range_curve(curve_table(toy), toy$spec)
  expect_identical(levels(p1$data$depth), "All depths")
  expect_identical(nrow(p1$data), 200L)
  path <- withr::local_tempfile(fileext = ".png")
  ggplot2::ggsave(path, p, width = 6, height = 4, dpi = 96)
  expect_gt(file.size(path), 1000)
  expect_null(problem_alert(character()))
})

test_that("the caption says when the lines coincide and names the range model", {
  expect_match(curve_caption(new_range_model(toy_spec())), "do not change with depth")
  expect_false(grepl("do not change", curve_caption(new_range_model(default_spec()))))
  expect_match(curve_caption(new_range_model(default_spec())),
               "Range model: App defaults, unsourced")
})

test_that("the range model is a fixed curve with no variation term", {
  area <- shiny::reactiveVal(toy_area())
  shiny::testServer(mod_range_model_server, args = list(area = area), {
    session$flushReact()
    for (lake in list(toy_area(), small_area())) {
      area(lake)
      session$flushReact()
      m <- session$returned()
      expect_named(m, c("spec", "model", "key", "label"))
      expect_true(all(is.na(unlist(m$model$sigma))))
    }
  })
  expect_named(range_presets$toy, c("spec", "name"))
  expect_named(range_presets$default, c("spec", "name"))
})

test_that("the range settings sit in a full-width bar above a plot of 420 px or more", {
  # A full-width settings bar leaves the plot the width of the page.
  html <- as.character(mod_range_model_ui("range"))
  settings <- regexpr("Detection range settings", html, fixed = TRUE)[1]
  plot <- regexpr("id=\"range-curve\"", html, fixed = TRUE)[1]
  expect_gt(settings, 0)
  expect_gt(plot, settings)
  # Every input keeps its id, so the coverage module and the tests still read them.
  for (id in range_inputs) {
    expect_match(html, sprintf("id=\"range-%s\"", id), fixed = TRUE)
  }
  expect_gte(as.numeric(sub("px", "", range_plot_height)), 420)
  expect_match(html, sprintf("height:%s", range_plot_height), fixed = TRUE)
  # The two receiver groups sit side by side, half the row each, with no third input.
  expect_match(html, "Shallow receiver.*Deep receiver")
  expect_identical(lengths(regmatches(html, gregexpr("<input", html, fixed = TRUE))),
                   length(range_inputs))
  # The boxes open on the app defaults.
  expect_match(html, "id=\"range-d05_2\"[^>]*value=\"1500\"")
})
