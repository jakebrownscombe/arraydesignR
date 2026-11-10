# Regression check: the app and the package path agree on per-cell detection
# probability for the default 20-receiver array and a 48-receiver grid. The
# package path here is written without app code (helper-app.R), at the same
# cutoff; the hand oracle in test-coverage.R checks the formula itself.

test_that("the app's default coverage on Stoney Lake equals the package path, cell by cell", {
  # Stoney Lake, the 20 receivers Generate places on the regular grid by
  # default, and the app-default curve. The pinned numbers, 13.4% and 648 of
  # 4,818 cells, come from the package path (package_coverage() in
  # helper-app.R, written without app code); the app must give the same.
  app <- app_coverage_for(study_areas(), "Stoney Lake (100 m)", 20, default_spec())
  expect_equal(nrow(app$layout), 20)
  res <- expect_app_matches_package(app, terra::unwrap(stoney_area()$grid), default_spec())
  expect_equal(res$app, res$package)
  expect_identical(fmt_pct(res$package), "13.4%")
  expect_equal(sum(app$cov$p_array >= 0.5), 648)
  expect_equal(length(app$cov$cells), 4818)
})

test_that("on Stoney Lake the 48-receiver grid matches the package path under both curves", {
  # The bundled Stoney raster by default (the 100 m one the 1.x app shipped);
  # ARRAYDESIGNR_STONEY_TIF points at another Stoney raster, and the test then
  # runs on that one instead. Two curves: the app defaults, which vary with
  # depth, and the toy lake's 250 m and 600 m curve, which does not.
  path <- Sys.getenv("ARRAYDESIGNR_STONEY_TIF")
  if (!nzchar(path)) {
    path <- system.file("extdata", "stoney_depth.tif", package = "arraydesignR", mustWork = TRUE)
  }
  skip_if(!file.exists(path), "ARRAYDESIGNR_STONEY_TIF points at no file")
  areas <- study_areas(list(Stoney = path))
  n_water <- areas$Stoney$n_water
  for (curve in list(list(name = "app-default curve", spec = default_spec()),
                     list(name = "250 m and 600 m curve", spec = toy_spec()))) {
    app <- app_coverage_for(areas, "Stoney", 48, curve$spec)
    expect_equal(nrow(app$layout), 48)
    expect_equal(length(app$cov$cells), n_water)
    res <- expect_app_matches_package(app, terra::rast(path), curve$spec)
    far <- package_coverage(terra::rast(path), app$layout, curve$spec, max_distance = 30000)
    shift <- 100 * abs(res$app - mean(far$p >= 0.5))
    expect_lt(shift, 0.05)
    # On the bundled raster, the app-default curve: 1,807 of 4,818 cells
    # (37.5%), from the package path in helper-app.R.
    if (!nzchar(Sys.getenv("ARRAYDESIGNR_STONEY_TIF")) && curve$name == "app-default curve") {
      expect_equal(sum(app$cov$p_array >= 0.5), 1807)
      expect_identical(fmt_pct(res$package), "37.5%")
    }
    message(sprintf(paste("Stoney, 48 receivers, %s: app %.4f%% (%d of %s cells), package",
                          "%.4f%%, largest per-cell difference %.2g; at 30 km %.4f%%; deaf",
                          "behind land %.4f%%; any one receiver %.4f%%"),
                    curve$name, 100 * res$app, sum(app$cov$p_array >= 0.5), fmt_int(n_water),
                    100 * res$package,
                    res$max_diff, 100 * mean(far$p >= 0.5),
                    100 * mean(deaf_p(app$cov) >= 0.5), 100 * res$single_app))
  }
})

test_that("NEWS gives the default simulation on Stoney Lake as the app computes it", {
  # tools/site_figures.R shoots the Simulation tab after one run at these
  # settings and man/figures/app_shots.json records what it printed. This
  # test runs the same request through the app's functions and reads the
  # sentence from NEWS.md, so a change in arraydesign's simulator fails here.
  area <- stoney_area()
  layout <- generate_layout(area, "grid", default_n_receivers, 1, default_min_depth)
  m <- movement_defaults
  req <- simulation_request(area, layout, new_range_model(default_spec()), simulation_defaults,
                            movement_settings(m$step_mean, m$step_sd, m$turn_sd, m$time_step))
  s <- simulation_summary(run_simulation(req))
  template <- paste("With the default %d receivers on Stoney Lake, %d animals for %d hours,",
                    "seed %d: mean detection rate %s (SE %s), median %s, %d of %d animals",
                    "never detected, %d of %s animal-hours with a detection, %s of %s",
                    "transmissions heard.")
  sentence <- sprintf(template,
                      nrow(layout), s$n_animals, s$hours, req$seed, fmt_pct(s$mean),
                      fmt_pct(s$se), fmt_pct(s$median), s$never, s$n_animals, s$hours_detected,
                      fmt_int(s$animal_hours), fmt_int(s$detected), fmt_int(s$sent))
  expect_identical(sentence, paste(
    "With the default 20 receivers on Stoney Lake, 20 animals for 6 hours, seed 1: mean",
    "detection rate 16.0% (SE 2.9%), median 17.9%, 4 of 20 animals never detected, 77 of 120",
    "animal-hours with a detection, 461 of 2,884 transmissions heard."
  ))
  news <- testthat::test_path("..", "..", "NEWS.md")
  if (!file.exists(news)) news <- system.file("NEWS.md", package = "arraydesignR")
  skip_if(!nzchar(news), "NEWS.md is not installed")
  text <- gsub("\\s+", " ", paste(readLines(news, warn = FALSE), collapse = " "))
  expect_match(text, sentence, fixed = TRUE)
})
