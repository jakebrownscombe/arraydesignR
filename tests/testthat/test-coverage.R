test_that("run_coverage() gives the package path's numbers for the same layout", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 7, 1)
  rm <- new_range_model(range_spec(1, 150, 450, 20, 300, 700))
  cov <- run_coverage(coverage_request(a, lay, rm))
  pkg <- package_coverage(terra::unwrap(a$grid), lay, rm$spec)
  expect_identical(cov$cells, pkg$field$cells)
  expect_equal(cov$p_array, pkg$p, tolerance = 1e-12)
  expect_equal(length(cov$cells), 2152)
  single <- arraydesign::coverage_summary(pkg$field, thresholds = c(0.05, 0.5, 0.8))
  tab <- coverage_table(cov, c(0.05, 0.5, 0.8))
  expect_equal(tab$single_cells, single$n_cells)
  expect_equal(tab$single_share, single$share)
  expect_equal(tab$array_share, vapply(c(0.05, 0.5, 0.8), function(t) mean(pkg$p >= t), 1))
  expect_true(all(tab$array_share >= tab$single_share))
  expect_equal(cov$depth, abs(terra::values(terra::unwrap(a$grid), mat = FALSE)[cov$cells]))
  expect_true(all(cov$nearest_m >= 0, na.rm = TRUE))
  # Point form: a headline, what it means, then one point per fact, each its
  # own list item.
  line <- as.character(coverage_line(cov))
  expect_match(line, sprintf("<strong>%s of the water covered</strong>",
                             gsub(".", "\\.", fmt_pct(mean(cov$p_array >= 0.5)), fixed = TRUE)))
  expect_match(line, "that any receiver detects one transmission)", fixed = TRUE)
  items <- regmatches(line, gregexpr("<li>[^<]*</li>", line))[[1]]
  want <- c(sprintf("<li>%s of 2,152 water cells</li>", fmt_int(sum(cov$p_array >= 0.5))),
            "<li>7 receivers</li>", "<li>Range model: Your values</li>",
            "<li>d50 150 m, d05 450 m at 1 m; d50 300 m, d05 700 m at 20 m</li>")
  expect_identical(items[1:4], want)
  expect_match(items[5], "^<li>Computed in [0-9.]+ s</li>$")
  expect_length(items, 5)
  expect_null(coverage_line(NULL))
  expect_length(cov$warnings, 0)
})

test_that("the single-receiver columns come from arraydesign::coverage_summary()", {
  cov <- perf_cov()
  local_mocked_bindings(coverage_summary = function(...) stop("coverage_summary was called"),
                        .package = "arraydesign")
  expect_error(coverage_table(cov, 0.5), "coverage_summary was called")
})

test_that("the cutoff is where the curve falls to 0.001 at the depth that sets it", {
  expect_identical(curve_cutoff(toy_spec()), 1071)
  expect_identical(curve_cutoff(default_spec()), 2510)
  expect_identical(curve_cutoff(default_spec()), hand_cutoff(default_spec()))
  m <- new_range_model(default_spec())$model
  p <- function(d, z) stats::plogis(m$predict(d, depth_m = z))
  expect_lte(p(2510, 35), 0.001)
  expect_gt(p(2509, 35), 0.001)
  expect_lt(p(2510, 1), 0.001)
  expect_lt(p(2510, 18), 0.001)
})

test_that("the cutoff moves the share at 0.5 by under 0.05 points against 30 km", {
  for (case in list(list(small_area(), 7, range_spec(1, 150, 450, 20, 300, 700)),
                    list(toy_area(), 5, toy_spec()), list(toy_area(), 5, default_spec()))) {
    lay <- generate_layout(case[[1]], "grid", case[[2]], 1)
    req <- coverage_request(case[[1]], lay, new_range_model(case[[3]]))
    cut <- run_coverage(req)
    req$max_distance <- 30000
    far <- run_coverage(req)
    expect_lt(100 * abs(mean(cut$p_array >= 0.5) - mean(far$p_array >= 0.5)), 0.05)
    expect_lte(max(far$p_array - cut$p_array), 1 - 0.999^case[[2]])
  }
})

test_that("on cells in clear sight of every receiver the app equals a hand oracle", {
  a <- toy_area()
  grid <- terra::unwrap(a$grid)
  lay <- generate_layout(a, "grid", 5, 1)
  spec <- range_spec(1, 150, 450, 14, 300, 700)
  cov <- run_coverage(coverage_request(a, lay, new_range_model(spec)))
  clear <- clear_sight_cells(grid, cov$cells, lay)
  expect_gt(sum(clear), 100)
  xy <- terra::xyFromCell(grid, cov$cells[clear])
  want <- oracle_p(xy, lay, spec, hand_cutoff(spec))
  expect_lte(max(abs(cov$p_array[clear] - want)), 1e-9)
  drawn <- terra::extract(map_layer(a, cov, "p_array")$raster, xy)[, 1]
  expect_lte(max(abs(drawn - want)), 1e-6)
})

test_that("with receivers deaf behind land the share is lower, and equal on open water", {
  # The app reads the curve at the distance through water and computes no
  # deaf bound; deaf_p() in helper-app.R gives it from the same result.
  cov <- perf_cov()
  expect_null(cov$p_array_deaf)
  expect_gt(cov$n_shadowed, 0)
  deaf <- deaf_p(cov)
  expect_true(all(deaf <= cov$p_array + 1e-12))
  expect_lt(mean(deaf >= 0.5), mean(cov$p_array >= 0.5))
  open <- terra::rast(nrows = 12, ncols = 12, xmin = 0, xmax = 360, ymin = 0, ymax = 360,
                      crs = "EPSG:32617", vals = 5)
  open[c(1, 144)] <- NA
  oa <- new_study_area(open, "open")
  ocov <- run_coverage(coverage_request(oa, generate_layout(oa, "grid", 2, 1),
                                        new_range_model(toy_spec())))
  expect_equal(ocov$n_shadowed, 0)
  expect_equal(deaf_p(ocov), ocov$p_array, tolerance = 1e-12)
  # The deaf bound is never shown.
  expect_false(grepl("behind land", as.character(coverage_line(cov))))
})

test_that("warnings from the packages travel back with the result", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 3, 1)
  lay$station_id[2] <- lay$station_id[1]
  cov <- run_coverage(coverage_request(a, lay, new_range_model(toy_spec())))
  expect_length(cov$warnings, 1)
  expect_match(cov$warnings, "more than one cell")
})

test_that("pairs beyond the maximum distance leave cells with no nearest receiver", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 1, 1)
  req <- coverage_request(a, lay, new_range_model(toy_spec()))
  req$max_distance <- 300
  cov <- run_coverage(req)
  expect_true(anyNA(cov$nearest_m))
  expect_true(all(cov$p_array[is.na(cov$nearest_m)] == 0))
})

test_that("coverage_request() refuses missing inputs and keys on what changes the result", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 3, 1)
  rm <- new_range_model(toy_spec())
  expect_error(coverage_request(NULL, lay, rm), "study area")
  expect_error(coverage_request(a, NULL, rm), "at least one receiver")
  expect_error(coverage_request(a, lay[0, ], rm), "at least one receiver")
  req <- coverage_request(a, lay, rm)
  expect_identical(req$max_distance, 1071)
  expect_identical(coverage_request(a, lay, rm)$key, req$key)
  expect_false(identical(coverage_request(a, lay, new_range_model(default_spec()))$key, req$key))
})

test_that("a computation above the pair limit is refused with the way under it", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 6, 1)
  rm <- new_range_model(toy_spec())
  pairs <- estimate_pairs(a, lay, 1071)
  cov <- run_coverage(coverage_request(a, lay, rm))
  expect_gte(pairs, cov$n_pairs)
  expect_silent(coverage_request(a, lay, rm, max_pairs = pairs))
  err <- tryCatch(coverage_request(a, lay, rm, max_pairs = pairs - 1), error = identity)
  first <- sprintf("^%s receiver-to-cell distances lie within 1,071 m", fmt_int(pairs))
  expect_match(conditionMessage(err), first)
  expect_match(conditionMessage(err), "These 5 receivers fit together: ", fixed = TRUE)
  expect_match(conditionMessage(err), "Keep only these, or aggregate the raster by 2", fixed = TRUE)
  expect_match(work_line(a, lay, rm, 1e6),
               sprintf("^Size: %s receiver-to-cell distances within 1,071 m", fmt_int(pairs)))
  expect_match(work_line(a, lay, rm, 1e6), "\\(limit 1,000,000\\)")
  expect_match(work_line(a, lay, rm, 1e6), "out of range: there the curve is below 0.001.$")
  expect_identical(work_line(a, lay, NULL), "")
  expect_identical(estimate_pairs(a, NULL, 1071), 0)
})

test_that("the pair-limit advice names receivers that fit, and the limit when none does", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 6, 1)
  rm <- new_range_model(toy_spec())
  each <- estimate_pairs_each(a, lay, 1071)
  expect_length(each, 6)
  expect_identical(sum(each), estimate_pairs(a, lay, 1071))
  expect_gt(length(unique(each)), 1)
  # At each limit, the receivers the message names fit together, and no set
  # of one more receiver fits: the lightest such set is already over.
  named <- function(msg) {
    if (grepl("Only receiver ", msg, fixed = TRUE)) {
      return(sub(" fits:.*$", "", sub("^.*Only receiver ", "", msg)))
    }
    strsplit(sub("\\. Keep only these.*$", "", sub("^.*fit together: ", "", msg)), ", ")[[1]]
  }
  for (limit in c(min(each), sum(sort(each)[1:3]), sum(each) - 1)) {
    msg <- conditionMessage(tryCatch(coverage_request(a, lay, rm, max_pairs = limit),
                                     error = identity))
    ids <- named(msg)
    expect_true(all(ids %in% lay$station_id), info = msg)
    expect_silent(coverage_request(a, lay[lay$station_id %in% ids, ], rm, max_pairs = limit))
    expect_gt(sum(sort(each)[seq_len(length(ids) + 1)]), limit)
  }
  one <- conditionMessage(tryCatch(coverage_request(a, lay, rm, max_pairs = min(each)),
                                   error = identity))
  lightest <- lay$station_id[which.min(each)]
  expect_match(one, sprintf("Only receiver %s fits: keep it alone", lightest), fixed = TRUE)
  # One below what the lightest receiver takes: no receiver fits, so the
  # advice gives the limit the whole layout needs, which then works, and the
  # aggregation factor. A deployed app takes no run_app() call, so the advice
  # names none.
  err <- tryCatch(coverage_request(a, lay, rm, max_pairs = min(each) - 1), error = identity)
  advice <- sprintf(paste("Not one of these receivers fits: each takes at least %s distances, and",
                          "all 6 together need a limit (max_pairs) of %s or more."),
                    fmt_int(min(each)), fmt_int(sum(each)))
  expect_match(conditionMessage(err), advice, fixed = TRUE)
  expect_match(conditionMessage(err), "Aggregate the raster by [0-9]+ before upload")
  expect_false(grepl("run_app(", conditionMessage(err), fixed = TRUE))
  expect_false(grepl("at most 0", conditionMessage(err)))
  expect_silent(coverage_request(a, lay, rm, max_pairs = sum(each)))
  # The simulation takes every water cell for each receiver, so any set of
  # the advised size fits.
  ids <- c("S001", "S002", "S003", "S004")
  sim <- simulation_refusal(a$n_water, ids, a$n_water)
  expect_match(sim, "Keep any one of these 4 receivers", fixed = TRUE)
  sim <- simulation_refusal(a$n_water, ids, 2 * a$n_water)
  expect_match(sim, "Keep any 2 of these 4 receivers", fixed = TRUE)
  sim <- simulation_refusal(a$n_water, ids, a$n_water - 1)
  expect_match(sim, sprintf("all 4 together need a limit (max_pairs) of %s or more",
                            fmt_int(4 * a$n_water)), fixed = TRUE)
  expect_false(grepl("run_app(", sim, fixed = TRUE))
  # One receiver that does not fit: the message speaks of it alone, in the
  # coverage refusal and in the simulation's.
  lone <- lay[which.min(each), , drop = FALSE]
  err <- tryCatch(coverage_request(a, lone, rm, max_pairs = min(each) - 1), error = identity)
  msg <- conditionMessage(err)
  expect_match(msg, sprintf(paste("This receiver does not fit: it takes %s distances, and the",
                                  "limit (max_pairs) must be %s or more. Aggregate the raster by"),
                            fmt_int(min(each)), fmt_int(min(each))), fixed = TRUE)
  expect_no_match(msg, "all 1|each takes|these receivers")
  expect_silent(coverage_request(a, lone, rm, max_pairs = min(each)))
  sim <- simulation_refusal(a$n_water, "S001", a$n_water - 1)
  expect_match(sim, sprintf("This receiver does not fit: it takes %s distances",
                            fmt_int(a$n_water)), fixed = TRUE)
  expect_no_match(sim, "all 1|each takes|these receivers")
})

test_that("coverage_state() says none, running, current, stale or invalid, with the reasons", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 3, 1)
  rm <- new_range_model(toy_spec())
  req <- coverage_request(a, lay, rm)
  cov <- run_coverage(req)
  expect_identical(coverage_state(NULL, req, "initial", a)$kind, "none")
  expect_identical(coverage_state(cov, req, "running", a)$kind, "running")
  expect_identical(coverage_state(cov, req, "success", a)$kind, "current")
  st <- coverage_state(cov, coverage_request(a, add_receiver(lay, a, 725300, 4935600), rm),
                       "success", a)
  expect_identical(st$kind, "stale")
  expect_identical(st$reasons, "the results are for 3 receivers and the layout now has 4")
  moved <- lay
  moved$x[1] <- moved$x[1] + 30
  expect_identical(coverage_state(cov, coverage_request(a, moved, rm), "success", a)$reasons,
                   "receivers moved")
  st <- coverage_state(cov, coverage_request(a, lay, new_range_model(default_spec())), "success",
                       a)
  expect_identical(st$reasons, "the range model changed")
  other <- new_study_area(-terra::unwrap(a$grid), "neg")
  expect_match(coverage_state(cov, coverage_request(other, lay, rm), "success",
                              other)$reasons[1],
               "study area changed")
  no_rx <- tryCatch(coverage_request(a, NULL, rm), error = identity)
  expect_identical(coverage_state(NULL, no_rx, "initial", a)$kind, "none")
  st <- coverage_state(cov, no_rx, "success", a)
  expect_identical(st[c("kind", "reasons")], list(kind = "invalid",
                                                  reasons = "Place at least one receiver."))
  expect_true(st$has_result)
  # After a switch of study area the result is for the other lake and is not
  # shown, so the banner must not say that results are on screen.
  st <- coverage_state(cov, no_rx, "success", other)
  expect_identical(st$kind, "invalid")
  expect_false(st$has_result)
  expect_false(grepl("results on screen", as.character(banner_ui(st))))
  expect_false(coverage_state(cov, no_rx, "success", NULL)$has_result)
  bad <- simpleError("The range model is invalid: At 1 m, d05 (250 m) must be larger.")
  expect_identical(coverage_state(NULL, bad, "initial", a)$kind, "invalid")
  expect_identical(invalid_reason(simpleError("")), "An input is not ready yet.")
})

test_that("banner_ui() draws one alert per state", {
  expect_match(as.character(banner_ui(list(kind = "none"))), "No coverage yet")
  expect_match(as.character(banner_ui(list(kind = "running"))), "Computing")
  expect_match(as.character(banner_ui(list(kind = "current"))), "alert-success")
  html <- as.character(banner_ui(list(kind = "stale", reasons = c("a", "b"))))
  expect_match(html, "alert-warning")
  expect_match(html, "a; b. Press Compute coverage.")
  html <- as.character(banner_ui(list(kind = "invalid", reasons = "Why.", has_result = TRUE)))
  expect_match(html, "Coverage cannot be computed. </strong>\\s*Why. The results on screen are for")
  html <- as.character(banner_ui(list(kind = "invalid", reasons = "Why.", has_result = FALSE)))
  expect_false(grepl("results on screen", html))
})

test_that("a result from the cache gives no compute time", {
  cov <- perf_cov()
  expect_match(as.character(coverage_line(cov)), "Computed in")
  cov$from_cache <- TRUE
  line <- as.character(coverage_line(cov))
  expect_false(grepl("Computed in", line))
  expect_match(line, "<li>Taken from the cache: computed earlier for the same inputs</li>",
               fixed = TRUE)
})

test_that("the range model's name travels from the request to the result", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 3, 1)
  req <- coverage_request(a, lay, new_range_model(default_spec()))
  expect_identical(req$range_label, range_model_label(default_spec()))
  relabelled <- new_range_model(default_spec())
  relabelled$label <- "another name"
  expect_identical(coverage_request(a, lay, relabelled)$key, req$key)
  cov <- run_coverage(req)
  expect_identical(cov$range_label, range_model_label(default_spec()))
  expect_match(as.character(coverage_line(cov)),
               "Range model: App defaults, unsourced")
})

test_that("detfield's message about land between pairs stays out of the console", {
  cov <- expect_silent(perf_cov())
  expect_gt(cov$n_shadowed, 0)
})

test_that("a stale result with no other change names the settings", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 3, 1)
  req <- coverage_request(a, lay, new_range_model(toy_spec()))
  cov <- run_coverage(req)
  req$key <- "another"
  expect_identical(coverage_state(cov, req, "success", a)$reasons,
                   "the simulation settings changed")
})

test_that("the map headline gives the share and its cells, and nothing else", {
  cov <- perf_cov()
  line <- map_headline(cov)
  html <- as.character(line)
  expect_match(html, sprintf("<strong>%s of the water covered</strong>",
                             gsub(".", "\\.", fmt_pct(mean(cov$p_array >= 0.5)), fixed = TRUE)))
  expect_match(html, sprintf("%s of %s water cells", fmt_int(sum(cov$p_array >= 0.5)),
                             fmt_int(length(cov$cells))), fixed = TRUE)
  expect_false(grepl("Range model|receivers|Computed", html))
  banner <- as.character(banner_ui(list(kind = "current"), headline = line))
  expect_match(banner, paste0("alert-success\"[^>]*>\\s*<div class=\"coverage-headline\">",
                              "\\s*<strong>[0-9.]+% of"))
  expect_false(grepl("Coverage is for the layout", banner))
  # Without a headline, as the sidebar and the Simulation tab draw it, the
  # banner is one plain line.
  expect_identical(as.character(banner_ui(list(kind = "current"))),
                   paste0("<div class=\"alert py-2 mb-0 alert-success\" role=\"status\">",
                          "Coverage is from the specified receiver arrangement and ",
                          "detection range model.</div>"))
})
