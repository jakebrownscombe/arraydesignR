test_that("thresholds parse from text and refuse values outside (0, 1]", {
  expect_identical(parse_thresholds("0.5, 0.05;0.75 0.5"), c(0.05, 0.5, 0.75))
  expect_error(parse_thresholds("0, 0.5"), "above 0 and at most 1")
  expect_error(parse_thresholds("1.5"), "above 0 and at most 1")
  expect_error(parse_thresholds(""), "above 0 and at most 1")
  expect_error(parse_thresholds(NULL), "above 0 and at most 1")
  expect_error(parse_thresholds("0.5, 0.75%"), "numbers; \"0.75%\" is not.", fixed = TRUE)
  expect_error(parse_thresholds("0.5 abc x"), "\"abc\", \"x\" are not.", fixed = TRUE)
  expect_identical(parse_thresholds(" 0.5, "), 0.5)
})

test_that("value boxes, the table and the depth bands count the same cells", {
  cov <- perf_cov()
  b <- performance_boxes(cov)
  expect_equal(b$cells_50, sum(cov$p_array >= 0.5))
  expect_equal(b$share_50, mean(cov$p_array >= 0.5))
  expect_equal(b$n_water, 2152)
  expect_equal(b$n_receivers, 4)
  # The tiles hold the shares at p 0.5 and 0.05 and the share within an hour.
  expect_null(b$share_50_deaf)
  expect_null(b$mean_p)
  expect_false(grepl("behind land", as.character(value_boxes_ui(b))))
  tab <- threshold_display(cov, c(0.05, 0.5))
  expect_identical(tab[[3]][2], fmt_int(b$cells_50))
  expect_identical(tab[[2]][2], fmt_pct(b$share_50))
  bands <- depth_bands(cov, 0.5)
  expect_equal(sum(bands$n_cells), 2152)
  expect_equal(sum(bands$n_covered), b$cells_50)
  expect_equal(attr(bands, "overall"), b$share_50)
  expect_true(all(is.na(bands$share) | (bands$share >= 0 & bands$share <= 1)))
  expect_match(depth_caption(bands, cov), "all 2,152 water cells")
  expect_match(depth_caption(bands, cov), "the bars are not cumulative")
  expect_match(depth_caption(bands, cov), "Range model: Your values")
  expect_match(histogram_caption(cov), "all 2,152 water cells for 4 receivers", fixed = TRUE)
  expect_match(histogram_caption(cov), "one transmission")
  expect_match(histogram_caption(cov), "Range model: Your values")
})

test_that("the histogram is a ggplot of every water cell with a dashed line per threshold", {
  cov <- perf_cov()
  p <- plot_p_histogram(cov, c(0.05, 0.5))
  expect_s3_class(p, "ggplot")
  expect_identical(nrow(p$data), length(cov$p_array))
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], "", USE.NAMES = FALSE)
  expect_identical(geoms, c("GeomBar", "GeomVline"))
  b <- ggplot2::ggplot_build(p)
  expect_equal(sum(b$data[[1]]$count), length(cov$p_array))
  expect_equal(diff(b$data[[1]]$xmin), rep(0.02, nrow(b$data[[1]]) - 1))
  expect_equal(b$data[[2]]$xintercept, c(0.05, 0.5))
  expect_identical(unique(b$data[[2]]$linetype), "dashed")
  expect_identical(p$labels$x, "Array detection probability")
  expect_identical(p$labels$y, "Water cells")
  # The thresholds are labelled on the top axis, at the dashed lines.
  expect_identical(b$layout$panel_params[[1]]$x.sec$get_labels(), c("0.05", "0.50"))
  path <- withr::local_tempfile(fileext = ".png")
  ggplot2::ggsave(path, p, width = 5, height = 3.5, dpi = 96)
  expect_gt(file.size(path), 1000)
})

test_that("the depth bands are a ggplot of the share per band, against the overall share", {
  cov <- perf_cov()
  bands <- depth_bands(cov, 0.5)
  p <- plot_depth_bands(bands)
  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], "", USE.NAMES = FALSE)
  expect_identical(geoms, c("GeomCol", "GeomHline"))
  b <- ggplot2::ggplot_build(p)
  bars <- b$data[[1]]
  # Each bar is the share of its own band's cells, so the bars need not sum to 1.
  expect_equal(bars$y[order(bars$x)], bands$share[!is.na(bands$share)])
  expect_equal(bands$share, bands$n_covered / bands$n_cells)
  expect_equal(b$data[[2]]$yintercept, attr(bands, "overall"))
  expect_identical(unique(b$data[[2]]$linetype), "dashed")
  expect_identical(p$labels$y, "Share of the band's water cells\nat p of 0.50 or more")
  expect_identical(p$labels$x, "Depth band (m), water cells in it, and share at the threshold")
  expect_equal(b$layout$panel_params[[1]]$y$limits, c(0, 1))
  path <- withr::local_tempfile(fileext = ".png")
  ggplot2::ggsave(path, p, width = 9, height = 3.3, dpi = 96)
  expect_gt(file.size(path), 1000)
})

test_that("the performance module draws from the coverage result and waits for one", {
  res <- perf_cov()
  coverage <- shiny::reactiveVal(NULL)
  args <- list(coverage = coverage, area = shiny::reactiveVal(small_area()))
  shiny::testServer(mod_performance_server, args = args, {
    session$setInputs(thresholds = "0.05, 0.5", depth_threshold = 0.5)
    expect_error(output$table, "Compute coverage")
    coverage(res)
    session$flushReact()
    expect_equal(session$returned$boxes()$cells_50, sum(res$p_array >= 0.5))
    expect_match(output$table, fmt_int(sum(res$p_array >= 0.5)))
    expect_match(output$boxes$html, "Water covered at p &ge; 0.5", fixed = TRUE)
    expect_match(output$boxes$html, "Water detected within an hour", fixed = TRUE)
    expect_match(output$boxes$html, "30 transmissions at a 120 s delay", fixed = TRUE)
    expect_identical(output$context, "Example lake, 4 receivers.")
    expect_identical(session$returned$delay(), 120)
    session$setInputs(delay = 60)
    expect_match(output$boxes$html, "60 transmissions at a 60 s delay", fixed = TRUE)
    expect_identical(session$returned$delay(), 60)
    session$setInputs(delay = 0)
    expect_error(output$boxes, "above 0")
    expect_identical(session$returned$delay(), NA_real_)
    session$setInputs(delay = 120)
    expect_match(output$range, "^Your values")
    expect_true(nzchar(output$histogram$src))
    expect_true(nzchar(output$depth$src))
    expect_match(output$histogram_caption, "2,152")
    expect_match(output$depth_caption, "0.50 or more")
    session$setInputs(thresholds = "2")
    expect_error(output$table, "at most 1")
    session$setInputs(thresholds = "0.5, 0.75%")
    expect_error(output$table, "\"0.75%\" is not")
    session$setInputs(depth_threshold = 0)
    expect_error(output$depth, "above 0")
  })
})

test_that("the three tiles share one structure: title, number, two lines, equal heights", {
  b <- performance_boxes(perf_cov())
  html <- as.character(value_boxes_ui(b))
  # Equal heights across the row.
  expect_match(html, "grid-auto-rows:1fr", fixed = TRUE)
  boxes <- strsplit(html, "bslib-value-box", fixed = TRUE)[[1]][-1]
  expect_length(boxes, 3)
  # Each tile: exactly two lines under its number, each its own paragraph.
  lines <- lapply(boxes, function(x) regmatches(x, gregexpr("<p class=\"mb-0\">[^<]*</p>", x))[[1]])
  expect_identical(lengths(lines), c(2L, 2L, 2L))
  expect_identical(lines[[1]][2], sprintf("<p class=\"mb-0\">%s of %s water cells</p>",
                                          fmt_int(b$cells_50), fmt_int(b$n_water)))
  hour_lines <- c("<p class=\"mb-0\">p &ge; 0.95 of at least one detection in an hour</p>",
                  "<p class=\"mb-0\">30 transmissions at a 120 s delay</p>")
  expect_identical(lines[[3]], hour_lines)
  for (title in c("Water covered at p &ge; 0.5", "Water covered at p &ge; 0.05",
                  "Water detected within an hour")) {
    expect_match(html, title, fixed = TRUE)
    expect_lte(lengths(strsplit(title, " ")), 6)
  }
  expect_false(grepl("behind land|Mean probability", html))
  heads <- paste(names(threshold_display(perf_cov(), 0.5)), collapse = " ")
  expect_false(grepl("Single share", heads))
})

test_that("the hour share is 1 - (1 - p)^(3600 / delay) at 0.95, against a hand count", {
  cov <- perf_cov()
  expect_equal(transmissions_per_hour(120), 30)
  expect_equal(p_hour(0.1, 120), 1 - 0.9^30)
  expect_equal(p_hour(c(0, 1), 120), c(0, 1))
  b <- performance_boxes(cov, 120)
  hand <- 1 - (1 - cov$p_array)^30
  expect_equal(b$share_hour, mean(hand >= 0.95))
  expect_equal(b$cells_hour, sum(hand >= 0.95))
  # A cell at 0.5 for one transmission is all but certain within an hour.
  expect_gte(b$share_hour, b$share_50)
  expect_lte(performance_boxes(cov, 600)$share_hour, b$share_hour)
  expect_length(delay_problems(120), 0)
  expect_match(delay_problems(0), "above 0")
  expect_match(delay_problems(NA_real_), "above 0")
})

test_that("depth bars are one uniform fill, with counts and shares under the bands", {
  bands <- depth_bands(perf_cov(), 0.5)
  p <- plot_depth_bands(bands)
  bars <- ggplot2::ggplot_build(p)$data[[1]]
  # One fill and no transparency: the cell count is in the label only.
  expect_identical(unique(bars$fill), band_fill)
  expect_true(all(is.na(bars$alpha)))
  # The count and the share are the second and third lines of each band's
  # axis label, below the panel, where the dashed line does not run.
  labels <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
  expect_identical(labels, paste0(sprintf("%g-%g", bands$lower, bands$upper), "\n",
                                  fmt_int(bands$n_cells), "\n", fmt_pct(bands$share)))
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], "", USE.NAMES = FALSE)
  expect_false(any(geoms %in% c("GeomText", "GeomLabel")))
})
