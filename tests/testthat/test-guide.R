test_that("every number in the guide carries its source", {
  lines <- readLines(guide_path(), warn = FALSE)
  body <- lines[seq_len(grep("^### Sources", lines) - 1)]
  paras <- split(body, cumsum(!nzchar(body) | grepl("^- ", body)))
  paras <- vapply(paras, paste, "", collapse = " ")
  # Names that contain digits but are not quantities.
  names_re <- "d50|d05|WGS 84|positionR 1\\.x|Array DesignR 1\\.x|positionR 2\\.0|1\\.x"
  numeric <- grepl("[0-9]", gsub(names_re, "", paras))
  sourced <- grepl("Source:", paras, fixed = TRUE)
  expect_gt(sum(numeric), 5)
  expect_identical(unname(paras[numeric & !sourced]), character())
})

test_that("every source in the guide is public", {
  # A reader of the installed app can open a help page, the NEWS or the paper,
  # and cannot open a test file of the source tree.
  text <- paste(readLines(guide_path(), warn = FALSE), collapse = " ")
  sources <- regmatches(text, gregexpr("\\(Source: [^)]*\\)", text))[[1]]
  expect_gt(length(sources), 5)
  expect_identical(grep("tests/testthat|test-[a-z_]+\\.R", sources, value = TRUE),
                   character())
})

test_that("the guide renders and quotes Pedersen et al. verbatim", {
  html <- as.character(mod_guide_ui("guide"))
  expect_match(html, "<h3[^>]*>What the app does</h3>")
  text <- paste(readLines(guide_path(), warn = FALSE), collapse = " ")
  quote <- "the probability of at least one sensor detecting a signal emitted in that grid cell"
  expect_match(text, paste0("\"", quote, "\""), fixed = TRUE)
  expect_match(text, "10.1111/2041-210X.12255", fixed = TRUE)
})

test_that("the guide's numbers for the app-default curve and the hour tile are the ones the app computes", { # nolint: line_length_linter.
  text <- paste(readLines(guide_path(), warn = FALSE), collapse = " ")
  spec <- default_spec()
  expect_match(text, sprintf("out of range: %s m for the App defaults",
                             fmt_int(curve_cutoff(spec))), fixed = TRUE)
  model <- new_range_model(spec)$model
  p0 <- stats::plogis(model$predict(0, depth_m = spec$depth))
  expect_equal(p0, c(0.95, 0.95))
  expect_match(text, "detection probability of 0.95 at 0 m", fixed = TRUE)
  expect_equal(transmissions_per_hour(default_delay), 30)
  expect_match(text, sprintf("(%d s to start, so n = 30)", default_delay), fixed = TRUE)
  # The sections before Simulation describe the lakes the app offers, and the
  # app does not offer the toy lake of the tests.
  expect_false(grepl("toy lake", sub("### Simulation.*", "", text)))
})

test_that("every link in the guide opens in a new tab", {
  html <- as.character(mod_guide_ui("guide"))
  anchors <- regmatches(html, gregexpr("<a [^>]*>", html))[[1]]
  expect_gt(length(anchors), 2)
  expect_true(all(grepl("target=\"_blank\" rel=\"noopener\"", anchors, fixed = TRUE)))
})
