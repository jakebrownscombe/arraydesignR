test_that("the array module opens empty, then generates, clears and edits", {
  a <- small_area()
  area <- shiny::reactiveVal(a)
  map <- fake_map()
  shiny::testServer(mod_array_server, args = list(area = area, map = map), {
    session$flushReact()
    expect_null(session$returned())
    expect_match(output$count, "^0 receivers on Example lake.")
    # With the box left alone, Generate places the default number.
    session$setInputs(generate = 1)
    expect_equal(nrow(session$returned()), default_n_receivers)
    session$setInputs(method = "grid", n = 9, seed = 1, min_depth = 1, generate = 2)
    expect_equal(nrow(session$returned()), 9)
    expect_match(output$count, "9 receivers on Example lake.")
    expect_match(output$table, "S009")
    expect_match(output$table, "Depth \\(m\\)")
    expect_match(output$table_note, "EPSG 32617")
    map$click(list(x = 725300, y = 4935600))
    session$flushReact()
    expect_equal(nrow(session$returned()), 10)
    map$remove("S003")
    session$flushReact()
    expect_false("S003" %in% session$returned()$station_id)
    map$click(list(x = 724000, y = 4935600))
    session$flushReact()
    expect_equal(nrow(session$returned()), 9)
    session$setInputs(n = 0, generate = 3)
    expect_equal(nrow(session$returned()), 9)
    # Clear asks first; the layout goes only on "Remove all".
    session$setInputs(clear = 1)
    expect_equal(nrow(session$returned()), 9)
    session$setInputs(clear_ok = 1)
    expect_null(session$returned())
    expect_match(output$count, "^0 receivers")
  })
})

test_that("the minimum depth in the sidebar reaches the grid and the map click", {
  area <- shiny::reactiveVal(shallow_area())
  map <- fake_map()
  shiny::testServer(mod_array_server, args = list(area = area, map = map), {
    session$setInputs(method = "grid", n = 30, seed = 1, min_depth = 6, generate = 1)
    expect_true(all(session$returned()$deploy_z >= 6))
    a <- area()
    xy <- a$water_xy[which(a$water_depth == 0)[1], ]
    map$click(list(x = xy[1], y = xy[2]))
    session$flushReact()
    expect_equal(nrow(session$returned()), 31)
    expect_gte(session$returned()$deploy_z[31], 6)
  })
})

test_that("the download is the layout on screen, and an upload replaces it", {
  a <- small_area()
  shiny::testServer(mod_array_server, args = list(area = shiny::reactiveVal(a), map = fake_map()), {
    session$setInputs(method = "random", n = 4, seed = 2, generate = 1)
    file <- output$download
    got <- utils::read.csv(file, stringsAsFactors = FALSE)
    expect_equal(nrow(got), 4)
    expect_identical(names(got), layout_columns)
    expect_equal(got$x, session$returned()$x)
    session$setInputs(n = 6, generate = 2)
    session$setInputs(csv = list(name = "rx.csv", datapath = file))
    expect_equal(nrow(session$returned()), 4)
    expect_equal(session$returned()$y, got$y)
  })
})

test_that("a new study area starts with no receivers; Generate uses the controls", {
  area <- shiny::reactiveVal(small_area())
  shiny::testServer(mod_array_server, args = list(area = area, map = fake_map()), {
    session$setInputs(method = "grid", n = 3, seed = 1, min_depth = 1, generate = 1)
    expect_equal(nrow(session$returned()), 3)
    area(stoney_area())
    session$flushReact()
    expect_null(session$returned())
    expect_match(output$count, "0 receivers on Stoney Lake (100 m).", fixed = TRUE)
    session$setInputs(generate = 2)
    expect_equal(nrow(session$returned()), 3)
    expect_match(output$count, "3 receivers on Stoney Lake (100 m).", fixed = TRUE)
  })
})

test_that("Generate sits beside Compute coverage, and Clear and Download look enabled", {
  html <- as.character(app_ui(NULL))
  row <- regmatches(html, regexpr("(?s)<div class=\"d-flex gap-2 mb-2\">.*?</div>", html,
                                  perl = TRUE))
  expect_length(row, 1)
  expect_match(row, "id=\"array-generate\"")
  expect_match(row, "id=\"coverage-compute\"")
  expect_match(html, "btn-outline-primary btn-sm\" id=\"array-clear\"")
  expect_match(html, "btn-outline-primary btn-sm mb-2\" download href=\"\" id=\"array-download\"")
  expect_match(as.character(mod_array_table_ui("array")), "class=\"table-responsive\"",
               fixed = TRUE)
})
