test_that("generate_layout() returns exactly n receivers in the contract columns", {
  a <- small_area()
  for (n in c(1, 5, 12, 50)) expect_equal(nrow(generate_layout(a, "grid", n, 1)), n)
  r <- generate_layout(a, "random", 7, 3)
  expect_equal(nrow(r), 7)
  expect_identical(r, generate_layout(a, "random", 7, 3))
  expect_true(all(layout_columns %in% names(r)))
  expect_equal(nrow(generate_layout(a, "random", 4, NA)), 4)
  expect_error(generate_layout(a, "grid", 2.5, 1), "whole number")
  expect_error(generate_layout(a, "grid", 0, 1), "whole number")
  expect_error(generate_layout(a, "grid", NA, 1), "whole number")
})

test_that("a downloaded layout uploads back to the same receivers", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 6, 1)
  path <- withr::local_tempfile(fileext = ".csv")
  write_layout(lay, path)
  back <- read_layout_csv(path, a)
  expect_equal(as.data.frame(back)[, layout_columns], as.data.frame(lay)[, layout_columns],
               tolerance = 1e-9)
  expect_equal(nrow(utils::read.csv(path)), 6)
  expect_identical(layout_filename(a, lay), "receivers_example_lake_n6_epsg32617.csv")
})

test_that("CSV uploads take longitude and latitude, snap near-shore points and refuse far ones", {
  a <- small_area()
  path <- withr::local_tempfile(fileext = ".csv")
  lay <- generate_layout(a, "grid", 3, 1)
  utils::write.csv(data.frame(receiver_serial = c("A", "B", "C"), deploy_lon = lay$deploy_lon,
                              deploy_lat = lay$deploy_lat), path, row.names = FALSE)
  got <- read_layout_csv(path, a)
  expect_equal(got$x, lay$x, tolerance = 1e-6)
  expect_identical(got$receiver_serial, c("A", "B", "C"))
  # 725015 is in the one-cell land margin of the example lake: it moves to water.
  utils::write.csv(data.frame(x = c(725015, 725300), y = c(4935600, 4935600)), path,
                   row.names = FALSE)
  expect_message(read_layout_csv(path, a), "moved off land")
  # Every land cell of the example lake touches water, so the refused point
  # lies outside the raster.
  utils::write.csv(data.frame(x = 724000, y = 4935600), path, row.names = FALSE)
  expect_error(read_layout_csv(path, a), "outside the raster")
  utils::write.csv(data.frame(station_id = c("s", "s"), x = c(725300, 725400),
                              y = c(4935600, 4935600)), path, row.names = FALSE)
  expect_error(read_layout_csv(path, a), "repeats")
  utils::write.csv(data.frame(x = numeric(), y = numeric()), path, row.names = FALSE)
  expect_error(read_layout_csv(path, a), "no rows")
})

test_that("added receivers take the next free number and removed ones go", {
  a <- small_area()
  first <- add_receiver(NULL, a, 725300, 4935600)
  expect_identical(first$station_id, "S001")
  lay <- generate_layout(a, "grid", 4, 1)
  more <- add_receiver(lay, a, 725300, 4935600)
  expect_equal(nrow(more), 5)
  expect_identical(more$station_id[5], "S005")
  expect_identical(more$receiver_serial[5], "R005")
  expect_s3_class(more, "receiver_layout")
  expect_identical(attr(more, "epsg"), 32617L)
  less <- remove_receiver(more, "S002")
  expect_identical(less$station_id, c("S001", "S003", "S004", "S005"))
  expect_identical(add_receiver(less, a, 725400, 4935700)$station_id[5], "S006")
  expect_identical(remove_receiver(less, "nope"), less)
  expect_null(remove_receiver(NULL, "S001"))
  odd <- less
  odd$station_id <- c("north", "south", "east", "west")
  odd$receiver_serial <- c("VR2W-a", "VR2W-b", "VR2W-c", "VR2W-d")
  expect_identical(next_number(odd), 5L)
  expect_error(add_receiver(lay, a, 724000, 4935600), "outside the raster")
})

test_that("the receiver table shows readable headers with units, and a note on them", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 2, 1)
  tab <- layout_table(lay)
  expect_identical(names(tab), c("Receiver", "Station", "x (m)", "y (m)", "Latitude",
                                 "Longitude", "Depth (m)"))
  expect_match(tab$Latitude, "^[0-9]+\\.[0-9]{5}$")
  expect_null(layout_table(NULL))
  expect_match(layout_table_note(a), "x and y: metres in EPSG 32617")
  expect_match(layout_table_note(a), "Depth: metres below the surface")
})

test_that("no grid or random receiver sits in 0 m water or below the minimum depth", {
  a <- shallow_area()
  expect_equal(sum(a$water_depth == 0), sum(arraydesign::example_lake()[] <= 3, na.rm = TRUE))
  # Without the floor a random draw puts receivers on the 0 m shore.
  bare <- arraydesign::place_receivers(terra::unwrap(a$grid), n = 300, method = "random", seed = 1)
  expect_gt(sum(bare$deploy_z == 0), 0)
  for (method in c("grid", "random")) {
    lay <- generate_layout(a, method, 30, 1, min_depth = 1)
    expect_equal(nrow(lay), 30)
    expect_true(all(lay$deploy_z >= 1))
    deep <- generate_layout(a, method, 30, 1, min_depth = 10)
    expect_true(all(deep$deploy_z >= 10))
  }
  # A floor of 0 still keeps receivers out of 0 m cells.
  expect_true(all(generate_layout(a, "grid", 30, 1, min_depth = 0)$deploy_z > 0))
  expect_error(generate_layout(a, "grid", 5, 1, min_depth = 25),
               "Only 0 water cells are 25 m deep or more")
  n_ok <- sum(a$water_depth >= 15)
  expect_error(generate_layout(a, "random", n_ok + 1, 1, min_depth = 15),
               sprintf("Ask for at most %s receivers", fmt_int(n_ok)))
  expect_error(generate_layout(a, "grid", 5, 1, min_depth = -1), "0 m or more")
  expect_error(generate_layout(a, "grid", 5, 1, min_depth = NA), "0 m or more")
})

test_that("a click on water shallower than the minimum moves to the nearest deep enough cell", {
  a <- shallow_area()
  grid <- terra::unwrap(a$grid)
  cell <- which(a$water_depth == 0)[1]
  xy <- a$water_xy[cell, ]
  expect_message(new <- add_receiver(NULL, a, xy[1], xy[2], min_depth = 1),
                 "fell on water 0 m deep, below the 1 m minimum. The receiver moved",
                 class = "arraydesignR_message")
  expect_gte(new$deploy_z, 1)
  ok <- which(a$water_depth >= 1)
  d <- sqrt((a$water_xy[ok, 1] - xy[1])^2 + (a$water_xy[ok, 2] - xy[2])^2)
  expect_equal(unname(sqrt((new$x - xy[1])^2 + (new$y - xy[2])^2)), min(d))
  deep <- which(a$water_depth >= 5)[1]
  expect_silent(stay <- add_receiver(NULL, a, a$water_xy[deep, 1], a$water_xy[deep, 2]))
  expect_equal(c(stay$x, stay$y), unname(a$water_xy[deep, ]))
  expect_error(add_receiver(NULL, a, xy[1], xy[2], min_depth = 50), "No water cell is 50 m deep")
  expect_identical(terra::res(grid)[1], 30)
})

test_that("an uploaded layout keeps shallow receivers where the file puts them, and names them", {
  a <- shallow_area()
  shallow <- which(a$water_depth == 0)[1:2]
  deep <- which(a$water_depth >= 5)[1]
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(station_id = c("A", "B", "C"),
                              x = a$water_xy[c(shallow, deep), 1],
                              y = a$water_xy[c(shallow, deep), 2]), path, row.names = FALSE)
  expect_warning(got <- read_layout_csv(path, a, min_depth = 1),
                 "2 of 3 receivers in the file sit too shallow .* 1 m minimum\\): A, B")
  expect_equal(got$x, unname(a$water_xy[c(shallow, deep), 1]))
  expect_warning(read_layout_csv(path, a, min_depth = 0), "\\(0 m, or under the 0 m minimum\\)")
})

test_that("GLATOS, OTN and short-form receiver files upload, with their depth", {
  a <- small_area()
  lay <- generate_layout(a, "grid", 3, 1)
  path <- withr::local_tempfile(fileext = ".csv")
  files <- list(
    glatos = data.frame(station = c("A", "B", "C"), ins_serial_no = 481001:481003,
                        deploy_lat = lay$deploy_lat, deploy_long = lay$deploy_lon),
    otn = data.frame(station_name = c("A", "B", "C"), latitude = lay$deploy_lat,
                     longitude = lay$deploy_lon),
    short_form = data.frame(STATION_NO = c("A", "B", "C"), DEPLOY_LAT = lay$deploy_lat,
                            DEPLOY_LONG = lay$deploy_lon, INS_SERIAL_NO = 481001:481003)
  )
  for (f in files) {
    utils::write.csv(f, path, row.names = FALSE)
    got <- read_layout_csv(path, a)
    expect_equal(got$x, lay$x, tolerance = 1e-6)
    expect_identical(got$station_id, c("A", "B", "C"))
  }
  expect_identical(got$receiver_serial, c("481001", "481002", "481003"))
  expect_match(layout_table_note(a, got), "Depth: metres below the surface, from the depth raster")
  # An empty depth column leaves the raster's depth, with no message.
  utils::write.csv(cbind(files$glatos, instrument_depth = NA), path, row.names = FALSE)
  expect_silent(blank <- read_layout_csv(path, a))
  expect_equal(blank$deploy_z, lay$deploy_z)
  # A receiver depth in the file replaces the raster cell's, and the note says so.
  utils::write.csv(cbind(files$glatos, instrument_depth = c(2.5, 3, 13.8)), path, row.names = FALSE)
  expect_message(got <- read_layout_csv(path, a),
                 paste("Receiver depth from the file's `instrument_depth` column for 3 of 3",
                       "receivers \\(2.5 to 13.8 m\\); the raster gives .* to .* m at their cells"),
                 class = "arraydesignR_message")
  expect_equal(got$deploy_z, c(2.5, 3, 13.8))
  expect_match(layout_table_note(a, got), "from the uploaded file\\.")
  deep <- a$water_xy[which.max(a$water_depth), ]
  more <- add_receiver(got, a, deep[[1]], deep[[2]])
  expect_match(layout_table_note(a, more),
               "from the uploaded file for 3 receivers and from the depth raster for the rest")
  expect_identical(attr(remove_receiver(more, "A"), "depth_from_file"), c("B", "C"))
  # With no coordinates the refusal names what the file has, in the user's terms.
  utils::write.csv(data.frame(site = "A", lon = lay$deploy_lon[1], lat = lay$deploy_lat[1]), path,
                   row.names = FALSE)
  err <- tryCatch(read_layout_csv(path, a), error = conditionMessage)
  expect_match(err, "^The file needs longitude and latitude columns")
  expect_match(err, "It has: site, lon, lat.", fixed = TRUE)
  expect_false(grepl("`coords`", err, fixed = TRUE))
  # A deployment history repeats its stations; the refusal names them.
  utils::write.csv(rbind(files$glatos, files$glatos[1, ]), path, row.names = FALSE)
  expect_error(read_layout_csv(path, a), "`station` repeats in the CSV file \\(A\\)")
  serials <- files$otn
  serials$receiver_serial_number <- 7
  utils::write.csv(serials, path, row.names = FALSE)
  expect_error(read_layout_csv(path, a),
               "`receiver_serial_number` repeats in the CSV file \\(7\\): one row per receiver")
  many <- data.frame(station = rep(letters[1:7], 2), x = lay$x[1], y = lay$y[1])
  utils::write.csv(many, path, row.names = FALSE)
  expect_error(read_layout_csv(path, a), "\\(a, b, c, d, e and 2 more\\)")
})

test_that("layout messages read in the singular at one cell and one receiver", {
  # The example lake with one cell set to 50 m, the only cell 30 m deep or more.
  g <- arraydesign::example_lake()
  g[which(!is.na(terra::values(g)))[1]] <- 50
  a <- new_study_area(g, "One deep cell")
  expect_identical(sum(a$water_depth >= 30), 1L)
  expect_error(generate_layout(a, "random", 2, 1, min_depth = 30),
               paste("^Only 1 water cell is .* m deep or more. Ask for at most 1 receiver,",
                     "or lower"), class = "arraydesignR_error")
  lay <- data.frame(station_id = "A", deploy_z = 0.5)
  expect_warning(warn_shallow(lay, 1),
                 "^1 of 1 receiver in the file sits too shallow .*: A\\. It is kept\\.$",
                 class = "arraydesignR_warning")
  two <- data.frame(station_id = c("A", "B"), deploy_z = c(0.5, 5))
  expect_warning(warn_shallow(two, 1), "^1 of 2 receivers in the file sits .* It is kept\\.$")
  expect_match(layout_table_note(a, structure(two, depth_from_file = "A")),
               "from the uploaded file for 1 receiver and from the depth raster for the rest",
               fixed = TRUE)
  # One receiver with a depth in the file: one depth each side, one cell.
  one <- generate_layout(a, "grid", 1, 1)
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(station_id = "A", x = one$x, y = one$y, deploy_z = 7.5), path,
                   row.names = FALSE)
  expect_message(read_layout_csv(path, a),
                 sprintf(paste0("^Receiver depth from the file's `deploy_z` column for 1 of 1",
                                " receiver \\(7.5 m\\); the raster gives %s m at its cell\\.\n$"),
                         format(round(one$deploy_z, 1))),
                 class = "arraydesignR_message")
  # Two receivers at one depth print that depth once.
  two_at <- generate_layout(a, "grid", 2, 1)
  utils::write.csv(data.frame(station_id = c("A", "B"), x = two_at$x, y = two_at$y,
                              deploy_z = c(4, 4)), path, row.names = FALSE)
  expect_message(read_layout_csv(path, a), "for 2 of 2 receivers \\(4 m\\)")
  expect_identical(span_text(c(2.54, 2.46)), "2.5")
  expect_identical(span_text(c(2.5, 13.8)), "2.5 to 13.8")
})
