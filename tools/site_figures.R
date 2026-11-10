#!/usr/bin/env Rscript
# ---- README figures from the running app ----
# Usage: Rscript tools/site_figures.R [package-dir] [app-url]
# Shoots the app after Generate and Compute coverage at the default settings
# (Stoney Lake, 20 receivers on the grid, seed 1, the App defaults range
# model; the app opens with no receivers) with tools/app_shots.mjs and writes the
# README figures to man/figures:
#   app_map.png         README and vignette section 3: the Map tab after
#                       Compute coverage, 1400 x 900
#   app_performance.png vignette section 4: the Performance tab, 1400 x 900
#   app_range_model.png vignette section 2: the Range model tab, 1400 x 900
#   app_simulation.png  vignette section 5: the Simulation tab after one run
# With no app-url the script copies app.R alone to a temporary folder and runs
# it there on port 4839, never inside the package folder, and stops it after.
# Needs Google Chrome (or the browser named in CHROME), node, processx and magick.
# After a new shoot, record the values each screen shows in
# man/figures/app_shots.json, the record of what each screenshot printed.

# ---- Settings ----
port <- 4839L
# The shots are kept as Chrome writes them. Reduced to 256 colours (the detqc
# pattern) the navbar turns black and the magma scale bands, so they stay
# full colour. The README and the vignette include them, so the four ship in
# the tarball; .Rbuildignore keeps only concept_suite.png out.

# ---- Helpers ----
start_app <- function(pkg_dir, port) {
  dir <- tempfile("adr_app_")
  dir.create(dir)
  file.copy(file.path(pkg_dir, "app.R"), dir)
  proc <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c("-e", sprintf("shiny::runApp('app.R', port = %d, launch.browser = FALSE)", port)),
    wd = dir, stdout = "|", stderr = "|"
  )
  url <- sprintf("http://127.0.0.1:%d", port)
  for (i in 1:120) {
    ok <- tryCatch({
      readLines(url, n = 1, warn = FALSE)
      TRUE
    }, error = function(e) FALSE)
    if (ok) break
    Sys.sleep(0.5)
  }
  list(proc = proc, url = url)
}

describe <- function(file) {
  info <- magick::image_info(magick::image_read(file))
  cat(sprintf("%-20s %4d x %4d px, %s bytes\n", basename(file), info$width, info$height,
              format(file.size(file), big.mark = ",")))
}

# ---- Shoot ----
site_figures <- function(pkg_dir = ".", url = NULL) {
  pkg_dir <- normalizePath(pkg_dir)
  if (is.null(url)) {
    app <- start_app(pkg_dir, port)
    on.exit(app$proc$kill(), add = TRUE)
    url <- app$url
  }
  out_dir <- tempfile("adr_shots_")
  dir.create(out_dir)
  res <- system2("node", c(file.path(pkg_dir, "tools", "app_shots.mjs"), url, shQuote(out_dir)),
                 stdout = TRUE)
  cat(res, sep = "\n")
  shots <- list.files(out_dir, pattern = "\\.png$", full.names = TRUE)
  stopifnot(length(shots) == 4L)
  fig_dir <- file.path(pkg_dir, "man", "figures")
  for (f in shots) {
    describe(f)
    file.copy(f, fig_dir, overwrite = TRUE)
  }
  invisible(file.path(fig_dir, basename(shots)))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  site_figures(if (length(args) >= 1) args[1] else ".", if (length(args) >= 2) args[2] else NULL)
}
