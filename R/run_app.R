#' Run the Array DesignR app
#'
#' Starts the app on the first study area given in `areas`, or on the bundled
#' Stoney Lake raster when none is given. The app opens on the depth raster
#' with no receivers; click the map or press Generate (20 receivers by
#' default), then press Compute coverage. Stoney Lake, Ontario, is the one
#' bundled lake, offered after the given areas
#' (`system.file("extdata", "stoney_depth.tif", package = "arraydesignR")`).
#' It is a 100 m depth raster derived from
#' the bathymetry made before 2024. The Array DesignR 1.x app came with this
#' raster, and it has the same grid and depths as `depth_raster` of positionR
#' 1.7.0, with the CRS set to EPSG:32617. It holds 4,818 water cells. Depth is
#' zero or negative metres; 56 of the water cells hold 0. Each area is read
#' once when the app starts.
#'
#' A raster in longitude and latitude is projected to the UTM zone of its
#' centre: the standard six-degree zone, 1 to 60, north or south by the centre
#' latitude. The exceptions round Norway and Svalbard are not applied. A
#' raster whose longitudes all lie from 180 to 360 is read as 360 degrees
#' less, and a message says so; a raster that crosses 180 degrees keeps its
#' longitudes and takes the zone of its centre. A raster in longitude and
#' latitude is refused when it spans more than 12 degrees of longitude or 12
#' degrees of latitude. Its longitudes must lie from -180 to 180 or from 0 to
#' 360, and its latitudes from -90 to 90. A larger area is given as a raster in
#' a projected coordinate reference system in metres, and a lake across 180
#' degrees in the 0 to 360 form. A projected raster is refused when its scale
#' at the raster centre, on its two axes or its diagonals, is more than 1
#' percent from 1. It is also refused when that scale cannot be measured, as
#' in a local engineering CRS with no geographic datum.
#'
#' Coverage is computed as a [shiny::ExtendedTask] through
#' [promises::future_promise()]. Under the default sequential future plan the
#' computation runs in the app's own R process. Set
#' `future::plan(future::multisession)` before calling `run_app()` to run it
#' in a background process, so one user's computation does not hold up
#' another's; the deployed app does this in its `app.R`.
#'
#' Two limits keep one computation within the memory of a small server. A
#' study area given to `run_app()` or uploaded may hold at most
#' `max_water_cells` water cells; the bundled lake is not limited. One
#' computation may take at most `max_pairs` receiver and cell pairs, counted two ways.
#' Coverage counts only the pairs within the distance at which the detection
#' curve falls below 0.001. The simulation counts every receiver and water-cell
#' pair, since an animal can be in any water cell. Set the limits from the
#' memory of the server the app runs on.
#'
#' @param areas A named list of study areas: [terra::SpatRaster] objects or
#'   raster file paths of water depth in metres, with land as NA. The names
#'   label them in the app, which opens on the first. With none, the app
#'   opens on Stoney Lake.
#' @param options Passed to [shiny::shinyApp()], such as `list(port = 4321)`.
#' @param max_water_cells The largest number of water cells a study area,
#'   given here or uploaded, may hold.
#' @param max_pairs The largest number of receiver and cell pairs one
#'   coverage computation or simulation may take. It must be at least the
#'   largest number of water cells among the study areas, which one receiver
#'   takes; 4,818 for Stoney Lake alone.
#'
#' @return A Shiny app object; printing it runs the app.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(areas = NULL, options = list(), max_water_cells = 250000, max_pairs = 1e6) {
  limits <- app_limits(max_water_cells, max_pairs)
  loaded <- study_areas(areas, limits$water_cells)
  check_pairs_floor(limits$pairs, loaded)
  server <- function(input, output, session) {
    app_server(input, output, session, areas = loaded, limits = limits)
  }
  shiny::shinyApp(ui = app_ui, server = server, options = options)
}

# One receiver takes one distance per water cell in the simulation, and at
# most that many in the coverage computation. Below the largest water-cell
# count of the areas the app opens with, one receiver could not be placed on
# that area.
check_pairs_floor <- function(max_pairs, areas) {
  n <- vapply(areas, function(a) as.numeric(a$n_water), numeric(1))
  if (max_pairs < max(n)) {
    big <- names(n)[which.max(n)]
    stop_app(sprintf("`max_pairs` must be at least %s: one receiver on %s takes %s.",
                     fmt_int(max(n)), big, fmt_count(max(n), "receiver-to-cell distance")))
  }
  invisible(max(n))
}

app_limits <- function(water_cells = default_max_water_cells, pairs = default_max_pairs) {
  ok <- function(v) is.numeric(v) && length(v) == 1 && !is.na(v) && v >= 1
  if (!ok(water_cells) || !ok(pairs)) {
    stop_app("`max_water_cells` and `max_pairs` must each be one number of at least 1.")
  }
  list(water_cells = water_cells, pairs = pairs)
}
