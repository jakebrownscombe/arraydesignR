# ---- App server ----
# Wiring only: each module returns reactives and the next one reads them. The
# map and the array module read each other, so the map is given a function
# that looks the layout up when it is first used. The map's hour layer reads
# the transmission delay of the Performance tab the same way. `areas` comes
# from run_app(); without it the session offers Stoney Lake. `limits` holds
# the largest water cells per study area and pairs per computation.

app_server <- function(input, output, session, areas = study_areas(), limits = app_limits()) {
  area <- mod_study_area_server("area", areas, limits$water_cells)
  range_model <- mod_range_model_server("range", area)
  map <- mod_map_server("map", area, layout = function() layout(),
                        coverage = function() coverage$coverage(),
                        delay = function() performance$delay())
  layout <- mod_array_server("array", area, map)
  coverage <- mod_coverage_server("coverage", area, layout, range_model,
                                  max_pairs = limits$pairs)
  performance <- mod_performance_server("performance", coverage$coverage, area)
  score <- mod_score_server("score", area, layout, range_model, max_pairs = limits$pairs)
  invisible(list(area = area, layout = layout, range_model = range_model,
                 coverage = coverage, performance = performance, score = score))
}
