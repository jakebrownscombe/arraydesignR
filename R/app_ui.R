# ---- App UI ----
# The page frame only; each module draws its own part (Mastering Shiny,
# chapter 19). Generate and Compute coverage share one row near the top of
# the sidebar, so both are in view at 1280 x 720 without scrolling.

app_ui <- function(request) {
  bslib::page_navbar(
    title = "Array DesignR",
    theme = app_theme(),
    fillable = FALSE,
    # On a phone the controls come first, above the map and the tables.
    sidebar = bslib::sidebar(
      width = 340, open = list(desktop = "open", mobile = "always-above"),
      mod_study_area_ui("area"),
      shiny::hr(class = "my-2"),
      mod_array_ui("array"),
      shiny::div(class = "d-flex gap-2 mb-2", mod_array_generate_ui("array"),
                 mod_coverage_button_ui("coverage")),
      mod_coverage_ui("coverage"),
      shiny::hr(class = "my-2"),
      shiny::h6("4. Files"),
      mod_study_area_upload_ui("area"),
      mod_array_files_ui("array")
    ),
    bslib::nav_panel(
      "Map",
      mod_coverage_banner_ui("coverage", "map"),
      mod_map_ui("map"),
      bslib::card(bslib::card_header("Receivers"), mod_array_table_ui("array"))
    ),
    bslib::nav_panel("Range model", mod_range_model_ui("range")),
    bslib::nav_panel("Performance", mod_coverage_banner_ui("coverage", "performance"),
                     mod_performance_ui("performance")),
    bslib::nav_panel("Simulation", mod_score_ui("score")),
    bslib::nav_panel("Guide", mod_guide_ui("guide"))
  )
}

# The map key sits beside the map, outside it, so it covers no lake; on
# a phone it sits below the map. The navbar and the top of the
# main column take 8 px of padding (flatly's is 16), so at 1280 x 720 the
# 500 px map's scale bar ends 12 px above the window with the three-line
# banner above it; banner and map card sit 8 px apart.
app_theme <- function() {
  bslib::bs_add_rules(
    bslib::bs_theme(version = 5, preset = "flatly", "navbar-padding-y" = "0.5rem"),
    # bslib's own selector, so this rule outranks its component CSS, which loads later.
    paste(".bslib-page-navbar > div > .bslib-sidebar-layout > .main { padding-top: 0.5rem; }",
          ".map-row { display: flex; gap: 12px; align-items: flex-start; }",
          ".map-canvas { flex: 1 1 auto; min-width: 0; }",
          ".map-key { flex: 0 0 130px; font-size: 0.75rem; line-height: 1.2; }",
          ".map-key-title { font-weight: 600; margin-bottom: 8px; }",
          ".map-key-scale { position: relative; height: 220px; }",
          ".map-key-bar { position: absolute; left: 0; top: 0; bottom: 0; width: 16px;",
          "opacity: 0.85; border: 1px solid #888; }",
          ".map-key-label { position: absolute; left: 24px; transform: translateY(-50%);",
          "white-space: nowrap; }",
          ".map-key-mark { display: flex; align-items: center; gap: 6px; margin-top: 16px; }",
          "@media (max-width: 576px) { .map-row { flex-direction: column; }",
          ".map-key { flex-basis: auto; } .map-key-scale { height: 110px; } }")
  )
}
