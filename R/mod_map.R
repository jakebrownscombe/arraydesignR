# ---- Module: map ----
# One leaflet map. The study area sets the view; the layer and the receivers
# are redrawn through leafletProxy(), so a click re-sends the rings and not
# the raster. Clicks go back to the array module. The map opens on depth,
# with no receivers, and moves to the array detection probability when the
# first coverage result arrives, once. It is 500 px tall, so
# Stoney Lake opens at zoom 12 (the extent needs 488 px); at 1280 x 720 its
# scale bar ends 12 px above the window with the three-line banner above it,
# given the padding set in app_theme(). The map
# key sits to the right of the map, outside it, and below it on a phone.

mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(shiny::radioButtons(ns("layer"), NULL, choices = layer_choices,
                                           inline = TRUE)),
    shiny::div(
      class = "map-row",
      shiny::div(class = "map-canvas", leaflet::leafletOutput(ns("map"), height = map_height)),
      shiny::uiOutput(ns("key"), class = "map-key")
    ),
    shiny::p(class = "small text-muted", shiny::textOutput(ns("caption"), inline = TRUE))
  )
}

map_height <- "500px"

# `delay` gives the mean transmission delay of the Performance tab, which
# sets the hour layer; NA while that box holds no usable number.
mod_map_server <- function(id, area, layout, coverage, delay = function() default_delay) {
  shiny::moduleServer(id, function(input, output, session) {
    show_first_result(coverage, session)
    # area() is read before any terra call: a req() inside it must stay silent,
    # and S4 dispatch in terra::unwrap() would turn it into an error.
    bounds <- shiny::reactive({
      a <- area()
      area_bounds(a)
    })
    layer <- shiny::reactive({
      a <- area()
      map_layer(a, coverage(), shiny::req(input$layer), delay())
    })
    ring <- shiny::reactive(ring_colour(layer()))
    # A new study area renders a new map, which carries the layer and rings it
    # starts with; later changes travel through the proxy.
    output$map <- leaflet::renderLeaflet({
      # bounds() first: a req() in area() must leave the map depending on it,
      # so the map draws once the study area arrives.
      b <- bounds()
      m <- base_map(b, tiles = !isTRUE(area()$synthetic))
      shiny::isolate({
        l <- tryCatch(layer(), error = function(e) NULL)
        draw_receivers(draw_layer(m, l), layout(), ring_colour(l))
      })
    })
    shiny::observe(draw_layer(leaflet::leafletProxy("map", session), layer()))
    output$key <- shiny::renderUI({
      lay <- layout()
      map_key(layer(), if (is.null(lay)) 0L else nrow(lay), ring())
    })
    shiny::observe(draw_receivers(leaflet::leafletProxy("map", session), layout(), ring()))
    output$caption <- shiny::renderText(map_caption(input$layer, layer(), layout(), coverage(),
                                                    isTRUE(area()$synthetic), delay()))
    list(
      # Only a new click fires: a new study area must not replay the last one.
      click = shiny::reactive(click_xy(shiny::req(input$map_click), shiny::isolate(area()))),
      remove = shiny::reactive(input$map_marker_click$id)
    )
  })
}

# Selects the probability layer when the first coverage result exists, and
# then never again: a layer the user chooses later stays chosen.
show_first_result <- function(coverage, session) {
  first <- shiny::observe({
    if (is.null(coverage())) return()
    first$destroy()
    shiny::updateRadioButtons(session, "layer", selected = "p_array")
  })
  invisible(first)
}
