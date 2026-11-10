# ---- Module: study area ----
# Chooses the depth raster: the bundled Stoney Lake, an area given to run_app(), or an
# upload. Returns a reactive study area (see new_study_area()).

mod_study_area_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h6("1. Study area"),
    shiny::selectInput(ns("choice"), "Depth raster", choices = NULL),
    shiny::p(class = "small", shiny::textOutput(ns("summary"), inline = TRUE))
  )
}

# The upload sits with the other files, below the coverage controls, so the
# buttons the first screen needs stay in view.
mod_study_area_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(ns("upload"), "Upload a depth raster (GeoTIFF; land as NA)",
                     accept = c(".tif", ".tiff")),
    shiny::uiOutput(ns("refused"))
  )
}

mod_study_area_server <- function(id, areas, max_water_cells = default_max_water_cells) {
  shiny::moduleServer(id, function(input, output, session) {
    uploaded <- shiny::reactiveVal(NULL)
    refused <- shiny::reactiveVal(NULL)
    shiny::updateSelectInput(session, "choice", choices = names(areas),
                             selected = names(areas)[1])
    shiny::observeEvent(input$upload, {
      name <- paste("Upload:", input$upload$name)
      up <- tryCatch(warn_notify(read_upload(input$upload, name, max_water_cells), session),
                     error = identity)
      if (inherits(up, "error")) return(refused(conditionMessage(up)))
      refused(NULL)
      uploaded(up)
      shiny::updateSelectInput(session, "choice", choices = c(names(areas), name),
                               selected = name)
    })
    shiny::observeEvent(input$choice, refused(NULL), ignoreInit = TRUE)
    area <- shiny::reactive({
      shiny::req(input$choice)
      up <- uploaded()
      if (!is.null(up) && identical(input$choice, up$name)) return(up)
      shiny::req(input$choice %in% names(areas))
      areas[[input$choice]]
    })
    output$summary <- shiny::renderText(area_summary(area()))
    output$refused <- shiny::renderUI(refusal_alert(refused(), area()$name))
    area
  })
}

# An ASCII grid keeps its coordinate system in a .prj file beside it, which a
# one-file upload cannot carry, so only GeoTIFF is read.
read_upload <- function(upload, name, max_water_cells) {
  if (!grepl("\\.tiff?$", upload$name, ignore.case = TRUE)) {
    stop_app(sprintf(paste("%s is not a GeoTIFF. Save the raster as a GeoTIFF (.tif) with",
                           "its coordinate system."), upload$name))
  }
  new_study_area(upload$datapath, name, max_water_cells = max_water_cells)
}

refusal_alert <- function(message, current) {
  if (is.null(message)) return(NULL)
  shiny::div(class = "alert alert-danger py-2 mt-2", role = "alert",
             shiny::strong("Upload refused. "), message,
             sprintf(" The app stays on %s.", current))
}
