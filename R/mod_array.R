# ---- Module: receiver array ----
# Holds the receiver layout, the one piece of state the app edits. Layouts
# come from arraydesign::place_receivers(): a regular grid or a random draw
# of exactly n receivers, a CSV upload, or clicks on the map. Each study area
# opens with no receivers, so the map shows the depth raster alone.

# Receivers that Generate places unless the box says otherwise.
default_n_receivers <- 20

# The layout controls. Generate sits in its own function so the page can put
# it beside Compute coverage, where both are in view on a small screen.
mod_array_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h6("2. Receivers"),
    shiny::radioButtons(ns("method"), NULL, inline = TRUE,
                        choices = c("Regular grid" = "grid", "Random" = "random")),
    bslib::layout_columns(
      col_widths = c(6, 6), gap = "0.5rem",
      shiny::numericInput(ns("n"), "Receivers", value = default_n_receivers, min = 1, step = 1),
      shiny::numericInput(ns("seed"), "Random seed", value = 1, step = 1)
    ),
    shiny::numericInput(ns("min_depth"), "Minimum receiver depth (m)",
                        value = default_min_depth, min = 0, step = 0.5)
  )
}

mod_array_generate_ui <- function(id) {
  shiny::actionButton(shiny::NS(id, "generate"), "Generate", class = "btn-primary")
}

# Clear, the files and the count, below the coverage controls.
mod_array_files_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "d-flex align-items-center gap-2 mb-2",
               shiny::actionButton(ns("clear"), "Clear", class = "btn-outline-primary btn-sm"),
               shiny::textOutput(ns("count"), inline = TRUE)),
    shiny::helpText("Click the map to add a receiver; click a receiver to remove it."),
    shiny::fileInput(ns("csv"), "Upload receivers (CSV: longitude and latitude, or x and y)",
                     accept = ".csv"),
    shiny::downloadButton(ns("download"), "Download receivers (CSV)",
                          class = "btn-outline-primary btn-sm mb-2")
  )
}

# On a phone the table scrolls inside its card, so the page keeps its width.
mod_array_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "table-responsive", shiny::tableOutput(ns("table"))),
    shiny::p(class = "small text-muted", shiny::textOutput(ns("table_note"), inline = TRUE))
  )
}

mod_array_server <- function(id, area, map) {
  shiny::moduleServer(id, function(input, output, session) {
    layout <- shiny::reactiveVal(NULL)
    edit <- function(new) if (!is.null(new)) layout(new)
    floor_m <- function() input$min_depth %||% default_min_depth
    make <- function() {
      try_notify(generate_layout(area(), input$method %||% "grid", input$n %||% default_n_receivers,
                                 input$seed %||% 1, floor_m()), session)
    }
    # A new study area starts empty: a layout belongs to the raster it was made on.
    shiny::observeEvent(area(), layout(NULL))
    # Clear asks first because a layout placed by hand cannot be recovered.
    shiny::observeEvent(input$clear, {
      n <- if (is.null(layout())) 0L else nrow(layout())
      if (!n) return(invisible(NULL))
      shiny::showModal(shiny::modalDialog(
        sprintf("Remove %s from %s?", if (n == 1) "the 1 receiver" else
                  paste("all", fmt_count(n, "receiver")), area()$name),
        title = "Clear the layout", easyClose = TRUE,
        footer = shiny::tagList(shiny::modalButton("Cancel"),
                                shiny::actionButton(session$ns("clear_ok"), "Remove all",
                                                    class = "btn-danger"))
      ))
    })
    shiny::observeEvent(input$clear_ok, {
      layout(NULL)
      shiny::removeModal()
    })
    shiny::observeEvent(input$generate, edit(make()))
    shiny::observeEvent(input$csv, {
      edit(try_notify(read_layout_csv(input$csv$datapath, area(), floor_m()), session))
    })
    shiny::observeEvent(map$click(), {
      xy <- map$click()
      edit(try_notify(add_receiver(layout(), area(), xy$x, xy$y, floor_m()), session))
    })
    shiny::observeEvent(map$remove(), edit(remove_receiver(layout(), map$remove())))
    count <- shiny::reactive({
      n <- if (is.null(layout())) 0L else nrow(layout())
      sprintf("%s on %s.", fmt_count(n, "receiver"), area()$name)
    })
    table <- shiny::reactive(layout_table(layout()))
    output$count <- shiny::renderText(count())
    output$table <- shiny::renderTable(table())
    output$table_note <- shiny::renderText(layout_table_note(area(), layout()))
    output$download <- shiny::downloadHandler(
      filename = function() layout_filename(area(), layout()),
      content = function(file) write_layout(shiny::req(layout()), file),
      contentType = "text/csv"
    )
    shiny::reactive(layout())
  })
}
