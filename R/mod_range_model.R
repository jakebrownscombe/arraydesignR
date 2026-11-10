# ---- Module: range model ----
# d50 and d05 at two receiver depths give the detection curve. Each study area
# starts on its named curve (range_presets); values the user typed are kept
# when the area changes. The module refuses a curve that rises with distance
# and returns the model as a reactive for the coverage module.

range_inputs <- c("depth_1", "d50_1", "d05_1", "depth_2", "d50_2", "d05_2")

# The settings sit in one full-width card above the plot, so the plot gets
# the width of the page. Each receiver depth is one group of three boxes in a
# row, the two groups half the row each. The inputs open on the app defaults.
mod_range_model_ui <- function(id) {
  ns <- shiny::NS(id)
  s <- range_presets$default$spec
  num <- function(name, label, value) {
    shiny::numericInput(ns(name), label, value, min = 0, width = "100%")
  }
  group <- function(title, i) {
    shiny::div(
      shiny::h6(title),
      bslib::layout_columns(
        col_widths = c(4, 4, 4), gap = "0.5rem",
        num(paste0("depth_", i), "Depth (m)", s$depth[i]),
        num(paste0("d50_", i), "d50 (m)", s$d50[i]),
        num(paste0("d05_", i), "d05 (m)", s$d05[i])
      )
    )
  }
  shiny::tagList(
    bslib::card(
      bslib::card_header("Detection range settings"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        group("Shallow receiver", 1),
        group("Deep receiver", 2)
      ),
      shiny::helpText("d50 and d05: the distances at which 50% and 5% of transmissions are",
                      "detected, interpolated linearly between the two depths and held outside",
                      "them."),
      shiny::uiOutput(ns("problems"))
    ),
    bslib::card(
      bslib::card_header("Detection probability against distance"),
      shiny::p(shiny::strong("In use: "), shiny::textOutput(ns("name"), inline = TRUE)),
      shiny::plotOutput(ns("curve"), height = range_plot_height),
      shiny::p(class = "small text-muted", shiny::textOutput(ns("caption"), inline = TRUE))
    )
  )
}

range_plot_height <- "440px"

mod_range_model_server <- function(id, area) {
  shiny::moduleServer(id, function(input, output, session) {
    spec <- shiny::reactiveVal(range_presets$default$spec)
    shiny::observeEvent(area(), {
      # A named starting curve follows the area; the user's own values stay.
      if (range_model_name(spec()) == "Your values") return()
      start <- area_preset(area())
      spec(start$spec)
      vals <- unlist(start$spec[c("depth", "d50", "d05")])[c(1, 3, 5, 2, 4, 6)]
      for (i in seq_along(range_inputs)) {
        shiny::updateNumericInput(session, range_inputs[i], value = vals[[i]])
      }
    })
    shiny::observeEvent(lapply(range_inputs, function(nm) input[[nm]]), {
      spec(do.call(range_spec, lapply(range_inputs, function(nm) input[[nm]])))
    }, ignoreInit = TRUE)
    problems <- shiny::reactive(spec_problems(spec()))
    model <- shiny::reactive({
      shiny::validate(shiny::need(!length(problems()), paste(problems(), collapse = " ")))
      new_range_model(spec())
    })
    curves <- shiny::reactive(curve_table(model()))
    curve <- shiny::renderPlot(plot_range_curve(curves(), model()$spec), res = 96)
    output$curve <- shiny::bindCache(curve, model()$key)
    output$problems <- shiny::renderUI(problem_alert(problems()))
    output$name <- shiny::renderText(model()$label)
    output$caption <- shiny::renderText(curve_caption(model()))
    model
  })
}

problem_alert <- function(problems) {
  if (!length(problems)) return(NULL)
  shiny::div(class = "alert alert-danger mt-2", role = "alert",
             shiny::tags$ul(class = "mb-0", lapply(problems, shiny::tags$li)))
}
