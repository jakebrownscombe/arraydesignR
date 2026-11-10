# ---- Module: array performance ----
# Value boxes, the threshold table, the histogram and the depth bands, all
# drawn from the coverage object of the coverage module. The mean
# transmission delay typed here also sets the map's hour layer.

table_note <- paste(
  "Share and number of water cells at or above each threshold. Array: probability that at",
  "least one receiver detects one transmission. Any one receiver: the highest probability",
  "from any one receiver."
)

mod_performance_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("boxes")),
    shiny::div(
      class = "d-flex flex-wrap align-items-end gap-3 mt-3",
      shiny::numericInput(ns("delay"), "Mean transmission delay (s)", default_delay, min = 1,
                          step = 10, width = "220px"),
      shiny::p(class = "small text-muted mb-3",
               "Sets n = 3600 / delay transmissions an hour, for the hour tile and the map's",
               "hour layer.")
    ),
    shiny::p(class = "small", shiny::textOutput(ns("context"), inline = TRUE), " ",
             shiny::strong("Range model: "), shiny::textOutput(ns("range"), inline = TRUE)),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Share of the water at each threshold"),
        shiny::textInput(ns("thresholds"), "Thresholds (probabilities, comma separated)",
                         "0.05, 0.25, 0.5, 0.75"),
        shiny::div(class = "table-responsive", shiny::tableOutput(ns("table"))),
        shiny::p(class = "small text-muted", table_note)
      ),
      bslib::card(
        bslib::card_header("Detection probability of one transmission over the water"),
        shiny::plotOutput(ns("histogram"), height = "300px"),
        shiny::p(class = "small text-muted",
                 shiny::textOutput(ns("histogram_caption"), inline = TRUE))
      )
    ),
    bslib::card(
      bslib::card_header("Coverage by depth: share of each band's water cells"),
      shiny::numericInput(ns("depth_threshold"), "Threshold", 0.5, min = 0.01, max = 1,
                          step = 0.05),
      shiny::plotOutput(ns("depth"), height = "340px"),
      shiny::p(class = "small text-muted", shiny::textOutput(ns("depth_caption"), inline = TRUE))
    )
  )
}

mod_performance_server <- function(id, coverage, area) {
  shiny::moduleServer(id, function(input, output, session) {
    # A result for another study area is not shown: its numbers belong to that lake.
    cov <- shiny::reactive({
      shiny::validate(shiny::need(coverage(), "Compute coverage to see the array's performance."))
      shiny::validate(shiny::need(for_area(coverage(), area()),
                                  other_area_note(area(), coverage())))
      coverage()
    })
    thresholds <- shiny::reactive(tryCatch(parse_thresholds(input$thresholds), error = function(e) {
      shiny::validate(shiny::need(FALSE, conditionMessage(e)))
    }))
    # The delay as typed, or NA while it is not a usable number; the map reads
    # it for its hour layer.
    delay <- shiny::reactive({
      d <- input$delay
      if (is.null(d)) return(default_delay)
      if (length(delay_problems(d))) NA_real_ else d
    })
    boxes <- shiny::reactive({
      shiny::validate(shiny::need(!is.na(delay()), delay_problems(input$delay)))
      performance_boxes(cov(), delay())
    })
    table <- shiny::reactive(threshold_display(cov(), thresholds()))
    bands <- shiny::reactive({
      t <- input$depth_threshold
      shiny::validate(shiny::need(is.numeric(t) && !is.na(t) && t > 0 && t <= 1,
                                  "The threshold must be above 0 and at most 1."))
      depth_bands(cov(), t)
    })
    output$boxes <- shiny::renderUI(value_boxes_ui(boxes()))
    output$context <- shiny::renderText({
      n <- nrow(cov()$receivers)
      sprintf("%s, %s.", cov()$area_name, fmt_count(n, "receiver"))
    })
    output$range <- shiny::renderText(cov()$range_label)
    output$table <- shiny::renderTable(table(), align = "r")
    histogram <- shiny::renderPlot(plot_p_histogram(cov(), thresholds()), res = 96)
    output$histogram <- shiny::bindCache(histogram, cov()$key, thresholds())
    depth <- shiny::renderPlot(plot_depth_bands(bands()), res = 96)
    output$depth <- shiny::bindCache(depth, cov()$key, input$depth_threshold)
    output$histogram_caption <- shiny::renderText(histogram_caption(cov()))
    output$depth_caption <- shiny::renderText(depth_caption(bands(), cov()))
    list(boxes = boxes, table = table, bands = bands, delay = delay)
  })
}

# Three tiles of one structure, so they align: a short title, one number,
# and exactly two short lines, what the number is and its denominator. The
# row makes the three the same height. The sign is the HTML entity &ge;: an R
# session in the C locale prints the Unicode sign as <U+2265>.
value_boxes_ui <- function(b) {
  cells <- function(k) sprintf("%s of %s", fmt_int(k), fmt_count(b$n_water, "water cell"))
  tile <- function(title, value, what, denominator) {
    bslib::value_box(shiny::HTML(title), value, shiny::p(class = "mb-0", shiny::HTML(what)),
                     shiny::p(class = "mb-0", denominator), fill = TRUE)
  }
  bslib::layout_column_wrap(
    width = 1 / 3, fill = FALSE, heights_equal = "all",
    tile("Water covered at p &ge; 0.5", fmt_pct(b$share_50),
         "one transmission, heard by any receiver", cells(b$cells_50)),
    tile("Water covered at p &ge; 0.05", fmt_pct(b$share_05),
         "one transmission, heard by any receiver", cells(b$cells_05)),
    tile("Water detected within an hour", fmt_pct(b$share_hour),
         sprintf("p &ge; %.2f of at least one detection in an hour", hour_threshold),
         sprintf("%s %s at a %s s delay", format(signif(b$n_tx, 3)),
                 noun_for(signif(b$n_tx, 3), "transmission"), format(b$delay)))
  )
}
