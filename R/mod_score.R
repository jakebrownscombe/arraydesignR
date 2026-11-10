# ---- Module: score (the Simulation tab) ----
# Detection rates of simulated animals under the layout, as an ExtendedTask
# with the same cache and stale banner as coverage. Computed only on request.

simulation_note <- shiny::p(
  class = "small mb-2",
  paste("Animals follow the movement model below. Each tag transmits at a random delay between",
        "the shortest and the longest delay; set both equal for a fixed delay. Each receiver",
        "hears each transmission, or not, under the range model of the Range model tab. The",
        "seed makes a run repeatable. The rates depend on the movement and range models: use",
        "them to compare layouts.")
)

mod_score_ui <- function(id) {
  ns <- shiny::NS(id)
  d <- simulation_defaults
  num <- function(id, label, value, ...) shiny::numericInput(ns(id), label, value, ...)
  shiny::tagList(
    shiny::uiOutput(ns("banner"), class = "mb-2"),
    bslib::card(
      bslib::card_header("Detection rates by simulation"),
      simulation_note,
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        num("n_paths", "Simulated animals", d$n_paths, min = 1, max = 50),
        num("hours", "Hours per animal", d$hours, min = 1, max = 24),
        num("seed", "Random seed", d$seed, step = 1),
        num("delay_min", "Shortest delay (s)", d$delay_min, min = 5, max = 3600),
        num("delay_max", "Longest delay (s)", d$delay_max, min = 5, max = 3600)
      )
    ),
    movement_card_ui(ns),
    # The button follows both cards, so every setting is above it.
    shiny::div(class = "mb-3",
               bslib::input_task_button(ns("score"), "Run simulation",
                                        label_busy = "Simulating..."),
               shiny::p(class = "small mt-2", shiny::uiOutput(ns("line"), inline = TRUE))),
    shiny::uiOutput(ns("boxes")),
    sim_figure_card("Detection rate per animal", ns("distribution"), ns("distribution_caption")),
    sim_figure_card("Simulated tracks and heard transmissions", ns("map"), ns("map_caption"),
                    height = "460px"),
    bslib::card(bslib::card_header("Detection by animal"),
                # 50 animals make a long table: it scrolls inside its card.
                shiny::div(class = "table-responsive",
                           style = "max-height: 420px; overflow-y: auto;",
                           shiny::tableOutput(ns("table"))),
                shiny::p(class = "small text-muted", paste(
                  "Detection rate: the share of an animal's transmissions heard by at least",
                  "one receiver. Highest rate first; the last row is over all animals, with the",
                  "SE across animals."
                )))
  )
}

movement_card_ui <- function(ns) {
  d <- movement_defaults
  num <- function(id, label, value, ...) shiny::numericInput(ns(id), label, value, ...)
  # arraydesign's walker turns about 0 (a wrapped normal centred on zero), so
  # the turn mean is shown and cannot be changed.
  turn_mean <- num("turn_mean", "Turn angle mean (degrees)", 0)
  turn_mean <- shiny::tagAppendAttributes(turn_mean, disabled = NA, .cssSelector = "input")
  bslib::card(
    bslib::card_header("Movement model: correlated random walk"),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      num("step_mean", "Step length mean (m per step)", d$step_mean, min = 0.01),
      num("step_sd", "Step length SD (m)", d$step_sd, min = 0.01),
      num("time_step", "Time step (s)", d$time_step, min = 1, max = 3600)
    ),
    bslib::layout_columns(
      col_widths = c(6, 6), turn_mean,
      num("turn_sd", "Turn angle SD (degrees)", d$turn_sd, min = 0, max = 360)
    ),
    shiny::p(class = "small mb-0", shiny::strong("In use: "),
             shiny::uiOutput(ns("movement_line"), inline = TRUE))
  )
}

sim_figure_card <- function(title, plot_id, caption_id, height = "360px") {
  bslib::card(
    bslib::card_header(title),
    shiny::plotOutput(plot_id, height = height),
    shiny::p(class = "small text-muted", shiny::textOutput(caption_id, inline = TRUE))
  )
}

mod_score_server <- function(id, area, layout, range_model, cache = app_cache(),
                             max_pairs = default_max_pairs) {
  shiny::moduleServer(id, function(input, output, session) {
    job <- cached_task(run_simulation, "score", cache, session)
    current <- job$current
    s0 <- simulation_defaults
    settings <- shiny::reactive(list(
      n_paths = input$n_paths %||% s0$n_paths, hours = input$hours %||% s0$hours,
      seed = input$seed %||% s0$seed,
      delay_min = input$delay_min %||% s0$delay_min, delay_max = input$delay_max %||% s0$delay_max
    ))
    d <- movement_defaults
    movement <- shiny::reactive(movement_settings(
      input$step_mean %||% d$step_mean, input$step_sd %||% d$step_sd,
      input$turn_sd %||% d$turn_sd, input$time_step %||% d$time_step
    ))
    request <- shiny::reactive(simulation_request(area(), layout(), checked_model(range_model),
                                                  settings(), movement(), max_pairs))
    request_or_error <- shiny::reactive(tryCatch(request(), error = function(e) e))
    shiny::observeEvent(input$score, {
      args <- try_notify(request(), session)
      if (!is.null(args)) job$start(args)
    })
    state <- shiny::reactive(coverage_state(current(), request_or_error(), job$task$status(),
                                            area()))
    output$banner <- shiny::renderUI(banner_ui(state(), "Simulation", "Run simulation"))
    output$movement_line <- shiny::renderUI({
      m <- movement()
      tryCatch({
        check_movement(m)
        movement_line(m)
      }, error = function(e) conditionMessage(e))
    })
    # A result for another study area is not shown: its numbers belong to that lake.
    result <- shiny::reactive({
      res <- shiny::req(current())
      shiny::validate(shiny::need(for_area(res, area()), other_area_simulation(area(), res)))
      res
    })
    output$line <- shiny::renderUI(simulation_line(for_area(current(), area())))
    output$boxes <- shiny::renderUI(simulation_boxes_ui(simulation_summary(result())))
    output$table <- shiny::renderTable(simulation_table(result()), align = "r")
    output$distribution <- shiny::renderPlot(plot_animal_rates(result()), res = 96)
    # The map reads the result before any terra call. Read inside terra's S4
    # dispatch, the silent req() of result() becomes a visible R error. The map
    # stays empty until there is a result for the area on screen; the banner
    # and the boxes say why.
    output$map <- shiny::renderPlot({
      res <- shiny::req(for_area(current(), area()))
      plot_simulation_map(res)
    }, res = 96)
    output$distribution_caption <- shiny::renderText(sim_animal_caption(result()))
    output$map_caption <- shiny::renderText(simulation_map_caption(result()))
    list(score = shiny::reactive(current()), state = state)
  })
}
