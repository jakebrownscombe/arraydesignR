# ---- Module: coverage ----
# Computes coverage on request, as an ExtendedTask so a slow raster does not
# block the session, and keeps the last result. A process-wide cache returns
# a layout already scored without computing it again. The banner compares the
# result with the current inputs, so it goes stale the moment they change.
# With `auto = TRUE` the first valid inputs are computed without a click. The
# app does not use it: it opens with no receivers, and a first click on the
# map must not start a computation that later clicks do not.

mod_coverage_button_ui <- function(id) {
  bslib::input_task_button(shiny::NS(id, "compute"), "Compute coverage",
                           label_busy = "Computing...")
}

mod_coverage_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h6("3. Coverage"),
    shiny::uiOutput(ns("banner")),
    shiny::uiOutput(ns("summary"), class = "small mt-2 mb-1"),
    shiny::p(class = "small text-muted", shiny::textOutput(ns("work"), inline = TRUE))
  )
}

# The same banner above the map and the performance tab, where the numbers are
# read; `where` names the copy. The margin sits on a wrapper, because the
# output is display: contents and drops its own margin.
mod_coverage_banner_ui <- function(id, where) {
  shiny::div(class = "mb-2", shiny::uiOutput(shiny::NS(id, paste0("banner_", where))))
}

mod_coverage_server <- function(id, area, layout, range_model, cache = app_cache(),
                                max_pairs = default_max_pairs, auto = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    job <- cached_task(run_coverage, "compute", cache, session)
    current <- job$current
    request <- shiny::reactive(coverage_request(area(), layout(), checked_model(range_model),
                                                max_pairs))
    request_or_error <- shiny::reactive(tryCatch(request(), error = function(e) e))
    shiny::observeEvent(input$compute, {
      args <- try_notify(request(), session)
      if (!is.null(args)) job$start(args)
    })
    if (auto) auto_start(request_or_error, job$start)
    state <- shiny::reactive(coverage_state(current(), request_or_error(), job$task$status(),
                                            area()))
    for (nm in c("banner", "banner_performance")) {
      output[[nm]] <- shiny::renderUI(banner_ui(state()))
    }
    # Above the map the banner leads with the number, while it is current.
    output$banner_map <- shiny::renderUI({
      s <- state()
      banner_ui(s, headline = if (s$kind == "current") map_headline(current()))
    })
    output$summary <- shiny::renderUI(coverage_line(current(), area()))
    output$work <- shiny::renderText(work_line(area(), layout(), tryCatch(range_model(),
                                                                          error = function(e) NULL),
                                               max_pairs))
    list(coverage = shiny::reactive(current()), state = state)
  })
}

# Starts the first request that is valid, once; later ones wait for a click.
auto_start <- function(request_or_error, start) {
  first <- shiny::observe({
    args <- request_or_error()
    if (inherits(args, "error")) return()
    first$destroy()
    shiny::isolate(start(args))
  })
  invisible(first)
}

# The range model module refuses a curve with a validation error; the reason
# reaches the banner and the notification with what it is about. A req()
# that is still waiting raises an error with no message. It is raised again
# as it is, so it keeps Shiny's silent class and coverage_state() reads it as
# an input that is not ready yet.
checked_model <- function(range_model) {
  tryCatch(range_model(), error = function(e) {
    msg <- conditionMessage(e)
    if (!nzchar(msg)) stop(e)
    stop_app(paste("The range model is invalid:", msg))
  })
}

earlier_note <- function(state) {
  if (isTRUE(state$has_result)) "The results on screen are for earlier inputs." else NULL
}

# `what` names the result and `button` the button that makes it, so the
# Simulation tab can use the same banner. A `headline` (tags) replaces the line of
# a current banner: above the map it is the share and the cell count, and the
# green banner says the numbers are current.
banner_ui <- function(state, what = "Coverage", button = "Compute coverage", headline = NULL) {
  box <- function(cls, ...) shiny::div(class = paste("alert py-2 mb-0", cls), role = "status", ...)
  switch(state$kind,
    none = box("alert-secondary", sprintf("No %s yet. Place receivers and press %s.",
                                          tolower(what), button)),
    running = box("alert-info", sprintf("Computing %s...", tolower(what))),
    current = if (!is.null(headline)) {
      box("alert-success", headline)
    } else {
      box("alert-success", sprintf(paste("%s is from the specified receiver arrangement and",
                                         "detection range model."), what))
    },
    stale = box("alert-warning", shiny::strong(sprintf("%s is out of date: ", what)),
                paste0(paste(state$reasons, collapse = "; "), sprintf(". Press %s.", button))),
    invalid = box("alert-warning", shiny::strong(sprintf("%s cannot be computed. ", what)),
                  paste(state$reasons, earlier_note(state)))
  )
}
