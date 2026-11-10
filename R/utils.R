# ---- Small helpers shared by the modules ----

fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")

# A noun for a count: singular at exactly 1, plural otherwise ("0 receivers",
# "1 receiver", "1.5 transmissions").
noun_for <- function(n, noun, plural = paste0(noun, "s")) {
  if (isTRUE(n == 1)) noun else plural
}

# A whole count and its noun, with thousands marks: "1 receiver", "4,818 water
# cells".
fmt_count <- function(n, noun, plural = paste0(noun, "s")) {
  paste(fmt_int(n), noun_for(n, noun, plural))
}

fmt_pct <- function(x, digits = 1) sprintf("%.*f%%", digits, 100 * x)

# The range of `x` as printed: "2.5 to 13.8", or one value when the lowest
# and highest print the same, as at one receiver.
span_text <- function(x, fmt = function(v) format(round(v, 1))) {
  lo <- fmt(min(x))
  hi <- fmt(max(x))
  if (identical(lo, hi)) lo else paste(lo, "to", hi)
}

# The app draws its figures at 96 pixels an inch, where the theme's default
# base size of 11 sets axis text at 8.8 pt, about 12 px. At 14 the axis titles
# are 14 pt and the axis text 11.2 pt, which read at the panel sizes of the app.
app_base_size <- 14

# One process-wide cache of coverage results, shared by every session, so a
# layout another user already scored is not computed again. A result holds
# per-cell vectors, the cell-by-receiver detection field and the packed
# raster: about 1 MB for 20 receivers on the 4,818 water cells of Stoney Lake.
pkg_env <- new.env(parent = emptyenv())

app_cache <- function() {
  if (is.null(pkg_env$cache)) pkg_env$cache <- cachem::cache_mem(max_size = 256 * 1024^2)
  pkg_env$cache
}

# ---- Conditions ----
# Every error, warning and message the app raises itself carries the class
# "arraydesignR_error", "arraydesignR_warning" or "arraydesignR_message", so
# code and tests can catch them by class. The call is dropped: the message is written for the user.
stop_app <- function(message) {
  stop(errorCondition(message, class = "arraydesignR_error", call = NULL))
}

warn_app <- function(message) {
  warning(warningCondition(message, class = "arraydesignR_warning", call = NULL))
}

inform_app <- function(message) {
  message(structure(class = c("arraydesignR_message", "message", "condition"),
                    list(message = paste0(message, "\n"), call = NULL)))
}

# Runs `expr` and returns its value with the messages of the warnings it
# raised; messages are dropped. A warning raised in a worker process would
# reach only that worker's console, so it travels back with the value.
collect_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(expr, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }, message = function(m) invokeRestart("muffleMessage"))
  list(value = value, warnings = warnings)
}

# ---- Cached computations ----
# One computation a module runs on request: `run(args)` as an ExtendedTask
# bound to the task button `button`, so a slow raster does not block the
# session. The result is kept in `current`, and in the process-wide `cache`
# under `args$key`. start(args) takes a cached result when there is one, and
# runs the task otherwise. The warnings a result carries become
# notifications, and an error becomes an error notification. Call it inside
# a module server, with that module's session.
cached_task <- function(run, button, cache, session = shiny::getDefaultReactiveDomain()) {
  task <- shiny::ExtendedTask$new(function(args) {
    promises::future_promise(run(args), seed = NULL)
  })
  bslib::bind_task_button(task, button, session = session)
  current <- shiny::reactiveVal(NULL)
  start <- function(args) {
    hit <- cache$get(args$key)
    if (cachem::is.key_missing(hit)) return(task$invoke(args))
    hit$from_cache <- TRUE
    current(hit)
  }
  shiny::observeEvent(task$status(), {
    if (task$status() == "success") {
      res <- task$result()
      for (w in res$warnings) shiny::showNotification(w, type = "warning", session = session)
      cache$set(res$key, res)
      current(res)
    }
    if (task$status() == "error") try_notify(task$result(), session)
  })
  list(task = task, current = current, start = start)
}

# Runs `expr` and turns its messages and warnings into notifications. An error
# becomes an error notification and the value NULL, so a bad upload or click
# never ends the session. An error with no text, such as shiny::req() on an
# input the browser has not sent yet, gives NULL and no notification: the
# banner already says what is missing.
try_notify <- function(expr, session = shiny::getDefaultReactiveDomain()) {
  notify <- function(text, type) {
    if (!is.null(session) && nzchar(text)) {
      shiny::showNotification(text, type = type, session = session)
    }
  }
  withCallingHandlers(
    tryCatch(expr, error = function(e) {
      notify(conditionMessage(e), "error")
      NULL
    }),
    warning = function(w) {
      notify(conditionMessage(w), "warning")
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      notify(trimws(conditionMessage(m)), "message")
      invokeRestart("muffleMessage")
    }
  )
}

# Runs `expr` and turns its warnings and messages into notifications; errors
# pass through.
warn_notify <- function(expr, session = shiny::getDefaultReactiveDomain()) {
  notify <- function(text, type) {
    if (!is.null(session)) shiny::showNotification(text, type = type, session = session)
  }
  withCallingHandlers(expr, warning = function(w) {
    notify(conditionMessage(w), "warning")
    invokeRestart("muffleWarning")
  }, message = function(m) {
    notify(trimws(conditionMessage(m)), "message")
    invokeRestart("muffleMessage")
  })
}

# A raster of `values` on `cells` of `grid`; every other cell is NA.
cells_raster <- function(grid, cells, values) {
  out <- terra::rast(grid)
  v <- rep(NA_real_, terra::ncell(grid))
  v[cells] <- values
  terra::values(out) <- v
  out
}
