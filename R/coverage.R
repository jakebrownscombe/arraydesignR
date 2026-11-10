# ---- Coverage of the water by the array ----
# Everything here runs outside Shiny, so every number the app shows can be
# tested without it. run_coverage() may run in a worker process, so it takes
# only plain values and a packed raster.

# Pairs farther apart than the distance at which the curve falls below
# `cutoff_p` at every depth are left out, since they cost most of the time and
# memory. For the default 20 receivers on Stoney Lake, leaving them out moves
# no cell's probability by as much as 1e-4 and leaves the share at 0.5 as it is.
cutoff_p <- 0.001

# The largest number of receiver and cell pairs within the cutoff that one
# computation may take. Time and peak memory grow with it.
default_max_pairs <- 1e6

# The distance in metres beyond which the curve is below `cutoff_p` at every
# receiver depth. d50 and d05 are interpolated linearly in depth and held
# outside the two depths, so the largest value is at one of the two depths.
curve_cutoff <- function(spec, p = cutoff_p) {
  ceiling(max(spec$d50 + (spec$d05 - spec$d50) * stats::qlogis(p) / stats::qlogis(0.05)))
}

# The receivers a computation takes, once a study area and at least one
# receiver are there. Coverage and the simulation share the check.
request_receivers <- function(area, layout) {
  shiny::validate(
    shiny::need(!is.null(area), "Choose a study area."),
    shiny::need(!is.null(layout) && nrow(layout) > 0, "Place at least one receiver.")
  )
  rx <- as.data.frame(layout)[, c("receiver_serial", "station_id", "x", "y", "deploy_z")]
  rownames(rx) <- NULL
  rx
}

coverage_request <- function(area, layout, range_model, max_pairs = default_max_pairs) {
  rx <- request_receivers(area, layout)
  cutoff <- curve_cutoff(range_model$spec)
  each <- estimate_pairs_each(area, layout, cutoff)
  shiny::validate(shiny::need(sum(each) <= max_pairs,
                              pairs_refusal(each, cutoff, max_pairs, rx$station_id)))
  # The label follows from the spec, so it stays out of the cache key.
  args <- list(grid = area$grid, area_hash = area$hash, area_name = area$name, receivers = rx,
               spec = range_model$spec, max_distance = cutoff, range_label = range_model$label)
  args$key <- rlang::hash(args[c("area_hash", "receivers", "spec", "max_distance")])
  args
}

# Receiver and cell pairs within `cutoff` in a straight line, per receiver. The
# distance through water is never shorter, so the computation keeps at most
# this many.
estimate_pairs_each <- function(area, layout, cutoff) {
  if (is.null(layout) || !nrow(layout)) return(numeric())
  xy <- area$water_xy
  vapply(seq_len(nrow(layout)), function(i) {
    sum((xy[, 1] - layout$x[i])^2 + (xy[, 2] - layout$y[i])^2 <= cutoff^2)
  }, numeric(1))
}

estimate_pairs <- function(area, layout, cutoff) sum(estimate_pairs_each(area, layout, cutoff))

# Why a computation is refused, and the two ways to bring it under the limit.
pairs_refusal <- function(each, cutoff, max_pairs, ids) {
  paste(sprintf("%s within %s m, above this app's limit of %s.",
                fmt_count(sum(each), "receiver-to-cell distance lies",
                          "receiver-to-cell distances lie"),
                fmt_int(cutoff), fmt_int(max_pairs)),
        limit_advice(each, max_pairs, ids))
}

# The ways under the pair limit, given the distances each receiver takes and
# the station ids the map labels them with. The receivers that take the
# fewest are kept first, so no larger set of these receivers fits. A count of
# receivers holds only for that set, so the advice names it. When every
# receiver takes the same number, any set of that size fits. When not one
# fits, removing receivers cannot help, so the advice gives the limit all of
# them need and the factor to aggregate by.
limit_advice <- function(each, max_pairs, ids) {
  ord <- order(each)
  k <- sum(cumsum(each[ord]) <= max_pairs)
  by <- ceiling(sqrt(sum(each) / max_pairs))
  agg <- sprintf("aggregate the raster by %d before upload (terra::aggregate()).", by)
  n <- length(each)
  if (k >= 1 && length(unique(each)) == 1) {
    size <- if (k == 1) "one" else fmt_int(k)
    return(sprintf("Keep any %s of these %s, or %s", size, fmt_count(n, "receiver"), agg))
  }
  if (k == 1) {
    return(sprintf("Only receiver %s fits: keep it alone, or %s", ids[ord[1]], agg))
  }
  if (k > 1) {
    return(sprintf("These %s fit together: %s. Keep only these, or %s",
                   fmt_count(k, "receiver"),
                   paste(ids[ord[seq_len(k)]], collapse = ", "), agg))
  }
  agg <- sprintf("Aggregate the raster by %d before upload (terra::aggregate()).", by)
  if (n == 1) {
    return(sprintf(paste("This receiver does not fit: it takes %s, and the limit",
                         "(max_pairs) must be %s or more. %s"), fmt_count(each, "distance"),
                   fmt_int(each), agg))
  }
  sprintf(paste("Not one of these receivers fits: each takes at least %s, and all %s",
                "together need a limit (max_pairs) of %s or more. %s"),
          fmt_count(min(each), "distance"), fmt_int(n), fmt_int(sum(each)), agg)
}

# The size of the computation, under the button: how many receiver-to-cell
# distances it takes against the limit, and why farther cells count as 0.
work_line <- function(area, layout, range_model, max_pairs = default_max_pairs) {
  if (is.null(area) || is.null(range_model)) return("")
  cutoff <- curve_cutoff(range_model$spec)
  sprintf(paste("Size: %s within %s m (limit %s). Farther cells",
                "count as out of range: there the curve is below %s."),
          fmt_count(estimate_pairs(area, layout, cutoff), "receiver-to-cell distance"),
          fmt_int(cutoff), fmt_int(max_pairs),
          format(cutoff_p))
}

# Warnings travel back with the result and the module shows them. detfield's
# message about receiver-cell pairs with land on the straight line between
# them is dropped: the app reads the curve on those pairs at the distance
# through water (blocked = 0) by design. `n_shadowed` counts every pair with
# land on its straight line.
run_coverage <- function(args) {
  got <- collect_warnings(coverage_core(args))
  res <- got$value
  res$warnings <- got$warnings
  res
}

# Distances through water, the detection field and the array's detection
# probability per water cell, all from detfield. The curve is read at the
# distance through water (blocked = 0).
coverage_core <- function(args) {
  t0 <- proc.time()[["elapsed"]]
  grid <- terra::unwrap(args$grid)
  rx <- args$receivers[, c("station_id", "x", "y", "deploy_z")]
  d <- detfield::water_distances(grid, rx, max_distance = args$max_distance)
  model <- detfield::de_logistic(args$spec$d50, args$spec$d05, args$spec$depth)
  field <- detfield::de_field(d, model)
  # One nominal day on which every receiver listens. The app scores a design,
  # and a design has no deployment dates.
  listening <- data.frame(station_id = field$receivers, date = as.Date("2000-01-01"))
  nearest <- tapply(d$distance_m, d$cell, min)
  structure(list(
    key = args$key, area_hash = args$area_hash, area_name = args$area_name, grid = args$grid,
    cells = field$cells,
    p_array = detfield::system_de(field, listening)$p,
    eta = unname(field$eta), keys = field$receivers,
    depth = abs(terra::values(grid, mat = FALSE)[field$cells]),
    nearest_m = as.vector(nearest[match(field$cells, as.integer(names(nearest)))]),
    receivers = args$receivers, spec = args$spec, max_distance = args$max_distance,
    range_label = args$range_label,
    n_pairs = nrow(d), n_shadowed = sum(!d$in_water), seconds = proc.time()[["elapsed"]] - t0
  ), class = "app_coverage")
}

# Share of water cells at or above each threshold: for the array (at least one
# receiver detects) and for the highest single-receiver probability in each cell, the second from
# arraydesign::coverage_summary() on the result's field.
coverage_table <- function(cov, thresholds) {
  field <- list(eta = cov$eta, cells = cov$cells, receivers = cov$keys,
                grid = terra::unwrap(cov$grid))
  single <- arraydesign::coverage_summary(field, thresholds = thresholds)
  data.frame(
    threshold = thresholds,
    array_cells = vapply(thresholds, function(t) sum(cov$p_array >= t), numeric(1)),
    array_share = vapply(thresholds, function(t) mean(cov$p_array >= t), numeric(1)),
    single_cells = single$n_cells,
    single_share = single$share,
    n_water_cells = length(cov$cells)
  )
}

# Where the coverage on screen stands against the current inputs: none yet,
# running, current, stale with the reasons, or invalid with the reason the
# inputs cannot be computed. `request` is a coverage_request() or the error
# it raised. `area` is the study area on screen: a result for another area is
# not shown, so an invalid state then has no result on screen.
coverage_state <- function(cov, request, status, area) {
  if (identical(status, "running")) return(list(kind = "running", reasons = character()))
  if (inherits(request, "error")) {
    why <- invalid_reason(request)
    # Before the first result, an empty layout is the normal start.
    if (is.null(cov) && why %in% startup_reasons) return(list(kind = "none", reasons = character()))
    return(list(kind = "invalid", reasons = why, has_result = !is.null(for_area(cov, area))))
  }
  if (is.null(cov)) return(list(kind = "none", reasons = character()))
  if (identical(cov$key, request$key)) return(list(kind = "current", reasons = character()))
  list(kind = "stale", reasons = stale_reasons(cov, request))
}

startup_reasons <- c("Choose a study area.", "Place at least one receiver.",
                     "An input is not ready yet.")

# req() raises an error with no message while an input is still loading.
invalid_reason <- function(e) {
  msg <- conditionMessage(e)
  if (!nzchar(msg)) "An input is not ready yet." else msg
}

stale_reasons <- function(cov, request) {
  out <- character()
  if (!identical(cov$area_hash, request$area_hash)) out <- "the study area changed"
  n_old <- nrow(cov$receivers)
  n_new <- nrow(request$receivers)
  if (n_old != n_new) {
    out <- c(out, sprintf("the results are for %s and the layout now has %d",
                          fmt_count(n_old, "receiver"), n_new))
  } else if (!identical(cov$receivers, request$receivers)) {
    out <- c(out, "receivers moved")
  }
  if (!identical(cov$spec, request$spec)) out <- c(out, "the range model changed")
  # The score also keys on its simulation settings.
  if (!length(out)) out <- "the simulation settings changed"
  out
}

# The headline and its sub-line, shared by the banner above the map and the
# point list under the button: the share of the water at 0.5 or more, then
# what that share means, as short points.
coverage_headline <- function(cov) {
  shiny::tagList(
    shiny::div(class = "coverage-headline", shiny::strong(
      sprintf("%s of the water covered", fmt_pct(mean(cov$p_array >= 0.5)))
    )),
    shiny::div(class = "small", shiny::HTML(headline_meaning))
  )
}

# The sign is an HTML entity: an R session in the C locale prints the Unicode
# sign as <U+2265>.
headline_meaning <- "(p &ge; 0.5 that any receiver detects one transmission)"

cells_point <- function(cov) {
  sprintf("%s of %s", fmt_int(sum(cov$p_array >= 0.5)),
          fmt_count(length(cov$cells), "water cell"))
}

# The banner above the map, where a room reads it without scrolling: the
# headline and the cell count only. The fuller list stays under the button.
map_headline <- function(cov) {
  shiny::tagList(coverage_headline(cov), shiny::div(class = "small", cells_point(cov)))
}

# A result is shown only for the study area it was computed on; after a switch
# its numbers belong to another lake. NULL when there is nothing to show.
for_area <- function(res, area) {
  if (is.null(res) || is.null(area) || !identical(res$area_hash, area$hash)) return(NULL)
  res
}

# What the Performance tab and the sidebar show in place of another lake's numbers.
other_area_note <- function(area, res) {
  sprintf("No coverage for %s yet: the last result is for %s. Press Compute coverage.",
          area$name, res$area_name)
}

# The point list under the button. A result from the cache took no computing
# in this request, so it gives no time.
coverage_line <- function(cov, area = NULL) {
  if (is.null(cov)) return(NULL)
  if (!is.null(area) && is.null(for_area(cov, area))) return(other_area_note(area, cov))
  when <- if (isTRUE(cov$from_cache)) {
    "Taken from the cache: computed earlier for the same inputs"
  } else {
    sprintf("Computed in %.1f s", cov$seconds)
  }
  n_rx <- nrow(cov$receivers)
  points <- c(cells_point(cov), fmt_count(n_rx, "receiver"),
              paste("Range model:", range_model_name(cov$spec)), range_numbers(cov$spec), when)
  shiny::tagList(
    coverage_headline(cov),
    shiny::tags$ul(class = "coverage-points ps-3 mb-0", lapply(points, shiny::tags$li))
  )
}
