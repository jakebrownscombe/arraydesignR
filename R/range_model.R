# ---- Range model ----
# The detection curve is detfield::de_logistic(): a logistic curve through
# 0.5 at d50 and 0.05 at d05, with both distances given at two receiver depths
# and interpolated linearly between them.

# Inputs arrive as integers or doubles; the spec holds doubles, so equal
# values give equal cache keys.
range_spec <- function(depth_1, d50_1, d05_1, depth_2, d50_2, d05_2) {
  num <- function(...) as.numeric(c(...))
  list(depth = num(depth_1, depth_2), d50 = num(d50_1, d50_2), d05 = num(d05_1, d05_2))
}

# ---- Starting curves ----
# Each study area starts on a named curve, and the name travels with every
# coverage number. A synthetic area (the toy lake of the tests) starts on the
# curve its detections were simulated with: d50 250 m and d05 600 m at every
# depth. No Stoney Lake curve with a stated source exists, so every other area
# starts on the app defaults, named as unsourced: no range test measured
# them. At the shallow receiver they are d50 250 m and d05 500 m; the deep
# receiver keeps the Array DesignR 1.x values (d50 750 m, d05 1,500 m at 35 m).
range_presets <- list(
  toy = list(spec = range_spec(1, 250, 600, 14, 250, 600), name = "Toy lake simulation curve"),
  default = list(spec = range_spec(1, 250, 500, 35, 750, 1500), name = "App defaults, unsourced")
)

area_preset <- function(area) {
  range_presets[[if (isTRUE(area$synthetic)) "toy" else "default"]]
}

same_spec <- function(a, b) isTRUE(all.equal(a, b, check.attributes = FALSE))

range_model_name <- function(spec) {
  for (p in range_presets) if (same_spec(spec, p$spec)) return(p$name)
  "Your values"
}

# The name and the numbers, as every coverage caption shows them.
range_model_label <- function(spec) {
  m <- function(v) paste(fmt_int(round(v)), "m")
  nums <- if (spec$d50[1] == spec$d50[2] && spec$d05[1] == spec$d05[2]) {
    sprintf("d50 %s and d05 %s at every depth", m(spec$d50[1]), m(spec$d05[1]))
  } else {
    sprintf("d50 %s and d05 %s at %s m; d50 %s and d05 %s at %s m", m(spec$d50[1]),
            m(spec$d05[1]), format(spec$depth[1]), m(spec$d50[2]), m(spec$d05[2]),
            format(spec$depth[2]))
  }
  sprintf("%s (%s)", range_model_name(spec), nums)
}

# The curve's numbers alone, short, for the point list under the button.
range_numbers <- function(spec) {
  m <- function(v) paste(fmt_int(round(v)), "m")
  if (spec$d50[1] == spec$d50[2] && spec$d05[1] == spec$d05[2]) {
    return(sprintf("d50 %s, d05 %s at every depth", m(spec$d50[1]), m(spec$d05[1])))
  }
  sprintf("d50 %s, d05 %s at %s m; d50 %s, d05 %s at %s m", m(spec$d50[1]), m(spec$d05[1]),
          format(spec$depth[1]), m(spec$d50[2]), m(spec$d05[2]), format(spec$depth[2]))
}

# Plain-language reasons the spec cannot make a curve; none when it can.
spec_problems <- function(spec) {
  v <- unlist(spec)
  if (length(v) != 6 || anyNA(v) || !is.numeric(v)) return("Fill in all six boxes.")
  out <- character()
  if (any(v < 0)) out <- c(out, "Depths and distances must be 0 or more.")
  if (spec$depth[2] <= spec$depth[1]) {
    out <- c(out, "The deep receiver depth must be larger than the shallow one.")
  }
  bad <- which(spec$d05 <= spec$d50)
  for (i in bad) {
    out <- c(out, sprintf("At %s m, d05 (%s m) must be larger than d50 (%s m).",
                          format(spec$depth[i]), format(spec$d05[i]), format(spec$d50[i])))
  }
  out
}

# The range model is the curve alone, with no variation between receivers or
# hours: a design is compared under one fixed curve.
new_range_model <- function(spec) {
  model <- detfield::de_logistic(spec$d50, spec$d05, spec$depth)
  list(spec = spec, model = model, key = rlang::hash(spec), label = range_model_label(spec))
}

# Detection probability against distance at the two given depths and halfway.
curve_table <- function(range_model) {
  spec <- range_model$spec
  depths <- c(spec$depth[1], mean(spec$depth), spec$depth[2])
  distance <- seq(0, 1.25 * max(spec$d05), length.out = 200)
  d <- expand.grid(distance_m = distance, depth_m = depths)
  d$p <- stats::plogis(range_model$model$predict(d$distance_m, depth_m = d$depth_m))
  d
}

# When d50 and d05 do not change with depth the three lines coincide, so one
# line and one legend entry, "All depths", are drawn. The open circles at d50
# and d05 take the colour of the shallow and the deep line.
plot_range_curve <- function(curves, spec) {
  one <- nzchar(same_at_depths(spec))
  depths <- if (one) curves$depth_m[1] else unique(curves$depth_m)
  entries <- if (one) "All depths" else paste0(format(signif(depths, 3), trim = TRUE), " m")
  lines <- curves[curves$depth_m %in% depths, ]
  lines$depth <- factor(entries[match(lines$depth_m, depths)], levels = entries)
  ends <- if (one) c(1, 1) else c(1, length(depths))
  marks <- data.frame(distance_m = c(spec$d50, spec$d05), p = rep(c(0.5, 0.05), each = 2),
                      depth = factor(entries[rep(ends, 2)], levels = entries))
  ggplot2::ggplot(lines, ggplot2::aes(x = .data$distance_m, y = .data$p, colour = .data$depth)) +
    ggplot2::geom_hline(yintercept = c(0.5, 0.05), linetype = "dotted", colour = "grey50") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(data = marks, shape = 21, fill = "white", size = 2.5, stroke = 1,
                        show.legend = FALSE) +
    # Viridis without its yellow end, which is hard to see on white.
    ggplot2::scale_colour_viridis_d(name = "Receiver depth", end = 0.7) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Distance from receiver (m)", y = "Detection probability") +
    detfield::theme_positionr(base_size = app_base_size)
}

curve_caption <- function(range_model) {
  s <- range_model$spec
  lines <- if (nzchar(same_at_depths(s))) {
    "at any receiver depth (line); d50 and d05 do not change with depth."
  } else {
    sprintf("at %s m, %s m and %s m receiver depth (lines; the middle one is interpolated).",
            format(s$depth[1]), format(mean(s$depth)), format(s$depth[2]))
  }
  sprintf(paste("Detection probability of one transmission against distance from a receiver %s",
                "Open circles: d50 and d05. Dotted lines: probability 0.5 and 0.05. Range model:",
                "%s."),
          lines, range_model$label)
}

same_at_depths <- function(s) {
  if (s$d50[1] != s$d50[2] || s$d05[1] != s$d05[2]) return("")
  "same"
}
