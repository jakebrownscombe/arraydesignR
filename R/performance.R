# ---- Performance summaries and plots ----
# Read from the coverage object only; no distances or fields are recomputed.

parse_thresholds <- function(text) {
  if (is.null(text)) text <- ""
  tokens <- strsplit(trimws(text), "[,; ]+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  v <- suppressWarnings(as.numeric(tokens))
  if (anyNA(v)) {
    stop_app(sprintf("Thresholds must be numbers; %s %s not.",
                     paste0("\"", tokens[is.na(v)], "\"", collapse = ", "),
                     if (sum(is.na(v)) == 1) "is" else "are"))
  }
  v <- sort(unique(v))
  if (!length(v) || any(v <= 0 | v > 1)) {
    stop_app("Thresholds must be probabilities above 0 and at most 1, separated by commas.")
  }
  v
}

# ---- Detection within an hour ----
# The probability of at least one detection in an hour from a cell, from the
# probability for one transmission: p_hour = 1 - (1 - p_array)^n, with
# n = 3600 / delay transmissions an hour at the tag's mean transmission delay.
# ASSUMPTION: successive transmissions are detected independently. Detection
# clustered in time, which range tests show, makes this an overstatement.
# The hour tile reports this probability.
default_delay <- 120

hour_threshold <- 0.95

transmissions_per_hour <- function(delay) 3600 / delay

p_hour <- function(p_array, delay = default_delay) {
  1 - (1 - p_array)^transmissions_per_hour(delay)
}

delay_problems <- function(delay) {
  if (!is_one_number(delay) || delay <= 0) {
    return("The mean transmission delay must be a number of seconds above 0.")
  }
  character()
}

performance_boxes <- function(cov, delay = default_delay) {
  tab <- coverage_table(cov, c(0.05, 0.5))
  hour <- p_hour(cov$p_array, delay)
  list(
    share_50 = tab$array_share[2], cells_50 = tab$array_cells[2],
    share_05 = tab$array_share[1], cells_05 = tab$array_cells[1],
    share_hour = mean(hour >= hour_threshold), cells_hour = sum(hour >= hour_threshold),
    delay = delay, n_tx = transmissions_per_hour(delay),
    n_water = length(cov$cells), n_receivers = nrow(cov$receivers), area_name = cov$area_name
  )
}

threshold_display <- function(cov, thresholds) {
  tab <- coverage_table(cov, thresholds)
  data.frame(
    Threshold = sprintf("%.2f", tab$threshold),
    `Array: share of water` = fmt_pct(tab$array_share),
    `Array: cells` = fmt_int(tab$array_cells),
    `Any one receiver: share` = fmt_pct(tab$single_share),
    `Any one receiver: cells` = fmt_int(tab$single_cells),
    check.names = FALSE
  )
}

# Each threshold is labelled on a top axis, in the colour of its line.
plot_p_histogram <- function(cov, thresholds) {
  red <- "#B2182B"
  ggplot2::ggplot(data.frame(p = cov$p_array), ggplot2::aes(x = .data$p)) +
    ggplot2::geom_histogram(breaks = seq(0, 1, by = 0.02), fill = "grey70", colour = "white",
                            linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = thresholds, linetype = "dashed", linewidth = 0.6,
                        colour = red) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.02)),
      sec.axis = ggplot2::dup_axis(breaks = thresholds, labels = sprintf("%.2f", thresholds),
                                   name = NULL)
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Array detection probability", y = "Water cells") +
    detfield::theme_positionr(base_size = app_base_size) +
    ggplot2::theme(axis.text.x.top = ggplot2::element_text(colour = red),
                   axis.ticks.x.top = ggplot2::element_blank())
}

# Share of the water cells in each depth band that reach the threshold.
depth_bands <- function(cov, threshold, n_bands = 8) {
  breaks <- pretty(cov$depth, n = n_bands)
  band <- cut(cov$depth, breaks, include.lowest = TRUE)
  hit <- cov$p_array >= threshold
  d <- data.frame(band = levels(band), lower = utils::head(breaks, -1),
                  upper = utils::tail(breaks, -1),
                  n_cells = as.vector(table(band)),
                  n_covered = as.vector(tapply(hit, band, sum)))
  d$n_covered[is.na(d$n_covered)] <- 0
  d$share <- ifelse(d$n_cells > 0, d$n_covered / d$n_cells, NA_real_)
  attr(d, "overall") <- mean(hit)
  attr(d, "threshold") <- threshold
  d
}

# Equal-width bars, one per band, each as tall as the share of that band's
# own water cells at the threshold: a proportion per band, never cumulative.
# The fill is uniform, so no shade reads as a second variable. The axis label
# under each band gives its water cells and its share as a percentage, below
# the panel, where no label can sit on the dashed line.
plot_depth_bands <- function(bands) {
  d <- data.frame(band = sprintf("%g-%g", bands$lower, bands$upper), share = bands$share)
  d$band <- factor(d$band, levels = d$band)
  shown <- ifelse(is.na(d$share), "no water", fmt_pct(d$share))
  ylab <- sprintf("Share of the band's water cells\nat p of %.2f or more", attr(bands, "threshold"))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$band, y = .data$share)) +
    ggplot2::geom_col(fill = band_fill, width = 0.8, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = attr(bands, "overall"), linetype = "dashed",
                        linewidth = 0.6) +
    ggplot2::scale_x_discrete(labels = paste0(d$band, "\n", fmt_int(bands$n_cells), "\n", shown)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = "Depth band (m), water cells in it, and share at the threshold", y = ylab) +
    detfield::theme_positionr(base_size = app_base_size)
}

# The single-series blue of theme_positionr().
band_fill <- "#31688E"

histogram_caption <- function(cov) {
  sprintf(paste("Water cells by the probability that at least one receiver detects one",
                "transmission, over all %s for %s; bins of 0.02. Dashed",
                "lines: the table's thresholds. Range model: %s."),
          fmt_count(length(cov$cells), "water cell"), fmt_count(nrow(cov$receivers), "receiver"),
          cov$range_label)
}

depth_caption <- function(bands, cov) {
  sprintf(paste("Each bar is the share of a depth band's water cells where one transmission is",
                "detected with probability %.2f or more; the bars are not cumulative. Dashed",
                "line: the share over all %s (%s). Under each band: its water cells,",
                "then its share. Range model: %s."),
          attr(bands, "threshold"), fmt_count(sum(bands$n_cells), "water cell"),
          fmt_pct(attr(bands, "overall")),
          cov$range_label)
}
