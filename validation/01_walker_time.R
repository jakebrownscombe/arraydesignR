# Title: Time of the Simulation tab's walker at its step limit
# Description: The number that the comment on max_steps_per_run in R/score.R
#   states. The walk is the one the Simulation tab runs, with the tab's
#   movement defaults (arraydesign::crw_model(30, 15, 40, time_step_s = 60)),
#   on the bundled Stoney Lake raster: 50 animals for 24 hours at 60 s, which
#   is 72,000 steps, the largest run the tab's limits allow at that time step.
#   The walk is timed 5 times, seeds 1 to 5, and the median is reported. The
#   time depends on the machine and its load; the log names both.
# Run from the package root:
#   Rscript -e 'source("validation/01_walker_time.R", echo = TRUE)' \
#     > validation/outputs/01_walker_time.log 2>&1
# Output: validation/outputs/01_walker_time.log

# ---- Libraries ----
library(arraydesignR)

# ---- Machine ----
R.version.string
Sys.info()[c("sysname", "machine")]
packageVersion("arraydesignR")
packageVersion("arraydesign")
system("sysctl -n vm.loadavg 2>/dev/null || cat /proc/loadavg", intern = TRUE)  # load

# ---- The walk of the Simulation tab ----
ns <- asNamespace("arraydesignR")
lake <- terra::rast(system.file("extdata", "stoney_depth.tif", package = "arraydesignR"))
m <- ns$movement_defaults
walk <- ns$build_movement(m)
n_animals <- 50
hours <- 24
n_steps <- round(hours * 3600 / m$time_step)
n_animals * n_steps  # steps in the run
n_animals * n_steps <= ns$max_steps_per_run  # within the tab's step limit
control <- arraydesign::track_control(start_time = ns$sim_start)

# ---- Time 5 runs ----
secs <- vapply(1:5, function(seed) {
  system.time(arraydesign::simulate_tracks(lake, walk, n_animals = n_animals, n_steps = n_steps,
                                           control = control, seed = seed))[["elapsed"]]
}, numeric(1))
round(secs, 2)  # seconds per run, seeds 1 to 5
round(median(secs), 2)  # median seconds
