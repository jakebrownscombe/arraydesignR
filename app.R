# Entry point for shinyapps.io. R CMD build leaves this file out of the package.
# Coverage runs in a background process, so one user's computation does not
# hold up another user's session.
# The app opens on the Stoney Lake raster bundled in the package, the one lake
# the package bundles. Another raster is given as
# run_app(areas = list(`Lake name` = "depth.tif")).
library(arraydesignR)
future::plan(future::multisession, workers = 2)
run_app()
