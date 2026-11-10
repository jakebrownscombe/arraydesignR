# arraydesignR 0.1.0

First release. The app replaces Array DesignR 1.x and the array design app of
positionR 1.x.

## Study area

- `run_app()` opens on Stoney Lake, Ontario, the depth raster that came with the 1.x app: 100 m cells
  in UTM zone 17N, 4,818 water cells. It is the only bundled lake; areas passed to `run_app()`
  are offered before it.
- Another lake is uploaded as a GeoTIFF of water depth with land as NA. A raster in longitude
  and latitude is projected to the standard six-degree UTM zone of its centre, without the
  exceptions round Norway and Svalbard. A raster whose longitudes all lie from 180 to 360 is
  read as 360 degrees less, and a message says so; a raster that crosses 180 degrees keeps its
  longitudes and takes the zone of its centre. A raster in longitude and latitude is refused when it
  spans more than 12 degrees of longitude or 12 degrees of latitude. Its longitudes must lie from
  -180 to 180 or from 0 to 360, and its latitudes from -90 to 90. A larger area is given as a raster
  in a projected coordinate reference system in metres, and a lake across 180 degrees in the 0 to
  360 form. A projected raster is refused when its scale at the raster centre, on its two axes or
  its diagonals, is more than 1 percent from 1. It is also refused when that scale cannot be
  measured, as in a local engineering CRS with no geographic datum. A raster with more than
  250,000 water cells is refused, and the message gives the factor to aggregate it by.

## Receivers

- The app opens on the depth raster with no receivers, and a new study area starts with none.
  **Generate** places 20 receivers on a regular grid by default, seed 1; nothing is computed
  until **Compute coverage** is pressed. Grid and random receivers sit at cell centres.
- A minimum receiver depth, 1 m at open, keeps the grid, random draws and map clicks out of
  shallower water and out of 0 m cells. A click on shallower water moves the receiver to the
  nearest cell deep enough and says how far.
- A receiver CSV file may use the column names of the positionR table contract, GLATOS and OTN,
  in any case. A file without coordinates is refused with the columns it has. A receiver depth
  in the file replaces the depth of the raster cell.
- **Clear** asks for confirmation before it removes the layout.

## Range model

- The range model is a logistic curve through 0.5 at d50 and 0.05 at d05, set at a shallow and a
  deep receiver depth and read at each receiver's depth (`detfield::de_logistic()`). It is the
  same for every receiver and every hour.
- Every lake opens on "App defaults, unsourced": d50 250 m and d05 500 m at 1 m, d50 750 m and
  d05 1,500 m at 35 m. No range test stands behind these values. Typed values are named "Your
  values" and are kept when the lake changes.

## Coverage and the map

- Coverage, for each water cell, is the probability that any receiver detects one transmission
  from it, computed with `detfield::water_distances()`, `de_field()` and `system_de()`. With the 20
  receivers **Generate** places on Stoney Lake by default it is 13.4% of the water at 0.5 or
  more, 648 of 4,818 cells. The map opens on depth and moves to array detection probability when
  the first coverage result arrives.
- Distances through water are **detfield**'s exact shortest path, which bends at the shore.
  The coverage and simulation numbers in this file come from it, and so do the values on the
  screenshots (`man/figures/app_shots.json`).
- The curve is read at the distance through water on every receiver-cell pair, including pairs
  with land on the straight line between them.
- Cells farther from every receiver than the distance at which the curve falls below 0.001 are
  out of range: 2,510 m for the app defaults. One computation takes at most 1,000,000
  receiver-to-cell distances: coverage counts those within that distance, and the simulation
  counts every receiver and water-cell pair. A larger computation is refused. The message names
  by station id the receivers that fit together, or gives their number when each receiver takes
  the same count, and the factor to aggregate the raster by. When not one receiver fits, it
  gives the `max_pairs` that all of them need.
  `run_app()` refuses a `max_pairs` below the water-cell count of its largest study area, which
  one receiver takes in the simulation.
- Every coverage number names the range model it was computed with. The banner above the map
  turns amber when the study area, the layout or the range model changes. It names the change
  once the layout holds a receiver. A new study area starts with none, so after a change of
  study area the banner reads "Coverage cannot be computed. Place at least one receiver." until
  one is placed.
  A banner says that results are on screen only when they are for the study area on screen.
- The map has four layers: depth (cornflower blue to dark blue), distance through water to the
  nearest receiver, array detection probability and detection within an hour (magma, 0 to 1).
  Receivers are yellow rings with a dark edge, in the ring colours of detfield, read when a map
  is drawn. The map key sits beside the map: the colour bar of the layer and the receiver ring.

## Performance

- Three tiles give the share of the water at 0.5 or more, at 0.05 or more, and the share
  detected at least once within an hour with probability 0.95 or more. The hourly probability is
  1 - (1 - p)^n with n = 3600 / delay, at a mean transmission delay set on the tab (120 s at
  open). It assumes successive transmissions are detected independently, so it overstates the
  chance of a detection where detection is clustered in time.
- A table compares the array with the highest single-receiver probability in each cell at each threshold
  (`arraydesign::coverage_summary()`). Coverage by depth gives the share of each depth band's
  own water cells at the threshold; the bars are not cumulative.

## Simulation

- The Simulation tab simulates tracks, transmissions and detections with arraydesign under the
  Range model tab's curve and reports detection rates per animal: the mean with its SE, the
  median and range, the animals never detected and the share of animal-hours with a detection.
  It computes no positions.
- The movement model is a correlated random walk, starting at a mean step of 30 m (SD 15 m) per
  60 s with a turning SD of 40 degrees. The transmission delay is random between a shortest and a
  longest delay, 120 and 180 s at open; each must be 5 to 3,600 s.
- A run simulates the animals set, once. The random seed makes it repeatable. A run is refused
  above 100,000 walker steps or 400,000 expected transmissions.
- With the default 20 receivers on Stoney Lake, 20 animals for 6 hours, seed 1: mean detection rate 16.0% (SE 2.9%),
  median 17.9%, 4 of 20 animals never detected, 77 of 120 animal-hours with a detection, 461 of
  2,884 transmissions heard.
- The figure gives one point per animal with a dashed line at the mean; animals never detected
  are hollow. The track map shows heard transmissions as green points, the others as red
  crosses and receivers as yellow rings, and its key names each mark.

## Guide and figures

- The Guide tab explains every control and gives a source for every number.
- Get started, `vignette("arraydesignR")`, shows each tab of the app with the R calls that give
  its numbers. The articles of arraydesign ("Design an array for your own water body: compare
  layouts", "Specifying and testing movement models", "Simulate and score an array step by
  step") give the code in full.
- Figures are drawn with ggplot2 and `detfield::theme_positionr()`.

## Errors

- Errors, warnings and messages the app raises itself carry the classes `arraydesignR_error`,
  `arraydesignR_warning` and `arraydesignR_message`.
- An input the app cannot use is refused through `shiny::validate(shiny::need())`. These
  checks carry Shiny's `validation` class and no arraydesignR class. A check inside a panel
  shows its message in that panel. A check on what Compute coverage or Run simulation needs
  shows its message as an error notification when the button is pressed, and in the banner.
  An input that is still loading has no message and sends no notification.
  Before the first result, with no study area or no receiver, or while an input is still
  loading, the banner reads "No coverage yet. Place receivers and press Compute coverage." (or
  the same for the simulation), because an empty layout is how the app starts.
