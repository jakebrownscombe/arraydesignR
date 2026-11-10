### What the app does

Array DesignR places acoustic receivers on a depth raster, applies a detection range model, and
maps the probability that the array detects a transmission from each water cell. Use it to design an
array for a new study or to test and refine an array already in the water. Distances, detection
probabilities and coverage come from the
[**detfield**](https://jakebrownscombe.github.io/detfield/) R package; receiver placement and the
simulation come from the
[**arraydesign**](https://jakebrownscombe.github.io/arraydesign/) R package.

### Study area

The app opens on Stoney Lake, Ontario, the one bundled lake: a 100 m depth raster with 4,818
water cells in UTM zone 17N (Source: `?arraydesignR::run_app`).

Another water body is uploaded as a GeoTIFF of water depth, with land as NA and the coordinate system
saved in the file. Depth may be positive or negative, with one sign throughout. A raster in
longitude and latitude is projected to the standard six-degree UTM zone of its centre, without
the exceptions round Norway and Svalbard. A raster whose longitudes all lie from 180 to 360 is
read as 360 degrees less, and a message says so; a raster that crosses 180 degrees keeps its
longitudes and takes the zone of its centre. A raster in longitude and latitude is refused when it
spans more than 12 degrees of longitude or 12 degrees of latitude. Its longitudes must lie from -180
to 180 or from 0 to 360, and its latitudes from -90 to 90. A larger area is given as a raster in a
projected coordinate reference system in metres, and a lake across 180 degrees in the 0 to 360 form.
A projected raster is refused when its scale at the raster centre, on its two axes or its diagonals,
is more than 1 percent from 1. It is also refused when that scale cannot be measured, as in a local
engineering CRS with no geographic datum. A raster may hold at most 250,000 water cells; a larger
one is refused, and the message gives the factor to aggregate it by (Source: `?arraydesignR::run_app`).

### Receivers

- **Regular grid** places the number of receivers set on a square lattice over the water;
  **Random** draws that number of water cells. The app opens with no receivers, and a new study
  area starts with none; **Generate** places 20 on the grid unless the box says otherwise. The
  random seed makes a draw repeatable and breaks ties on the grid (Source:
  `?arraydesign::place_receivers` and the **arraydesignR** NEWS).
- **Minimum receiver depth**, 1 m at open, keeps the grid, the random draw and map clicks out of
  shallower water and out of cells of 0 m. A click on shallower water moves the receiver to the
  nearest cell deep enough (Source: `?arraydesign::place_receivers`, which shows how to set cells
  under 1 m to NA the same way).
- **Click** the map to add a receiver; click a receiver to remove it.
- **Upload** a CSV file with `x` and `y` in the coordinate system of the raster, or longitude and
  latitude in WGS 84. Column names of the positionR table contract, GLATOS and OTN are read. A
  point on land within one cell diagonal of water moves to that cell; a point farther away is
  refused. A receiver depth in the file replaces the raster depth.
- **Download** writes the layout as a CSV file with the column names of the positionR table
  contract.

### Range model

The detection curve is a logistic through probability 0.5 at d50 and 0.05 at d05, each set at a
shallow and a deep receiver depth. Both distances are interpolated linearly between the two
depths and held at the nearest depth outside them. The receiver's depth is the depth of its
cell, or the depth given in an uploaded file. d05 must be larger than d50. The curve is the same
for every receiver and every hour (Source: `?detfield::de_logistic`).

Every water body opens on "App defaults, unsourced": d50 250 m and d05 500 m at 1 m, d50 750 m and d05
1,500 m at 35 m. No range test stands behind these values. Because d05 is twice d50 at both
depths, the curve gives a detection probability of 0.95 at 0 m. Typed values are named "Your
values" and are kept when the water body changes (Source: the **arraydesignR** NEWS, and
`?detfield::de_logistic`).

### Coverage

Coverage is computed on each press of **Compute coverage**, never on its own. For every water cell the
app takes the distance through water to each receiver and the probability that at least one
receiver detects one transmission (`detfield::water_distances`, `de_field` and `system_de`). This
probability is the coverage of Pedersen et al. (2014): "the probability of at least one sensor
detecting a signal emitted in that grid cell" (Source: Pedersen et al. 2014, p. 1047). It applies
to one transmission. The Simulation tab estimates the share of animals detected.

Receivers are independent given the cell. Where the straight line from a receiver to a cell
crosses land, the curve is read at the distance through water, which overstates detection behind
land.

Cells farther from every receiver than the distance at which the curve falls below 0.001 are
out of range: 2,510 m for the App defaults. One computation may take at most 1,000,000
receiver-to-cell distances within that cutoff; the line under the button gives the count
(Source: `?arraydesignR::run_app` and the **arraydesignR** NEWS).

The banner turns amber and names the change when the study area, the layout or the range model
changes. It turns green when coverage is computed again.

### Map layers

The buttons above the map set which layer the Map tab draws under the receivers. The map opens on
**Depth** and moves to **Array detection probability** when the first coverage result arrives.

- **Depth** gives the depth of each water cell.
- **Distance through water to nearest receiver** is blank beyond the out-of-range distance.
- **Array detection probability** gives the coverage of each cell.
- **Detection within an hour** gives p_hour, defined under Performance.

### What the map shows

The Map tab and the track map of the Simulation tab use these marks and colours.

- **Cornflower blue to dark blue** is depth, from the shallows to the deepest cell (Source:
  `?detfield::scale_fill_depth`).
- **Yellow near a receiver to dark purple far from it** (viridis) is the distance through water
  to the nearest receiver.
- **Black to pale yellow** (magma) is the array detection probability or the detection within an
  hour, from 0 to 1 (Source: `?detfield::scale_fill_probability`).
- **Yellow rings with a dark edge** are receivers.
- **Grey lines** are simulated tracks, on the track map.
- **Green points** are transmissions heard by at least one receiver, on the track map.
- **Red crosses** are transmissions that no receiver heard, on the track map.

### Performance

The tab reads the last coverage result for the water body on screen. Three tiles give the share of the
water where any receiver detects one transmission with probability 0.5 or more,
the same at 0.05 or more, and the share where an animal is detected at least once within an hour
with probability 0.95 or more (Source: the **arraydesignR** NEWS).

The hourly probability is p_hour = 1 - (1 - p)^n, where p is the coverage for one transmission
and n = 3600 / delay is the number of transmissions in an hour at the mean transmission delay set
on the tab (120 s to start, so n = 30). The formula assumes that successive transmissions are
detected independently. Where detection is clustered in time, p_hour overstates the chance of a
detection (Source: the **arraydesignR** NEWS).

The table gives, for each threshold, the share and number of water cells reached by the array
and by the highest single-receiver probability in each cell (`arraydesign::coverage_summary`). The depth plot gives the share
of each depth band's own water cells at the chosen threshold.

### Simulation

Simulated animals move through the water, each tag transmits at a random delay between the shortest
and the longest delay, and receivers detect under the curve of the Range model tab. The delays
start at 120 and 180 s and must each be 5 to 3,600 s; equal values give a fixed delay. The tab
reports detections and computes no positions (Source: `?arraydesign::simulate_tracks`,
`?arraydesign::simulate_transmissions`, `?arraydesign::simulate_detections` and the **arraydesignR**
NEWS).

The movement model is a correlated random walk: gamma step lengths with the mean and SD set per
time step, and turning angles from a wrapped normal distribution centred on 0 with the SD set.
It starts at a mean step of 30 m, an SD of 15 m, a time step of 60 s and a turning SD of 40
degrees (Source: `?arraydesign::crw_model` and `?arraydesign::evaluate_array`).

A run simulates the number of animals set, each for the hours set. The same seed and settings
give the same animals and detections. A run is refused above 100,000 walker steps or 400,000
expected transmissions (Source: the **arraydesignR** NEWS).

The detection rate of an animal is the share of its transmissions heard by at least one
receiver. The tiles give the mean rate with its SE across animals, the median, the animals never
detected, and the share of animal-hours with a detection. The figure and the table give one entry
per animal. The track map shows every track with its transmissions, detected or not, and the
receivers; "What the map shows" gives the marks. The rates depend on the movement model and the
range model; use them to compare layouts.

### Sources

- Pedersen, M.W., Burgess, G. & Weng, K.C. (2014) A quantitative approach to static sensor
  network design. Methods in Ecology and Evolution 5(10), 1043-1051.
  https://doi.org/10.1111/2041-210X.12255
- The help pages of **detfield**, **arraydesign** and **arraydesignR**, and the **arraydesignR** NEWS.
