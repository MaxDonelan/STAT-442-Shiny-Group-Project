# ------------------------------------------------------------------------------
# Pre-builds small data files the app reads at startup.
#
# Run this from the project root whenever the source data changes:
#   Rscript src/unsourced/prepare_app_data.R
#
# Outputs (in data/):
#   fatalities_thomas.rds  - fatal-injury persons with only the columns the
#                            Thomas tab uses (replaces loading persons_thomas.rds,
#                            ~134 MB in memory, at runtime)
#   us_states.rds          - state outlines (lower 48 + AK, HI, DC), WGS84, for
#                            the leaflet maps (removes the Census download)
#   us_states_shifted.rds  - same outlines with Alaska and Hawaii moved below the
#                            lower 48, for the static ggplot maps
# ------------------------------------------------------------------------------

library(dplyr)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

# Thomas: fatality-level data --------------------------------------------------
# Built by Thomas's own cleaning script (also writes data/fatalities_thomas.rds)

source("src/unsourced/fatalities_cleaning_thomas.R")

# State outlines ---------------------------------------------------------------

us_states <- states(cb = TRUE, year = 2022, resolution = "20m") |>
  filter(NAME %in% c(state.name, "District of Columbia")) |>
  st_transform(crs = 4326)

saveRDS(us_states, "data/us_states.rds")

# shift_geometry() needs lwgeom
saveRDS(shift_geometry(us_states), "data/us_states_shifted.rds")
