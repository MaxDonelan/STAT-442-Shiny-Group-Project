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

fatalities_thomas <- readRDS("data/persons_thomas.rds") |>
  filter(INJ_SEVNAME == "Fatal Injury (K)") |>
  transmute(
    YEAR,
    PER_TYP,
    SEX_LABEL = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      TRUE ~ "Unknown / Other"
    ),
    ROAD_TYPE = case_when(
      FUNC_SYS == 1 ~ "Interstate",
      FUNC_SYS == 2 ~ "Other Freeway / Expressway",
      FUNC_SYS == 3 ~ "Other Principal Arterial",
      FUNC_SYS == 4 ~ "Minor Arterial",
      FUNC_SYS == 5 ~ "Major Collector",
      FUNC_SYS == 6 ~ "Minor Collector",
      FUNC_SYS == 7 ~ "Local Road",
      TRUE ~ "Unknown / Other"
    ),
    AREA_TYPE = case_when(
      RUR_URB == 1 ~ "Rural",
      RUR_URB == 2 ~ "Urban",
      TRUE ~ "Unknown / Other"
    )
  )

saveRDS(fatalities_thomas, "data/fatalities_thomas.rds")

# State outlines ---------------------------------------------------------------

us_states <- states(cb = TRUE, year = 2022, resolution = "20m") |>
  filter(NAME %in% c(state.name, "District of Columbia")) |>
  st_transform(crs = 4326)

saveRDS(us_states, "data/us_states.rds")

# shift_geometry() needs lwgeom
saveRDS(shift_geometry(us_states), "data/us_states_shifted.rds")
