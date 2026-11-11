# Runs once when the app starts (sourced from app.R), not per user session.
# Data prep for the Survival Analysis (server_arni.R) tab; the matching server file only holds reactives and outputs.
# ------------------------------------------------------------
# 2. PREPARE CRASH DATA
# ------------------------------------------------------------

# One row per crash with valid people/death counts. Shared by this tab and the
# State Survival Comparison tab (src/global/global_arni_states.R).
arni_crashes <- accident |>
  distinct(STATE, ST_CASE, .keep_all = TRUE) |>
  mutate(
    # PERSONS only counts people in vehicles; PEDS adds pedestrians,
    # cyclists and other non-motorists, who are included in FATALS.
    People_Involved = suppressWarnings(
      as.numeric(as.character(PERSONS)) + as.numeric(as.character(PEDS))
    ),
    Deaths = suppressWarnings(
      as.numeric(as.character(FATALS))
    )
  ) |>
  filter(
    !is.na(People_Involved),
    !is.na(Deaths),
    People_Involved >= Deaths,
    Deaths >= 0
  )

# Crashes with valid coordinates for the map
arni_data <- arni_crashes |>
  mutate(
    map_lat = suppressWarnings(
      as.numeric(as.character(LATITUDE))
    ),
    map_lon = suppressWarnings(
      as.numeric(as.character(LONGITUD))
    )
  ) |>
  filter(
    is.finite(map_lat),
    is.finite(map_lon),
    between(map_lat, -90, 90),
    between(map_lon, -180, 180)
  ) |>
  mutate(
    Survivors = People_Involved - Deaths
  )

# ------------------------------------------------------------
# 3. CREATE POPUP INFORMATION
# ------------------------------------------------------------

fmt_arni <- function(x) {
  format(
    round(x),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

arni_data <- arni_data |>
  mutate(
    Death_Pct = if_else(
      People_Involved > 0,
      100 * Deaths / People_Involved,
      NA_real_
    ),
    
    Survival_Pct = if_else(
      People_Involved > 0,
      100 * Survivors / People_Involved,
      NA_real_
    ),

    # Marker ID sent back to the server when a marker is clicked
    arni_id = row_number()
  )

# Popup HTML for one crash. Built only when its marker is clicked, so the
# browser doesn't have to download a popup for all ~36k crashes up front.
arni_popup <- function(crash) {
  paste0(
    "<b>State:</b> ",
    htmltools::htmlEscape(as.character(crash$STATENAME)),
    "<br><b>People Involved:</b> ",
    crash$People_Involved,
    "<br><b>Deaths:</b> ",
    crash$Deaths,
    "<br><b>Survivors:</b> ",
    crash$Survivors,
    "<br><b>Death Rate:</b> ",
    ifelse(
      is.na(crash$Death_Pct),
      "N/A",
      sprintf("%.2f%%", crash$Death_Pct)
    ),
    "<br><b>Survival Rate:</b> ",
    ifelse(
      is.na(crash$Survival_Pct),
      "N/A",
      sprintf("%.2f%%", crash$Survival_Pct)
    )
  )
}

# Keep only the columns the map and statistics use. Coordinates are rounded to
# 5 decimals (about 1 m), which is plenty for the map and shrinks what is sent
# to the browser.
arni_data <- arni_data |>
  transmute(
    arni_id, STATENAME,
    map_lat = round(map_lat, 5),
    map_lon = round(map_lon, 5),
    People_Involved, Deaths, Survivors, Death_Pct, Survival_Pct
  )
