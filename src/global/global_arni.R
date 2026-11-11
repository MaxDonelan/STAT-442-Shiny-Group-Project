# Runs once when the app starts (sourced from app.R), not per user session.
# Data prep for the Survival Analysis (server_arni.R) tab; the matching server file only holds reactives and outputs.
# ------------------------------------------------------------
# 2. PREPARE CRASH DATA
# ------------------------------------------------------------

arni_data <- accident |>
  distinct(STATE, ST_CASE, .keep_all = TRUE) |>
  mutate(
    map_lat = suppressWarnings(
      as.numeric(as.character(LATITUDE))
    ),
    map_lon = suppressWarnings(
      as.numeric(as.character(LONGITUD))
    ),
    People_Involved = suppressWarnings(
      as.numeric(as.character(PERSONS))
    ),
    Deaths = suppressWarnings(
      as.numeric(as.character(FATALS))
    )
  ) |>
  filter(
    is.finite(map_lat),
    is.finite(map_lon),
    between(map_lat, -90, 90),
    between(map_lon, -180, 180),
    !is.na(People_Involved),
    !is.na(Deaths),
    People_Involved >= Deaths,
    Deaths >= 0
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
    
    popup_text = paste0(
      "<b>State:</b> ",
      htmltools::htmlEscape(as.character(STATENAME)),
      "<br><b>People Involved:</b> ",
      People_Involved,
      "<br><b>Deaths:</b> ",
      Deaths,
      "<br><b>Survivors:</b> ",
      Survivors,
      "<br><b>Death Rate:</b> ",
      ifelse(
        is.na(Death_Pct),
        "N/A",
        sprintf("%.2f%%", Death_Pct)
      ),
      "<br><b>Survival Rate:</b> ",
      ifelse(
        is.na(Survival_Pct),
        "N/A",
        sprintf("%.2f%%", Survival_Pct)
      )
    )
  )
