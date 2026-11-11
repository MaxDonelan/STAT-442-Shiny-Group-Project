
# ============================================================
# STAT 442 - Shiny Group Project
# Author: Arni Mwalumba
# Server - Interactive Survival Analysis
#
# Dataset: FARS 2024
#
# Features:
# - Interactive USA accident map
# - Automatic marker clustering
# - Zoom and pan
# - Dynamic accident and survival statistics
# - Red death rate and green survival rate
# ============================================================

library(shiny)
library(dplyr)
library(leaflet)

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------

# The main app already loads accident.rds.
# Fallback if the object is missing.

if (!exists("accident")) {
  accident <- readRDS("data/accident.rds")
}

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

# ------------------------------------------------------------
# 4. CREATE THE INTERACTIVE MAP
# ------------------------------------------------------------

output$arni_map <- renderLeaflet({
  
  leaflet(
    data = arni_data,
    options = leafletOptions(
      minZoom = 2,
      maxZoom = 18
    )
  ) |>
    
    addTiles(
      urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
      attribution = '&copy; OpenStreetMap contributors'
    )|>
    
    setView(
      lng = -98,
      lat = 39,
      zoom = 4
    ) |>
    
    addCircleMarkers(
      lng = ~map_lon,
      lat = ~map_lat,
      
      radius = 5,
      
      color = "#DC5757",
      fillColor = "#DC5757",
      
      fillOpacity = 0.75,
      stroke = FALSE,
      
      popup = ~popup_text,
      
      clusterOptions = markerClusterOptions(
        showCoverageOnHover = FALSE,
        zoomToBoundsOnClick = TRUE,
        spiderfyOnMaxZoom = TRUE,
        maxClusterRadius = 65
      )
    )
})

# ------------------------------------------------------------
# 5. IDENTIFY ACCIDENTS IN VISIBLE MAP AREA
# ------------------------------------------------------------

arni_visible_data <- reactive({
  
  bounds <- input$arni_map_bounds
  
  req(bounds)
  
  req(
    bounds$north,
    bounds$south,
    bounds$east,
    bounds$west
  )
  
  # Ensure longitude comparisons work when the map
  # crosses the international date line.
  
  west <- bounds$west
  east <- bounds$east
  
  # Keep longitudes in the standard [-180, 180] range.
  # If a viewport covers the whole world, include all
  # longitudes.
  
  if ((east - west) >= 360) {
    
    lon_inside <- rep(
      TRUE,
      nrow(arni_data)
    )
    
  } else {
    
    west <- ((west + 180) %% 360) - 180
    east <- ((east + 180) %% 360) - 180
    
    if (west <= east) {
      
      lon_inside <- (
        arni_data$map_lon >= west &
          arni_data$map_lon <= east
      )
      
    } else {
      
      lon_inside <- (
        arni_data$map_lon >= west |
          arni_data$map_lon <= east
      )
    }
  }
  
  arni_data |>
    filter(
      map_lat >= bounds$south,
      map_lat <= bounds$north,
      lon_inside
    )
})

# ------------------------------------------------------------
# 6. CALCULATE DYNAMIC SURVIVAL STATISTICS
# ------------------------------------------------------------

arni_summary <- reactive({
  
  data <- arni_visible_data()
  
  total_accidents <- nrow(data)
  
  total_people <- sum(
    data$People_Involved,
    na.rm = TRUE
  )
  
  total_deaths <- sum(
    data$Deaths,
    na.rm = TRUE
  )
  
  total_survivors <- sum(
    data$Survivors,
    na.rm = TRUE
  )
  
  if (total_people > 0) {
    
    death_rate <- 100 * total_deaths / total_people
    
    survival_rate <- 100 * total_survivors / total_people
    
  } else {
    
    death_rate <- NA_real_
    survival_rate <- NA_real_
  }
  
  list(
    accidents = total_accidents,
    people = total_people,
    deaths = total_deaths,
    survivors = total_survivors,
    death_rate = death_rate,
    survival_rate = survival_rate
  )
})

# ------------------------------------------------------------
# 7. STATISTIC CARD FUNCTION
# ------------------------------------------------------------

arni_card <- function(title, value, color) {
  
  tags$div(
    style = paste0(
      "background: #29313D;",
      "border-radius: 10px;",
      "padding: 12px;",
      "margin-bottom: 10px;"
    ),
    
    tags$div(
      title,
      style = "color: #D1D5DB; font-size: 13px;"
    ),
    
    tags$div(
      value,
      style = paste0(
        "font-size: 26px;",
        "font-weight: bold;",
        "color:", color, ";"
      )
    )
  )
}

# ------------------------------------------------------------
# 8. PERCENTAGE BAR FUNCTION
# ------------------------------------------------------------

arni_bar <- function(title, percentage, color) {
  
  valid <- is.finite(percentage)
  
  pct_text <- if (valid) {
    sprintf("%.2f%%", percentage)
  } else {
    "N/A"
  }
  
  bar_width <- if (valid) {
    min(100, max(0, percentage))
  } else {
    0
  }
  
  tags$div(
    style = "margin-bottom: 20px;",
    
    tags$div(
      style = paste0(
        "font-weight: bold;",
        "margin-bottom: 6px;",
        "color:", color, ";"
      ),
      title
    ),
    
    tags$div(
      style = paste0(
        "width: 100%;",
        "background: #555;",
        "border-radius: 8px;",
        "height: 17px;",
        "overflow: hidden;"
      ),
      
      tags$div(
        style = sprintf(
          paste0(
            "width: %.2f%%;",
            "background: %s;",
            "height: 100%%;"
          ),
          bar_width,
          color
        )
      )
    ),
    
    tags$div(
      pct_text,
      style = paste0(
        "font-size: 24px;",
        "font-weight: bold;",
        "margin-top: 6px;",
        "color:", color, ";"
      )
    )
  )
}

# ------------------------------------------------------------
# 9. DISPLAY STATISTICS IN THE LEFT SIDEBAR
# ------------------------------------------------------------

output$arni_survival_stats <- renderUI({
  
  s <- arni_summary()
  
  tags$div(
    
    style = paste0(
      "background: #1E2530;",
      "color: white;",
      "padding: 14px;",
      "border-radius: 12px;"
    ),
    
    tags$h4(
      "Current Map Area",
      style = "font-weight: bold;"
    ),
    
    tags$p(
      "Statistics update automatically with zoom.",
      style = "font-size: 12px; color: #D1D5DB;"
    ),
    
    arni_card(
      "Total Accidents",
      fmt_arni(s$accidents),
      "#FFFFFF"
    ),
    
    arni_card(
      "People Involved",
      fmt_arni(s$people),
      "#6CB4EE"
    ),
    
    arni_card(
      "Total Deaths",
      fmt_arni(s$deaths),
      "#FF6B6B"
    ),
    
    arni_card(
      "Total Survivors",
      fmt_arni(s$survivors),
      "#55D6A0"
    ),
    
    tags$hr(),
    
    arni_bar(
      "Death Rate",
      s$death_rate,
      "#DC5757"
    ),
    
    arni_bar(
      "Survival Rate",
      s$survival_rate,
      "#259D75"
    ),
    
    tags$hr(),
    
    tags$p(
      "Move or zoom the map to explore another area.",
      style = "font-size: 12px; color: #D1D5DB;"
    )
  )
})
