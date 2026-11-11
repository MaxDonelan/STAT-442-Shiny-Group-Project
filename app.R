# ------------------------------------------------------------------------------

# STAT 442 - Shiny Group Project
# Authors:
# ...

# ------------------------------------------------------------------------------

# add libraries as you need
library(shiny)
library(bslib)
library(readr)
library(ggplot2)
library(htmltools)
library(dplyr)
library(paletteer)
library(leaflet)
library(tigris)
library(sf)
library(bsicons)
library(tigris)
library(sf)
library(bsicons)
library(plotly)
library(tidyr)   # pivot_wider() in src/global/global_riddick.R

# data -------------------------------------------------------------------------

accident_cleaned <- readRDS("data/accident_cleaned.rds") |>
  mutate(
    Hour = factor(Hour,
                  levels = c(
                    "12:00am-12:59am",
                    "1:00am-1:59am",
                    "2:00am-2:59am",
                    "3:00am-3:59am",
                    "4:00am-4:59am",
                    "5:00am-5:59am",
                    "6:00am-6:59am",
                    "7:00am-7:59am",
                    "8:00am-8:59am",
                    "9:00am-9:59am",
                    "10:00am-10:59am",
                    "11:00am-11:59am",
                    "12:00pm-12:59pm",
                    "1:00pm-1:59pm",
                    "2:00pm-2:59pm",
                    "3:00pm-3:59pm",
                    "4:00pm-4:59pm",
                    "5:00pm-5:59pm",
                    "6:00pm-6:59pm",
                    "7:00pm-7:59pm",
                    "8:00pm-8:59pm",
                    "9:00pm-9:59pm",
                    "10:00pm-10:59pm",
                    "11:00pm-11:59pm"
                  ))
  )
accident <- readRDS("data/accident.rds") # I'll probably create a cleaned version eventually (as an FYI if you end up using it)

data24    <- readRDS("data/accidents_24.rds")
data23    <- readRDS("data/accidents_23.rds")

# Pre-built by src/unsourced/prepare_app_data.R (re-run it if the source data changes)
fatality_data     <- readRDS("data/fatalities_thomas.rds") # fatal-injury persons, Thomas tab
us_states         <- readRDS("data/us_states.rds")         # state outlines for leaflet maps
us_states_shifted <- readRDS("data/us_states_shifted.rds") # AK/HI moved, for static maps

VMT_data <- readRDS("data/VMT_data.RDS")
Rural_Urban_2024 <- readRDS("data/Rural_Urban_2024.RDS")


# -------------------------------------------------------------------------
# PREPARE RURAL VS. URBAN POPULATION AND VMT DATA
# -------------------------------------------------------------------------

# Calculate rural and urban VMT by state
VMT_by_state <- VMT_data |>
  group_by(State, Area) |>
  summarise(
    VMT = sum(VMT, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate total VMT by state
Total_VMT_by_state <- VMT_by_state |>
  group_by(State) |>
  summarise(
    Total_VMT = sum(VMT, na.rm = TRUE),
    .groups = "drop"
  )

# Combine population and VMT information
State_comparison <- Rural_Urban_2024 |>
  left_join(
    Total_VMT_by_state,
    by = "State"
  )


# -------------------------------------------------------------------------
# PREPARE FATAL CRASH RATE DATA
# -------------------------------------------------------------------------

# Count fatal crashes by state and rural/urban classification
Fatal_crashes_by_state <- accident_cleaned |>
  filter(Rurality %in% c("Rural", "Urban")) |>
  group_by(State, Area = Rurality) |>
  summarise(
    Fatal_Crashes = n_distinct(ST_CASE),
    .groups = "drop"
  )

# Ensure every state has both rural and urban observations
Fatal_crashes_by_state <- Fatal_crashes_by_state |>
  tidyr::complete(
    State = Rural_Urban_2024$State,
    Area = c("Rural", "Urban"),
    fill = list(Fatal_Crashes = 0)
  )

# Calculate fatal crashes per 100 million vehicle miles traveled
Crash_rates <- Fatal_crashes_by_state |>
  left_join(
    VMT_by_state,
    by = c("State", "Area")
  ) |>
  mutate(
    Fatal_Crash_Rate = if_else(
      VMT > 0,
      (Fatal_Crashes / VMT) * 100000000,
      NA_real_
    )
  )


# -------------------------------------------------------------------------
# PER-TAB DATA PREP
# -------------------------------------------------------------------------

# Anything that doesn't depend on user input goes in src/global/ so it runs
# once at startup instead of once per user session in the server files.
source("src/global/global_arni.R", local = TRUE)
source("src/global/global_arni_states.R", local = TRUE)
source("src/global/global_riddick.R", local = TRUE)


# ui ---------------------------------------------------------------------------

# IMPORTANT
# ---------
# Unless you are editing the global ui / theming or sourcing a new tab, leave this as is

ui <- page_navbar(
  theme = bs_theme(
    bootswatch = "zephyr",
    base_font = font_google("Lexend"), # feel free to change, not sure if I like this font yet
    heading_font = font_google("Lexend")) |>
    bs_add_rules("
      .navbar {
        padding-top: 0.25rem;
        padding-bottom: 0.25rem;
      }
      .navbar-brand {
        font-size: 3rem;
        font-weight: bold;
        color: orange;
        padding: 0 1rem 0 0;
        margin-right: 1rem;
        border-right: 3px solid var(--bs-border-color);
      }
    "),
  title = "Traffic Fatalities",
  header = div(
    class = "d-flex justify-content-between align-items-center px-3",
    HTML("<i>Source: NHTSA Fatality Analysis Reporting System</i>"),
    input_dark_mode(id = "ide_mode", mode = "dark")
  ),
  # each file is a tab
  source("src/ui/ui_interactive_map.R", local = TRUE)$value, # need local = TRUE to use environment created by the app as it runs
  source("src/ui/ui_thomas.R", local = TRUE)$value,
  source("src/ui/ui_carter.R", local = TRUE)$value,
  source("src/ui/ui_seasons.R", local = TRUE)$value,
  source("src/ui/ui_arni.R", local = TRUE)$value,
  source("src/ui/ui_arni_states.R", local = TRUE)$value,
  source("src/ui/ui_riddick.R", local = TRUE)$value
  
  
)

# server -----------------------------------------------------------------------

# IMPORTANT
# ---------
# Unless you are sourcing a new tab, leave this as is

server <- function(input, output, session) {
  
  source("src/server/server_interactive_map.R", local = TRUE)$value
  
  source("src/server/server_thomas.R", local = TRUE)$value
  
  source("src/server/server_carter.R", local = TRUE)$value
  
  source("src/server/server_seasons.R", local = TRUE)$value
  
  source("src/server/server_arni.R", local = TRUE)$value
  
  source("src/server/server_arni_states.R", local = TRUE)$value
  
  source("src/server/server_riddick.R", local = TRUE)$value
}

shinyApp(ui = ui, server = server)
