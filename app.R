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

# data -------------------------------------------------------------------------

accident <- readRDS("data/accident.rds") # I'll probably create a cleaned version eventually (as an FYI if you end up using it)


person_cleaned <- readRDS("data/person_cleaned.rds") |>
  mutate(
    Fatal = factor(Fatal, levels = c("Dead", "Alive", "Unknown/Other"))
)


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
  header = div(input_dark_mode(id = "ide_mode", mode = "dark"),
               style = "text-align: right;"),
  # each file is a tab
  source("src/ui/ui_interactive_map.R", local = TRUE)$value, # need local = TRUE to use environment created by the app as it runs
  source("src/ui/ui_thomas.R", local = TRUE)$value,
  source("src/ui/ui_arni.R", local = TRUE)$value,
  source("src/ui/ui_arni_states.R", local = TRUE)$value
  
)



# server -----------------------------------------------------------------------

# IMPORTANT
# ---------
# Unless you are sourcing a new tab, leave this as is

server <- function(input, output) {
  
  source("src/server/server_interactive_map.R", local = TRUE)$value
  
  source("src/server/server_thomas.R", local = TRUE)$value
  
  source("src/server/server_arni.R", local = TRUE)$value
  
  source("src/server/server_arni_states.R", local = TRUE)$value
}

shinyApp(ui = ui, server = server)


