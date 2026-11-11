
# ==================================================
# 2024 RURAL VS. URBAN FATAL CRASH ANALYSIS
# SERVER
# ==================================================

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(leaflet)
library(htmltools)
library(scales)


# ==================================================
# 1. PREPARE MAP DATA
# ==================================================

# Join population and VMT information to state map
map_data <- us_states %>%
  left_join(
    State_comparison,
    by = c("NAME" = "State")
  )


# Reshape fatal crash rates into rural and urban columns
crash_rate_data <- Crash_rates %>%
  select(State, Area, Fatal_Crash_Rate) %>%
  pivot_wider(
    names_from = Area,
    values_from = Fatal_Crash_Rate
  ) %>%
  mutate(
    Rate_Difference = Rural - Urban
  )


# Join crash rates to state map
crash_map_data <- us_states %>%
  left_join(
    crash_rate_data,
    by = c("NAME" = "State")
  )


# ==================================================
# 2. MAP COORDINATES
# ==================================================

# Geographic extent that includes all 50 states
map_coordinates <- coord_sf(
  xlim = c(-179, -66),
  ylim = c(17, 72),
  expand = FALSE
)


# ==================================================
# 3. RURAL POPULATION MAP
# ==================================================

output$rural_population_map <- renderPlot({
  
  ggplot(map_data) +
    
    geom_sf(
      aes(fill = Rural_Pct),
      color = "white",
      linewidth = 0.3
    ) +
    
    map_coordinates +
    
    scale_fill_viridis_c(
      option = "C",
      limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1),
      name = "Rural Population"
    ) +
    
    labs(
      title = "Rural Population by State",
      subtitle = "Percentage of total state population"
    ) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(
        size = 16,
        face = "bold"
      ),
      plot.subtitle = element_text(size = 11),
      legend.position = "bottom",
      plot.margin = margin(5, 5, 5, 5),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
  
}, res = 96)


# ==================================================
# 4. URBAN POPULATION MAP
# ==================================================

output$urban_population_map <- renderPlot({
  
  ggplot(map_data) +
    
    geom_sf(
      aes(fill = Urban_Pct),
      color = "white",
      linewidth = 0.3
    ) +
    
    map_coordinates +
    
    scale_fill_viridis_c(
      option = "C",
      limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1),
      name = "Urban Population"
    ) +
    
    labs(
      title = "Urban Population by State",
      subtitle = "Percentage of total state population"
    ) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(
        size = 16,
        face = "bold"
      ),
      plot.subtitle = element_text(size = 11),
      legend.position = "bottom",
      plot.margin = margin(5, 5, 5, 5),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
  
}, res = 96)


# ==================================================
# 5. TOTAL VEHICLE MILES TRAVELED MAP
# ==================================================

output$vmt_map <- renderPlot({
  
  ggplot(map_data) +
    
    geom_sf(
      aes(fill = Total_VMT),
      color = "white",
      linewidth = 0.3
    ) +
    
    map_coordinates +
    
    scale_fill_viridis_c(
      option = "C",
      labels = scales::label_number(
        scale = 1e-9,
        suffix = "B"
      ),
      name = "Total VMT"
    ) +
    
    labs(
      title = "Total Vehicle Miles Traveled by State",
      subtitle = "2024 rural and urban VMT combined"
    ) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(
        size = 17,
        face = "bold"
      ),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom",
      plot.margin = margin(10, 10, 10, 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
  
}, res = 96)


# ==================================================
# 6. INTERACTIVE FATAL CRASH RATE MAP
# ==================================================

output$crash_rate_map <- renderLeaflet({
  
  # Color palette for rural minus urban rate
  rate_palette <- colorNumeric(
    palette = "RdYlBu",
    domain = crash_map_data$Rate_Difference,
    reverse = TRUE,
    na.color = "#D3D3D3"
  )
  
  
  # Hover labels
  hover_labels <- sprintf(
    paste0(
      "<strong>%s</strong><br/>",
      "Rural fatal crash rate: %s<br/>",
      "Urban fatal crash rate: %s<br/>",
      "Rural minus urban difference: %s"
    ),
    
    crash_map_data$NAME,
    
    ifelse(
      is.na(crash_map_data$Rural),
      "No data",
      round(crash_map_data$Rural, 3)
    ),
    
    ifelse(
      is.na(crash_map_data$Urban),
      "No data",
      round(crash_map_data$Urban, 3)
    ),
    
    ifelse(
      is.na(crash_map_data$Rate_Difference),
      "No data",
      round(crash_map_data$Rate_Difference, 3)
    )
  )
  
  
  # Build map
  leaflet(crash_map_data) %>%
    
    addProviderTiles(
      providers$OpenStreetMap.Mapnik
    ) %>%
    
    addPolygons(
      
      fillColor = ~rate_palette(Rate_Difference),
      
      fillOpacity = 0.8,
      
      color = "white",
      
      weight = 1,
      
      label = lapply(
        hover_labels,
        htmltools::HTML
      ),
      
      labelOptions = labelOptions(
        sticky = TRUE,
        direction = "auto",
        textsize = "13px"
      ),
      
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#333333",
        fillOpacity = 0.95,
        bringToFront = TRUE
      )
    ) %>%
    
    addLegend(
      position = "bottomright",
      pal = rate_palette,
      values = ~Rate_Difference,
      title = "Rural - Urban<br>Rate Difference",
      labFormat = labelFormat(digits = 2),
      na.label = "No data"
    )
  
})
