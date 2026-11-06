# Reactive: aggregate data based on sidebar inputs
map_data_code <- reactive({
  if (input$year_choice == "2024") {
    data <- data24
  } else {
    data <- data23
  }
  
  #Time of Day
  if (input$time_choice != "All Day") {
    data <- data[data$time_of_day == input$time_choice, ]
  }
  
  #Add up by state
  data <- data |>
    group_by(STATE, STATENAME) |>
    summarise(
      total_crashes        = sum(total_crashes, na.rm = TRUE),
      total_fatalities     = sum(total_fatalities, na.rm = TRUE),
      distracted_crashes   = sum(distracted_crashes, na.rm = TRUE),
      distracted_fatalities = sum(distracted_fatalities, na.rm = TRUE),
      impaired_crashes     = sum(impaired_crashes, na.rm = TRUE),
      impaired_fatalities  = sum(impaired_fatalities, na.rm = TRUE),
      Population           = first(Population),
      .groups = "drop"
    )
  
  # Pick the right count column based on crash type + crashes/fatalities
  if (input$crash_choice == "all" && input$fatal_choice == "crashes") {
    data$chosen_count <- data$total_crashes
  } else if (input$crash_choice == "all" && input$fatal_choice == "fatalities") {
    data$chosen_count <- data$total_fatalities
  } else if (input$crash_choice == "distracted" && input$fatal_choice == "crashes") {
    data$chosen_count <- data$distracted_crashes
  } else if (input$crash_choice == "distracted" && input$fatal_choice == "fatalities") {
    data$chosen_count <- data$distracted_fatalities
  } else if (input$crash_choice == "impaired" && input$fatal_choice == "crashes") {
    data$chosen_count <- data$impaired_crashes
  } else {
    data$chosen_count <- data$impaired_fatalities
  }
  
  if (input$fatal_choice == "crashes") {
    data$chosen_total <- data$total_crashes
  } else {
    data$chosen_total <- data$total_fatalities
  }
  
  data$pct_value  <- data$chosen_count / data$chosen_total * 100
  data$rate_value <- data$chosen_count / data$Population * 100000
  
  if (input$pct_choice == "pct") {
    data$map_value <- data$pct_value
  } else {
    data$map_value <- data$rate_value
  }
  data
})

# Reactive legend label
map_legend <- reactive({
  if (input$pct_choice == "pct") {
    if (input$fatal_choice == "crashes") "% of Crashes" else "% of Fatalities"
  } else {
    if (input$fatal_choice == "crashes") "Crashes per 100k" else "Fatalities per 100k"
  }
})

# Render interactive leaflet map
output$state_map <- renderLeaflet({
  data <- map_data_code()
  
  # Join accident data to state geometries by state name
  map_sf <- us_states |>
    left_join(data, by = c("NAME" = "STATENAME"))
  
  # Color palette: light blue -> navy (matches original)
  pal <- colorNumeric(
    palette = c("#deebf7", "navy"),
    domain  = map_sf$map_value,
    na.color = "grey80"
  )
  
  # Hover tooltip for each state
  labels <- sprintf(
    "<strong>%s</strong><br/>
       Total Crashes: %s<br/>
       Total Fatalities: %s<br/>
       Chosen Count: %s<br/>
       %s: %.1f",
    map_sf$NAME,
    format(map_sf$total_crashes,   big.mark = ","),
    format(map_sf$total_fatalities, big.mark = ","),
    format(map_sf$chosen_count,    big.mark = ","),
    map_legend(),
    ifelse(is.na(map_sf$map_value), NA_real_, map_sf$map_value)
  ) |> lapply(htmltools::HTML)
  
  leaflet(map_sf) |>
    addTiles() |>
    setView(lng = -96, lat = 38, zoom = 4) |>
    addPolygons(
      fillColor   = ~pal(map_value),
      fillOpacity = 0.8,
      color       = "white",
      weight      = 1,
      highlightOptions = highlightOptions(
        weight      = 3,
        color       = "#444",
        fillOpacity = 0.95,
        bringToFront = TRUE
      ),
      label        = labels,
      labelOptions = labelOptions(
        style     = list("font-weight" = "normal", padding = "4px 8px"),
        textsize  = "14px",
        direction = "auto"
      )
    ) |>
    addLegend(
      pal      = pal,
      values   = ~map_value,
      title    = map_legend(),
      position = "bottomright",
      na.label = "No data"
    )
})