# server code for the interactive map tab

# reactives --------------------------------------------------------------------

im_data <- reactive({
  hrs <- as.integer(format(input$im_hour, "%H", tz = "UTC"))
  
  im_filtered_data <- accident_cleaned |>
    filter(
      between(Latitude, -90, 90),
      between(Longitude, -180, 180),
      between(Fatalities, input$im_fatal[1], input$im_fatal[2]),
      between(Date, input$im_date[1], input$im_date[2]),
      between(as.integer(Hour) - 1, hrs[1], hrs[2])
    )
  if (input$im_weather != "All") {
    im_filtered_data <- im_filtered_data |> 
      filter(
        Weather %in% input$im_weather
      )
  }
  if (input$im_light != "All") {
    im_filtered_data <- im_filtered_data |> 
      filter(
        `Light Conditions` %in% input$im_light
      )
  }
  im_filtered_data
})

im_labels <- reactive({
  req(im_data)
  im_data |> transmute(
    Label = paste0(
      "<b>Crash Info</b>",
      "<hr>",
      "<i>Date: </i>", Date, "<br>"  
    )
  )
})

# observers --------------------------------------------------------------------

observe({
  updateSliderInput(session, "im_fatal", value = c(1, max(accident_cleaned$Fatalities)))
  updateSliderInput(session, "im_hour",
                    value = c(as.POSIXct("2024-01-01 00:00", tz = "UTC"), as.POSIXct("2024-01-01 23:00", tz = "UTC")),
                    timeFormat = "%I %p",
                    timezone = "+0000")
  updateDateRangeInput(session, "im_date", 
                       start = "2024-01-01", 
                       end = "2024-12-31")
  updateSelectizeInput(session, "im_weather", selected = "All")
  updateSelectizeInput(session, "im_light", selected = "All")
  
}) |> bindEvent(input$im_reset)

observe({
  leafletProxy("im_leaflet_map", data = im_data()) |>
    clearMarkerClusters() |>
    clearMarkers() |>
    addCircleMarkers(
      lng = ~Longitude,
      lat = ~Latitude,
      radius = 5,
      stroke = FALSE,
      fillOpacity = 0.5,
      clusterOptions = markerClusterOptions(),
      label = im_labels()$Label
   )
})

# renders ----------------------------------------------------------------------

output$im_leaflet_map <- renderLeaflet({
  leaflet() |>
    addTiles() |>
    setView(lng = -98.5, lat = 39.8, zoom = 4)   # continental US
})