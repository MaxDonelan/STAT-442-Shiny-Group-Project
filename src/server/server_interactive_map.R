# server code for the interactive map tab

# renders ----------------------------------------------------------------------

output$leaflet_map <- renderLeaflet({
  map_data <- accident |>
    filter(
      between(LATITUDE, -90, 90),
      between(LONGITUD, -180, 180)
    )
  
  leaflet(map_data) |>
    addTiles() |>
    addCircleMarkers(
      lng = ~LONGITUD,
      lat = ~LATITUDE,
      radius = 5,
      stroke = FALSE,
      fillOpacity = 0.5,
      clusterOptions = markerClusterOptions(),
    )
})