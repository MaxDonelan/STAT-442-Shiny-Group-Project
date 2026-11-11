# ui for the interactive map tab

nav_panel(
  title = "Interactive Map",
  icon = icon("fa-regular fa-map"),
  layout_sidebar(
    padding = 0,
    border = FALSE,
    border_radius = FALSE,
    sidebar = sidebar(
      h3("Filters"),
      hr(),
      sliderInput("im_fatal", 
                  label = "Number of fatalities", 
                  min = 1, 
                  max = max(accident_cleaned$Fatalities), 
                  value = c(1, max(accident_cleaned$Fatalities))
      ),
      sliderInput("im_hour",
                  label = "Select Hours",
                  min = as.POSIXct("2024-01-01 00:00", tz = "UTC"),
                  max = as.POSIXct("2024-01-01 23:00", tz = "UTC"),
                  value = c(as.POSIXct("2024-01-01 00:00", tz = "UTC"), as.POSIXct("2024-01-01 23:00", tz = "UTC")),
                  step = 3600,
                  timeFormat = "%I %p",   # 12 AM, 01 AM, ... 11 PM
                  timezone = "+0000"),
      helpText("Crashes with an unknown time are only shown when all hours are selected."),
      dateRangeInput("im_date", 
                     label = "Date Range", 
                     start = "2024-01-01", 
                     end = "2024-12-31",
                     min = "2024-01-01",
                     max = "2024-12-31" 
      ),
      selectizeInput("im_weather",
                     label = "Weather Conditions",
                     choices = c("All", unique(accident_cleaned$Weather))
      ),
      selectizeInput("im_light", 
                     label = "Light Conditions", 
                     choices = c("All", unique(accident_cleaned$`Light Conditions`))
      ),
      HTML("<i>Click on a individual node (blue) to see details</i>"),
      hr(),
      input_task_button("im_reset",
                        label = "Reset",
                        icon = icon("rotate-left"), 
                        auto_reset = TRUE)
    ),
    leafletOutput("im_leaflet_map", height = "100%"),
    # If the user leaves this tab before the map loads, leaflet holds off
    # drawing it until its next resize. Shiny only resizes outputs whose size
    # changed, so the map stays grey when the user returns. Force a resize
    # whenever a tab is shown to draw the held-back map.
    tags$script(HTML("
      $(document).on('shown.bs.tab', function() {
        var widget = HTMLWidgets.find('#im_leaflet_map');
        if (widget && $('#im_leaflet_map').is(':visible')) widget.resize();
      });
    "))
  )
)