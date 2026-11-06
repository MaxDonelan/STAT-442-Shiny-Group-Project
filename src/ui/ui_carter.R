nav_panel(
  title = tagList(bsicons::bs_icon("geo-alt-fill"), " Analysis by State"),
  
  layout_sidebar(
    sidebar = sidebar(
      
      # Choose Year
      selectInput(
        inputId  = "year_choice",
        label    = "Year:",
        choices  = c("2024", "2023"),
        selected = "2024"
      ),
      
      # Choose Metric
      radioButtons(
        inputId  = "pct_choice",
        label    = "Show map as:",
        choices  = c("Rate per 100,000 people" = "rate", "Percentage" = "pct"),
        selected = "rate"
      ),
      
      # Choose Time of Day
      selectInput(
        inputId  = "time_choice",
        label    = "Time of Day:",
        choices  = c("All Day", "Morning", "Afternoon", "Night"),
        selected = "All Day"
      ),
      
      # Choose Crash Type
      radioButtons(
        inputId  = "crash_choice",
        label    = "Type of Crash:",
        choices  = c("All Crashes" = "all", "Impaired Crashes" = "impaired", "Distracted Crashes" = "distracted"),
        selected = "all"
      ),
      
      # Crashes or Fatalities
      radioButtons(
        inputId  = "fatal_choice",
        label    = "Look at:",
        choices  = c("Crashes" = "crashes", "Fatalities" = "fatalities"),
        selected = "crashes"
      )
    ),
    
    leafletOutput(outputId = "state_map", height = "600px")
  )
)
