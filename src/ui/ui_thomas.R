
# ============================================================
# UI: U.S. Traffic Fatalities
# Author: Thomas Shields
# ============================================================

nav_panel(
  title = "Male V.S. Female",
  icon = icon("venus-mars"),
  
  layout_sidebar(
    
    sidebar = sidebar(
      title = "Filters",
      
      selectInput(
        inputId = "thomas_year",
        label = "Year",
        choices = c(
          "Both Years (Combined)" = "both",
          "2023" = "2023",
          "2024" = "2024"
        ),
        selected = "both"
      ),
      
      hr(),
      
      checkboxInput(
        inputId = "thomas_compare",
        label = "Show drivers only",
        value = FALSE
      ),
      
      hr(),
      
      p(
        strong("All fatalities"),
        br(),
        "Every person who died in a traffic crash,
        including drivers, passengers, and pedestrians."
      ),
      
      p(
        strong("Drivers only"),
        br(),
        "Only people who died while operating a motor vehicle."
      )
    ),
    
    div(
      class = "container-fluid",
      
      h2("U.S. Traffic Fatalities"),
      
      p(
        "Explore how fatality records are distributed by
        sex, road classification, and area."
      ),
      
      p(
        class = "text-muted",
        "When both years are selected, 2023 and 2024
        are combined before percentages are calculated."
      ),
      
      br(),
      
      # ------------------------------------------------------
      # Fatalities by sex
      # ------------------------------------------------------
      
      h4("Fatalities by Sex"),
      
      p(
        "Percentage of fatalities involving males and females.
        Unknown sex records are excluded."
      ),
      
      card(
        card_body(
          plotOutput(
            "thomas_sex_all",
            height = "400px"
          )
        )
      ),
      
      br(),
      
      # ------------------------------------------------------
      # Fatalities by road type
      # ------------------------------------------------------
      
      h4("Fatalities by Road Type"),
      
      p(
        "Within each road classification, compare the
        percentage of male and female fatalities."
      ),
      
      card(
        card_body(
          plotOutput(
            "thomas_road_all",
            height = "550px"
          )
        )
      ),
      
      br(),
      
      # ------------------------------------------------------
      # Fatalities by area
      # ------------------------------------------------------
      
      h4("Fatalities: Rural vs. Urban"),
      
      p(
        "Within each area type, compare the percentage
        of male and female fatalities."
      ),
      
      card(
        card_body(
          plotOutput(
            "thomas_area_all",
            height = "400px"
          )
        )
      )
    )
  )
)
