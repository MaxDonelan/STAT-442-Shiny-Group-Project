
# ==================================================
# 2024 RURAL VS. URBAN FATAL CRASH ANALYSIS
# UI
# ==================================================

nav_panel(
  
  title = "Rural vs. Urban",
  
  icon = icon("tree-city"),
  
  layout_sidebar(
    
    # SIDEBAR
    sidebar = sidebar(
      
      title = "Select Analysis",
      
      radioButtons(
        inputId = "graph_choice",
        
        label = "Choose a visualization:",
        
        choices = c(
          "Population Distribution" = "population",
          "Total VMT by State" = "vmt",
          "Fatal Crash Rates" = "rates"
        ),
        
        selected = "population"
      )
    ),
    
    
    # ==================================================
    # POPULATION DISTRIBUTION
    # ==================================================
    
    conditionalPanel(
      
      condition = "input.graph_choice == 'population'",
      
      h3("2024 Rural vs. Urban Population Distribution"),
      
      p(
        "Compare the percentage of each state's population living in rural and urban areas."
      ),
      
      layout_columns(
        
        col_widths = c(6, 6),
        
        card(
          
          card_header("Rural Population (%)"),
          
          plotOutput(
            "rural_population_map",
            height = "500px",
            width = "100%"
          )
        ),
        
        card(
          
          card_header("Urban Population (%)"),
          
          plotOutput(
            "urban_population_map",
            height = "500px",
            width = "100%"
          )
        )
      )
    ),
    
    
    # ==================================================
    # TOTAL VMT
    # ==================================================
    
    conditionalPanel(
      
      condition = "input.graph_choice == 'vmt'",
      
      card(
        
        card_header("2024 Total Vehicle Miles Traveled"),
        
        p(
          "Total rural and urban vehicle miles traveled combined by state."
        ),
        
        plotOutput(
          "vmt_map",
          height = "600px",
          width = "100%"
        )
      )
    ),
    
    
    # ==================================================
    # FATAL CRASH RATES
    # ==================================================
    
    conditionalPanel(
      
      condition = "input.graph_choice == 'rates'",
      
      card(
        
        card_header("Rural vs. Urban Fatal Crash Rates"),
        
        p(
          "Hover over a state to compare rural and urban fatal crash rates. Positive differences indicate a higher rural rate."
        ),
        
        leafletOutput(
          "crash_rate_map",
          height = "650px",
          width = "100%"
        )
      )
    )
  )
)
