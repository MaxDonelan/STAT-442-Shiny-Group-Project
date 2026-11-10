
# ==================================================
# RURAL VS. URBAN FATAL CRASH ANALYSIS
# 2024 DATA
# USER INTERFACE
# ==================================================



nav_panel(
  
  # Page title
  title = ("2024 Rural vs. Urban Fatal Crash Analysis"),
  
  # Sidebar and main panel
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Select Analysis"),
      
      # Choose which graph to display
      radioButtons(
        inputId = "graph_choice",
        label = "Choose a visualization:",
        choices = c(
          "Population Distribution" = "population",
          "Total VMT by State" = "vmt",
          "Fatal Crash Rates" = "rates"
        ),
        selected = "population"
      ),
      
      # Only show state selection for crash rates
      conditionalPanel(
        condition = "input.graph_choice == 'rates'",
        
        selectInput(
          inputId = "selected_state",
          label = "Select a State:",
          choices = NULL
        )
      )
    ),
    
    # Main graph display
    mainPanel(
      
      h3(textOutput("graph_title")),
      
      plotOutput(
        outputId = "comparison_plot",
        height = "850px"
      )
    )
  )
)
