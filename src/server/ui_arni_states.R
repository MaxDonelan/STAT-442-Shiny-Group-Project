# ==========================================================
# STAT 442 - Shiny Group Project
# Author: Arni Mwalumba
# UI - State Survival Comparison
# ==========================================================

nav_panel(
  title = "State Survival Comparison",
  icon = icon("chart-column"),
  
  div(
    style = "padding: 20px;",
    
    h2("Survival vs Deaths Across U.S. States"),
    
    p(
      "2024 Fatal Traffic Crashes | Arni Mwalumba"
    ),
    
    p(
      "Regions and states are ranked from highest to
       lowest survival rate. Bar heights represent the
       actual number of people involved."
    ),
    
    p(
      "Scroll horizontally to explore all states.",
      style = "font-size: 13px; color: #999;"
    ),
    
    div(
      style = paste0(
        "width: 100%;",
        "overflow-x: auto;",
        "overflow-y: hidden;"
      ),
      
      plotly::plotlyOutput(
        "arni_state_plot",
        width = "2200px",
        height = "760px"
      )
    ),
    
    hr(),
    
    p(
      "Survival rate = Total Survivors / People Involved",
      style = "font-size: 13px;"
    ),
    
    p(
      "These rates describe people involved in recorded
       fatal crashes, not the probability of surviving
       all traffic accidents.",
      style = "font-size: 12px; color: #999;"
    )
  )
)