# ============================================================
# STAT 442 - Shiny Group Project
# Author: Arni Mwalumba
# UI - Interactive Survival Analysis
# ============================================================

nav_panel(
  title = "Survival Analysis",
  icon = icon("heart-pulse"),
  
  layout_sidebar(
    padding = 0,
    border = FALSE,
    border_radius = FALSE,
    sidebar = sidebar(
      width = 330,
      open = "always",
      
      tags$div(
        style = "padding: 10px;",
        
        tags$h3(
          "Survival Analysis",
          style = "font-weight: bold;"
        ),
        
        # tags$p(
        #   "Fatal Traffic Crashes - USA 2024"
        # ),
        
        # tags$p(
        #   "Zoom or move the map to explore survival
        #    statistics for any visible geographic area.",
        #   style = "font-size: 13px;"
        # ),
        
        tags$hr(),
        
        # Dynamic statistics from server_arni.R
        uiOutput("arni_survival_stats"),
        
        tags$hr(),
        
        tags$p(
          "Survival Rate = Survivors / People Involved",
          style = "font-size: 12px;"
        ),
        
        tags$p(
          "Based on people involved in recorded fatal crashes.",
          style = "font-size: 11px; opacity: 0.75;"
        ),
        tags$p(
          "170 crashes (181 deaths) with unknown locations are not shown on the map.",
          style = "font-size: 12px; color: #D1D5DB;"
        )
      )
    ),
    
    # Our own interactive map
    leafletOutput(
      "arni_map",
      height = "780px"
    )
  )
)