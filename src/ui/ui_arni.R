
1 + 1

# ============================================================
# STAT 442 - Shiny Group Project
# Author: Arni Mwalumba
# UI - Interactive Survival Analysis
# ============================================================

nav_panel(
  title = "Survival Analysis - Arni",
  icon = icon("heart-pulse"),
  
  layout_sidebar(
    
    sidebar = sidebar(
      width = 330,
      open = "always",
      
      tags$div(
        style = "padding: 10px;",
        
        tags$h3(
          "Survival Analysis",
          style = "font-weight: bold;"
        ),
        
        tags$p(
          "Fatal Traffic Crashes - USA 2024"
        ),
        
        tags$p(
          "Zoom or move the map to explore survival
           statistics for any visible geographic area.",
          style = "font-size: 13px;"
        ),
        
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