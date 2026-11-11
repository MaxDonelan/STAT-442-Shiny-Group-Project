# ============================================================
# STAT 442 - Shiny Group Project
# Author: Arni Mwalumba
# State Survival Comparison - USA 2024
#
# Y: Actual number of people involved
# Green: Survivors
# Red: Deaths
# Regions: Descending survival rate
# States: Descending survival rate within each region
# ============================================================

output$arni_state_plot <- renderPlotly({
  
  p <- plot_ly(
    data = state_survival
  ) |>
    
    # GREEN = SURVIVORS
    
    add_bars(
      x = ~x_position,
      y = ~Total_Survivors,
      
      name = "Survivors",
      
      marker = list(
        color = "#2563EB"
      ),
      
      text = ~sprintf(
        "%.1f%%",
        Survival_Rate
      ),
      
      textposition = "inside",
      
      insidetextanchor = "middle",
      
      textfont = list(
        color = "white",
        size = 11
      ),
      
      hovertext = ~Hover,
      hoverinfo = "text"
    ) |>
    
    # RED = DEATHS
    
    add_bars(
      x = ~x_position,
      y = ~Total_Deaths,
      
      name = "Deaths",
      
      marker = list(
        color = "#DC5757"
      ),
      
      text = ~sprintf(
        "%.1f%%",
        Death_Rate
      ),
      
      textposition = "inside",
      
      insidetextanchor = "middle",
      
      textfont = list(
        color = "white",
        size = 11
      ),
      
      hovertext = ~Hover,
      hoverinfo = "text"
    ) |>
    
    layout(
      
      title = list(
        text = paste0(
          "Survival vs Deaths by State",
          "<br>",
          "<sup>Fatal traffic crashes - USA 2024</sup>"
        )
      ),
      
      barmode = "stack",
      
      bargap = 0.15,
      
      xaxis = list(
        title = "States grouped by region",
        
        tickmode = "array",
        
        tickvals = state_survival$x_position,
        
        ticktext = state_survival$Code,
        
        tickangle = -55,
        
        range = c(
          0,
          max(state_survival$x_position) + 1
        ),
        
        showgrid = FALSE
      ),
      
      yaxis = list(
        title = "Total People Involved",
        
        tickformat = ",d",
        
        range = c(
          0,
          max_people * 1.36
        ),
        
        rangemode = "tozero",
        
        zeroline = TRUE
      ),
      
      annotations = region_annotations,
      
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.08
      ),
      
      margin = list(
        l = 100,
        r = 40,
        b = 105,
        t = 105
      ),
      
      hovermode = "closest",
      
      paper_bgcolor = "rgba(0,0,0,0)",
      
      plot_bgcolor = "rgba(0,0,0,0)"
    )
  
  p
})