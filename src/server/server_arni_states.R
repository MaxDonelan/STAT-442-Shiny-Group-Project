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
  
  # Text and gridlines follow the app's light/dark mode toggle
  fg <- plot_foreground(input$ide_mode)
  grid_color <- if (identical(input$ide_mode, "light")) {
    "rgba(0,0,0,0.12)"
  } else {
    "rgba(255,255,255,0.12)"
  }
  
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
      
      font = list(color = fg),
      
      # Title, legend and region headers are stacked above the bars
      # (top to bottom) so none of them overlap
      title = list(
        text = paste0(
          "<b>Survival vs Deaths by State</b>",
          "<br>",
          "<sup>Fatal traffic crashes - USA 2024</sup>"
        ),
        x = 0.5,
        xanchor = "center",
        y = 0.98,
        yanchor = "top",
        font = list(size = 20, color = fg)
      ),
      
      barmode = "stack",
      
      bargap = 0.15,
      
      # Labels that would have to shrink below a readable size are hidden
      # (the values remain in the hover text)
      uniformtext = list(
        minsize = 9,
        mode = "hide"
      ),
      
      xaxis = list(
        title = list(
          text = "States grouped by region",
          standoff = 12
        ),
        
        tickmode = "array",
        
        tickvals = state_survival$x_position,
        
        ticktext = state_survival$Code,
        
        tickangle = -55,
        
        tickfont = list(size = 12, color = fg),
        
        range = c(
          0,
          max(state_survival$x_position) + 1
        ),
        
        showgrid = FALSE,
        
        linecolor = grid_color
      ),
      
      yaxis = list(
        title = list(
          text = "Total People Involved",
          standoff = 15
        ),
        
        tickformat = ",d",
        
        tickfont = list(size = 12, color = fg),
        
        range = c(
          0,
          max_people * 1.08
        ),
        
        rangemode = "tozero",
        
        gridcolor = grid_color,
        
        zeroline = TRUE,
        
        zerolinecolor = grid_color
      ),
      
      annotations = region_annotations,
      
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        # Container coordinates: between the title and the region headers
        yref = "container",
        y = 0.895,
        yanchor = "top",
        font = list(size = 13, color = fg),
        bgcolor = "rgba(0,0,0,0)"
      ),
      
      margin = list(
        l = 100,
        r = 40,
        b = 105,
        t = 170
      ),
      
      hovermode = "closest",
      
      hoverlabel = list(
        font = list(color = "#212529"),
        bgcolor = "white"
      ),
      
      paper_bgcolor = "rgba(0,0,0,0)",
      
      plot_bgcolor = "rgba(0,0,0,0)"
    )
  
  p
})