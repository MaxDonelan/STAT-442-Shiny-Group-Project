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

library(shiny)
library(dplyr)
library(plotly)

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------

if (!exists("accident")) {
  accident <- readRDS("data/accident.rds")
}

# ------------------------------------------------------------
# 2. CREATE STATE AND REGION LOOKUP
# ------------------------------------------------------------

# R's built-in state.region uses "North Central"
# for the Midwest. DC is assigned to the South.

state_lookup <- data.frame(
  STATENAME = toupper(
    c(state.name, "District of Columbia")
  ),
  
  Region = c(
    as.character(state.region),
    "South"
  ),
  
  Code = c(
    state.abb,
    "DC"
  ),
  
  stringsAsFactors = FALSE
) |>
  mutate(
    Region = if_else(
      Region == "North Central",
      "Midwest",
      Region
    )
  )

# ------------------------------------------------------------
# 3. SUMMARIZE FATAL CRASHES BY STATE
# ------------------------------------------------------------

state_survival <- accident |>
  
  distinct(STATE, ST_CASE, .keep_all = TRUE) |>
  
  mutate(
    STATENAME = toupper(
      trimws(as.character(STATENAME))
    ),
    
    People_Involved = suppressWarnings(
      as.numeric(as.character(PERSONS))
    ),
    
    Deaths = suppressWarnings(
      as.numeric(as.character(FATALS))
    )
  ) |>
  
  filter(
    !is.na(People_Involved),
    !is.na(Deaths),
    People_Involved >= Deaths,
    Deaths >= 0
  ) |>
  
  inner_join(
    state_lookup,
    by = "STATENAME"
  ) |>
  
  group_by(STATENAME, Code, Region) |>
  
  summarise(
    Total_Accidents = n(),
    
    Total_People = sum(People_Involved),
    
    Total_Deaths = sum(Deaths),
    
    .groups = "drop"
  ) |>
  
  mutate(
    Total_Survivors = Total_People - Total_Deaths,
    
    Survival_Rate = if_else(
      Total_People > 0,
      100 * Total_Survivors / Total_People,
      NA_real_
    ),
    
    Death_Rate = if_else(
      Total_People > 0,
      100 * Total_Deaths / Total_People,
      NA_real_
    )
  )

# ------------------------------------------------------------
# 4. REGIONAL STATISTICS
# ------------------------------------------------------------

region_survival <- state_survival |>
  
  group_by(Region) |>
  
  summarise(
    Total_Accidents = sum(Total_Accidents),
    
    Total_People = sum(Total_People),
    
    Total_Deaths = sum(Total_Deaths),
    
    Total_Survivors = sum(Total_Survivors),
    
    .groups = "drop"
  ) |>
  
  mutate(
    Survival_Rate =
      100 * Total_Survivors / Total_People,
    
    Death_Rate =
      100 * Total_Deaths / Total_People
  ) |>
  
  arrange(desc(Survival_Rate))

# ------------------------------------------------------------
# 5. ORDER REGIONS BY SURVIVAL RATE
# ------------------------------------------------------------

region_order <- region_survival$Region

state_survival <- state_survival |>
  
  mutate(
    Region = factor(
      Region,
      levels = region_order
    )
  ) |>
  
  arrange(
    Region,
    desc(Survival_Rate),
    desc(Total_Survivors)
  )

# ------------------------------------------------------------
# 6. ADD SPACES BETWEEN REGIONS
# ------------------------------------------------------------

# Three blank positions between successive regions.

state_survival <- state_survival |>
  
  mutate(
    Region_Number = as.integer(Region),
    
    x_position = row_number() +
      3 * (Region_Number - 1)
  )

# ------------------------------------------------------------
# 7. REGIONAL HEADERS ABOVE EACH GROUP
# ------------------------------------------------------------

region_positions <- state_survival |>
  
  group_by(Region) |>
  
  summarise(
    x_center = mean(range(x_position)),
    .groups = "drop"
  ) |>
  
  left_join(
    region_survival,
    by = "Region"
  )

# Maximum bar height determines label positioning.

max_people <- max(
  state_survival$Total_People,
  na.rm = TRUE
)

region_positions <- region_positions |>
  mutate(
    y_position = max_people * 1.19,
    
    Header = sprintf(
      paste0(
        "<b>%s</b>",
        "<br>Survival: %.1f%%",
        "<br>Deaths: %.1f%%"
      ),
      Region,
      Survival_Rate,
      Death_Rate
    )
  )

# ------------------------------------------------------------
# 8. CREATE HOVER INFORMATION
# ------------------------------------------------------------

state_survival <- state_survival |>
  
  mutate(
    Hover = paste0(
      "<b>", STATENAME, "</b>",
      "<br>Region: ", Region,
      
      "<br>Total Accidents: ",
      format(
        Total_Accidents,
        big.mark = ",",
        scientific = FALSE
      ),
      
      "<br>People Involved: ",
      format(
        Total_People,
        big.mark = ",",
        scientific = FALSE
      ),
      
      "<br>Deaths: ",
      format(
        Total_Deaths,
        big.mark = ",",
        scientific = FALSE
      ),
      
      "<br>Survivors: ",
      format(
        Total_Survivors,
        big.mark = ",",
        scientific = FALSE
      ),
      
      "<br>Death Rate: ",
      sprintf("%.2f%%", Death_Rate),
      
      "<br>Survival Rate: ",
      sprintf("%.2f%%", Survival_Rate)
    )
  )

# ------------------------------------------------------------
# 9. CREATE REGIONAL ANNOTATIONS
# ------------------------------------------------------------

region_annotations <- lapply(
  seq_len(nrow(region_positions)),
  
  function(i) {
    
    list(
      x = region_positions$x_center[i],
      y = region_positions$y_position[i],
      
      xref = "x",
      yref = "y",
      
      text = region_positions$Header[i],
      
      showarrow = FALSE,
      
      align = "center",
      
      font = list(
        size = 14,
        color = "#E5A92B"
      )
    )
  }
)

# ------------------------------------------------------------
# 10. INTERACTIVE STACKED BARPLOT
# ------------------------------------------------------------

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