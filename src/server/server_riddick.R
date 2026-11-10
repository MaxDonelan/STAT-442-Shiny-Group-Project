
# ==================================================
# RURAL VS. URBAN FATAL CRASH ANALYSIS
# 2024 DATA - ALL 50 U.S. STATES
# ==================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)



# Load RDS files (use these lines when reopening the project)
VMT_2024 <- readRDS("VMT_data.rds")
RvU2024 <- readRDS("Rural_Urban_2024.rds")


# --------------------------------------------------
# 2. CLEAN DATA
# --------------------------------------------------

# Clean state names
accidents$STATENAME <- trimws(as.character(accidents$STATENAME))
VMT$State <- trimws(as.character(VMT$State))
R_U_2024_pop$State <- trimws(as.character(R_U_2024_pop$State))

# Convert VMT to numeric
VMT$VMT <- as.numeric(gsub(",", "", as.character(VMT$VMT)))

# Filter VMT for 2024
if ("Year" %in% names(VMT)) {
  VMT <- VMT %>%
    filter(as.numeric(as.character(Year)) == 2024)
}


# --------------------------------------------------
# 3. KEEP ONLY THE 50 U.S. STATES
# --------------------------------------------------

valid_states <- unique(R_U_2024_pop$State)

VMT <- VMT %>%
  filter(State %in% valid_states)

accidents <- accidents %>%
  filter(STATENAME %in% valid_states)


# --------------------------------------------------
# 4. POPULATION DISTRIBUTION
# --------------------------------------------------

Population_by_state <- R_U_2024_pop %>%
  select(State, Rural_Pct, Urban_Pct)


# --------------------------------------------------
# 5. VMT CALCULATIONS
# --------------------------------------------------

# Combine road classifications into rural/urban totals
VMT_by_state <- VMT %>%
  group_by(State, Area) %>%
  summarise(
    VMT = sum(VMT, na.rm = TRUE),
    .groups = "drop"
  )

# Total rural + urban VMT by state
Total_VMT_by_state <- VMT_by_state %>%
  group_by(State) %>%
  summarise(
    Total_VMT = sum(VMT, na.rm = TRUE),
    .groups = "drop"
  )

# Rural and urban percentages of VMT
VMT_percentages <- VMT_by_state %>%
  group_by(State) %>%
  mutate(
    Total_VMT = sum(VMT, na.rm = TRUE),
    VMT_Pct = VMT / Total_VMT * 100
  ) %>%
  ungroup()


# --------------------------------------------------
# 6. FATAL CRASH CALCULATIONS
# --------------------------------------------------

# Count fatal crashes by state and rural/urban area
Fatal_crashes_by_state <- accidents %>%
  filter(RUR_URBNAME %in% c("Rural", "Urban")) %>%
  group_by(STATENAME, Area = RUR_URBNAME) %>%
  summarise(
    Fatal_Crashes = n(),
    .groups = "drop"
  )


# --------------------------------------------------
# 7. CALCULATE FATAL CRASH RATES
# --------------------------------------------------

# Start with VMT so every state/area combination is included
Crash_rates <- VMT_by_state %>%
  left_join(
    Fatal_crashes_by_state,
    by = c("State" = "STATENAME", "Area")
  ) %>%
  mutate(
    Fatal_Crashes = tidyr::replace_na(Fatal_Crashes, 0L),
    
    # Fatal crashes per 100 million vehicle miles traveled
    Fatal_Crash_Rate = ifelse(
      !is.na(VMT) & VMT > 0,
      Fatal_Crashes / VMT * 100000000,
      NA_real_
    )
  )


# --------------------------------------------------
# 8. COMBINE POPULATION, VMT, AND CRASH DATA
# --------------------------------------------------

State_comparison <- Population_by_state %>%
  left_join(
    Total_VMT_by_state,
    by = "State"
  ) %>%
  left_join(
    VMT_percentages %>%
      select(State, Area, VMT, VMT_Pct) %>%
      tidyr::pivot_wider(
        names_from = Area,
        values_from = c(VMT, VMT_Pct)
      ),
    by = "State"
  )




# ==================================================
# INTERACTIVE RURAL VS. URBAN DASHBOARD SERVER
# ==================================================

interactive_map_server <- function(input, output, session) {
  
  # Populate state dropdown
  observe({
    
    updateSelectInput(
      session,
      inputId = "selected_state",
      choices = sort(unique(Crash_rates$State)),
      selected = "South Dakota"
    )
    
  })
  
  
  # --------------------------------------------------
  # GRAPH TITLES
  # --------------------------------------------------
  
  output$graph_title <- renderText({
    
    req(input$graph_choice)
    
    if (input$graph_choice == "population") {
      "Rural vs. Urban Population Distribution by State"
      
    } else if (input$graph_choice == "vmt") {
      "Total Vehicle Miles Traveled by State"
      
    } else {
      paste(
        "Rural vs. Urban Fatal Crash Rates:",
        input$selected_state
      )
    }
    
  })
  
  
  # --------------------------------------------------
  # INTERACTIVE GRAPH
  # --------------------------------------------------
  
  output$comparison_plot <- renderPlot({
    
    req(input$graph_choice)
    
    
    # ================================================
    # GRAPH 1: POPULATION DISTRIBUTION
    # ================================================
    
    if (input$graph_choice == "population") {
      
      population_plot_data <- State_comparison %>%
        select(State, Rural_Pct, Urban_Pct) %>%
        tidyr::pivot_longer(
          cols = c(Rural_Pct, Urban_Pct),
          names_to = "Area",
          values_to = "Population_Pct"
        ) %>%
        mutate(
          Area = recode(
            Area,
            Rural_Pct = "Rural",
            Urban_Pct = "Urban"
          ),
          Population_Pct = Population_Pct * 100
        )
      
      ggplot(
        population_plot_data,
        aes(
          x = reorder(State, Population_Pct),
          y = Population_Pct,
          fill = Area
        )
      ) +
        
        geom_col(
          position = "dodge"
        ) +
        
        coord_flip() +
        
        labs(
          title = "Rural vs. Urban Population",
          x = "State",
          y = "Population (%)",
          fill = "Area"
        ) +
        
        theme_minimal() +
        
        theme(
          plot.title = element_text(
            size = 16,
            face = "bold"
          ),
          axis.text.y = element_text(size = 8)
        )
      
      
      # ================================================
      # GRAPH 2: TOTAL VMT BY STATE
      # ================================================
      
    } else if (input$graph_choice == "vmt") {
      
      ggplot(
        State_comparison,
        aes(
          x = reorder(State, Total_VMT),
          y = Total_VMT / 1000000000
        )
      ) +
        
        geom_col(
          fill = "steelblue"
        ) +
        
        coord_flip() +
        
        labs(
          title = "Total Vehicle Miles Traveled by State",
          x = "State",
          y = "Total VMT (Billions)"
        ) +
        
        theme_minimal() +
        
        theme(
          plot.title = element_text(
            size = 16,
            face = "bold"
          ),
          axis.text.y = element_text(size = 8)
        )
      
      
      # ================================================
      # GRAPH 3: RURAL VS. URBAN FATAL CRASH RATES
      # ================================================
      
    } else if (input$graph_choice == "rates") {
      
      req(input$selected_state)
      
      selected_rate_data <- Crash_rates %>%
        filter(State == input$selected_state)
      
      validate(
        need(
          nrow(selected_rate_data) == 2,
          "Rural and urban rate data are not available for this state."
        )
      )
      
      ggplot(
        selected_rate_data,
        aes(
          x = Area,
          y = Fatal_Crash_Rate,
          fill = Area
        )
      ) +
        
        geom_col(
          width = 0.6
        ) +
        
        geom_text(
          aes(
            label = round(Fatal_Crash_Rate, 3)
          ),
          vjust = -0.5,
          size = 5
        ) +
        
        labs(
          title = paste(
            "Fatal Crash Rates in",
            input$selected_state
          ),
          x = "Area",
          y = "Fatal Crashes per 100 Million VMT",
          fill = "Area"
        ) +
        
        expand_limits(
          y = max(selected_rate_data$Fatal_Crash_Rate) * 1.15
        ) +
        
        theme_minimal() +
        
        theme(
          plot.title = element_text(
            size = 16,
            face = "bold"
          ),
          axis.text = element_text(size = 12)
        )
      
    }
    
  })
  
}






















