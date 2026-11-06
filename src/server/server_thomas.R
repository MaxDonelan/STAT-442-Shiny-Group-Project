
# ============================================================
# Server: U.S. Traffic Fatalities
# Author: Thomas Shields
# ============================================================

# Load person-level data
persons <- readRDS("data/persons_thomas.rds")

# ============================================================
# Prepare fatality-level data
# ============================================================

fatality_data <- persons |>
  dplyr::filter(INJ_SEVNAME == "Fatal Injury (K)") |>
  dplyr::mutate(
    SEX_LABEL = dplyr::case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      TRUE ~ "Unknown / Other"
    ),
    
    ROAD_TYPE = dplyr::case_when(
      FUNC_SYS == 1 ~ "Interstate",
      FUNC_SYS == 2 ~ "Other Freeway / Expressway",
      FUNC_SYS == 3 ~ "Other Principal Arterial",
      FUNC_SYS == 4 ~ "Minor Arterial",
      FUNC_SYS == 5 ~ "Major Collector",
      FUNC_SYS == 6 ~ "Minor Collector",
      FUNC_SYS == 7 ~ "Local Road",
      TRUE ~ "Unknown / Other"
    ),
    
    AREA_TYPE = dplyr::case_when(
      RUR_URB == 1 ~ "Rural",
      RUR_URB == 2 ~ "Urban",
      TRUE ~ "Unknown / Other"
    )
  )

# ============================================================
# Filter by year
# ============================================================

selected_fatalities <- reactive({
  
  data <- fatality_data
  
  if (input$thomas_year != "both") {
    data <- data |>
      dplyr::filter(YEAR == as.integer(input$thomas_year))
  }
  
  data
})

# ============================================================
# All fatalities vs. drivers only
# ============================================================

all_fatalities <- reactive({
  selected_fatalities()
})

driver_fatalities <- reactive({
  selected_fatalities() |>
    dplyr::filter(PER_TYP == 1)
})

# Select population based on checkbox
plot_fatalities <- reactive({
  
  if (isTRUE(input$thomas_compare)) {
    driver_fatalities()
  } else {
    all_fatalities()
  }
  
})

# ============================================================
# Dynamic light/dark chart theme
# ============================================================

get_chart_theme <- function(mode) {
  
  dark_mode <- identical(mode, "dark")
  
  if (dark_mode) {
    
    theme_minimal(base_size = 18) +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 24,
          color = "#F8FAFC"
        ),
        
        plot.subtitle = element_text(
          size = 18,
          color = "#CBD5E1"
        ),
        
        axis.title = element_text(
          face = "bold",
          size = 20,
          color = "#E2E8F0"
        ),
        
        axis.text = element_text(
          size = 17,
          color = "#E2E8F0"
        ),
        
        panel.grid.minor = element_blank(),
        
        panel.grid.major.y = element_blank(),
        
        panel.grid.major.x = element_line(
          color = "#475569"
        ),
        
        legend.position = "bottom",
        
        legend.title = element_blank(),
        
        legend.text = element_text(
          size = 18,
          color = "#E2E8F0"
        ),
        
        plot.background = element_rect(
          fill = "#212529",
          color = NA
        ),
        
        panel.background = element_rect(
          fill = "#212529",
          color = NA
        ),
        
        plot.margin = margin(20, 25, 20, 20)
      )
    
  } else {
    
    theme_minimal(base_size = 18) +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 24,
          color = "#243447"
        ),
        
        plot.subtitle = element_text(
          size = 18,
          color = "#68778A"
        ),
        
        axis.title = element_text(
          face = "bold",
          size = 20,
          color = "#435266"
        ),
        
        axis.text = element_text(
          size = 17,
          color = "#435266"
        ),
        
        panel.grid.minor = element_blank(),
        
        panel.grid.major.y = element_blank(),
        
        panel.grid.major.x = element_line(
          color = "#E8EDF2"
        ),
        
        legend.position = "bottom",
        
        legend.title = element_blank(),
        
        legend.text = element_text(
          size = 18,
          color = "#435266"
        ),
        
        plot.background = element_rect(
          fill = "#FFFFFF",
          color = NA
        ),
        
        panel.background = element_rect(
          fill = "#FFFFFF",
          color = NA
        ),
        
        plot.margin = margin(20, 25, 20, 20)
      )
  }
}

# ============================================================
# Chart colors
# ============================================================

chart_colors <- c(
  "Male" = "#3977A8",
  "Female" = "#C76D7E"
)

# ============================================================
# Summary functions
# ============================================================

summarise_sex <- function(data) {
  
  data |>
    dplyr::filter(SEX_LABEL %in% c("Male", "Female")) |>
    dplyr::count(SEX_LABEL) |>
    dplyr::mutate(
      PERCENTAGE = n / sum(n) * 100
    )
  
}

summarise_road <- function(data) {
  
  data |>
    dplyr::filter(SEX_LABEL %in% c("Male", "Female")) |>
    dplyr::count(ROAD_TYPE, SEX_LABEL) |>
    dplyr::group_by(ROAD_TYPE) |>
    dplyr::mutate(
      TOTAL = sum(n),
      PERCENTAGE = n / TOTAL * 100
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ROAD_TYPE = factor(
        ROAD_TYPE,
        levels = c(
          "Interstate",
          "Other Freeway / Expressway",
          "Minor Arterial",
          "Other Principal Arterial",
          "Major Collector",
          "Minor Collector",
          "Local Road",
          "Unknown / Other"
        )
      )
    )
  
}

summarise_area <- function(data) {
  
  data |>
    dplyr::filter(SEX_LABEL %in% c("Male", "Female")) |>
    dplyr::count(AREA_TYPE, SEX_LABEL) |>
    dplyr::group_by(AREA_TYPE) |>
    dplyr::mutate(
      TOTAL = sum(n),
      PERCENTAGE = n / TOTAL * 100
    ) |>
    dplyr::ungroup()
  
}

# ============================================================
# Plot functions
# ============================================================

make_sex_plot <- function(data) {
  
  plot_data <- summarise_sex(data)
  
  validate(
    need(nrow(plot_data) > 0, "No records available.")
  )
  
  ggplot(
    plot_data,
    aes(
      x = "Fatalities",
      y = PERCENTAGE,
      fill = SEX_LABEL
    )
  ) +
    geom_col(width = 0.55) +
    
    geom_text(
      aes(label = paste0(round(PERCENTAGE, 1), "%")),
      position = position_stack(vjust = 0.5),
      color = "white",
      fontface = "bold",
      size = 6
    ) +
    
    scale_fill_manual(values = chart_colors) +
    
    scale_y_continuous(
      breaks = NULL,
      labels = NULL
    ) +
    
    coord_cartesian(ylim = c(0, 100)) +
    
    labs(
      x = NULL,
      y = NULL
    ) +
    
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
}

make_road_plot <- function(data) {
  plot_data <- summarise_road(data)
  
  validate(need(nrow(plot_data) > 0, "No records available."))
  
  ggplot(
    plot_data,
    aes(x = ROAD_TYPE, y = PERCENTAGE, fill = SEX_LABEL)
  ) +
    geom_col(width = 0.7) +
    geom_text(
      aes(label = paste0(round(PERCENTAGE, 1), "%")),
      position = position_stack(vjust = 0.5),
      color = "white",
      fontface = "bold",
      size = 5
    ) +
    scale_x_discrete(
      labels = c(
        "Other Freeway / Expressway" = "Other Freeway /\nExpressway",
        "Other Principal Arterial" = "Other Principal\nArterial"
      )
    ) +
    scale_y_continuous(breaks = NULL, labels = NULL) +
    scale_fill_manual(values = chart_colors) +
    labs(x = NULL, y = NULL)
}

make_area_plot <- function(data) {
  
  plot_data <- summarise_area(data)
  
  validate(
    need(nrow(plot_data) > 0, "No records available.")
  )
  
  ggplot(
    plot_data,
    aes(
      x = AREA_TYPE,
      y = PERCENTAGE,
      fill = SEX_LABEL
    )
  ) +
    geom_col(width = 0.6) +
    
    geom_text(
      aes(label = paste0(round(PERCENTAGE, 1), "%")),
      position = position_stack(vjust = 0.5),
      color = "white",
      fontface = "bold",
      size = 5.5
    ) +
    
    scale_fill_manual(values = chart_colors) +
    
    scale_y_continuous(
      breaks = NULL,
      labels = NULL
    ) +
    
    coord_cartesian(ylim = c(0, 100)) +
    
    labs(
      x = NULL,
      y = NULL
    ) +
    
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(size = 17),
      axis.ticks = element_blank()
    )
}

# ============================================================
# Render plots
# ============================================================

output$thomas_sex_all <- renderPlot({
  
  make_sex_plot(plot_fatalities()) +
    get_chart_theme(input$ide_mode)
  
})

output$thomas_road_all <- renderPlot({
  
  make_road_plot(plot_fatalities()) +
    get_chart_theme(input$ide_mode)
  
})

output$thomas_area_all <- renderPlot({
  
  make_area_plot(plot_fatalities()) +
    get_chart_theme(input$ide_mode)
  
})
