
# ============================================================
# Server: How Men and Women Die on U.S. Roads
# Author: Thomas Shields
#
# Data: thomas_fatalities (loaded in app.R from
#       data/fatalities_thomas.rds, built by
#       src/unsourced/fatalities_cleaning_thomas.R)
# ============================================================

# ============================================================
# Filtered data
# ============================================================

# Year filter only (used by the "who died" and alcohol charts)
thomas_year_data <- reactive({
  data <- thomas_fatalities

  if (input$thomas_year != "both") {
    data <- data |>
      dplyr::filter(YEAR == as.integer(input$thomas_year))
  }

  data
})

# Year filter + optional drivers-only filter
thomas_plot_data <- reactive({
  data <- thomas_year_data()

  if (isTRUE(input$thomas_drivers_only)) {
    data <- data |>
      dplyr::filter(IS_DRIVER)
  }

  data
})

# ============================================================
# Colors and helpers
# ============================================================

thomas_chart_colors <- c(
  "Male"   = "#3977A8",
  "Female" = "#C76D7E"
)

thomas_belt_colors <- c(
  "Restrained"     = "#3A9E6F",
  "Not restrained" = "#D1495B",
  "Unknown"        = "#9AA5B1"
)

thomas_fmt_n <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

thomas_fmt_pct <- function(x, digits = 1) {
  paste0(round(x, digits), "%")
}

# ============================================================
# Light / dark chart theme
# ============================================================

thomas_get_chart_theme <- function(mode) {

  dark_mode <- identical(mode, "dark")

  title_col <- if (dark_mode) "#F8FAFC" else "#243447"
  text_col  <- if (dark_mode) "#E2E8F0" else "#435266"
  grid_col  <- if (dark_mode) "#3B4554" else "#E8EDF2"

  theme_minimal(base_size = 16) +
    theme(
      plot.title    = element_text(face = "bold", size = 20, color = title_col),
      plot.subtitle = element_text(size = 15, color = text_col),
      plot.caption  = element_text(size = 12, color = text_col),
      axis.title    = element_text(face = "bold", size = 15, color = text_col),
      axis.text     = element_text(size = 14, color = text_col),
      legend.position = "bottom",
      legend.title  = element_blank(),
      legend.text   = element_text(size = 14, color = text_col),
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(color = grid_col),
      # transparent so the plot blends into the card in both modes
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(10, 15, 10, 10)
    )
}

thomas_label_color <- function(mode) {
  if (identical(mode, "dark")) "#F8FAFC" else "#243447"
}

# ============================================================
# Value boxes
# ============================================================

thomas_sex_counts <- reactive({
  counts <- table(thomas_plot_data()$SEX_LABEL)
  list(
    male   = as.integer(counts[["Male"]]),
    female = as.integer(counts[["Female"]]),
    total  = sum(counts)
  )
})

output$thomas_vb_total <- renderText({
  thomas_fmt_n(thomas_sex_counts()$total)
})

output$thomas_vb_total_note <- renderText({
  who  <- if (isTRUE(input$thomas_drivers_only)) "Drivers" else "All people"
  year <- if (input$thomas_year == "both") "2023-2024" else input$thomas_year
  paste0(who, ", ", year)
})

output$thomas_vb_male <- renderText({
  thomas_fmt_n(thomas_sex_counts()$male)
})

output$thomas_vb_male_pct <- renderText({
  s <- thomas_sex_counts()
  paste(thomas_fmt_pct(100 * s$male / s$total), "of deaths")
})

output$thomas_vb_female <- renderText({
  thomas_fmt_n(thomas_sex_counts()$female)
})

output$thomas_vb_female_pct <- renderText({
  s <- thomas_sex_counts()
  paste(thomas_fmt_pct(100 * s$female / s$total), "of deaths")
})

output$thomas_vb_age <- renderText({
  data <- thomas_plot_data()
  med_m <- median(data$AGE[data$SEX_LABEL == "Male"], na.rm = TRUE)
  med_f <- median(data$AGE[data$SEX_LABEL == "Female"], na.rm = TRUE)
  paste(med_m, "/", med_f)
})

# ============================================================
# 1. Who died (person type) - counts by sex
# ============================================================

output$thomas_person_type <- renderPlot({

  plot_data <- thomas_year_data() |>
    dplyr::count(PERSON_TYPE, SEX_LABEL)

  validate(need(nrow(plot_data) > 0, "No records available."))

  ggplot(plot_data, aes(x = PERSON_TYPE, y = n, fill = SEX_LABEL)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    geom_text(
      aes(label = thomas_fmt_n(n)),
      position = position_dodge(width = 0.8),
      vjust = -0.4,
      size = 4.2,
      fontface = "bold",
      color = thomas_label_color(input$ide_mode)
    ) +
    scale_fill_manual(values = thomas_chart_colors) +
    scale_y_continuous(
      labels = thomas_fmt_n,
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(x = NULL, y = "Deaths") +
    thomas_get_chart_theme(input$ide_mode) +
    theme(panel.grid.major.x = element_blank())

}, bg = "transparent")

# ============================================================
# 2. Seat belt use - 100% stacked bar by sex
# ============================================================

output$thomas_belt <- renderPlot({

  plot_data <- thomas_plot_data() |>
    dplyr::filter(!is.na(BELT_STATUS)) |>
    dplyr::count(SEX_LABEL, BELT_STATUS) |>
    dplyr::group_by(SEX_LABEL) |>
    dplyr::mutate(PERCENTAGE = n / sum(n) * 100) |>
    dplyr::ungroup() |>
    # reverse so "Restrained" sits on the left
    dplyr::mutate(BELT_STATUS = forcats::fct_rev(BELT_STATUS))

  validate(need(nrow(plot_data) > 0, "No records available."))

  ggplot(plot_data, aes(x = PERCENTAGE, y = SEX_LABEL, fill = BELT_STATUS)) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = ifelse(PERCENTAGE >= 6, thomas_fmt_pct(PERCENTAGE, 0), "")),
      position = position_stack(vjust = 0.5),
      color = "white",
      fontface = "bold",
      size = 5.5
    ) +
    scale_fill_manual(
      values = thomas_belt_colors,
      breaks = names(thomas_belt_colors)
    ) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_y_discrete(limits = rev) +
    labs(x = "Share of occupant deaths", y = NULL) +
    thomas_get_chart_theme(input$ide_mode) +
    theme(panel.grid.major.y = element_blank())

}, bg = "transparent")

# ============================================================
# 3. Blood alcohol (drivers only) - % of tested drivers
# ============================================================

thomas_bac_data <- reactive({
  thomas_year_data() |>
    dplyr::filter(IS_DRIVER, !is.na(BAC_GROUP))
})

output$thomas_bac_note <- renderText({
  data <- thomas_bac_data()
  tested <- mean(data$BAC_GROUP != "Not tested / unknown") * 100
  paste0(
    "Only ", thomas_fmt_pct(tested, 0),
    " of drivers who died had a reported test result."
  )
})

output$thomas_bac <- renderPlot({

  plot_data <- thomas_bac_data() |>
    dplyr::filter(BAC_GROUP != "Not tested / unknown") |>
    droplevels() |>
    dplyr::count(SEX_LABEL, BAC_GROUP) |>
    dplyr::group_by(SEX_LABEL) |>
    dplyr::mutate(PERCENTAGE = n / sum(n) * 100) |>
    dplyr::ungroup()

  validate(need(nrow(plot_data) > 0, "No records available."))

  ggplot(plot_data, aes(x = BAC_GROUP, y = PERCENTAGE, fill = SEX_LABEL)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    geom_text(
      aes(label = thomas_fmt_pct(PERCENTAGE, 0)),
      position = position_dodge(width = 0.8),
      vjust = -0.4,
      size = 4.8,
      fontface = "bold",
      color = thomas_label_color(input$ide_mode)
    ) +
    scale_fill_manual(values = thomas_chart_colors) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(x = "Blood alcohol concentration (g/dL)", y = "Share of tested drivers") +
    thomas_get_chart_theme(input$ide_mode) +
    theme(panel.grid.major.x = element_blank())

}, bg = "transparent")

# ============================================================
# 4. Age at death - density by sex with median lines
# ============================================================

output$thomas_age <- renderPlot({

  plot_data <- thomas_plot_data() |>
    dplyr::filter(!is.na(AGE))

  validate(need(nrow(plot_data) > 0, "No records available."))

  medians <- plot_data |>
    dplyr::group_by(SEX_LABEL) |>
    dplyr::summarise(MEDIAN = median(AGE), .groups = "drop")

  ggplot(plot_data, aes(x = AGE, fill = SEX_LABEL, color = SEX_LABEL)) +
    geom_density(alpha = 0.35, linewidth = 1, adjust = 0.8) +
    geom_vline(
      data = medians,
      aes(xintercept = MEDIAN, color = SEX_LABEL),
      linetype = "dashed",
      linewidth = 1,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = thomas_chart_colors) +
    scale_color_manual(values = thomas_chart_colors) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_y_continuous(breaks = NULL) +
    coord_cartesian(xlim = c(0, 100)) +
    labs(x = "Age", y = NULL) +
    thomas_get_chart_theme(input$ide_mode)

}, bg = "transparent")

# ============================================================
# 5. Road type and rural / urban - 100% stacked bars
# ============================================================

thomas_split_plot <- function(data, group_var, mode) {

  plot_data <- data |>
    dplyr::filter(!is.na({{ group_var }})) |>
    dplyr::count({{ group_var }}, SEX_LABEL) |>
    dplyr::group_by({{ group_var }}) |>
    dplyr::mutate(PERCENTAGE = n / sum(n) * 100) |>
    dplyr::ungroup() |>
    # Female on the right so Male bars line up on the left edge
    dplyr::mutate(SEX_LABEL = forcats::fct_rev(SEX_LABEL))

  validate(need(nrow(plot_data) > 0, "No records available."))

  ggplot(plot_data, aes(x = PERCENTAGE, y = {{ group_var }}, fill = SEX_LABEL)) +
    geom_col(width = 0.7) +
    geom_text(
      aes(label = thomas_fmt_pct(PERCENTAGE, 0)),
      position = position_stack(vjust = 0.5),
      color = "white",
      fontface = "bold",
      size = 4.8
    ) +
    scale_fill_manual(values = thomas_chart_colors, breaks = c("Male", "Female")) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_y_discrete(limits = rev) +
    labs(x = "Share of deaths", y = NULL) +
    thomas_get_chart_theme(mode) +
    theme(panel.grid.major.y = element_blank())
}

output$thomas_road <- renderPlot({
  thomas_split_plot(thomas_plot_data(), ROAD_TYPE, input$ide_mode)
}, bg = "transparent")

output$thomas_area <- renderPlot({
  thomas_split_plot(thomas_plot_data(), AREA_TYPE, input$ide_mode)
}, bg = "transparent")
