# Seasonal summaries use accident-level records, not person-level records.
seasons_filtered <- reactive({
  req(input$seasons_state)
  result <- accident |>
    filter(MONTH %in% 1:12)
  if (input$seasons_state != "all") {
    result <- result |> filter(STATENAME == input$seasons_state)
  }
  result
})

seasons_months <- reactive({
  result <- seasons_filtered() |>
    group_by(MONTH) |>
    summarise(crashes = n(), fatalities = sum(FATALS, na.rm = TRUE), .groups = "drop")
  # Retain all twelve months, including months with no recorded crashes.
  data.frame(MONTH = 1:12) |>
    left_join(result, by = "MONTH") |>
    mutate(
      crashes = coalesce(crashes, 0L),
      fatalities = coalesce(fatalities, 0),
      season = factor(
        c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer",
          "Summer", "Summer", "Fall", "Fall", "Fall", "Winter"),
        levels = c("Winter", "Spring", "Summer", "Fall")
      )
    )
})

seasons_totals <- reactive({
  seasons_months() |>
    group_by(season) |>
    summarise(crashes = sum(crashes), fatalities = sum(fatalities), .groups = "drop")
})

seasons_label <- reactive({
  req(input$seasons_metric)
  if (input$seasons_metric == "crashes") "Fatal crashes" else "Fatalities"
})

# Transparent backgrounds inherit the card color; labels follow the toggle.
seasons_plot_theme <- reactive({
  dark <- is.null(input$app_dark_mode) || identical(input$app_dark_mode, "dark")
  foreground <- if (dark) "#DEE2E6" else "#212529"
  theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      text = element_text(colour = foreground),
      axis.text = element_text(colour = foreground),
      axis.text.x = element_text(size = 18),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.line.y = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA)
    )
})

output$seasons_summary <- renderText({
  result <- seasons_filtered()
  paste0(
    paste(sort(unique(accident$YEAR)), collapse = ", "), " | ",
    if (input$seasons_state == "all") "All states" else input$seasons_state,
    ": ", format(nrow(result), big.mark = ","), " fatal crashes and ",
    format(sum(result$FATALS, na.rm = TRUE), big.mark = ","), " fatalities.",
    if (nrow(result) == 0) " No records match these filters." else ""
  )
})

output$seasons_comparison <- renderPlot({
  validate(need(nrow(seasons_filtered()) > 0, "No records match these filters."))
  ggplot(seasons_totals(), aes(x = season, y = .data[[input$seasons_metric]], fill = season)) +
    geom_col(width = 0.65) +
    geom_text(
      aes(
        y = .data[[input$seasons_metric]] / 2,
        label = format(.data[[input$seasons_metric]], big.mark = ",", trim = TRUE, scientific = FALSE)
      ),
      colour = "white", fontface = "bold", size = 8
    ) +
    scale_fill_manual(values = c(Winter = "#35658B", Spring = "#327348", Summer = "#946514", Fall = "#974A29")) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE), expand = expansion(mult = c(0, 0.1))) +
    labs(x = NULL, y = NULL, subtitle = seasons_label()) +
    seasons_plot_theme() +
    theme(legend.position = "none", panel.grid.major.x = element_blank())
}, bg = "transparent")

output$seasons_monthly <- renderPlot({
  validate(need(nrow(seasons_filtered()) > 0, "No records match these filters."))
  ggplot(seasons_months(), aes(x = MONTH, y = .data[[input$seasons_metric]])) +
    geom_line(color = "#5685AB", linewidth = 1) +
    geom_point(color = "#5685AB", size = 3) +
    geom_text(
      aes(label = format(.data[[input$seasons_metric]], big.mark = ",", trim = TRUE, scientific = FALSE)),
      vjust = -1,
      colour = if (identical(input$app_dark_mode, "light")) "#212529" else "#DEE2E6",
      size = 5, fontface = "bold"
    ) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
    labs(x = NULL, y = NULL, subtitle = seasons_label()) +
    seasons_plot_theme()
}, bg = "transparent")

output$seasons_table <- renderTable({
  seasons_totals() |>
    rename(Season = season, `Fatal crashes` = crashes, Fatalities = fatalities)
}, striped = TRUE, bordered = FALSE, spacing = "m")
