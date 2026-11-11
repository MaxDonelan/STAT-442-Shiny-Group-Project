# UI for the seasons tab; all input/output IDs are scoped with seasons_.
nav_panel(
  title = "Seasons",
  icon = icon("calendar"),
  div(
    style = "width: 100%; padding: 1rem;",
    h2("Seasonal patterns in traffic fatalities"),
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      selectInput(
        "seasons_state", "State",
        choices = c("All states" = "all", sort(unique(accident$STATENAME))),
        width = "100%"
      ),
      radioButtons(
        "seasons_metric", "Compare",
        choices = c("Fatal crashes" = "crashes", "Fatalities" = "fatalities"),
        inline = TRUE
      )
    ),
    helpText("Explore how fatal crashes and fatalities vary throughout the year. Counts do not adjust for traffic volume or the number of days in each season."),
    textOutput("seasons_summary"),
    card(
      full_screen = TRUE,
      card_header("Seasonal comparison"),
      plotOutput("seasons_comparison", height = "440px"),
      fill = FALSE
    ),
    helpText("Winter: Dec-Feb; spring: Mar-May; summer: Jun-Aug; fall: Sep-Nov. Winter combines January, February, and December within each calendar year."),
    card(
      full_screen = TRUE,
      card_header("Monthly trend"),
      plotOutput("seasons_monthly", height = "440px"),
      fill = FALSE
    ),
    card(card_header("Season totals"), tableOutput("seasons_table"), fill = FALSE)
  ),
  value = "seasons"
)
