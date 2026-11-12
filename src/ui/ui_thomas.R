
# ============================================================
# UI: How Men and Women Die on U.S. Roads
# Author: Thomas Shields
# ============================================================

nav_panel(
  title = "Men vs. Women",
  icon = icon("venus-mars"),

  layout_sidebar(

    sidebar = sidebar(
      title = "Filters",

      selectInput(
        inputId = "thomas_year",
        label = "Year",
        choices = c(
          "Both Years (Combined)" = "both",
          "2023" = "2023",
          "2024" = "2024"
        ),
        selected = "both"
      ),

      hr(),

      checkboxInput(
        inputId = "thomas_drivers_only",
        label = "Show drivers only",
        value = FALSE
      ),

      p(
        class = "small text-muted",
        "Applies to the summary boxes, seat belt, age, road, and area
        charts. The \"who died\" chart always shows everyone, and the
        alcohol chart is always drivers only."
      ),

      hr(),

      p(
        class = "small text-muted",
        "Fatal injuries from the NHTSA FARS person file.
        Records with unknown sex are excluded."
      )
    ),

    div(
      class = "container-fluid",

      h2("How Men and Women Die on U.S. Roads"),

      p(
        "Men account for most traffic deaths, but the gap isn't the
        same everywhere. Where they were, whether they were buckled,
        and whether they'd been drinking all differ by sex."
      ),

      # ------------------------------------------------------
      # Summary value boxes
      # ------------------------------------------------------

      layout_column_wrap(
        width = "220px",
        fill = FALSE,

        value_box(
          title = "Total deaths",
          value = textOutput("thomas_vb_total"),
          showcase = bs_icon("people-fill"),
          p(textOutput("thomas_vb_total_note", inline = TRUE))
        ),

        value_box(
          title = "Men",
          value = textOutput("thomas_vb_male"),
          showcase = bs_icon("gender-male"),
          theme = value_box_theme(bg = "#3977A8", fg = "#FFFFFF"),
          p(textOutput("thomas_vb_male_pct", inline = TRUE))
        ),

        value_box(
          title = "Women",
          value = textOutput("thomas_vb_female"),
          showcase = bs_icon("gender-female"),
          theme = value_box_theme(bg = "#C76D7E", fg = "#FFFFFF"),
          p(textOutput("thomas_vb_female_pct", inline = TRUE))
        ),

        value_box(
          title = "Median age (men / women)",
          value = textOutput("thomas_vb_age"),
          showcase = bs_icon("hourglass-split"),
          p("Age at time of crash")
        )
      ),

      br(),

      # ------------------------------------------------------
      # Main charts (2 x 2)
      # ------------------------------------------------------

      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(6, 6)),

        card(
          full_screen = TRUE,
          card_header("Who died: drivers, passengers, and people outside the car"),
          card_body(
            p(
              class = "small text-muted",
              "Number of deaths by role. Passengers are the one group
              where women outnumber men."
            ),
            plotOutput("thomas_person_type", height = "420px")
          )
        ),

        card(
          full_screen = TRUE,
          card_header("Seat belt use among vehicle occupants who died"),
          card_body(
            p(
              class = "small text-muted",
              "Share of people killed inside a vehicle who were restrained.
              Motorcycle riders are excluded since they have no seat belts."
            ),
            plotOutput("thomas_belt", height = "420px")
          )
        ),

        card(
          full_screen = TRUE,
          card_header("Blood alcohol of drivers who died"),
          card_body(
            p(
              class = "small text-muted",
              "Share of tested drivers in each BAC range.",
              textOutput("thomas_bac_note", inline = TRUE)
            ),
            plotOutput("thomas_bac", height = "420px")
          )
        ),

        card(
          full_screen = TRUE,
          card_header("Age at death"),
          card_body(
            p(
              class = "small text-muted",
              "Distribution of ages for each sex. Dashed lines mark the median."
            ),
            plotOutput("thomas_age", height = "420px")
          )
        )
      ),

      # ------------------------------------------------------
      # Road type and area
      # ------------------------------------------------------

      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(7, 5)),

        card(
          full_screen = TRUE,
          card_header("Male / female split by road type"),
          card_body(
            plotOutput("thomas_road", height = "420px")
          )
        ),

        card(
          full_screen = TRUE,
          card_header("Male / female split: rural vs. urban"),
          card_body(
            plotOutput("thomas_area", height = "420px")
          )
        )
      )
    )
  )
)
