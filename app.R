# ------------------------------------------------------------------------------

# STAT 442 - Exploratory Data Analysis
# Shiny app on the NHTSA accidents dataset
# By: Max Donelan

# Published online at: https://01a17020-b5ec-cf05-cc88-391ee4a1b5e0.share.connect.posit.cloud/

# ------------------------------------------------------------------------------

library(shiny)
library(bslib)
library(readr)
library(ggplot2)
library(htmltools)
library(dplyr)
library(paletteer)

# data -------------------------------------------------------------------------

person_cleaned <- read_csv("data/person_cleaned.csv") |>
  mutate(
    Fatal = factor(Fatal, levels = c("Dead", "Alive", "Unknown/Other"))
  )
filter_col <- c(
  "Rurality", "Sex", "Alcohol", "Fatal", "Role"
)
hist_col <- c(
  "Age", "Hour"
)

# ui ---------------------------------------------------------------------------

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "simplex"),
  title = HTML("<h1><b>Traffic Fatalities</h1></b>"),
  fillable = FALSE,
  
  # sidebar --------------------------------------------------------------------
  
  sidebar = sidebar(
    selectInput(
      inputId = "select_col",
      label = h3("Select a Column"),
      choices = colnames(person_cleaned), 
      selected = "Alcohol"
    ),
    uiOutput("group_setting"),
    card(
      card_header("Settings"),
      input_switch("toggle_grouped", "Grouped", value=TRUE),
      input_switch("fatalities_switch", "Only Fatalities"),
      input_dark_mode(mode = "light")
    ),
    card(
      accordion_panel(value = "info",
                      card_title("Click to see info"),
                      htmlOutput("information")
      )
    )
  ),
  
  # body -----------------------------------------------------------------------
  
  card(
    card_header(htmlOutput("name")),
    plotOutput(outputId = "plt", height = "auto"),
    height = "auto"
  ),
  card(
    card_header(HTML("<h2>More Dangerous to be Male?</h2>")),
    h3(
      "Ratio of fatality proportions between males and females",
      style = "text-align: center;"
    ),
    tags$style("#hazard_plt { display: block; margin: 0 auto; }"),
    plotOutput("hazard_plt", width = 1000),
    div(
      HTML("<i>In other words, given you're in a traffic accident with a least one fatality, are your odds of being alive better as a male or female?</i>"),
      style = "text-align: center; bottom-margin: 12px"
    ),
    hr(),
    div(
      input_switch("driver", HTML("<b>Exclude Non-Drivers</b>")),
      p("Males are most likely to be drivers, pedestrians, and bicyclists, which more dangerous than being a passenger."),
      input_switch("alcohol", HTML("<b>Only Alcohol-Unrelated</b>")),
      p("Males are more likely to be in alcohol-related fatal accidents."),
      input_switch("seatbelt", HTML("<b>Only Proper Seatbelt Use</b>")),
      p('Males are more likely to be in the "None Used/Not Applicable" and "Not a Motor Vehicle Occupant" restraint use categories.'), 
      style = "display: grid; place-items: center;")
  )
)

# server -----------------------------------------------------------------------

server <- function(input, output) {
  
  # reactives ------------------------------------------------------------------
  
  person_data <- reactive({
    if (input$fatalities_switch) {
      person_cleaned |> filter(Fatal == "Dead")
    }
    else {
      person_cleaned
    }
  })
  
  
  hazard_data <- reactive({
    hazard <- person_cleaned
    if (input$driver) {
      hazard <- hazard |> filter(Seat == "Front Seat, Left Side")
    }
    if (input$alcohol) {
      hazard <- hazard |> filter(Alcohol == "No (Alcohol Not Involved)")
    }
    if (input$seatbelt) {
      hazard <- hazard |> filter(Restraint == "Shoulder and Lap Belt Used")
    }
    hazard_count <- hazard |>
      group_by(Sex) |>
      count()
    hazard <-  hazard |>
      group_by(Sex) |>
      summarise(Fatal = sum(Fatal == "Dead"))
    hazard_df <- data.frame(
      M_Fatal_Prop = hazard$Fatal[hazard$Sex == "Male"] / hazard_count$n[hazard_count$Sex == "Male"],
      F_Fatal_Prop = hazard$Fatal[hazard$Sex == "Female"] / hazard_count$n[hazard_count$Sex == "Female"],
      HR = hazard$Fatal[hazard$Sex == "Male"] / hazard$Fatal[hazard$Sex == "Female"]
    ) |>
      mutate(
        HR = M_Fatal_Prop / F_Fatal_Prop
      )
    hazard_df
  })
  
  
  plt_height <- reactive({
    if (input$select_col %in% hist_col) {
      400
    } else {
      n <- length(unique(person_cleaned[[input$select_col]]))
      max(250, n * 20 + 80)
    }
  })
  
  # renders --------------------------------------------------------------------
  
  output$information <- renderText({
    HTML(paste0(
      "
      I used a selection of columns from the person.csv file, which contains information at an individual-level.
      For cleaning, I removed rows with unknown Sex, Age over 100, rurality that was not Rural or Urban, and
      unknown hour of accident. Data is from the 2024 NHTSA fatal accidents dataset.
      "
    ))
  })
  
  
  output$group_setting <- renderUI({
    if (input$toggle_grouped) {
      selectInput(
        "select_group",
        label = h3("Group By"),
        choices = filter_col,
        selected = "Fatal"
      )
    } else {
      NULL
    }
  })

  
  output$name <- renderText({
    if (input$toggle_grouped) {
      req(input$select_group)
      HTML(paste0("<h2>Persons involved in fatal accidents by <b>", input$select_col, "</b> and <b>", input$select_group, "</b></h2>"))
    } else {
      HTML(paste0("<h2>Persons involved in fatal accidents by <b>", input$select_col, "</b></h2>"))
    }
  })
  
  
  output$plt <- renderPlot({
    if (input$select_col == "Hour") {
      n_bins = 26
    }
    else {
      n_bins = 100
    }
    
    plt <- person_data() |>
      ggplot(aes(x = .data[[input$select_col]]))
    
    # grouped
    if (input$toggle_grouped) {
      req(input$select_group)
      # histogram
      if (input$select_col %in% hist_col) {
        plt <- plt +
          geom_histogram(aes(fill = .data[[input$select_group]]),
                         binwidth = 1,
                         alpha = 0.75,
                         position = "identity")
      }
      # not histogram
      else {
        plt <- plt + geom_bar(aes(fill = .data[[input$select_group]]), position = "dodge") +
          coord_flip()
      }
    }
    # not grouped
    else {
      # histogram
      if (input$select_col %in% hist_col) {
        plt <- plt + geom_histogram(binwidth = 1, fill = "#BF616AFF", alpha = 0.75)
      }
      # not histogram
      else {
        plt <- plt + geom_bar(fill = paletteer_d("nord::aurora", 1)) +
          coord_flip()
      }
    }
    plt + 
      theme_classic() +
      labs(y = "", x = "") +
      scale_fill_paletteer_d("nord::aurora") +
      theme(
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18)
      )
  }, height = plt_height)
  
  
  output$hazard_plt <- renderPlot({
    ggplot(hazard_data()) + 
      geom_segment( aes(x=1, xend=HR, y=0, yend=0), color="#BF616AFF", linewidth = 32) +
      geom_text(aes(x = HR+ifelse(HR > 1, 0.15, -0.15), y = 0, label = round(HR, 4)), size = 12, color = "#BF616AFF") +
      annotate("text", x = 0.5, y = 0.25, label = "Higher Female Fatality", size = 8) +
      annotate("text", x = 1.5, y = 0.25, label = "Higher Male Fatality", size = 8) +
      geom_vline(xintercept = 1, linewidth = 1) +
      lims(x = c(0, 2), y = c(-0.25, 0.25)) + 
      labs(x = "") +
      theme_minimal() + 
      theme(
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x.bottom = element_text(size = 25)
      )
  }, width = 1000)
}

shinyApp(ui = ui, server = server)


