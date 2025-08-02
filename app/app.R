# Load packages
library(shiny)
library(bslib)
library(foodrescue)
library(dplyr)
library(ggplot2)
library(forcats)
library(lubridate)
library(purrr)
library(stringr)
library(tidyr)
library(knitr)
library(kableExtra)
library(utils)

# Data
data(raw_food)
data <- clean_food(raw_food)
tidy_data <- data[['tidy_data']]
clean_data <- data[['clean_data']]

# Dining halls
dining_halls <- unique(tidy_data$dining_hall)


# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "Food Rescue Network",
  sidebar = sidebar(
    radioButtons(
      inputId = "location",
      label = "Choose a location:",
      choices = c("All locations", dining_halls),
      selected = "All locations"
    ),
    dateRangeInput(
      inputId = "date_range",
      label = "Select date range:",
      start = min(tidy_data$date),
      end = max(tidy_data$date)
    )
  ),
  tabsetPanel(
    id = "tabs",
    tabPanel("Home",
             card(
               full_screen = FALSE,
               card_header("About This Tool"),
               card_body(
                 tags$p("Welcome! This website can be used to view survey data from the Food Rescue Network."),
                 tags$p("Select options on the left to view the data.")
               )),
             card(
               full_screen = FALSE,
               card_header("Quick Stats"),
               htmlOutput("summary_table")
             )
             ),
    tabPanel("Recovery distribution",
             card(
               full_screen = FALSE,
               plotOutput("plot")
             ),
             card(
               full_screen = FALSE,
               htmlOutput("type_table"))
             ),
    tabPanel("Over time"),
    tabPanel("Data")
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  dining_names <- reactive({
    if (input$location == "All locations") {
      dining_halls
    } else {
      input$location
    }
  })

  start_date <- reactive({
    format(input$date_range[1], "%m-%d-%Y")
  })

  end_date <- reactive({
    format(input$date_range[2], "%m-%d-%Y")
  })


  output$summary_table <- renderUI({
    table <- weekly_stats(clean_data, tidy_data, start_date(), end_date(), dining_names()) |>
      row_spec(0, background = "#48D1CC")
    HTML(as.character(table))
  })

  output$type_table <- renderUI({
    table <- recovery_tables(tidy_data, start_date(), end_date(), dining_names(), "type")
    HTML(as.character(table))
  })

  output$hall_table <- renderUI({
    table <- recovery_tables(tidy_data, start_date(), end_date(), dining_names(), "hall")
    HTML(as.character(table))
  })

  output$plot <- renderPlot({
    if (length(dining_names) == 1) {
      fill <- FALSE
    } else {
      fill <- TRUE
    }
    plot_recovery(tidy_data, start_date(), end_date(), dining_names(), fill = TRUE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
