# Load packages
library(shiny)
library(bslib)
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
library(plotly)
library(foodrescue)

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
      label = "Select time range:",
      start = min(tidy_data$date),
      end = max(tidy_data$date)
    )
  ),
  tabsetPanel(
    id = "tabs",
    tabPanel("Home",
             card(
               full_screen = FALSE,
               card_header("About"),
               card_body(
                 tags$p("Welcome! This interactive website can be used to view survey data from the Food Rescue Network.
                        We provide broad overviews as well as more detailed information on recovery distributions by food type, dining hall, and time scale.
                        We hope this tool will be used by stakeholders at Smith such as dining staff, as well as anyone interested in the FRN.")
               )),

             card(
               full_screen = FALSE,
               card_header("Data and acknowledgements"),
               card_body(tags$p("Our data comes from our lovely FRN volunteers. At the end of every shift, volunteers fill out a Qualtrics survey and input their shift information.
                         In the Spring 2025 we phased out having volunteers subtract 2lbs per container; we now perform this calculation on the back end."),
                         tags$p("This app uses plotting functions in the",
                                tags$a(href = "https://github.com/ZoeKhan1/foodrescue", "foodrescue", target = "_self"),
                                " package, authored by Zoe Khan '27.
                                The functions in this package build off of the Fall 2024 FRN report by Zoe and Anabel Fletcher '26."))
             ),
             card(
               full_screen = FALSE,
               card_header("Feedback"),
               card_body(
                 tags$p("Have a question or found a bug? Please ", HTML(' <a href = "https://github.com/ZoeKhan1/shinyfrn/issues" target = "_self" >report here</a>.'))
               )
             )

             ),
    tabPanel("Overview",
             card(
               full_screen = FALSE,
               card_header("Quick Stats"),
               htmlOutput("summary_table")
             ),
             card(
               full_screen = FALSE,
               card_header("Bird's Eye View"),
               plotlyOutput("birds_eye")
             ),
             card(
               full_screen = FALSE,
               card_header("Top 25 Recovered Foods"),
               htmlOutput("top_table")
             )),
    tabPanel("Recovery distribution",
             card(
               full_screen = FALSE,
               plotOutput("plot")
             ),
             fluidRow(uiOutput("summary_ui"))),
    tabPanel("Over time"),
    tabPanel("Data",
             tableOutput("tidydata"),
             downloadButton("downloaddata", "Download tidy survey data"),
             tableOutput("cleandata"),
             downloadButton("downloaddata2", "Download wide survey data"))
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

  output$summary_ui <- renderUI({
    if (input$location == "All locations") {
      fluidRow(
        column(width = 6, card(htmlOutput("type_table"))),
        column(width = 6, card(htmlOutput("hall_table"))))
    } else {
      fluidRow(
        column(width = 12, card(htmlOutput("type_table"))))
    }
  })

  output$summary_table <- renderUI({
    table <- weekly_stats(clean_data, tidy_data, start_date(), end_date(), dining_names()) |>
      row_spec(0, background = "#48D1CC")
    HTML(as.character(table))
  })

  output$birds_eye <- renderPlotly({
    birds_eye(tidy_data, start_date(), end_date(), dining_names())
  })

  output$top_table <- renderUI({
    table <- recovery_tables(tidy_data, start_date(), end_date(), dining_halls, "top")
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

  data_subset <- reactive({
    tidy_data |>
      filter(dining_hall %in% dining_names()) |>
      filter(date >= input$date_range[1]) |>
      filter(date <= input$date_range[2])
  })

  data_subset2 <- reactive({
    clean_data |>
      filter(dining_hall %in% dining_names()) |>
      filter(date >= input$date_range[1]) |>
      filter(date <= input$date_range[2])
  })

  # Data section
  output$tidydata <- renderTable({
    export_data <- data_subset()
    export_data$date <- format(export_data$date, "%m-%d-%Y")
    head(export_data, n = 5)
  })

  output$downloaddata <- downloadHandler(
    filename = function() {paste0("FRN_", input$location, ".csv")},
    content = function(file) {
      export_data <- data_subset()
      export_data$date <- format(export_data$date, "%m-%d-%Y")
      write.csv(export_data, file)}
  )

  output$cleandata <- renderTable({
    export_data <- data_subset2()
    export_data$date <- format(export_data$date, "%m-%d-%Y")
    head(export_data, n = 5)
  })

  output$downloaddata2 <- downloadHandler(
    filename = function() {paste0("FRN_", input$location, ".csv")},
    content = function(file) {
      export_data <- data_subset2()
      export_data$date <- format(export_data$date, "%m-%d-%Y")
      write.csv(export_data, file)}
  )
}

# Run the application
shinyApp(ui = ui, server = server)
