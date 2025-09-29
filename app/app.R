library(shiny)
library(bslib)
library(tidyverse)
library(dygraphs)

source("fn.R")

syn <- readRDS("syndromes.rds")
ts <- readRDS("timeseries.rds")

ui <- page_sidebar(

  title = "Syndrome Dashboard",

  sidebar = sidebar(
    width = 300,
    bg = "#e1f7ed",
    selectInput(
      inputId = "syn",
      label = "Syndrome",
      choices = syn,
      multiple = TRUE,
      selected = syn[[1]][1]
    ),
    dateRangeInput(
      inputId = "date",
      label = "Date range",
      start = max(ts$date) - 7,
      end = max(ts$date),
      min = min(ts$date),
      max = max(ts$date)
    ),
    checkboxInput(
      inputId = "alert",
      label = "Show ESSENCE alerts",
      value = TRUE
    )
  ),

  card(
    full_screen = TRUE,
    plotOutput("plot")
  )

)

server <- function(input, output) {
  tsrctv <- reactive({
    req(input$syn, input$date)

    vsyn <- unlist(strsplit(input$syn, ";"))

    ts |>
      filter(
        syndrome %in% vsyn,
        date >= input$date[1],
        date <= input$date[2]
      )
  })

  output$plot <- renderPlot({
    req(tsrctv())

    if (input$alert) {
      p <- dbplot2(tsrctv())
    } else {
      p <- dbplot1(tsrctv())
    }

    p
  })
}

shinyApp(ui = ui, server = server)
