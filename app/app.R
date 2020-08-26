library(dplyr)
library(tidyr)
library(magrittr)
library(dygraphs)
library(xts)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(scales)

ui <- fluidPage(

  useShinyjs(),

  theme = shinythemes::shinytheme("lumen"),

  tags$head(includeCSS("www/styles.css")),

  titlePanel("COVID-19 Economic Tracker"),

  sidebarLayout(

    sidebarPanel(

      width = 3,

      p(id = "element",
        paste0("In Louisville, as of <b>July 30 2020</b> ,",
               "total spending by all consumers decreased by <b>",
               "-12.5%",
               " compared to January.")),

       #h4("Legend"),
       #uiOutput("legend"),

      div(style="text-align: center;",
          radioGroupButtons("variable",
                            h4("Select a Metric"),
                            direction = "vertical",
                            choices = c("Consumer Spending",
                                        "Small Business Revenue",
                                        "Small Businesses Open",
                                        "Time Spent Away from Home"))),

      #sliderInput("rolling_mean", "Rolling Mean", min = 1, max = 14, value = 1, step = 2),
      h4("Switch between weekly and daily data"),
      div(style="text-align: center;",
          switchInput("rolling_mean",
                      value = FALSE,
                      onLabel = "Daily",
                      offLabel = "Weekly",
                      onStatus = "primary", offStatus = "primary")),

      h4("Switch between summary and city data"),
      div(style="text-align: center;",
          switchInput("peer_option",
                      value = TRUE,
                      onLabel = "Summary",
                      offLabel = "Peers",
                      onStatus = "warning", offStatus = "warning")),

      h5("Data is from the Opprtunity Insights Economic Tracker at tracktherecovery.org."),
      h5("Chetty, Friedman, Hendren, Stepner, and the OI Team (2020)")

    ),

    mainPanel(

      width = 9,

      dygraphOutput(outputId = "p", height = "600px")

    )
  )
)

server <- function(input, output) {

  this_data <- reactive({
    data_fxn(input)
  })

  output$p <- renderDygraph({

    graph_fxn(this_data(), input)

  })
#
#   output$legend <- renderUI({
#
#     if(input$peer_option) {
#       div(style="text-align: center;",
#           tags$div(id="legendDivID"))
#     } else {
#       div(style="text-align: center;",
#           tags$div(id="legendDivID"))
#     }
#
#   })

  toListen <- reactive({
    input$hover_values
    input$variable
  })

  observeEvent(toListen(), {

    if (is.null(input$hover_values[[4]])) {
      null_or_zero = TRUE
    } else if (input$hover_values[[4]] == 0) {
      null_or_zero = TRUE
    } else {
      null_or_zero = FALSE
    }

    if (null_or_zero) {
      x_value <- index(this_data())[nrow(this_data())]

      if (input$peer_option) col_name <- "lou" else col_name <- "Louisville"

      y_value <- this_data()
      y_value <- y_value[nrow(y_value), col_name] %>%  as.numeric()

    } else {
      x_value <- index(this_data())[input$hover_values[[4]]]
      y_value <- input$hover_values[[3]][[1]]$yval
    }
    x_value <- format(x_value, "%B %d %Y")

    description_text <- switch(input$variable,
     "Consumer Spending" = "total spending by all consumers decreased by",
     "Small Business Revenue" = "total small business revenue decreased by",
     "Small Businesses Open" = " the number of small businesses open decreased by",
     "Time Spent Away from Home" = " total time spent away from home decreased by")

    html("element",
      paste0("<h5 style=font-size:18px>In Louisville, as of <b>", x_value, "</b> , ",
             description_text,
             " <b>", percent(y_value * -1, 0.1, scale = 1), "</b>",
             " compared to January.</h5>"))
  })

}

shinyApp(ui = ui, server = server)

