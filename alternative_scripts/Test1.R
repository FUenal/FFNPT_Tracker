
library(shiny)
library(knitr)
library(markdown)

ui <- shinyUI(
        fluidPage(
                fluidRow(
                        column(
                                width = 6,
                                tags$textarea(
                                        id = "text",
                                        cols = 50,
                                        rows = 30
                                )
                        ),
                        column(
                                width = 6,
                                uiOutput('markdown')
                        )
                )
        )
)

server <- function(input, output) {
        output$markdown <- renderUI({
                HTML(markdown::markdownToHTML(text = input$text))
        })
}

shinyApp(ui, server)
