shinyApp(
        ui = fluidPage(
                sliderInput("slider", "Slider", 1, 100, 50),
                downloadButton("report", "Generate report")
        ),
        server = function(input, output) {
                output$report <- downloadHandler(
                        # For PDF output, change this to "report.pdf"
                        filename = "report.html",
                        content = function(file) {
                                # Copy the report file to a temporary directory before processing it, in
                                # case we don't have write permissions to the current working dir (which
                                # can happen when deployed).
                                tempReport <- file.path(tempdir(), "report.Rmd")
                                file.copy("report.Rmd", tempReport, overwrite = TRUE)
                                
                                # Set up parameters to pass to Rmd document
                                params <- list(n = input$slider)
                                
                                # Knit the document, passing in the `params` list, and eval it in a
                                # child of the global environment (this isolates the code in the document
                                # from the code in this app).
                                rmarkdown::render(tempReport, output_file = file,
                                                  params = params,
                                                  envir = new.env(parent = globalenv())
                                )
                        }
                )
        }
)




#######
sidebarLayout(
        sidebarPanel(width = 2,
                     pickerInput("region_select", "Country:",   
                                 choices = as.character(country_overview_large$Country), 
                                 options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                 selected = as.character(country_overview_large$Country)[210],
                                 multiple = FALSE),
                     "Select country from drop-down menues to update geographical information.",
                     downloadButton("report", "Generate report")
        ),
        
        tabPanel("Country Profiles B",
                 
                 sidebarLayout(
                         sidebarPanel(width = 2,
                                      selectInput("region_select", "Country:",   
                                                  choices = sort(unique(country_overview_large$Country)), 
                                                  options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                                  multiple = FALSE),
                                      "Select country from drop-down menues to update geographical information.",
                                      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                                   inline = TRUE),
                                      downloadButton('downloadReport')
                         ),
                         
                         mainPanel(
                                 tabsetPanel(
                                         tabPanel("Country Overview", textOutput("text")),
                                         tabPanel("Moratora, Bans & Limits", tableOutput("mbl_table")),
                                         tabPanel("Subsidy Removals", tableOutput("sr_table")),
                                         tabPanel("Divestments", tableOutput("div_table"))
                                 )
                         )
                 )
        ),
        
        
        
        
### Other exaple of rendering input based markdown

tabPanel("Country Profiles B",
         
         sidebarLayout(
                 sidebarPanel(width = 6,
                              selectInput("region_select", "Country:",   
                                          choices = c("Please make a selection!", sort(unique(country_overview_large$Country))), 
                                          multiple = FALSE),
                              actionButton(inputId = "report",label="View Report")
                 ),
                 
                 mainPanel(
                         # show loading screen
                         shinyjs::hidden(
                                 tags$div(id="loading",
                                          tags$style("#loading{position:absolute;}"),
                                          tags$h1("Loading..."))
                         ),
                         
                         # report
                         tags$div(id="report-wrapper",
                                  tags$style("#report-wrapper p{font-size:14pt;}"),
                                  htmlOutput("renderedReport")
                         )
                 )
         )
),

## Country Profiles 2
output$renderedReport <- renderUI({
        includeHTML(
                rmarkdown::render(
                        "report_template.Rmd",
                        params = list(
                                selection = input$country,
                                data = country_overview_largeDF,
                                data_mbl = moratoria_bans_limitsDF,
                                data_sr = subsidiy_removalDF,
                                data_div = divestmentDF
                        )
                )
        )
})

# render report when button clicked
observeEvent(input$render, {
        country_overview_largeDF <- country_overview_large[country_overview_large$Country == input$country, ]
        moratoria_bans_limitsDF <- moratoria_bans_limits[moratoria_bans_limits$Country == input$country, ]
        subsidiy_removalDF <- subsidiy_removal[subsidiy_removal$Country == input$country, ]
        divestmentDF <- divestment[divestment$Country == input$country, ]
})