## Country Profile Alternative Set-ups

if(!require(shinyjs)) install.packages("shinyjs", repos = "http://cran.us.r-project.org")

## ui

tabPanel("Country Profiles B",
         
         sidebarLayout(
                 sidebarPanel(width = 2,
                              selectInput("region_select", "Country:",   
                                          choices = c("Please make a selection!", sort(unique(country_overview_large$Country))), 
                                          multiple = FALSE),
                              "Select country from drop-down menues to update geographical information.",
                              radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                           inline = TRUE),
                              downloadButton('downloadReport')
                 ),
                 
                 mainPanel(
                         tabsetPanel(
                                 tabPanel("Country Overview", htmlOutput("renderedReport")),
                                 tabPanel("Moratora, Bans & Limits", tableOutput("mbl_table")),
                                 tabPanel("Subsidy Removals", tableOutput("sr_table")),
                                 tabPanel("Divestments", tableOutput("div_table"))
                         )
                 )
         )
),


## Server side

## Country Profiles 2
reactive_mbl = reactive({
        moratoria_bans_limits %>%
                filter(input$region_select) 
})

output$mbl_table <- renderTable({
        mbl_table(reactive_mbl(), input$region_select)
        moratoria_bans_limits %>% 
                group_by(moratoria_bans_limits$City_state_or_province) %>% 
                select(c("City_state_or_province", "Category", "Fuel_type", "Fuel_subtype", "Sources_and_more_info")) %>% 
                replace(is.na(.), "--")
        names(mbl) = c("City state or province", "Category", "Fuel type", "Fuel subtype", "Sources")
        mbl
})

regFormula <- reactive({
        as.formula(paste('mpg ~', input$x))
})

output$renderedReport <- renderPlot({
        par(mar = c(4, 4, .1, .1))
        plot(regFormula(), data = mtcars, pch = 19)
})

output$downloadReport <- downloadHandler(
        filename = function() {
                paste('my-report', sep = '.', switch(
                        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
                ))
        },
        
        content = function(file) {
                src <- normalizePath('report.Rmd')
                
                # temporarily switch to the temp dir, in case you do not have write
                # permission to the current working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                file.copy(src, 'report.Rmd', overwrite = TRUE)
                
                library(rmarkdown)
                out <- render('report.Rmd', switch(
                        input$format,
                        PDF = pdf_document(), HTML = html_document(), Word = word_document()
                ))
                file.rename(out, file)
        }
)