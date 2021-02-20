## Supply-side policies interactive mapping tool
## Dr. Fatih Uenal, Faculty AI <-  Data Science Fellow (mars.fatih@gmail.com), last updated February 2021

## includes code adapted from the following sources:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
# https://github.com/eparker12/nCoV_tracker

# update data with automated script
# source("CAT_data_daily.R") # option to update daily new policy 


# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(sjmisc)) install.packages("sjmisc", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# set mapping color for each category of policy
All_policies  = "016c59"
Moratoria_bans_limits = "#cc4c02"
Subsidy_removal = "#662506"
Divestment = "#045a8d"


# Added ISO-normed country codes manually into the main xlsx data sheet (sheet 8) and manually updated country 
# names in all sheets to standard format. Cleaning & Wrangling Process is documented in the BitsBites.R file

# import data
country_overview = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 2)
regional_breakdown = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 3)
state_city_breakdown = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 4)
moratoria_bans_limits = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 5)
subsidiy_removal = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 6)
divestment = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 7)
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("input_data/50m.geojson", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")

# Add geo data to sheets
country_overview['latitude'] <- countries$latitude[match(country_overview$ISO3, countries$ISO3)]
country_overview['longitude'] <- countries$longitude[match(country_overview$ISO3, countries$ISO3)]
country_overview['global_level'] <- countries$global_level[match(country_overview$ISO3, countries$ISO3)]
country_overview['continent_level'] <- countries$continent_level[match(country_overview$ISO3, countries$ISO3)]

### MAP FUNCTIONS ###
# select large countries for mapping polygons
country_overview_large = country_overview %>% filter(ISO3 %in% worldcountry$ADM0_A3)
if (all(country_overview_large$ISO3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
country_overview_large = country_overview_large[order(country_overview_large$ISO3),]

### DATA PROCESSING: Policy Tracker Mapping ###
country_overview_large <- country_overview_large %>% replace(is.na(.), 0)
country_overview_large <- country_overview_large %>% mutate(policy_total = country_overview_large$Moratoria_bans_limits+country_overview_large$Subsidy_removal +
                                                                country_overview_large$Divestment)

# sum(country_overview_large$policy_total)

# Factor and label CAT_rating
country_overview_large$CAT_rating <- factor(country_overview_large$CAT_rating, levels = c(0,1,2,3,4,5,6,7), 
                                            labels = c("Critically Insufficient","Highly Insufficient","Insufficient", 
                                                       "2°C Compatible","1.5°C Paris Agreement Compatible", "Role Model","No Rating","No Data"))

# Controlling for factorization success
#class(country_overview_large$CAT_rating)
#table(country_overview_large$CAT_rating)
#country_overview_large$CAT_rating

### DATA PROCESSING: Policies converting Year from numeric to date format###
moratoria_bans_limits$Start <-lubridate::ymd(moratoria_bans_limits$Start, truncated = 2L)
subsidiy_removal$Start <-lubridate::ymd(subsidiy_removal$Start, truncated = 2L)
divestment$Start <-lubridate::ymd(divestment$Start, truncated = 2L)

# extract dates from moratoria_bans_limits data
moratoria_bans_limits$date = as.Date(moratoria_bans_limits$Start, format="%d/%m/%Y")
moratoria_bans_limits_min_date = min(moratoria_bans_limits$Start)
moratoria_bans_limits_max_date = max(moratoria_bans_limits$Start)
moratoria_bans_limits_max_date_clean = format(as.POSIXct(moratoria_bans_limits_max_date),"%d/%m/%Y")

# extract dates from moratoria_bans_limits data
subsidiy_removal$date = as.Date(subsidiy_removal$Start, format="%d/%m/%Y")
subsidiy_removal_min_date = min(subsidiy_removal$Start)
subsidiy_removal_max_date = max(subsidiy_removal$Start)
subsidiy_removal_max_date_clean = format(as.POSIXct(subsidiy_removal_max_date),"%d/%m/%Y")

# extract dates from moratoria_bans_limits data
divestment$date = as.Date(divestment$Start, format="%d/%m/%Y")
divestment_min_date = min(divestment$Start)
divestment_max_date = max(divestment$Start)
divestment_max_date_clean = format(as.POSIXct(divestment_max_date),"%d/%m/%Y")

# Set current date general
current_date = as.Date(max(moratoria_bans_limits$date),"%Y-%m-%d") 

#Set Main Plot output

policy_count = function(country_overview_large){
    x <- sum(country_overview_large$policy_total)
    print(x)
}

mbl_count = function(moratoria_bans_limits){
    x <- sum(moratoria_bans_limits$Policy)
    x
}

sr_count = function(subsidiy_removal){
    x <- sum(subsidiy_removal$Policy)
    x
}

div_count = function(divestment){
    x <- sum(divestment$Policy)
    x
}

### MAP FUNCTIONS ###
# function to plot cumulative Moratoria, Bans, and Limit Policies by date
cumulative_mbl_plot = function(moratoria_bans_limits) {
    g1 <- ggplot(moratoria_bans_limits, aes(x = date, y = Policy)) + 
        geom_bar(position="stack", stat="identity", fill = Moratoria_bans_limits) + 
        ylab("Moratoria, Bans, & Limit Policies") +  xlab("Year") + theme_bw() + ylim(0,50) +
        scale_fill_manual(values=c(Moratoria_bans_limits)) +
        xlim(c(moratoria_bans_limits_min_date,moratoria_bans_limits_max_date)) + 
        scale_x_date(date_labels = "%Y", limits=c(moratoria_bans_limits_min_date,moratoria_bans_limits_max_date)) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 10, 5, 5))
    g1
}

# function to plot cumulative Divestments by date
cumulative_div_plot = function(divestment) {
    g1 <- ggplot(divestment, aes(x = date, y = Policy)) + 
        geom_bar(position="stack", stat="identity", fill = Divestment) + 
        ylab("Divestments") +  xlab("Year") + theme_bw() + ylim(0,500) +
        scale_fill_manual(values=c(Divestment)) +
        xlim(c(divestment_min_date,divestment_max_date)) + 
        scale_x_date(date_labels = "%Y", limits=c(divestment_min_date,divestment_max_date)) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 10, 5, 5))
    g1
}

# function to plot cumulative subsidiy_removal by date
cumulative_sr_plot = function(subsidiy_removal) {
    g1 <- ggplot(subsidiy_removal, aes(x = date, y = Policy)) + 
        geom_bar(position="stack", stat="identity", fill = Subsidy_removal) + 
        ylab("Subsidy Removals") +  xlab("Year") + theme_bw() + ylim(0,10) +
        scale_fill_manual(values=c(Subsidy_removal)) +
        xlim(c(subsidiy_removal_min_date,subsidiy_removal_max_date)) + 
        scale_x_date(date_labels = "%Y", limits=c(subsidiy_removal_min_date,subsidiy_removal_max_date)) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 10, 5, 5))
    g1
}

# create plotting parameters for map
cv_pal <- colorFactor(palette = c("#FF0D0D","#FF4E11", "#FF8E15", "#FAB733", "#ACB334", "#69B34C", "#B1B6B9", "#EFEFEF"), country_overview_large$CAT_rating)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% country_overview_large$ISO3, ]

# create base map 
basemap = leaflet(plot_map) %>% 
    addTiles() %>% 
    addLayersControl(
        position = "topright",
        overlayGroups = c("All Policies", "Moratoria, Bans, & Limits", "Subsidy Removal", "Divestment"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c("Moratoria, Bans, & Limits", "Subsidy Removal", "Divestment")) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(~-100,-60,~60,70) %>%
    addLegend("bottomright", colors = c("#FF0D0D","#FF4E11", "#FF8E15", "#FAB733", "#ACB334", "#69B34C", "#B1B6B9", "#EFEFEF"), 
              labels =  c("<small>Critically Insufficient</small>","<small>Highly Insufficient</small>","<small>Insufficient</small>", 
                          "<small>2°C Compatible</small>","<small>1.5°C Paris Agreement Compatible</small>","<small>Role Model</small>", 
                          "<small>No Rating</small>", "<small>No Data</small>"),values = ~country_overview_large$CAT_rating,
              title = "<small>Climate Action Tracker Rating</small>") %>% 
    
    addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4, fillColor = ~cv_pal(country_overview_large$CAT_rating),
                label = sprintf("<strong>%s</strong><br/>CAT Rating: %s<br/>Moratoria, bans, & limits: %g<br/>Subsidy Removals: %d<br/>Divestments: %g", 
                                country_overview_large$Country, country_overview_large$CAT_rating, country_overview_large$Moratoria_bans_limits, country_overview_large$Subsidy_removal, 
                                country_overview_large$Divestment) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = All_policies),
                    textsize = "15px", direction = "auto")) %>%
    
    addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(policy_total)^(1/1.5), 
                     fillOpacity = 0.2, color = All_policies, group = "All Policies",
                     label = sprintf("<strong>%s</strong><br/>Moratoria, bans, & limits: %g<br/>Subsidy Removals: %d<br/>Divestments: %g", 
                                     country_overview_large$Country, country_overview_large$Moratoria_bans_limits, country_overview_large$Subsidy_removal, 
                                     country_overview_large$Divestment) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = All_policies),
                         textsize = "15px", direction = "auto")) %>% 
    
    addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Moratoria_bans_limits)^(1/1.5), 
                     fillOpacity = 0.2, color = Moratoria_bans_limits, group = "Moratoria, Bans, & Limits",
                     label = sprintf("<strong>%s</strong><br/>Moratoria, bans, & limits: %g", 
                                     country_overview_large$Country, country_overview_large$Moratoria_bans_limits) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = Moratoria_bans_limits),
                         textsize = "15px", direction = "auto")) %>% 
    
    addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Subsidy_removal)^(1/1.5), 
                     fillOpacity = 0.2, color = Subsidy_removal, group = "Subsidy Removal",
                     label = sprintf("<strong>%s</strong><br/>Subsidy Removals: %g", 
                                     country_overview_large$Country, country_overview_large$Subsidy_removal) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = Subsidy_removal),
                         textsize = "15px", direction = "auto")) %>% 
    
    addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Divestment)^(1/1.5), 
                     fillOpacity = 0.2, color = Divestment, group = "Divestment",
                     label = sprintf("<strong>%s</strong><br/>Divestments: %g", 
                                     country_overview_large$Country, country_overview_large$Divestment) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = Divestment),
                         textsize = "15px", direction = "auto"))


### SHINY UI ###
ui <- bootstrapPage(
    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Fossil Fuel Policy Tracker</a>'), id="nav",
               windowTitle = "Fossil Fuel Policy Tracker",
               
               tabPanel("Supply Side Policy Mapper",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("mymap", width="100%", height="100%"),
                            
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 75, left = 55, width = 300, fixed=TRUE,
                                          draggable = TRUE, height = "auto",
                                          
                                          span(tags$i(h6("Reported numbers of policies are subject to variation in policy types across countries.")), style="color:#045a8d"),
                                          h4(textOutput("policy_count"), align = "right"),
                                          h6(textOutput("mbl_count"), align = "right"), 
                                          h6(textOutput("sr_count"), align = "right"), 
                                          h6(textOutput("div_count"), align = "right"), 
                                          plotOutput("cumulative_mbl_plot", height="185px", width="100%"),
                                          plotOutput("cumulative_div_plot", height="175px", width="100%"),
                                          plotOutput("cumulative_sr_plot", height="175px", width="100%"),
                                          
                            ),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://fossilfueltreaty.org', tags$img(src='output-onlinepngtools.png',height='80',width='80')))
                            
                        )
               ),
               
               tabPanel("About this site",
                        tags$div(
                            tags$h4("Last update: 14.02.2021"), 
                            #h6(paste0(update)),
                            "This site will be updated on a regular basis (weekly). There are several other excellent Climate Change mapping tools available, including those run by", 
                            tags$a(href="https://www.climate-laws.org/#map-section", "the Grantham Research Institute on Climate Change and the Environment,"),
                            tags$a(href="https://climateactiontracker.org", "The Climate Action Tracker,"),"and",
                            tags$a(href="http://cait.wri.org/historical/Country%20GHG%20Emissions#", "CAIT Climate Data Explorer."),
                            "Our aim is to complement these resources with a particular focus on supply-side policies, including a Fossil Fuel City Impact Map to help organizations to 
                        identify best practices in the field and facilitate target city choices by provinding a propensity action score.",
                            
                            tags$br(),tags$br(),tags$h4("Background"), 
                            "FFNPT Initiative is a coalition of civil society organisations, research institutions, grassroot activists and other partners around the world, working to 
                        influence policy making and investment decisions from local to global level, and lay foundation for a coordinated, rapid and equitable global phase out of fossil fuels.
                        In the context of the urgent need to decarbonise our economy to prevent catastrophic climate change and the fossil fuel industry’s plans to extract far more fossil fuels 
                        than we can safely burn, the goal of the coalition is to foster a global equitable transition away from fossil fuels by spurring international cooperation to end new development, 
                        phase out existing production, and develop plans to support workers, communities and countries dependent on fossil fuels to create secure and healthy livelihoods.",
                            tags$br(),tags$br(),
                            "Our work is organised around three core areas: ", tags$br(), tags$br(),
                            tags$b("Campaign strategy: "), "to support movements around the world in holding governments and corporations to account and joining in call for a treaty; ",tags$br(),
                            tags$b("Diplomatic engagement strategy: "), "to support movements around the world in holding governments and corporations to account and joining in call for a treaty; ",tags$br(),
                            tags$b("Research strategy: "), "to build an evidence base for this work.",tags$br(),
                            tags$br(),
                            "More information on our work is avaialble  ",tags$a(href="https://fossilfueltreaty.org", "Fossil Fuely Treaty. "),
                            "An article discussing our campaign was published in ",tags$a(href="https://mondediplo.com/outsidein/fossil-fuel-disarmament", "Le Monde Diplomatique. "),
                            tags$br(),tags$br(),tags$h4("Code"), 
                            "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/FUenal/FFNPT_Tracker", "Github."),
                            tags$br(),tags$br(),tags$h4("Sources"),
                            tags$b("Supply Side Policies: "), tags$a(href="https://fossilfueltreaty.org/home", "Sussex University,"),
                            tags$br(),tags$br(),tags$h4("Author"),
                            "Dr Fatih Uenal, Fellow @ Faculty AI & University of Cambridge",tags$br(),
                            tags$br(),tags$br(),tags$h4("Contact"),
                            "mars.fatih@gmail.com",tags$br(),tags$br(),
                            tags$img(src = "FFNPT_Logo.png", width = "100px", height = "100px"), tags$img(src = "faculty.png", width = "175px", height = "65px")
                        )
               )
    )          
    
)

### SHINY SERVER ###

server = function(input, output, session) {
    
    # Policy tab 
    output$policy_count <- renderText({
        paste0(prettyNum(sum(country_overview_large$policy_total), big.mark=","), " Policies Overall")
    })
    
    output$mbl_count <- renderText({
        paste0(prettyNum(sum(moratoria_bans_limits$Policy), big.mark=","), " Moratoria, bans, limits")
    })
    
    output$sr_count <- renderText({
        paste0(prettyNum(sum(subsidiy_removal$Policy), big.mark=","), " Subsidy removals")
    })
    
    output$div_count <- renderText({
        paste0(prettyNum(sum(divestment$Policy), big.mark=","), "  Divestments")
    })
    
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    observeEvent(input$plot_date, {
        leafletProxy("mymap") %>% 
            clearMarkers() %>%
            clearShapes() %>%
            
            addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4, fillColor = ~cv_pal(country_overview_large$CAT_rating),
                        label = sprintf("<strong>%s</strong><br/>CAT Rating: %s<br/>Moratoria, bans, & limits: %g<br/>Subsidy Removals: %d<br/>Divestments: %g", 
                                        country_overview_large$Country, country_overview_large$CAT_rating, country_overview_large$Moratoria_bans_limits, country_overview_large$Subsidy_removal, 
                                        country_overview_large$Divestment) %>% lapply(htmltools::HTML),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px", "color" = All_policies),
                            textsize = "15px", direction = "auto")) %>%
            
            
            addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(policy_total)^(1/3), 
                             fillOpacity = 0.2, color = All_policies, group = "All Policies",
                             label = sprintf("<strong>%s</strong><br/>Moratoria, bans, & limits: %g<br/>Subsidy Removals: %d<br/>Divestments: %g", 
                                             country_overview_large$Country, country_overview_large$Moratoria_bans_limits, country_overview_large$Subsidy_removal, 
                                             country_overview_large$Divestment) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = All_policies),
                                 textsize = "15px", direction = "auto")) %>% 
            
            addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Moratoria_bans_limits)^(1/3), 
                             fillOpacity = 0.2, color = Moratoria_bans_limits, group = "Moratoria, Bans, & Limits",
                             label = sprintf("<strong>%s</strong><br/>Moratoria, bans, & limits: %g", 
                                             country_overview_large$Country, country_overview_large$Moratoria_bans_limits) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = Moratoria_bans_limits),
                                 textsize = "15px", direction = "auto")) %>% 
            
            addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Subsidy_removal)^(1/3), 
                             fillOpacity = 0.2, color = Subsidy_removal, group = "Subsidy Removal",
                             label = sprintf("<strong>%s</strong><br/>Subsidy Removals: %g", 
                                             country_overview_large$Country, country_overview_large$Subsidy_removal) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = Subsidy_removal),
                                 textsize = "15px", direction = "auto")) %>% 
            
            addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Divestment)^(1/3), 
                             fillOpacity = 0.2, color = Divestment, group = "Divestment",
                             label = sprintf("<strong>%s</strong><br/>Divestments: %g", 
                                             country_overview_large$Country, country_overview_large$Divestment) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = Divestment),
                                 textsize = "15px", direction = "auto"))
    })
    output$cumulative_mbl_plot <- renderPlot({
        cumulative_mbl_plot(moratoria_bans_limits)
    })
    
    output$cumulative_div_plot <- renderPlot({
        cumulative_div_plot(divestment)
    })
    output$cumulative_sr_plot <- renderPlot({
        cumulative_sr_plot(subsidiy_removal)
    })
}


#runApp(shinyApp(ui, server), launch.browser = FALSE)
shinyApp(ui, server)
#stopApp(returnValue = invisible())

## Deploying app to shiny.io
#library(rsconnect)
#rsconnect::deployApp(account="fuenal")


