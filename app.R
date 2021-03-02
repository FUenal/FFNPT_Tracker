#'//////////////////////////////////////////////////////////////////////////////
#' FILE: app.R
#' AUTHOR: Fatih Uenal
#' CREATED: 19-03-2021
#' MODIFIED: 19-03-2021
#' PURPOSE: Supply-side policies interactive mapping tool
#' PACKAGES: see below
#' COMMENTS: NA
#'//////////////////////////////////////////////////////////////////////////////

## includes code adapted from the following sources:
# https://davidruvolo51.github.io/shinytutorials/tutorials/rmarkdown-shiny/
# https://github.com/eparker12/nCoV_tracker
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example


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
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
    
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
divestment_scrape = read.csv("input_data/divestment_scrape.csv")
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("input_data/50m.geojson", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")

# Add geo data to sheets
country_overview['latitude'] <- countries$latitude[match(country_overview$ISO3, countries$ISO3)]
country_overview['longitude'] <- countries$longitude[match(country_overview$ISO3, countries$ISO3)]
country_overview['global_level'] <- countries$global_level[match(country_overview$ISO3, countries$ISO3)]
country_overview['continent_level'] <- countries$continent_level[match(country_overview$ISO3, countries$ISO3)]

# Added ISO Codes to all sheets manually due to mismatching name.

### MAP FUNCTIONS ###
# select large countries for mapping polygons
country_overview_large = country_overview %>% filter(ISO3 %in% worldcountry$ADM0_A3)
if (all(country_overview_large$ISO3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
country_overview_large = country_overview_large[order(country_overview_large$ISO3),]

### DATA PROCESSING: Policy Tracker Mapping: number of governmental and non-governmental and total number of policies and transfer to country_overview_large file###
country_overview_large <- country_overview_large %>% replace(is.na(.), 0)

## Calculate number of governmental and non-governmental policies 
moratoria_bans_limits <- moratoria_bans_limits %>% group_by(ISO3) %>% mutate(a = sum(mbl_country))
moratoria_bans_limits <- moratoria_bans_limits %>% group_by(ISO3) %>% mutate(b = sum(mbl_city_region))
divestment <- divestment %>% group_by(ISO3) %>% mutate(a = sum(divestment_city_region))
divestment <- divestment %>% group_by(ISO3) %>% mutate(b = sum(divestment_mixed))
divestment <- divestment %>% group_by(ISO3) %>% mutate(c = sum(divestment_non_government))
subsidiy_removal <- subsidiy_removal %>% group_by(ISO3) %>% mutate(a = sum(Policy))

## Transfer total number and breakdowns of policies to country_overview_large file
country_overview_large['mbl_country'] <- moratoria_bans_limits$a[match(country_overview_large$ISO3, moratoria_bans_limits$ISO3)]
country_overview_large['mbl_city_region'] <- moratoria_bans_limits$b[match(country_overview_large$ISO3, moratoria_bans_limits$ISO3)]
country_overview_large['divestment_city_region'] <- divestment$a[match(country_overview_large$ISO3, divestment$ISO3)]
#country_overview_large['divestment_mixed'] <- divestment$b[match(country_overview_large$ISO3, divestment$ISO3)] # (for mixed policies such as pension funds etc.)
country_overview_large['divestment_non_government'] <- divestment$c[match(country_overview_large$ISO3, divestment$ISO3)]
country_overview_large['subsidy_removal'] <- subsidiy_removal$a[match(country_overview_large$ISO3, subsidiy_removal$ISO3)]

## Total number of policies
country_overview_large <- country_overview_large %>% group_by(ISO3) %>%  
    mutate(Moratoria_bans_limits_total = mbl_country + mbl_city_region)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>% 
    mutate(Divestment_total = divestment_city_region + divestment_non_government) # add (+ divestment_mixed) for mixed policies such as pension funds etc.

country_overview_large <- country_overview_large %>% group_by(ISO3) %>% 
    mutate(Subsidy_removal_total = sum(subsidy_removal))

country_overview_large <- country_overview_large %>% replace(is.na(.), 0)

# Check for correct calculations from above
#sum(country_overview_large$mbl_country)
#sum(country_overview_large$mbl_city_region)
#sum(country_overview_large$divestment_city_region)
#sum(country_overview_large$divestment_mixed)
#sum(country_overview_large$divestment_non_government)
#sum(country_overview_large$Moratoria_bans_limits_total)
#sum(country_overview_large$Divestment_total)
#sum(country_overview_large$Subsidy_removal_total)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>% 
    mutate(Policy_total = Moratoria_bans_limits_total + Subsidy_removal_total + Divestment_total)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>% 
    mutate(Government_policies_total = Moratoria_bans_limits_total + Subsidy_removal_total + divestment_city_region)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>% 
    mutate(Non_Government_policies_total = divestment_non_government)

## Calculate total number of divestment policies per country 
#x <- divestment_scrape %>% group_by(Country) %>% mutate(Policy_score = sum(Policy))
#x
#y <-  nrow(filter(divestment_scrape, Country == "Malta"))
#y

#unique(divestment_scrape$Country)


# Factor and label CAT_rating
country_overview_large$CAT_rating <- factor(country_overview_large$CAT_rating, levels = c(0,1,2,3,4,5,6,7), 
                                            labels = c("Critically Insufficient","Highly Insufficient","Insufficient", 
                                                       "2°C Compatible","1.5°C Paris Agreement Compatible", "Role Model","No Rating","No Data"))

# Controlling for factorization success
#class(country_overview_large$CAT_rating)
#table(country_overview_large$CAT_rating)
#country_overview_large$CAT_rating

### Data processing divestment_scrape
## Add ISO codes
divestment_scrape <- divestment_scrape %>% select(-X)
str(divestment_scrape)
head(divestment_scrape)



### DATA PROCESSING: Policies converting Year from numeric to date format###
moratoria_bans_limits$Start <-lubridate::ymd(moratoria_bans_limits$Start, truncated = 2L)
subsidiy_removal$Start <-lubridate::ymd(subsidiy_removal$Start, truncated = 2L)
divestment$Start <-lubridate::ymd(divestment$Start, truncated = 2L)

# extract dates from moratoria_bans_limits data
moratoria_bans_limits$date = as.Date(moratoria_bans_limits$Start, format="%d/%m/%Y")
moratoria_bans_limits_min_date = min(moratoria_bans_limits$date)
moratoria_bans_limits_max_date = max(moratoria_bans_limits$Start)
moratoria_bans_limits_max_date_clean = format(as.POSIXct(moratoria_bans_limits_max_date),"%d/%m/%Y")

# extract dates from moratoria_bans_limits data
subsidiy_removal$date = as.Date(subsidiy_removal$Start, format="%d/%m/%Y")
subsidiy_removal_min_date = min(subsidiy_removal$date)
subsidiy_removal_max_date = max(subsidiy_removal$Start)
subsidiy_removal_max_date_clean = format(as.POSIXct(subsidiy_removal_max_date),"%d/%m/%Y")

# extract dates from moratoria_bans_limits data
divestment$date = as.Date(divestment$Start, format="%d/%m/%Y")
divestment_min_date = min(divestment$date)
divestment_max_date = max(divestment$Start)
divestment_max_date_clean = format(as.POSIXct(divestment_max_date),"%d/%m/%Y")

# Set current date general
current_date = as.Date(max(moratoria_bans_limits$date),"%Y-%m-%d") 

#Set Main Plot output

policy_count = function(country_overview_large){
    x <- sum(country_overview_large$Policy_total)
    print(x)
}

mbl_count = function(country_overview_large){
    x <- sum(country_overview_large$Moratoria_bans_limits_total)
    x
}

sr_count = function(country_overview_large){
    x <- sum(country_overview_large$Subsidy_removal_total)
    x
}

div_count = function(country_overview_large){
    x <- sum(country_overview_large$Divestment_total)
    x
}

gov_pol_count = function(country_overview_large){
    x <- sum(country_overview_large$Government_policies_total)
    x
}

Non_gov_pol_count = function(country_overview_large){
    x <- sum(country_overview_large$Non_Government_policies_total)
    x
}

### MAP FUNCTIONS ###
# function to plot cumulative Moratoria, Bans, and Limit Policies by date
cumulative_mbl_plot = function(moratoria_bans_limits) {
    g1 <- ggplot(moratoria_bans_limits, aes(x = date, y = Policy)) + 
        geom_bar(position="stack", stat="identity", fill = Moratoria_bans_limits) + 
        ylab("Moratoria, Bans, & Limit Policies") +  xlab("Year") + theme_bw() + ylim(0,30) +
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
        ylab("Divestments") +  xlab("Year") + theme_bw() + ylim(0,300) +
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
        ylab("Subsidy Removals") +  xlab("Year") + theme_bw() + ylim(0,8) +
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
    hideGroup(c("All Policies", "Moratoria, Bans, & Limits", "Subsidy Removal", "Divestment")) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(~-100,-60,~60,70) %>%
    addLegend("bottomright", colors = c("#FF0D0D","#FF4E11", "#FF8E15", "#FAB733", "#ACB334", "#69B34C", "#B1B6B9", "#EFEFEF"), 
              labels =  c("<small>Critically Insufficient</small>","<small>Highly Insufficient</small>","<small>Insufficient</small>", 
                          "<small>2°C Compatible</small>","<small>1.5°C Paris Agreement Compatible</small>","<small>Role Model</small>", 
                          "<small>No Rating</small>", "<small>No Data</small>"),values = ~country_overview_large$CAT_rating,
              title = "<small>Climate Action Tracker Rating</small>") %>% 
    
    addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4, fillColor = ~cv_pal(country_overview_large$CAT_rating),
                label = sprintf("<strong>%s</strong><br/>CAT Rating: %s<br/>Government policies: %g<br/>Non-governmental policies: %d", 
                                country_overview_large$Country, country_overview_large$CAT_rating, country_overview_large$Government_policies_total, 
                                country_overview_large$Non_Government_policies_total) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", "font-family" = "Roboto", padding = "3px 8px", "color" = cv_pal),
                    textsize = "15px", direction = "auto")) %>%
    
    addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Policy_total)^(1/1.5), 
                     fillOpacity = 0.2, color = All_policies, group = "All Policies",
                     label = sprintf("<strong>%s</strong><br/>Moratoria, bans, & limits: %g<br/>Subsidy Removals: %d<br/>Divestments: %g", 
                                     country_overview_large$Country, country_overview_large$Moratoria_bans_limits_total, country_overview_large$Subsidy_removal_total, 
                                     country_overview_large$Divestment_total) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                         style = list("font-weight" = "normal", "font-family" = "Roboto", padding = "3px 8px", "color" = All_policies),
                         textsize = "15px", direction = "auto")) %>% 
    
    addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Moratoria_bans_limits_total)^(1/1.5), 
                     fillOpacity = 0.2, color = Moratoria_bans_limits, group = "Moratoria, Bans, & Limits",
                     label = sprintf("<strong>%s</strong><br/>Moratoria, bans, & limits: %g", 
                                     country_overview_large$Country, country_overview_large$Moratoria_bans_limits_total) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                         style = list("font-weight" = "normal", "font-family" = "Roboto", padding = "3px 8px", "color" = Moratoria_bans_limits),
                         textsize = "15px", direction = "auto")) %>% 
    
    addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Subsidy_removal_total)^(1/1.5), 
                     fillOpacity = 0.2, color = Subsidy_removal, group = "Subsidy Removal",
                     label = sprintf("<strong>%s</strong><br/>Subsidy Removals: %g", 
                                     country_overview_large$Country, country_overview_large$Subsidy_removal_total) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                         style = list("font-weight" = "normal", "font-family" = "Roboto", padding = "3px 8px", "color" = Subsidy_removal),
                         textsize = "15px", direction = "auto")) %>% 
    
    addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Divestment_total)^(1/1.5), 
                     fillOpacity = 0.2, color = Divestment, group = "Divestment",
                     label = sprintf("<strong>%s</strong><br/>Divestments: %g", 
                                     country_overview_large$Country, country_overview_large$Divestment_total) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                         style = list("font-weight" = "normal", "font-family" = "Roboto", padding = "3px 8px", "color" = Divestment),
                         textsize = "15px", direction = "auto"))


### SHINY UI ###
ui <- bootstrapPage(
    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("journal"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#000000;" class="active" href="#">Fossil Fuel Policy Tracker</a>'), id="nav",
               windowTitle = "Fossil Fuel Policy Tracker",
               
               tabPanel("Supply Side Policy Mapper",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("mymap", width="100%", height="100%"),
                            
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 75, left = 55, width = 300, fixed=TRUE,
                                          draggable = TRUE, height = "auto",
                                          
                                          span(tags$i(h6("Reported numbers of policies are subject to variation in policy types between countries.")), style="color:#045a8d"),
                                          h4(textOutput("policy_count"), align = "right"),
                                          h5(textOutput("gov_pol_count"), align = "right"),
                                          h5(textOutput("Non_gov_pol_count"), align = "right"),
                                          #h6(textOutput("mbl_count"), align = "right"), 
                                          #h6(textOutput("div_count"), align = "right"), 
                                          #h6(textOutput("sr_count"), align = "right"), 
                                          plotOutput("cumulative_mbl_plot", height="185px", width="100%"),
                                          plotOutput("cumulative_div_plot", height="175px", width="100%"),
                                          plotOutput("cumulative_sr_plot", height="175px", width="100%"),
                                          
                            ),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://fossilfueltreaty.org', tags$img(src='output-onlinepngtools.png',height='80',width='80')))
                            
                        )
               ),

               tabPanel("Country Profiles",
                        tags$head(
                            tags$link(
                                type = "text/css",
                                rel = "stylesheet",
                                href = "css/styles.css"
                            )
                        ),
                        tags$header(
                            tags$h1(" Fossil Fuel Supply-Side Policies"),
                        ),
                        tags$main(
                            tags$form(
                                tags$legend("Select country from drop-down menu to show country profiles"),
                                tags$label(`for` = "country", ""),
                                tags$select(id = "country", HTML(
                                    c("Select a Country",
                                      sapply(
                                          sort(unique(country_overview_large$Country)),
                                          function(x) {
                                              paste0(
                                                  "<option value='", x, "'>",
                                                  x,
                                                  "</option>"
                                              )
                                          }
                                      )
                                    )
                                )
                                ),
                                tags$button(
                                    id = "render",
                                    class = "action-button shiny-bound-input",
                                    "Show"
                                )
                                
                            ),
                            tags$article(
                                `aria-label` = "report",
                                htmlOutput("report")
                            )
                        )
                    ),
       
               tabPanel("About this site",
                        tags$div(
                            tags$h4("Last update: "), 
                            h6(paste0(Sys.Date())),
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
        paste0(prettyNum(sum(country_overview_large$Policy_total), big.mark=","), " Policies Overall")
    })
    
    output$gov_pol_count <- renderText({
        paste0(prettyNum(sum(country_overview_large$Government_policies_total), big.mark=","), " Government policies")
    })
    
    output$Non_gov_pol_count <- renderText({
        paste0(prettyNum(sum(country_overview_large$Non_Government_policies_total), big.mark=","), " Non-government policies")
    })
    
    output$mbl_count <- renderText({
        paste0(prettyNum(sum(country_overview_large$Moratoria_bans_limits_total), big.mark=","), " Moratoria, bans, limits")
    })
    
    output$sr_count <- renderText({
        paste0(prettyNum(sum(country_overview_large$Subsidy_removal_total), big.mark=","), " Subsidy removals")
    })
    
    output$div_count <- renderText({
        paste0(prettyNum(sum(country_overview_large$Divestment_total), big.mark=","), "  Divestments")
    })
    
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    observeEvent(input$plot_date, {
        leafletProxy("mymap") %>% 
            clearMarkers() %>%
            clearShapes() %>%
            
            addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4, fillColor = ~cv_pal(country_overview_large$CAT_rating),
                        label = sprintf("<strong>%s</strong><br/>CAT Rating: %s<br/>Government policies: %g<br/>Non-governmental policies: %d", 
                                        country_overview_large$Country, country_overview_large$CAT_rating, country_overview_large$Government_policies_total, 
                                        country_overview_large$Non_Government_policies_total) %>% lapply(htmltools::HTML),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", "font-family" = "Roboto", padding = "3px 8px", "color" = cv_pal),
                            textsize = "15px", direction = "auto")) %>%
            
            
            addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Policy_total)^(1/3), 
                             fillOpacity = 0.2, color = All_policies, group = "All Policies",
                             label = sprintf("<strong>%s</strong><br/>Moratoria, bans, & limits: %g<br/>Subsidy Removals: %d<br/>Divestments: %g", 
                                             country_overview_large$Country, country_overview_large$Moratoria_bans_limits_total, country_overview_large$Subsidy_removal_total, 
                                             country_overview_large$Divestment_total) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", "font-family" = "Roboto", padding = "3px 8px", "color" = All_policies),
                                 textsize = "15px", direction = "auto")) %>% 
            
            addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Moratoria_bans_limits_total)^(1/3), 
                             fillOpacity = 0.2, color = Moratoria_bans_limits, group = "Moratoria, Bans, & Limits",
                             label = sprintf("<strong>%s</strong><br/>Moratoria, bans, & limits: %g", 
                                             country_overview_large$Country, country_overview_large$Moratoria_bans_limits_total) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", "font-family" = "Roboto", padding = "3px 8px", "color" = Moratoria_bans_limits),
                                 textsize = "15px", direction = "auto")) %>% 
            
            addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Subsidy_removal_total)^(1/3), 
                             fillOpacity = 0.2, color = Subsidy_removal, group = "Subsidy Removal",
                             label = sprintf("<strong>%s</strong><br/>Subsidy Removals: %g", 
                                             country_overview_large$Country, country_overview_large$Subsidy_removal_total) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", "font-family" = "Roboto", padding = "3px 8px", "color" = Subsidy_removal),
                                 textsize = "15px", direction = "auto")) %>% 
            
            addCircleMarkers(data = country_overview_large, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Divestment_total)^(1/3), 
                             fillOpacity = 0.2, color = Divestment, group = "Divestment",
                             label = sprintf("<strong>%s</strong><br/>Divestments: %g", 
                                             country_overview_large$Country, country_overview_large$Divestment_total) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", "font-family" = "Roboto", padding = "3px 8px", "color" = Divestment),
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
    ## country-specific profiles
    # set initial ui for htmlOutput
    output$report <- renderUI({
        tagList(
            tags$p(
                "The data presented here is based on the Fossil Fuel Database ",
                "which automatically searches the internet to identify ",
                "existing climate change upply-side policies. Click here ",
                tags$a(
                    href = "https://fossilfueltreaty.org",
                    "for more information"
                ),
                "."
            )
        )
    })
    
    # render report when button clicked
    observeEvent(input$render, {
        country_overview_largeDF <- country_overview_large[country_overview_large$Country == input$country, ]
        moratoria_bans_limitsDF <- moratoria_bans_limits[moratoria_bans_limits$Country == input$country, ]
        subsidiy_removalDF <- subsidiy_removal[subsidiy_removal$Country == input$country, ]
        divestmentDF <- divestment[divestment$Country == input$country, ]
        output$report <- renderUI({
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
    })
}


#runApp(shinyApp(ui, server), launch.browser = FALSE)
shinyApp(ui, server)
#stopApp(returnValue = invisible())

## Deploying app to shiny.io
#library(rsconnect)
#rsconnect::deployApp(account="fuenal")


