#'//////////////////////////////////////////////////////////////////////////////
#' FILE: app.R
#' AUTHOR: Fatih Uenal
#' CREATED: 19-03-2021
#' MODIFIED: 19-03-2021
#' PURPOSE: Supply-side policies interactive mapping tool
#' PACKAGES: various, see below
#' COMMENTS: NA
#'//////////////////////////////////////////////////////////////////////////////

## includes code adapted from the following sources:
# https://davidruvolo51.github.io/shinytutorials/tutorials/rmarkdown-shiny/
# https://github.com/eparker12/nCoV_tracker
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example


# update data with automated script
# source("divestment_scrap_weekly.R") # option to update weekly new divestment policies 
# source("manual_scrap_weekly.R") # option to update weekly new manual entry policies

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
#if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(sjmisc)) install.packages("sjmisc", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(xlsx)) install.packages("xlsx", repos = "http://cran.us.r-project.org")
#if(!require(shinyjs)) install.packages("shinyjs", repos = "http://cran.us.r-project.org")

# pkgs
suppressPackageStartupMessages(library(shiny))

# set mapping color for each category of policy
#All_policies  = "016c59"
Moratoria_bans_limits = "#253550" #"#cc4c02"
Subsidy_removal = "#0C101E" #"#662506"
Divestment = "#045a8d"
Government_policies = "#4d004b"
Non_Government_policies = "#016c59"
Cities_regions_states = "#FF9100"  #"#ACB334"

## Colour choices CAT Tracker
# #FF0D0D","#FF4E11", "#FF8E15", "#FAB733", "#ACB334", "#69B34C", "#B1B6B9"

# Added ISO-normed country codes manually into the main xlsx data sheet (sheet 8) and manually updated country 
# names in all sheets to standard format. Cleaning & Wrangling Process is documented in the BitsBites.R file

# import data
country_overview = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 2)
regional_breakdown = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 3)
state_city_breakdown = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 4)
moratoria_bans_limits = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 5)
subsidy_removal = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 6)
divestment = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 7)
divestment_new = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 8)
#world_cities = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 10)
divestment_scraped = read.csv("input_data/divestment_scraped.csv")
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("input_data/50m.geojson", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")
annual_share_of_co2_emissions = read.csv("input_data/annual-share-of-co2-emissions.csv")

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
divestment_new <- divestment_new %>% group_by(ISO3) %>% mutate(a = sum(divestment_city_region))
divestment_new <- divestment_new %>% group_by(ISO3) %>% mutate(b = sum(divestment_non_government))
divestment <- divestment %>% group_by(ISO3) %>% mutate(a = sum(divestment_city_region))
divestment <- divestment %>% group_by(ISO3) %>% mutate(b = sum(divestment_non_government))
subsidy_removal <- subsidy_removal %>% group_by(ISO3) %>% mutate(a = sum(Policy))

# Replace NAs
#country_overview_large <- country_overview_large %>% replace(is.na(.), 0)

## Transfer total number and breakdowns of policies to country_overview_large file
country_overview_large['mbl_country'] <- moratoria_bans_limits$a[match(country_overview_large$ISO3, moratoria_bans_limits$ISO3)]
country_overview_large['mbl_city_region'] <- moratoria_bans_limits$b[match(country_overview_large$ISO3, moratoria_bans_limits$ISO3)]
country_overview_large['divestment_city_region'] <- divestment_new$a[match(country_overview_large$ISO3, divestment_new$ISO3)]
country_overview_large['divestment_non_government'] <- divestment_new$b[match(country_overview_large$ISO3, divestment_new$ISO3)]
country_overview_large['subsidy_removal'] <- subsidy_removal$a[match(country_overview_large$ISO3, subsidy_removal$ISO3)]

## Calculate total number of policies
country_overview_large <- country_overview_large %>% group_by(ISO3) %>%  
    mutate(Moratoria_bans_limits_total = mbl_country + mbl_city_region)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>% 
    mutate(Divestment_total = divestment_city_region + divestment_non_government)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>% 
    mutate(Subsidy_removal_total = sum(subsidy_removal))

country_overview_large <- country_overview_large %>% replace(is.na(.), 0)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>% 
    mutate(Policy_total = Moratoria_bans_limits_total + Subsidy_removal_total + Divestment_total)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>% 
    mutate(Government_policies_total = Moratoria_bans_limits_total + Subsidy_removal_total + divestment_city_region)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>% 
    mutate(Non_Government_policies_total = divestment_non_government)


## Total number of state, city, region policies
state_city_breakdown_map <- state_city_breakdown %>% 
    select(c("State_city_region", "Country", "latitude", "longitude", "ISO3", "Moratoria_bans_limits", "Subsidy_removal", "Divestment", "FFNPT")) %>% 
    replace(is.na(.), 0)

state_city_breakdown_map <- state_city_breakdown_map %>% group_by(State_city_region) %>%  
    mutate(Moratoria_bans_limits_total = sum(Moratoria_bans_limits))

state_city_breakdown_map <- state_city_breakdown_map %>% group_by(State_city_region) %>% 
    mutate(Divestment_total = sum(Divestment))

state_city_breakdown_map <- state_city_breakdown_map %>% group_by(State_city_region) %>% 
    mutate(Subsidy_removal_total = sum(Subsidy_removal))

state_city_breakdown_map <- state_city_breakdown_map %>% group_by(State_city_region) %>% 
    mutate(ffnpt_total = sum(FFNPT))

state_city_breakdown_map <- state_city_breakdown_map %>% group_by(State_city_region) %>% 
    mutate(City_region_state_total = sum(Moratoria_bans_limits + Divestment + Subsidy_removal + FFNPT))

# Matching dates from original divestment file with newly scraped divestment file
# divestment_new$Start <- divestment$Start[match(divestment_new$Organisation, divestment$Organisation)]

# Factor and label CAT_rating
country_overview_large$CAT_rating <- factor(country_overview_large$CAT_rating, levels = c(0,1,2,3,4,5,6,7), 
                                            labels = c("Critically Insufficient","Highly Insufficient","Insufficient", 
                                                       "2°C Compatible","1.5°C Paris Agreement Compatible", "Role Model","No Rating","No Data"))

# Factor, label, and reorder MTCO2e
country_overview_large$MTCO2e_cat <- cut(country_overview_large$MTCO2e, breaks = c(-100, -1, 1, 169, 500, 1000, 5000, 10000, 20000), 
                                         labels = c("<0 MTCO2e", "No data", "1-169 MTCO2e", "169-500 MTCO2e", "500-1000 MTCO2e", "1000-5000 MTCO2e", 
                                                    "5000-10000 MTCO2e", ">10000 MTCO2e"), right = FALSE)

country_overview_large$MTCO2e_cat = factor(country_overview_large$MTCO2e_cat,levels(country_overview_large$MTCO2e_cat)[c(2,1,3:8)])

### Pull % Global Emissions to country_overview file via ISO3
annual_share_of_co2_emissions_2019 <- annual_share_of_co2_emissions %>% 
    filter(Year == 2019)

country_overview_large["global_emissions_percent"] <- annual_share_of_co2_emissions_2019$Share.of.global.CO2.emissions[match(country_overview_large$ISO3,
                                                                                                                             annual_share_of_co2_emissions_2019$Code)]
#country_overview_large <- country_overview_large %>% replace(is.na(.), 0)

# write country_overview_large file
write.csv(country_overview_large, file = "input_data/country_overview_large.csv")

### DATA PROCESSING: Policies converting Year from numeric to date format###
moratoria_bans_limits$Date <-lubridate::ymd(moratoria_bans_limits$Start, truncated = 2L)
subsidy_removal$Date <-lubridate::ymd(subsidy_removal$Start, truncated = 2L)
divestment$Date <-lubridate::ymd(divestment$Start, truncated = 2L)

# extract dates from moratoria_bans_limits data
moratoria_bans_limits$date = as.Date(moratoria_bans_limits$Date, format="%d/%m/%Y")
moratoria_bans_limits_min_date = min(moratoria_bans_limits$date)
moratoria_bans_limits_max_date = max(moratoria_bans_limits$date)
moratoria_bans_limits_max_date_clean = format(as.POSIXct(moratoria_bans_limits_max_date),"%d/%m/%Y")

# extract dates from moratoria_bans_limits data
subsidy_removal$date = as.Date(subsidy_removal$Date, format="%d/%m/%Y")
subsidy_removal_min_date = min(subsidy_removal$date)
subsidy_removal_max_date = max(subsidy_removal$date)
subsidy_removal_max_date_clean = format(as.POSIXct(subsidy_removal_max_date),"%d/%m/%Y")

# extract dates from moratoria_bans_limits data
divestment$date = as.Date(divestment$Date, format="%d/%m/%Y")
divestment_min_date = min(divestment$date)
divestment_max_date = max(divestment$date)
divestment_max_date_clean = format(as.POSIXct(divestment_max_date),"%d/%m/%Y")


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
cumulative_div_plot = function(divestment_new) {
    g1 <- ggplot(divestment_new, aes(x = date, y = Policy)) + 
        geom_bar(position="stack", stat="identity", fill = Divestment) + 
        ylab("Divestments") +  xlab("Year") + theme_bw() + ylim(0,300) +
        scale_fill_manual(values=c(Divestment)) +
        xlim(c(divestment_min_date,divestment_max_date)) + 
        scale_x_date(date_labels = "%Y", limits=c(divestment_min_date,divestment_max_date)) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 10, 5, 5))
    g1
}

# function to plot cumulative subsidy_removal by date
cumulative_sr_plot = function(subsidy_removal) {
    g1 <- ggplot(subsidy_removal, aes(x = date, y = Policy)) + 
        geom_bar(position="stack", stat="identity", fill = Subsidy_removal) + 
        ylab("Subsidy Removals") +  xlab("Year") + theme_bw() + ylim(0,8) +
        scale_fill_manual(values=c(Subsidy_removal)) +
        xlim(c(subsidy_removal_min_date,subsidy_removal_max_date)) + 
        scale_x_date(date_labels = "%Y", limits=c(subsidy_removal_min_date,subsidy_removal_max_date)) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 10, 5, 5))
    g1
}

## create plotting parameters for map
#cv_pal <- colorFactor(palette = c("#FF0D0D","#FF4E11", "#FF8E15", "#FAB733", "#ACB334", "#69B34C", "#B1B6B9"), country_overview_large$CAT_rating)
cv_pal <- colorFactor(palette = c("#EFEFEF", "#FCDE9C", "#BEC5A9", "#8DA8AD", "#668BA8", "#466A9F", "#2C4B93", "#062A89"), country_overview_large$MTCO2e_cat)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% country_overview_large$ISO3, ]


## Filter data for mapping by selecting only rows with policies > 0
country_overview_large_map = country_overview_large %>% filter(Government_policies_total > 0 & Non_Government_policies_total > 0)

#state_city_breakdown

# create base map 
basemap = leaflet(plot_map) %>% 
    addTiles() %>% 
    addLayersControl(
        position = "topright",
        overlayGroups = c("Cities, States, Regions", "Governmental Policies", "Non-Governmental Policies"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c("Cities, States, Regions", "Governmental Policies", "Non-Governmental Policies")) %>% 
    htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\"><strong>Policy Level/Type</strong><br/></label>');
        }
    ") %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(~-100,-60,~60,70) %>%
    addLegend("bottomright", colors = c("#EFEFEF", "#FCDE9C", "#BEC5A9", "#8DA8AD", "#668BA8", "#466A9F", "#2C4B93", "#062A89"), 
              labels =  c("No data", "< 0", "1-169","169-500","500-1000","1000-5000",  
                          "5000-10000","> 10000"),values = ~country_overview_large$MTCO2e_cat,
              title = "Country Greenhouse<br/>Gas Emissions (MtCO2e)<br/>(CAIT 2016)") %>% 
    
    addPolygons(stroke = FALSE, smoothFactor = 0.4, fillOpacity = 0.75, fillColor = ~cv_pal(country_overview_large$MTCO2e_cat),
                label = sprintf("<strong>%s</strong><br/><small>Greenhouse Gas Emissions: %s MtCO2e</small><br/><small>Climate Risk Index: %s</small><br/><small>Total number of policies: %g</small>", 
                                country_overview_large$Country, country_overview_large$MTCO2e, country_overview_large$cri, country_overview_large$Policy_total) %>% 
                    lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", "font-family" = "Poppins", padding = "3px 8px", "color" = cv_pal),
                    textsize = "15px", direction = "auto")) %>%
    
    addCircleMarkers(data = country_overview_large_map, lat = ~ latitude, lng = ~ longitude, weight = 1, 
                     radius = ~(Government_policies_total)^(0.55),
                     fillOpacity = 0.2, color = Government_policies, group = "Governmental Policies",
                     label = sprintf("<strong>%s</strong><br/>Governmental Policies: %g", country_overview_large_map$Country, 
                                     country_overview_large_map$Government_policies_total) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                         style = list("font-weight" = "normal", "font-family" = "Poppins", padding = "3px 8px", "color" = Government_policies),
                         textsize = "15px", direction = "auto")) %>% 
    
    addCircleMarkers(data = country_overview_large_map, lat = ~ latitude, lng = ~ longitude, weight = 1, 
                     radius = ~(Non_Government_policies_total)^(0.55),
                     fillOpacity = 0.2, color = Non_Government_policies, group = "Non-Governmental Policies",
                     label = sprintf("<strong>%s</strong><br/>Non-Governmental Policies: %g", country_overview_large_map$Country, 
                                     country_overview_large_map$Non_Government_policies_total) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                         style = list("font-weight" = "normal", "font-family" = "Poppins", padding = "3px 8px", "color" = Non_Government_policies),
                         textsize = "15px", direction = "auto")) %>%
    
    addCircleMarkers(data = state_city_breakdown_map, lat = ~ latitude, lng = ~ longitude, weight = 4, 
                     radius = ~(City_region_state_total)^(0.89),
                     fillOpacity = 0.2, color = Cities_regions_states, group = "Cities, States, Regions",
                     label = sprintf("<strong>%s</strong><br/><small>Moratoria, Bans, Limits: %g</small><br/><small>Subsidy Removals: %d</small><br/><small>Divestments: %g</small><br/><small>FF NPT: %g</small>", 
                                     state_city_breakdown_map$State_city_region, state_city_breakdown_map$Moratoria_bans_limits_total,state_city_breakdown_map$Subsidy_removal_total,
                                     state_city_breakdown_map$Divestment_total, state_city_breakdown_map$ffnpt_total) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                         style = list("font-weight" = "normal", "font-family" = "Poppins", padding = "3px 8px", "color" = Cities_regions_states),
                         textsize = "15px", direction = "auto"))

### SHINY UI ###
ui <- bootstrapPage(
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
                                          h3(textOutput("policy_count"), align = "right"),
                                          h5(textOutput("gov_pol_count"), align = "right"),
                                          h5(textOutput("Non_gov_pol_count"), align = "right"),
                                          #h6(textOutput("mbl_count"), align = "right"), # Displays total number of moratoria, bans, and limits on the control panel
                                          #h6(textOutput("div_count"), align = "right"), # Displays total number of divestments on the control panel
                                          #h6(textOutput("sr_count"), align = "right"), # Displays total number of subsidy removals on the control panel
                                          plotOutput("cumulative_mbl_plot", height="185px", width="100%"),
                                          plotOutput("cumulative_div_plot", height="175px", width="100%"),
                                          plotOutput("cumulative_sr_plot", height="175px", width="100%"),
                                          
                            ),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://fossilfueltreaty.org', target="_blank", tags$img(src='output-onlinepngtools.png',height='80',width='80')))
                            
                        )
               ),

               tabPanel("Country Profiles",
                        sidebarLayout(
                            sidebarPanel(width = 2,
                                         selectInput(inputId = "country",
                                                     label = "Select country from drop-down menu to show country profiles",
                                                     choices = c("Select a Country", sort(unique(country_overview_large$Country))),
                                                     selected = c("United Kingdom"),
                                                     multiple = FALSE),
                                         tags$p(
                                             "Rendering of profiles takes a couple of seconds. The data presented here is based on the Fossil Fuel Database ",
                                             "which automatically searches the internet to identify ",
                                             "existing climate change supply-side policies. Click here ",
                                             tags$a(
                                                 href = "https://fossilfueltreaty.org", target="_blank",
                                                 "for more information"
                                             ),
                                             "."
                                         )
                                         
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Overview", tags$article(
                                        htmlOutput("report")
                                    )),
                                    tabPanel("Moratorial, Limits, & Bans", tags$article(
                                        htmlOutput("report1")
                                    )),
                                    tabPanel("Subsidy Removals", tags$article(
                                        htmlOutput("report2")
                                    )),
                                    tabPanel("Divestments", tags$article(
                                        htmlOutput("report3")
                                    ))
                        )
                        )
                        )
               ),
               
               tabPanel("About this site",
                        tags$div(
                            tags$img(src = "FFNPT_Logo.png", width = "80px", height = "80px"),
                            tags$h3("About this app"), 
                            "This site will be updated on a regular basis (weekly). There are several other excellent Climate Change mapping tools available, including those run by", 
                            tags$a(href="https://www.climate-laws.org/#map-section", target="_blank", "the Grantham Research Institute on Climate Change and the Environment"), "and",
                            tags$a(href="http://cait.wri.org/historical/Country%20GHG%20Emissions#", target="_blank", "CAIT Climate Data Explorer."),
                            "Our aim is to complement these resources with a particular focus on supply-side policies, including a Fossil Fuel City Impact Map to help organizations to 
                        identify best practices in the field and facilitate target city choices by provinding a propensity action score.",
                            
                            tags$br(),tags$br(),tags$h3("Background"), 
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
                            "More information on our work is avaialble  ",tags$a(href="https://fossilfueltreaty.org", target="_blank", "Fossil Fuely Treaty. "),
                            "An article discussing our campaign was published in ",tags$a(href="https://mondediplo.com/outsidein/fossil-fuel-disarmament", target="_blank", "Le Monde Diplomatique. "), tags$br(), 
                            tags$br(),tags$h3("Code"), 
                            "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/FUenal/FFNPT_Tracker", target="_blank", "Github."),tags$br(),  
                            tags$br(),tags$h3("Sources"),
                            tags$p("The data presented here is based on the Fossil Fuel Database which automatically searches the internet to identify existing climate change supply-side policies. Click here ",
                            tags$a(href = "https://fossilfueltreaty.org", target="_blank","for more information"),"."), tags$p("We also rely on data from the", tags$a(href="http://cait.wri.org/historical/Country%20GHG%20Emissions#", target="_blank", "CAIT Climate Data Explorer.")),  
                            tags$br(),tags$h3("Author"),
                            "Dr Fatih Uenal, Fellow @ Faculty AI & University of Cambridge",tags$br(),  
                            tags$br(),tags$h3("Contact"),
                            "mars.fatih@gmail.com",tags$br(),
                            tags$h6("Last update: "),h6(paste0(Sys.Date())), tags$br()
                        )
               )
    )          
)

### SHINY SERVER ###

server = function(input, output) {
    
    # Policy tab 
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    output$cumulative_mbl_plot <- renderPlot({
        cumulative_mbl_plot(moratoria_bans_limits)
    })
    
    output$cumulative_div_plot <- renderPlot({
        cumulative_div_plot(divestment)
    })
    output$cumulative_sr_plot <- renderPlot({
        cumulative_sr_plot(subsidy_removal)
    })
    
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
    

    # render report when button clicked
    observeEvent(input$country, {
        country_overview_largeDF <- country_overview_large[country_overview_large$Country == input$country, ]
        moratoria_bans_limitsDF <- moratoria_bans_limits[moratoria_bans_limits$Country == input$country, ]
        subsidy_removalDF <- subsidy_removal[subsidy_removal$Country == input$country, ]
        divestmentDF <- divestment[divestment$Country == input$country, ]
        output$report <- renderUI({
            includeHTML(
                rmarkdown::render(
                    "report_template.Rmd",
                    params = list(
                        selection = input$country,
                        data = country_overview_largeDF,
                        data = country_overview_largeDF,
                        data_mbl = moratoria_bans_limitsDF,
                        data_sr = subsidy_removalDF,
                        data_div = divestmentDF
                    )
                )
            )
        })
    })
    
    
    # render report when button clicked
    observeEvent(input$country, {
        moratoria_bans_limitsDF <- moratoria_bans_limits[moratoria_bans_limits$Country == input$country, ]
        output$report1 <- renderUI({
            includeHTML(
                rmarkdown::render(
                    "report_template1.Rmd",
                    params = list(
                        selection = input$country,
                        data_mbl = moratoria_bans_limitsDF
                    )
                )
            )
        })
    })
 
    # render report when button clicked
    observeEvent(input$country, {
        subsidy_removalDF <- subsidy_removal[subsidy_removal$Country == input$country, ]
        output$report2 <- renderUI({
            includeHTML(
                rmarkdown::render(
                    "report_template2.Rmd",
                    params = list(
                        selection = input$country,
                        data_sr = subsidy_removalDF
                    )
                )
            )
        })
    })
   
    # render report when button clicked
    observeEvent(input$country, {
        divestmentDFN <- divestment_scraped[divestment_scraped$Country == input$country, ]
        output$report3 <- renderUI({
            includeHTML(
                rmarkdown::render(
                    "report_template3.Rmd",
                    params = list(
                        selection = input$country,
                        data_divN = divestmentDFN
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

