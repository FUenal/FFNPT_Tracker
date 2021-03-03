# Unused Code chunks Tracker

# import iso country codes
url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
iso_codes <- url %>%
        read_html() %>%
        html_nodes(xpath = '//*[@id="CountryCode"]') %>%
        html_table()
iso_codes <- iso_codes[[1]][, -1]
iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")
head(iso_codes)
write.csv(iso_codes, file = "input_data/iso_codes.csv")
iso_codes = read.csv("input_data/iso_codes.csv")

country_iso = read_xlsx("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 8)

# Load world data with geographical coordinates directly from the ggplot2 package. These data contain geographical coordinates of all countries worldwide, 
# which we’ll later need to plot the worldmaps.
library(maps)
library(ggplot2)
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
head(world_data)
write.csv(world_data, file = "input_data/world_data.csv")


# Rename Alpha into ISO
countries <- countries %>% rename(ISO2 = alpha2, ISO3 = alpha3)
countries$X <- NULL
write.csv(countries, file = "input_data/countries_codes_and_coordinates.csv")
countries <- read.csv("input_data/countries_codes_and_coordinates.csv")
head(countries)

write.csv(country_overview, file = "input_data/country_overview_full.csv")
head(country_overview)
countries = read.csv("input_data/country_overview_full.csv")

## Transfer total number and breakdowns of policies to country_overview_large file
## Total number of policies
country_overview_large['Moratoria_bans_limits'] <- sum(moratoria_bans_limits$c)[match(country_overview_large$ISO3, moratoria_bans_limits$ISO3)]
country_overview_large['Subsidy_removal'] <- sum(subsidiy_removal$a)[match(country_overview_large$ISO3, subsidiy_removal$ISO3)]
country_overview_large['Divestment'] <- divestment$d[match(country_overview_large$ISO3, divestment$ISO3)]
country_overview_large <- country_overview_large %>% mutate(policy_total = country_overview_large$Moratoria_bans_limits+country_overview_large$Subsidy_removal +
                                                                    country_overview_large$Divestment)
# create plotting parameters for map
bins = c(0,10,50,100,500) 
cv_pal <- colorBin("Greens", domain =  country_overview_large$policy_total, bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% country_overview_large$ISO3, ]

bins = c("No Rating", "Critically Insufficient","Highly Insufficient","Insufficient","2°C Compatible","1.5°C Paris Agreement Compatible", "Role Model") 
cv_pal <- colorFactor(palette = c("#B1B6B9","#686464", "#EF6948", "#EF9D24", "#F3DD3F", "#FFFFFF", "#568C45"), domain =  country_overview_large$CAT_rating)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% country_overview_large$ISO3, ]


# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
cls_names = c(as.character(unique(country_overview_large$Country)), as.character(unique(country_overview_large$continent_level)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names

#ä Polygons with types of policies instead of gov vs. non-gov policies
addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4, fillColor = ~cv_pal(country_overview_large$CAT_rating),
            label = sprintf("<strong>%s</strong><br/>CAT Rating: %s<br/>Moratoria, bans, & limits: %g<br/>Subsidy Removals: %d<br/>Divestments: %g", 
                            country_overview_large$Country, country_overview_large$CAT_rating, country_overview_large$Moratoria_bans_limits_total, 
                            country_overview_large$Subsidy_removal_total, 
                            country_overview_large$Divestment_total) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(
                    style = list("font-weight" = "normal", "font-family" = "Roboto", padding = "3px 8px", "color" = cv_pal),
                    textsize = "15px", direction = "auto"))

## Basemap with types of policies as layers and hovering with gov vs. non-gov policies
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
                                    label = sprintf("<strong>%s</strong><br/>CAT Rating: %s<br/>Moratoria, bans, & limits: %g<br/>Subsidy Removals: %d<br/>Divestments: %g", 
                                                    country_overview_large$Country, country_overview_large$CAT_rating, country_overview_large$Moratoria_bans_limits, 
                                                    country_overview_large$Subsidy_removal, country_overview_large$Divestment) %>% lapply(htmltools::HTML),
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
                                "existing climate change supply-side policies. Click here ",
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

###################################################
### SHINY SERVER ###

server = function(input, output, session) {
        
        # Policy tab 
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
        
}

runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)

## Deploying app to shiny.io
#library(rsconnect)
#rsconnect::deployApp(account="fuenal")

## Mapping with reactive dates
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

reactive_case_count <- sum(country_overview_large$policy_total)
clean_date_reactive <- Sys.Date()


### MAP FUNCTIONS ###
# function to plot cumulative Moratoria, Bans, and Limit Policies by date
cumulative_mbl_plot = function(moratoria_bans_limits, plot_date) {
        plot_df = subset(moratoria_bans_limits, date<=plot_date)
        g1 <- ggplot(moratoria_bans_limits, aes(x = date, y = Policy)) + 
                geom_bar(position="stack", stat="identity", fill = Moratoria_bans_limits) + 
                ylab("Number of Moratoria, Bans, and Limit Policies") +  xlab("Year") + theme_bw() + ylim(0,50) +
                scale_fill_manual(values=c(Moratoria_bans_limits)) +
                xlim(c(moratoria_bans_limits_min_date,moratoria_bans_limits_max_date)) + 
                scale_x_date(date_labels = "%Y", limits=c(moratoria_bans_limits_min_date,moratoria_bans_limits_max_date)) +
                theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
                      plot.margin = margin(5, 5, 5, 5))
        g1
}

# function to plot cumulative Divestments by date
cumulative_div_plot = function(divestment, plot_date) {
        plot_df = subset(divestment, date<=plot_date)
        g1 <- ggplot(divestment, aes(x = date, y = Policy)) + 
                geom_bar(position="stack", stat="identity", fill = Divestment) + 
                ylab("Number of Divestments") +  xlab("Year") + theme_bw() + ylim(0,500) +
                scale_fill_manual(values=c(Divestment)) +
                xlim(c(divestment_min_date,divestment_max_date)) + 
                scale_x_date(date_labels = "%Y", limits=c(divestment_min_date,divestment_max_date)) +
                theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
                      plot.margin = margin(5, 5, 5, 5))
        g1
}

# function to plot cumulative subsidiy_removal by date
cumulative_sr_plot = function(subsidiy_removal, plot_date) {
        plot_df = subset(subsidiy_removal, date<=plot_date)
        g1 <- ggplot(subsidiy_removal, aes(x = date, y = Policy)) + 
                geom_bar(position="stack", stat="identity", fill = Subsidy_removal) + 
                ylab("Number of Subsidy Removals") +  xlab("Year") + theme_bw() + ylim(0,10) +
                scale_fill_manual(values=c(Subsidy_removal)) +
                xlim(c(subsidiy_removal_min_date,subsidiy_removal_max_date)) + 
                scale_x_date(date_labels = "%Y", limits=c(subsidiy_removal_min_date,subsidiy_removal_max_date)) +
                theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
                      plot.margin = margin(5, 5, 5, 5))
        g1
}


})
output$cumulative_mbl_plot <- renderPlot({
        cumulative_mbl_plot(moratoria_bans_limits, formatted_date())
})

output$cumulative_div_plot <- renderPlot({
        cumulative_div_plot(divestment, formatted_date())
})
}

runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)


## Tabs in the website

tabPanel("Country Profiles",
         sidebarLayout(
                 sidebarPanel(width = 2,
                              pickerInput("region_select", "Country:",   
                                          choices = as.character(country_overview_large$Country), 
                                          options = list(`none-selected-text` = "Please make a selection!"),
                                          selected = as.character(country_overview_large$Country)["United Kingdom"],
                                          multiple = FALSE),
                              "Select country from drop-down menues to update geographical information.",
                 ),
                 
                 mainPanel(
                         tabsetPanel(
                                 tags$div(
                                         tags$h2("Country Name & Flag"),
                                         tags$br(),tags$br(),tags$h3("Overview & Context"), 
                                         tags$h4("Total number of supply-side policies:"), tags$br(), 
                                         tags$b("Moratoria, Bans, Limitations: "), "99",tags$br(),
                                         tags$b("Subsidy Removals: "), "13 ",tags$br(),
                                         tags$b("Number of Divestments: "), "213",tags$br(),
                                         
                                         tags$br(),tags$br(),
                                         tags$h4("Breakdown of policies across sectors (government vs. non-government) and levels (country vs. state/city vs. organization): "), tags$br(), 
                                         tags$b("Number of national level governmental policies: "), "99 ",tags$br(),
                                         tags$b("Number of state/city governmental level policies: "), "13 ",tags$br(),
                                         tags$b("Number of non-governmental policies: "), "213",tags$br(),
                                         tags$br(),
                                         tags$br(),tags$br(),tags$h4("Sources"), 
                                         tags$br(),tags$br(),tags$h4("Groups to contact"), tags$br(), tags$br(), 
                                         tags$div(
                                                 infoBox("All policies", 10 * 200, icon = icon("list")),
                                                 infoBox("Moratoria, bans, & limits", 30 * 2, icon = icon("list")),
                                                 infoBox("Subsidy removals", 40 * 2, icon = icon("list")),
                                                 infoBox("Divestment", 90 * 200, icon = icon("list")),
                                         ),
                                 )
                         )
                 )
         )
),
tabPanel("Country comparisons",
         
         sidebarLayout(
                 sidebarPanel(width = 2,
                              pickerInput("region_select", "Country:",   
                                          choices = as.character(country_overview_large$Country), 
                                          options = list(`none-selected-text` = "Please make a selection!"),
                                          selected = as.character(country_overview_large$Country)["United Kingdom"],
                                          multiple = FALSE),
                              "Select country from drop-down menues to update geographical information.",
                              radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                           inline = TRUE),
                              downloadButton('downloadReport')
                 ),
                 
                 mainPanel(plotlyOutput("comparison_plot"), width = 6)
         )
),

tabPanel("Country Profiles",
         sidebarLayout(
                 sidebarPanel(
                         
                         span(tags$i(h6("Reported numbers of policies are subject to variation in policy types between countries.")), style="color:#045a8d"),
                         
                         pickerInput("level_select", "Level:",   
                                     choices = c("Global", "Continent", "Country", "Sub-national"), 
                                     selected = c("Country"),
                                     multiple = FALSE),
                         
                         pickerInput("region_select", "Country/Region:",   
                                     choices = as.character(country_overview_large$Country), 
                                     options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                     selected = as.character(country_overview_large$country)[1:10],
                                     multiple = TRUE), 
                         
                         pickerInput("outcome_select", "Policy type:",   
                                     choices = c("Policies (total)", "Moratoria, Bans, Limitations", "Subsidy Removals", "Divestments"), 
                                     selected = c("Policies (total)"),
                                     multiple = FALSE),
                         
                         "Select level, regions, and policy types from drop-down menues to update geographical information."
                 ),
                 
                 mainPanel(
                         tabsetPanel(
                                 
                                 tags$div(
                                         tags$h2("Country Name & Flag"),
                                         tags$br(),tags$br(),tags$h3("Overview & Context"), 
                                         tags$h4("Total number of supply-side policies:"), tags$br(), 
                                         tags$b("Moratoria, Bans, Limitations: "), "99",tags$br(),
                                         tags$b("Subsidy Removals: "), "13 ",tags$br(),
                                         tags$b("Number of Divestments: "), "213",tags$br(),
                                         
                                         tags$br(),tags$br(),
                                         tags$h4("Breakdown of policies across sectors (government vs. non-government) and levels (country vs. state/city vs. organization): "), tags$br(), 
                                         tags$b("Number of national level governmental policies: "), "99 ",tags$br(),
                                         tags$b("Number of state/city governmental level policies: "), "13 ",tags$br(),
                                         tags$b("Number of non-governmental policies: "), "213",tags$br(),
                                         tags$br(),
                                         tags$br(),tags$br(),tags$h4("Sources"), 
                                         tags$br(),tags$br(),tags$h4("Groups to contact"), 
                                 )
                         )
                 )
         )
),

## RSelenium Sraping Divestment
## Testing out string processing
dfDemo2$Name_of_Organisation[1] <- str_replace(dfDemo2$Name_of_Organisation[1], pattern = '<a ', replacement = "")
dfDemo2$Name_of_Organisation[1] <- str_replace(dfDemo2$Name_of_Organisation[1], pattern = 'class=\"text-underline-none\"', replacement = "")
dfDemo2$Name_of_Organisation[1] <- str_replace(dfDemo2$Name_of_Organisation[1], pattern = ' href=\"', replacement = "'")
dfDemo2$Name_of_Organisation[1] <- str_replace(dfDemo2$Name_of_Organisation[1], pattern = ' ', replacement = '[')
dfDemo2$Name_of_Organisation[1] <- str_replace(dfDemo2$Name_of_Organisation[1], pattern = '\">', replacement = "' ")
dfDemo2$Name_of_Organisation[1] <- str_replace(dfDemo2$Name_of_Organisation[1], pattern = ' ', replacement = '[')
dfDemo2$Name_of_Organisation[1] <- str_replace(dfDemo2$Name_of_Organisation[1], pattern = '</a>', replacement = '')
dfDemo2$Name_of_Organisation[1]

#######
dfDemo2 <- dfDemo2 %>% mutate_at(1, function(t) {
        str_replace(t, pattern = '<a ', replacement = "")
        str_replace(t, pattern = 'class=\"text-underline-none\"', replacement = "")
        str_replace(t, pattern = 'href=', replacement = "")
        str_replace(t, pattern = ' \"', replacement = "[")
        str_replace(t, pattern = '\">', replacement = "]")
        str_replace(t, pattern = ' ', replacement = '(')
        str_replace(t, pattern = '</a>', replacement = ')')
})

## Same as above for column 5

dfDemo2 <- read.csv("input_data/divestment_scrape.csv")
dfDemo2 <- dfDemo2 %>% select(-X)
str(dfDemo2)
head(dfDemo2$More_Info)
tail(dfDemo2$More_Info)
print(dfDemo2$More_Info)

dfDemo2$More_Info[1] <- str_replace(dfDemo2$More_Info[1], pattern = '<a ', replacement = "")
#dfDemo2$More_Info[1] <- str_replace(dfDemo2$More_Info[1], pattern = 'class=\"text-underline-none\"', replacement = "")
dfDemo2$More_Info[1] <- str_replace(dfDemo2$More_Info[1], pattern = 'href=\"', replacement = "'")
#dfDemo2$More_Info[1] <- str_replace(dfDemo2$More_Info[1], pattern = ' ', replacement = '[')
dfDemo2$More_Info[1] <- str_replace(dfDemo2$More_Info[1], pattern = '\">→</a>', replacement = "'")
dfDemo2$More_Info[1] <- str_replace(dfDemo2$More_Info[1], pattern = ' ', replacement = '[')
dfDemo2$More_Info[1] <- str_replace(dfDemo2$More_Info[1], pattern = '</a>', replacement = '')
dfDemo2$More_Info[1]


# , data_mbl(), data_sr(), data_div())

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
                                        data_overview = country_overview_largeDF,
                                        data_mbl = moratoria_bans_limitsDF,
                                        data_sr = subsidiy_removalDF,
                                        data_div = divestmentDF
                                )
                        )
                )
        })
})
}


