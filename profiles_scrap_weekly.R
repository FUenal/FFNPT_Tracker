## RSelenium method of scraping interactive web pages

## Installing RSelenium:
## https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html
## Download the java binary:
## selenium-server-standalone-x.xx.x.jar
## Rename to: selenium-server-standalone.jar 

## Getting Started with RSelenium (necessary for mac users): 
## Open Terminal at Folder containing the Selenium Standalone Server which is the RProjects folder where the script is in as well
## Next, run the following command in the console (at your folder): java -jar selenium-server-standalone.jar -port 5556

## Install / Load packages
if(!require(RSelenium)) install.packages("RSelenium", repos = "https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(robotstxt)) install.packages("robotstxt", repos = "http://cran.us.r-project.org")

## Initialize port for browser (necessary for mac users)
browser <- remoteDriver(port = 5556)

## create remote driver server and client - using "firefox"
remDr <- rsDriver(browser="firefox",version="3.141.59")

## NOTE: If you want to end the selenium server, you must either quit RStudio or use the following code:
## remDr$server$stop()

## extract the client for navigation
rD <- remDr[['client']]

## navigate to page
rD$navigate("https://www.climate-laws.org/geographies/united-states-of-america")

# Check for webpage scraping legality
#paths_allowed(paths = c("https://www.climate-laws.org/geographies/united-states-of-america"))

## how to extract data? navigate the DOM
## Let's work on country demographics first
demoTable <- rD$findElement(using = "css", "body > main > div > div:nth-child(2) > div > div.container > div.columns > main > div > section:nth-child(2) > div > div:nth-child(3)")

demoTableHead <- demoTable$findChildElements(using = "css", "div:nth-child(1)")

demoTableHeadEntries <- map_chr(demoTableHead, function(t) t$getElementAttribute("innerHTML")[[1]])

demoTableRows <- demoTable$findChildElements(using = "css", "div.property-value")

## length(demoTableRows)
## 1310 rows - confirm with webpage

## process a row
demoTableRowEntries <- map(demoTableRows, function(t) {
        entries <- t$findChildElements(using = "css", "div:nth-child(1)")
        Region <- entries[[2]]$getElementAttribute("innerHTML")[[1]]
        Global_Emissions <- entries[[3]]$getElementAttribute('innerHTML')[[1]]
        Global_Climate_Risk_Index <- entries[[4]]$getElementAttribute('innerHTML')[[1]]
        return(data.frame(Region = Region, Global_Emissions = Global_Emissions, Global_Climate_Risk_Index = Global_Climate_Risk_Index))
})

## create larger data.frame to hold results
df <- do.call('rbind', demoTableRowEntries)

## Removing HTML tags and Attributes etc. in column 1
## using stringr for this:
df <- df %>% mutate_at(1, function(t) {
        str_replace(t, pattern = '<a ', replacement = "")
})

df <- df %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'class=\"text-underline-none\"', replacement = "")
})

df <- df %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'href=\"', replacement = "'")
})

df <- df %>% mutate_at(1, function(t) {
        str_replace(t, pattern = '\">', replacement = "' ")
})

df <- df %>% mutate_at(1, function(t) {
        str_replace(t, pattern = '</a>', replacement = '')
})

## Removing HTML tags and Attributes etc. in column 5
## using stringr for this:

df <- df %>% mutate_at(5, function(t) {
        str_replace(t, pattern = '<a ', replacement = "")
})

df <- df %>% mutate_at(5, function(t) {
        str_replace(t, pattern = 'href=\"', replacement = "'")
})

df <- df %>% mutate_at(5, function(t) {
        str_replace(t,  pattern = '\">→</a>', replacement = "'")
})

## Add Column value for Policy count

df$Policy <- 1 
        
## drop first column
#df <- df %>% select(-X)
#str(df)
#head(df)

## create csv file for usage
write.csv(df, file = "input_data/divestment_scrape.csv")
df <- read.csv("input_data/divestment_scrape.csv")

## This becomes more powerful when you go to other pages and take advantage of the same structure:
## First show navigation using the webpage, then show automation - steps below

## Want to do to things (demonstrate in window):
## (1) Select another state
## (2) Select county within state

## select state select input link
## this takes some time and experimentation
## in general, looking for input and a tags
stateSelectLink <- rD$findElement(using = "css", "#app-main > div.app-header.breadcrumb > div > div.app-state-select.ui-select-container.select2.select2-container.ng-empty.ng-valid > a")

## make search box visible
stateSelectLink$clickElement()

## click the div for the state we want:
rD$executeScript("jQuery('#ui-select-choices-1 > li:contains(\"Indiana\")').click();")

## select county select input
countySelectLink <- rD$findElement(using = "css", "#main > div.ng-scope > div > a")

## from the copy css in browser
## main > div.ng-scope > div > a

## WAIT - the countySelect can show up in one of two ways - see function below
countySelectLink <- rD$findElement(using = "css", 
                                   
                                   "#app-wrapper > div.app-header.nav.clearfix.ng-scope > div:nth-child(2) > div > a")


## make search box visible
countySelectLink$clickElement()

## change value and update

## click the div for the county we want - more JS:
rD$executeScript("jQuery('#ui-select-choices-4 > li:contains(\"Marion\")').click();")

## now grab demographic again:

demoTable <- rD$findElement(using = "css", "#main-inner-wrapper > div.ng-scope.demographics > div > table")

demoTableHead <- demoTable$findChildElements(using = "css", "thead > tr > th")

demoTableHeadEntries <- map_chr(demoTableHead, function(t) t$getElementAttribute("innerHTML")[[1]])
## &nbsp; is HTML speak for a 'space'

demoTableRows <- demoTable$findChildElements(using = "css", "tbody > tr")

## 12 rows - confirm with webpage

## proccess a row
demoTableRowEntries <- map(demoTableRows, function(t) {
        entries <- t$findChildElements(using = "css", "td")
        ## first entry has a span and a element within it:
        name <- entries[[1]]$findChildElements(using = 'css', 'a')[[1]]$getElementAttribute("innerHTML")[[1]]
        county <- entries[[3]]$getElementAttribute('innerHTML')[[1]]
        state <- entries[[4]]$getElementAttribute('innerHTML')[[1]]
        return(data.frame(header = name, county = county, state = state))
})

## create larger data.frame to hold results
dfDemo <- do.call('rbind', demoTableRowEntries)
dfDemo

## time for a function!
## run after starting selenium server with rsDriver()
extractDemographics <- function(state = "Indiana", county = "Marion")
{
        rD$navigate("http://www.countyhealthrankings.org/app/texas/2018/rankings/marion/county/outcomes/overall/snapshot")
        
        stateSelectLink <- rD$findElement(using = "css", "#app-main > div.app-header.breadcrumb > div > div.app-state-select.ui-select-container.select2.select2-container.ng-empty.ng-valid > a")
        
        ## make search box visible
        stateSelectLink$clickElement()
        
        ## click the div for the state we want:
        rD$executeScript(paste0("jQuery('#ui-select-choices-1 > li:contains(\"", state, "\")').click();"))
        
        Sys.sleep(1)
        ## select county select input
        ## need error catching here
        countySelectLink <- tryCatch({
                rD$findElement(using = "css", "#app-wrapper > div.app-header.nav.clearfix.ng-scope > div:nth-child(2) > div > a")}, 
                error = function(e) {"not found!"})
        
        ## this link shows up one of two ways - try both
        i <- 1
        
        while (identical(countySelectLink, "not found!"))
        {
                Sys.sleep(1)
                if (i %% 2 == 1)
                {
                        countySelectLink <- tryCatch({
                                rD$findElement(using = "css", "#main > div.ng-scope > div > a")
                        }, error = function(e) {"not found!"})      
                } else {
                        countySelectLink <- tryCatch({
                                rD$findElement(using = "css", "#app-wrapper > div.app-header.nav.clearfix.ng-scope > div:nth-child(2) > div > a")}, 
                                error = function(e) {"not found!"})      
                }
                i <- i + 1
        }
        
        ## make search box visible
        countySelectLink$clickElement()
        
        ## change value and update
        
        ## click the div for the county we want - more JS:
        
        ## Notice - this link can also change, i.e. the appended value at the end
        ## try all up to 4
        rD$executeScript(paste0("jQuery('#ui-select-choices > li:contains(\"", county, "\")').click();"))
        rD$executeScript(paste0("jQuery('#ui-select-choices-2 > li:contains(\"", county, "\")').click();"))
        rD$executeScript(paste0("jQuery('#ui-select-choices-3 > li:contains(\"", county, "\")').click();"))
        rD$executeScript(paste0("jQuery('#ui-select-choices-4 > li:contains(\"", county, "\")').click();"))
        
        ## now grab demographic again:
        Sys.sleep(1)
        
        ## notice that it takes time to load - can get an error if we're not careful!
        demoTable <- tryCatch(rD$findElement(using = "css", "#main-inner-wrapper > div.ng-scope.demographics > div > table"), error = function(e) {"not found!"})
        
        while(identical(demoTable, "not found!"))
        {
                Sys.sleep(1)
                demoTable <- tryCatch(rD$findElement(using = "css", "#main-inner-wrapper > div.ng-scope.demographics > div > table"), error = function(e) {"not found!"})
        }
        
        demoTableHead <- demoTable$findChildElements(using = "css", "thead > tr > th")
        
        demoTableHeadEntries <- map_chr(demoTableHead, function(t) t$getElementAttribute("innerHTML")[[1]])
        ## &nbsp; is HTML speak for a 'space'
        
        demoTableRows <- demoTable$findChildElements(using = "css", "tbody > tr")
        
        ## 12 rows - confirm with webpage
        
        ## proccess a row
        demoTableRowEntries <- map(demoTableRows, function(t) {
                entries <- t$findChildElements(using = "css", "td")
                ## first entry has a span and a element within it:
                name <- entries[[1]]$findChildElements(using = 'css', 'a')[[1]]$getElementAttribute("innerHTML")[[1]]
                county <- entries[[3]]$getElementAttribute('innerHTML')[[1]]
                state <- entries[[4]]$getElementAttribute('innerHTML')[[1]]
                return(data.frame(header = name, county = county, state = state))
        })
        
        ## create larger data.frame to hold results
        dfDemo <- do.call('rbind', demoTableRowEntries)
        dfDemo
        
}

## testing function
extractDemographics("Indiana", "Hamilton")
extractDemographics("Indiana", "Marion")
extractDemographics("Indiana", "Martin")
extractDemographics("Alabama", "Barbour")

## now, scale it up:
## let's extract all counties from Indiana from the web page, then iterate:
## if you already have all counties in your R session, you don't need this
## I just want to show the process of using extractDemographics on all Indy counties - it works fairly quickly!
rD$navigate("http://www.countyhealthrankings.org/app/texas/2018/rankings/marion/county/outcomes/overall/snapshot")

stateSelectLink <- rD$findElement(using = "css", "#app-main > div.app-header.breadcrumb > div > div.app-state-select.ui-select-container.select2.select2-container.ng-empty.ng-valid > a")

## make search box visible
stateSelectLink$clickElement()

## click the div for the state we want:
rD$executeScript(paste0("jQuery('#ui-select-choices-1 > li:contains(\"Indiana\")').click();"))

Sys.sleep(1)
## select county select input
## need error catching here
countySelectLink <- tryCatch({
        rD$findElement(using = "css", "#app-wrapper > div.app-header.nav.clearfix.ng-scope > div:nth-child(2) > div > a")}, 
        error = function(e) {"not found!"})

## this link shows up one of two ways - try both
i <- 1

while (identical(countySelectLink, "not found!"))
{
        Sys.sleep(1)
        if (i %% 2 == 1)
        {
                countySelectLink <- tryCatch({
                        rD$findElement(using = "css", "#main > div.ng-scope > div > a")
                }, error = function(e) {"not found!"})      
        } else {
                countySelectLink <- tryCatch({
                        rD$findElement(using = "css", "#app-wrapper > div.app-header.nav.clearfix.ng-scope > div:nth-child(2) > div > a")}, 
                        error = function(e) {"not found!"})      
        }
        i <- i + 1
}

## make search box visible
countySelectLink$clickElement()

## extract all county names
## remember the selector can be #ui-select-choices, #ui-select-choices-3, etc.
counties <- rD$findElements(using = "css", "#ui-select-choices-3 > li > div > span")

counties <- map_chr(counties, function(t) {t$getElementAttribute("innerHTML")[[1]]})

## iterate over counties:
dfList <- map(counties[1:5], function(t) {extractDemographics("Indiana", t)})

## Fun, but what next? We likely want more than demographics
extractHB <- function(state = "Indiana", county = "Marion")
{
        rD$navigate("http://www.countyhealthrankings.org/app/texas/2018/rankings/marion/county/outcomes/overall/snapshot")
        
        stateSelectLink <- rD$findElement(using = "css", "#app-main > div.app-header.breadcrumb > div > div.app-state-select.ui-select-container.select2.select2-container.ng-empty.ng-valid > a")
        
        ## make search box visible
        stateSelectLink$clickElement()
        
        ## click the div for the state we want:
        rD$executeScript(paste0("jQuery('#ui-select-choices-1 > li:contains(\"", state, "\")').click();"))
        
        Sys.sleep(1)
        ## select county select input
        ## need error catching here
        countySelectLink <- tryCatch({suppressMessages({
                rD$findElement(using = "css", "#app-wrapper > div.app-header.nav.clearfix.ng-scope > div:nth-child(2) > div > a")})}, 
                error = function(e) {"not found!"})
        
        ## this link shows up one of two ways - try both
        i <- 1
        
        while (identical(countySelectLink, "not found!"))
        {
                Sys.sleep(1)
                if (i %% 2 == 1)
                {
                        countySelectLink <- tryCatch({suppressMessages({
                                rD$findElement(using = "css", "#main > div.ng-scope > div > a")
                        })}, error = function(e) {"not found!"})      
                } else {
                        countySelectLink <- tryCatch({suppressMessages({
                                rD$findElement(using = "css", "#app-wrapper > div.app-header.nav.clearfix.ng-scope > div:nth-child(2) > div > a")})}, 
                                error = function(e) {"not found!"})      
                }
                i <- i + 1
        }
        
        ## make search box visible
        countySelectLink$clickElement()
        
        ## change value and update
        
        ## click the div for the county we want - more JS:
        
        ## Notice - this link can also change, i.e. the appended value at the end
        ## try all up to 4
        rD$executeScript(paste0("jQuery('#ui-select-choices > li:contains(\"", county, "\")').click();"))
        rD$executeScript(paste0("jQuery('#ui-select-choices-2 > li:contains(\"", county, "\")').click();"))
        rD$executeScript(paste0("jQuery('#ui-select-choices-3 > li:contains(\"", county, "\")').click();"))
        rD$executeScript(paste0("jQuery('#ui-select-choices-4 > li:contains(\"", county, "\")').click();"))
        
        ## now grab demographic again:
        Sys.sleep(1)
        
        ## notice that it takes time to load - can get an error if we're not careful!
        HBtbody <- tryCatch({suppressMessages({rD$findElement(using = "css", "#main-inner-wrapper > div.content.ng-scope > div > table > tbody.component-body.component-id-3-body")})}, error = function(e) {"not found!"})
        
        while(identical(HBtbody, "not found!"))
        {
                Sys.sleep(1)
                HBtbody <- tryCatch({supressMessages({rD$findElement(using = "css", "#main-inner-wrapper > div.content.ng-scope > div > table > tbody.component-body.component-id-3-body")})}, error = function(e) {"not found!"})
        }
        
        HBRows <- HBtbody$findChildElements(using = "css", "tr ")
        
        ## 12 rows - confirm with webpage
        
        ## proccess a row
        HBRowEntries <- map(HBRows[1:9], function(t) {
                entries <- t$findChildElements(using = "css", "td")
                ## first entry has a span and a element within it:
                ## td 1, 3, and 7 are what we want
                name <- entries[[1]]$findChildElement(using = "css", "a > span")$getElementAttribute("innerHTML")[[1]]
                ## county is sometimes div > span (if no %), otherwise div > div
                countyVal <- tryCatch({suppressMessages({entries[[3]]$findChildElement(using = "css", 
                                                                                       "div > div")$getElementAttribute('innerHTML')[[1]]})}, error = function(e) {"not found!"})
                if (identical(countyVal, "not found!"))
                {
                        countyVal <- tryCatch({suppressMessages({entries[[3]]$findChildElement(using = "css", 
                                                                                               "div > span")$getElementAttribute('innerHTML')[[1]]})}, error = function(e) {"not found!"})
                }
                stateVal <- entries[[6]]$getElementAttribute('innerHTML')[[1]]
                return(data.frame(header = name, county_value = countyVal, state = stateVal))
        })
        
        ## create larger data.frame to hold results
        dfHB <- do.call('rbind', HBRowEntries)
        dfHB$county <- county
        dfHB
        
}
## Notice use of suppressMessages() to get rid of warning messages that we don't need 
## since we've fixed the issues with tryCatch()


extractHB("Indiana", "Marion")
extractHB("Indiana", "Hamilton")

## again, iterate over all counties in Indiana:
dfHBList <- map(counties[1:10], function(t) {extractHB("Indiana", t)})