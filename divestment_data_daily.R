## Supply-side policies interactive mapping tool: script to scrape and reformat "Divestment" data from scratch
## Dr. Fatih Uenal, Faculty AI <-  Data Science Fellow (mars.fatih@gmail.com), last updated March 2021

## Data extracted from "Go fossil free." data obtained from following website
# https://gofossilfree.org/divestment/commitments/


# load libraries
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(robotstxt)) install.packages("robotstxt", repos = "http://cran.us.r-project.org")

# Check for webpage scraping legality
#paths_allowed(paths = c("https://www.climate-laws.org/geographies/united-states-of-america"))

# import webpage into R
url <- "https://www.climate-laws.org/geographies/united-states-of-america"
h <- read_html(url)
class(h)
h

#divestment-commitments 
tab <- h %>% html_nodes(css =  'body > main > div > div:nth-child(2) > div > div.container > div.title-page') 
tab 

tab$Country <- tab %>% html_text()
class(tab)

tab <- tab %>% setNames(c("Name of Organization", "Type", "Country", "Divestment Type", "More Info"))
head(tab$Country)

##
tab <- h %>% html_nodes(css =  'body > main > div > div:nth-child(2) > div > div.container > div.columns > main > div > section:nth-child(2)') 
tab 

tab <- tab %>% html_text()
class(tab)
head(tab)



