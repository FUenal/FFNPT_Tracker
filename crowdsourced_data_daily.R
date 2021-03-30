## Supply-side policies interactive mapping tool: script to periodically scrape and reformat crowdsourced and manually entered policy data
## Dr. Fatih Uenal, Faculty AI <-  Data Science Fellow, last updated March 2021

## Data extracted from "Excel Spreadsheet" data obtained from following website
# "https://docs.google.com/spreadsheets/d/1fPzJdaYUXAquqCap-ssR1yOvCcfC-254lieUsJRoan8/edit#gid=611799167" 
# For more information on this data please contact: XYZ


# load libraries
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(robotstxt)) install.packages("robotstxt", repos = "http://cran.us.r-project.org")
if(!require(googlesheets4)) install.packages("googlesheets4", repos = "http://cran.us.r-project.org")
if(!require(gargle)) install.packages("gargle", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(cronR)) install.packages("cronR", repos = "http://cran.us.r-project.org")
if(!require(miniUI)) install.packages("miniUI", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyFiles)) install.packages("shinyFiles", repos = "http://cran.us.r-project.org")


# Check for webpage scraping legality
#paths_allowed(paths = c("https://docs.google.com/spreadsheets/d/1fPzJdaYUXAquqCap-ssR1yOvCcfC-254lieUsJRoan8/edit#gid=611799167"))

# Provide credentials

token <- token_fetch()

# import googlesheet into R
gs4_auth(email = "mars.fatih@gmail.com")
sheet_mbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fPzJdaYUXAquqCap-ssR1yOvCcfC-254lieUsJRoan8/edit#gid=611799167", sheet = 1)
sheet_sr <- read_sheet("https://docs.google.com/spreadsheets/d/1fPzJdaYUXAquqCap-ssR1yOvCcfC-254lieUsJRoan8/edit#gid=611799167", sheet = 2)
sheet_div <- read_sheet("https://docs.google.com/spreadsheets/d/1fPzJdaYUXAquqCap-ssR1yOvCcfC-254lieUsJRoan8/edit#gid=611799167", sheet = 3)

# select relevant columns from mbl-sheet and store in new csv file
sheet_mbl = sheet_mbl %>% select(c("Country", "City_state_or_province", "Start", "End", "Category", "Fuel_type", "Fuel_subtype",  
                                   "Sources_and_more_info","Sources_2", "Sources_3", "Notes"))
names(sheet_mbl) = c("Country", "City, state, or province", "Start", "End", "Category", "Fuel Type", "Fuel Subtype",  
                     "Sources and more info", "Sources 2", "Sources 3", "Notes")

# select relevant columns from mbl-sheet and store in new csv file
sheet_sr = sheet_sr %>% select(c("Country", "City_state_or_province", "Start", "End", "Fuel_type", "Fuel_subtype", "Description",
                                 "Sources_and_more_info","Sources_2", "Sources_3", "Notes", "Category")) 
names(sheet_sr) = c("Country", "City, state, or province", "Start", "End", "Fuel Type", "Fuel Subtype", "Description",
                    "Sources and more info", "Sources 2", "Sources 3", "Notes", "Category")

# select relevant columns from mbl-sheet and store in new csv file
sheet_div = sheet_div %>% select(c("Country", "City_state_or_province", "Start", "Divestment_type", "Organisation", "Organisation_type", 
                                   "Sources_and_more_info","Sources_2", "Sources_3", "Notes", "Category", ))
names(sheet_div) = c("Country", "City, state, or province", "Start", "Divestment type", "Organisation", "Organisation_type",  
                     "Sources and more info", "Sources 2", "Sources 3", "Notes", "Category") 

# Drop first five rows
sheet_mbl = sheet_mbl[-c(1:3), ]
sheet_sr = sheet_sr[-c(1:3), ]
sheet_div = sheet_div[-c(1:3), ]

# Convert list to character ("Source" column)
sheet_mbl$'Sources 3' <- as.character(sheet_mbl$'Sources 3')
sheet_mbl$Notes <- as.character(sheet_mbl$Notes)
sheet_sr$'Sources 3' <- as.character(sheet_sr$'Sources 3')
sheet_sr$Notes <- as.character(sheet_sr$Notes)
sheet_div$'Divestment type' <- as.character(sheet_div$'Divestment type')
sheet_div$'Sources 3' <- as.character(sheet_div$'Sources 3')
sheet_div$Notes <- as.character(sheet_div$Notes)

# str(sheet_mbl)
# str(sheet_sr)
# str(sheet_div)

# Replace  NULLs with NAs and NAs with with appropriate values
sheet_mbl[sheet_mbl == "NULL"] <- NA
sheet_sr[sheet_sr == "NULL"] <- NA
sheet_div[sheet_div == "NULL"] <- NA

sheet_mbl <- sheet_mbl %>% 
        replace(is.na(.), "--")

sheet_sr <- sheet_sr %>% 
        replace(is.na(.), "--")

sheet_div <- sheet_div %>% 
        replace(is.na(.), "--")

# Convert character to date
sheet_mbl$Start <-lubridate::ymd(sheet_mbl$Start, truncated = 2L)
sheet_sr$Start <-lubridate::ymd(sheet_sr$Start, truncated = 2L)
sheet_mbl$End <-lubridate::ymd(sheet_mbl$End, truncated = 2L)
sheet_sr$End <-lubridate::ymd(sheet_sr$End, truncated = 2L)
sheet_div$Start <-lubridate::ymd(sheet_div$Start, truncated = 2L)

# extract dates from moratoria_bans_limits data
sheet_mbl$Start = as.Date(sheet_mbl$Start, format="%Y")
sheet_sr$Start = as.Date(sheet_sr$Start, format="%Y")
sheet_mbl$End = as.Date(sheet_mbl$End, format="%Y")
sheet_sr$End = as.Date(sheet_sr$End, format="%Y")
sheet_div$Start = as.Date(sheet_div$Start, format="%d/%m/%Y")

# class(sheet_mbl$Start)
# class(sheet_sr$End)
# class(sheet_mbl$Start)
# class(sheet_sr$End)
# class(sheet_div$Start)

# write cs file
write.csv(sheet_mbl, file = "input_data/moratoria_bans_limits_crowdsourced.csv")
write.csv(sheet_sr, file = "input_data/subsidy_removal_crowdsourced.csv")
write.csv(sheet_div, file = "input_data/divestment_crowdsourced.csv")



