## Supply-side policies interactive mapping tool: script to scrape and reformat "Divestment" data from scratch
## Dr. Fatih Uenal, Faculty AI <-  Data Science Fellow (mars.fatih@gmail.com), last updated March 2021

## Data extracted from "Go fossil free." data obtained from following website
# https://docs.google.com/spreadsheets/d/1ScfyuRP-CjoEZqq-MYho0CAWirCyWUhzsaBpEZ1uXZY/edit#gid=611799167" 
# For more information on this data see: https://gofossilfree.org/divestment/commitments/


# load libraries
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(robotstxt)) install.packages("robotstxt", repos = "http://cran.us.r-project.org")
if(!require(googlesheets4)) install.packages("googlesheets4", repos = "http://cran.us.r-project.org")
if(!require(DataCombine)) install.packages("DataCombine", repos = "http://cran.us.r-project.org")

# Check for webpage scraping legality
#paths_allowed(paths = c("https://docs.google.com/spreadsheets/d/1ScfyuRP-CjoEZqq-MYho0CAWirCyWUhzsaBpEZ1uXZY/edit#gid=611799167"))

# import googlesheet into R
sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1ScfyuRP-CjoEZqq-MYho0CAWirCyWUhzsaBpEZ1uXZY/edit#gid=611799167")

# select relevant columns from sheet and store in new csv file
divestment_scraped = sheet %>% select(c("Type of Divestment", "Organization", "Org_Type","Country", "City", "Announcement")) #, "Date of Record"
names(divestment_scraped) = c("Type", "Organisation", "Organisation_type", "Country", "City", "Source") #,  "Start"

# Drop first five rows
divestment_scraped = divestment_scraped[-c(1:5), ]

# Add Category column: Divestment
divestment_scraped$Category <- "Divestment"
divestment_scraped = divestment_scraped[c(7, 1:6)]

# Convert list to character ("Source" column)
divestment_scraped$Source <- as.character(divestment_scraped$Source)
divestment_scraped$Type <- as.character(divestment_scraped$Type)
divestment_scraped$Organisation <- as.character(divestment_scraped$Organisation)
divestment_scraped$Organisation_type <- as.character(divestment_scraped$Organisation_type)
divestment_scraped$Country <- as.character(divestment_scraped$Country)
divestment_scraped$City <- as.character(divestment_scraped$City)

# Replace  NULLs with NAs and NAs with with appropriate values
divestment_scraped[divestment_scraped == "NULL"] <- NA

divestment_scraped <- divestment_scraped %>% 
        replace(is.na(.), "--")

#divestment_scraped$Source[is.na(divestment_scraped$Source)] = 0

# write cs file
write.csv(divestment_scraped, file = "input_data/divestment_scraped.csv")

# Add ISOs
#divestment_scraped['ISO3'] <- country_overview$ISO3[match(country_overview$Country, moratoria_bans_limits$Country)]

# Adjust Country names




