## Supply-side policies interactive mapping tool: script to scrape and reformat "Climate Action Tracker" data from scratch
## Dr. Fatih Uenal, Faculty AI <-  Data Science Fellow (mars.fatih@gmail.com), last updated February 2021

## data extracted from "Climate Action Tracker" data obtained from following website
# https://climateactiontracker.org


# load libraries
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")

