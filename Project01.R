# Set local work directory 
setwd("./ExploratoryDataAnalysis")

# Installing all the required packages to read CSV files
library(RCurl)

# Read  file from GitHub repo
CSV_url <- getURL("https://raw.githubusercontent.com/Amirosimani/ExploratoryDataAnalysis/master/Survey.csv")
DataFrame <- read.csv(text = CSV_url)

# Delete empty columns in data frame
DataFrame <- Filter(function(x)!all(is.na(x)), DataFrame)

" Split tools in column 3
sapply(df, strsplit(df$'Experiences with tools', ","))
 "      
