# Set local work directory 
setwd("./ExploratoryDataAnalysis")

# Installing all the required packages to read xlsx files
library(readxl)

# Read  file from the local directory
# to change: read it from GitHub instead of local 
df = read_excel("Survey.xlsx")

# Delete empty columns 
df <- Filter(function(x)!all(is.na(x)), df)

" Split tools in column 3
sapply(df, strsplit(df$'Experiences with tools', ","))
 "      