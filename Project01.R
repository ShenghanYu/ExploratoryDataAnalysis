##set local work directory 
setwd("./ExploratoryDataAnalysis")

##installing all the required packages to read xlsx files
library(readxl)

##reading the file from your local directory
df = read_excel("Survey.xlsx")
