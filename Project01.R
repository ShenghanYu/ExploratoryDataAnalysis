# Installing all the required packages
needed.libraries <- c("RCurl", "data.table", "ggplot2")
install.packages(needed.libraries, dependencies = TRUE)

# Run packages
library(RCurl)   #to read CSV
library(data.table)  #Data table manipulation
library(ggplot2)

# Read  file from GitHub repo
CSV_url <- getURL("https://raw.githubusercontent.com/Amirosimani/ExploratoryDataAnalysis/master/Survey.csv")
DataFrame <- read.csv(text = CSV_url)

## Tidying the data frame
DataFrame <- Filter(function(x)!all(is.na(x)), DataFrame) #Delete empty columns
colnames(DataFrame) <- c("waitlist","degree","tools", 
                         "R_DataManipulation", "pronoun","editor","R_Graphics",
                         "R_MultiVariate","Markdown","Matlab_DataManipulation","GitHub") #Rename columns name
# cleaning up text
DataFrame$tools = gsub("\\(formerly docs\\)", "", DataFrame$tools)
DataFrame$tools =  gsub("\\(grep\\)", "", gsub("\\(terminal \\/ command line\\)","", DataFrame$tools))
                    
#splitting programs into a list
tools <- strsplit(as.character(DataFrame$tools),',') 
setDT(DataFrame)[, paste0("tools", 1:16) := tstrsplit(tools, ",")]   #Add seperate column for each program

# Converting columns to factor
col_names <- names(DataFrame)
DataFrame[,col_names] <- lapply(DataFrame[,col_names] , factor)

#producing graphs
c <- ggplot(DataFrame, aes(factor(DataFrame$waitlist)))
c + geom_bar(width=.5)
