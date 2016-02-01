# Installing all the required packages
needed.libraries <- c("RCurl", "data.table", "ggplot2", "grid", "dplyr")
install.packages(needed.libraries, dependencies = TRUE)

# Run packages
library(RCurl)
library(data.table) 
library(ggplot2)
library(grid)
library(dplyr)

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
"col_names <- names(DataFrame)
DataFrame[,col_names] <- lapply(DataFrame[,col_names] , factor)"

#removing non-unique degree values
degreeFactors <- DataFrame[,.N,by= degree]
degreeFactors$N[1] <- degreeFactors$N[1] + degreeFactors$N[6] + degreeFactors$N[8] + degreeFactors$N[12]
degreeFactors <- degreeFactors[-c(6, 8, 12), ]
degreeFactors <- data.frame(degreeFactors)


# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#to do: data label on bars

#waitlist graph
waitlistGraph <- ggplot(DataFrame, aes(factor(DataFrame$waitlist), fill = factor(DataFrame$waitlist))) + geom_bar(width=.5) +
                 labs(title = "Enrollement Status", x = "Waitlist", y = "Number of students")

#degrees graph             
degreeGraph <- ggplot(degreeFactors, aes(x = reorder(degree, -N), y = N)) + 
               geom_bar(stat = "identity") + 
      labs(title = "Programs", x = "Name of program", y = "Number of students")
              
multiplot(waitlistGraph, degreeGraph, cols=2)


#R Data manipulation graph
dataManiGraph <- ggplot(DataFrame, aes(factor(DataFrame$R_DataManipulation))) + geom_bar(width=.5) + 
                        labs(title = "Data Manipulation with R", x = "Confidence Level", y = "Number of students")
#R Graphics 
RGraphicsGraph <- ggplot(DataFrame, aes(factor(DataFrame$R_Graphics))) + geom_bar(width=.5) +
                        labs(title = "Basic Graphics with R", x = "Confidence Level", y = "Number of students")
#R Advanced amd  visualization
RAdvancedGraph <- ggplot(DataFrame, aes(factor(DataFrame$R_MultiVariate))) + geom_bar(width=.5) + 
                        labs(title = "Advanced Analysis and Visualization with R", x = "Confidence Level", y = "Number of students")
#R Markdown
RMDGraph <- ggplot(DataFrame, aes(factor(DataFrame$Markdown))) + geom_bar(width=.5) + 
                        labs(title ="R Markdown", x = "Confidence Level", y = "Number of students")

#Matlab Data Manipulation
MatlabGraph <- ggplot(DataFrame, aes(factor(DataFrame$Matlab_DataManipulation))) + geom_bar(width=.5) + 
                      labs(title = "Data Manipulation wiht Matlab", x = "Confidence Level", y = "Number of students")
#GitHub
GitHubGraph <- ggplot(DataFrame, aes(factor(DataFrame$GitHub))) + geom_bar(width=.5) + 
                      labs(title = "GitHub", x = "Confidence Level", y = "Number of students")

multiplot(dataManiGraph,RGraphicsGraph, RAdvancedGraph, RMDGraph, MatlabGraph, GitHubGraph, cols=3)

#to do: cleaner code using a for loop

c1 <- count(DataFrame, vars = R_DataManipulation )
colnames(c1) <- c("Level","R_DataManipulation")
c2 <- count(DataFrame, vars = R_Graphics )
colnames(c2) <- c(2,"R_Graphics")
c3 <- count(DataFrame, vars = R_MultiVariate )
colnames(c3) <- c(3,"R_MultiVariate")
c4 <- count(DataFrame, vars = Markdown )
colnames(c4) <- c(4,"Markdown")
c5 <- count(DataFrame, vars = Matlab_DataManipulation )
colnames(c5) <- c(5,"Matlab_DataManipulation")
c6 <- count(DataFrame, vars = GitHub )
colnames(c6) <- c(6,"GitHub")

expertise <- cbind(c1, c2, c3, c4, c5, c6)
expertise[,c('2', '3', '4', '5', '6') := NULL] 
  
