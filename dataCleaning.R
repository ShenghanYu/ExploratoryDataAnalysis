############################
## EDAV PROJECT ONE
## THE CLASS
## 
## Shenghan Yu
############################

###########################################################################################

setwd("~/GitHub/ExploratoryDataAnalysis")
require(xlsx)
library(ggplot2)
library(dummies)

# import raw data
raw_data <- read.xlsx("Survey.xlsx", sheetName = "Form Responses 1")

# This is the dataset we use in the following 
dataset <- raw_data

###########################################################################################
## 1.Gender
dataset$gender[raw_data$What.is.your.preferred.gender.pronoun.=="he/him"] = "Male"
dataset$gender[raw_data$What.is.your.preferred.gender.pronoun.=="she/her"] = "Female"
dataset$gender[raw_data$What.is.your.preferred.gender.pronoun.=="doesn't matter"] = "Declined to state"
dataset$gender[is.na(raw_data$What.is.your.preferred.gender.pronoun.)] = "Declined to state"
dataset$gender <- factor(dataset$gender)

## 2.Program
dataset$program <- raw_data$Program
dataset$program[raw_data$Program == 'Ms in ds'] = 'IDSE (master)'
dataset$program[raw_data$Program == 'MSDS'] = 'IDSE (master)'
dataset$program[raw_data$Program == 'Data Science'] = 'IDSE (master)'
dataset$program[raw_data$Program == 'Applied Math'] = 'Other masters'
dataset$program[raw_data$Program == 'PhD Biomedical Informatics'] = 'Ph.D.'
dataset$program[raw_data$Program == 'QMSS'] = 'QMSS (master)'
dataset$program <- factor(dataset$program)

## 3.Waitlist
dataset$waitlist = factor(raw_data$Are.you.on.the.waiting.list.)

## 4.skill Dummies
# Get the skills for each samples.
skill_list <- strsplit(as.character(raw_data$Experiences.with.tools), ", ")

# Create a list for all skill names
skill_name <- vector(mode="character", length=0)

# Create dummies for skills, loop through samples and skills
for (sample_index in 1:nrow(raw_data)) {
  
   for (skill in skill_list[[sample_index]]) {
     
     # append the names, create a new column if it is a new skill
     if (!is.element(skill, skill_name)) {
       skill_name = append(skill_name,skill)
       dataset[,skill] = 0
     }
     
      dataset[sample_index,skill] = 1
   }
   
}

## 5 text editor
dataset$text_editor <- raw_data$What.code.text.editor.do.you.use.most.


## 6. Confidence
dataset$conf_r_manipulation <- factor(dataset$Programming.and.Analytical.Experiences..R..data.manipulation.and.modeling.,levels=c('None','A little', 'Confident', 'Expert'), ordered= TRUE) 
dataset$conf_r_graphic <- factor(dataset$Programming.and.Analytical.Experiences..R..graphic.basics..base..lattice..grid.etc....  ,levels=c('None','A little', 'Confident', 'Expert'), ordered= TRUE)
dataset$conf_r_multivariate <- factor(dataset$Programming.and.Analytical.Experiences..R..advanced..multivariate.data.analysis..e.g..spatiotemporal.data..visualization.and.modeling..    ,levels=c('None','A little', 'Confident', 'Expert'), ordered= TRUE)
dataset$conf_r_markdown <- factor(dataset$Programming.and.Analytical.Experiences..Reproducible.documentation.with.R..e.g..R.Markdown..  ,levels=c('None','A little', 'Confident', 'Expert'), ordered= TRUE)

dataset$conf_github <- factor(dataset$Programming.and.Analytical.Experiences..Github.,levels=c('None','A little', 'Confident', 'Expert'), ordered= TRUE)
dataset$conf_matlab <- factor(dataset$Programming.and.Analytical.Experiences..Matlab..data.manipulation..analysis..visualization.and.modeling.  ,levels=c('None','A little', 'Confident', 'Expert'), ordered= TRUE)

## Drop old columns
dataset <- dataset[,-(1:38)]

##################################################################################################

## Making subgroups

subgroup_R <- dataset[dataset$R==1,] 

subgroup_IDSE <- dataset[dataset$program==c('IDSE (master)', 'Data Science Certification'),] 
subgroup_nonIDSE <- dataset[dataset$program!=c('IDSE (master)', 'Data Science Certification'),] 

subgroup_male <- dataset[dataset$gender=='Male',] 
subgroup_female <- dataset[dataset$gender=='Female',] 



