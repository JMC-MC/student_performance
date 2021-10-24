##################################################

# Main code for Student Performance Project EDX 

##################################################

# Check for and install required packages

list_of_packages <- c("ggplot2", "tidyverse","corrplot")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

#Attach required packages
lapply(list_of_packages,library,character.only = TRUE)

# Load data from local file using relative file path

df_math <- read.csv("data/student-mat.csv",sep=";",header=TRUE)
df_port <- read.csv("data/student-por.csv",sep=";",header=TRUE)

# Finding correlation
crs <- cor(df_math[,c(24:30)])
corrplot(crs)


