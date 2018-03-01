# library 
library(ggplot2)
library(ggthemes)
library(plyr)
library(dplyr)
library(dtplyr)
library(reshape2)
library('knitr')
require(tidyr)
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('stringr') # string manipulation
library('RColorBrewer') # visualisation


# Functions

# Read dataset

loadCsv<-function(fileName,location){
  
  loc<-paste(getwd(),location,sep = "")
  
  loc<-paste(loc,fileName,sep = "")
  
  #location<- "C:/Users/rifat/Desktop/R_milan/githubRepo/KBQ2/ExperimentalData/DBpedia/DBpedia10ClassEntityCount.csv"
  
  dataset <- read.csv(loc,header=T)
  
  return(dataset)
  
}

## Calculate Days

days<-function(ver){
  
  startdate <- as.Date(ver[1])
  
  NumDays <- difftime(as.Date(ver),startdate ,units="days")
  
  return(as.numeric(NumDays))
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# plot