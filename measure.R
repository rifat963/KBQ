library(ggplot2)
library(plyr)
library(dplyr)
library(dtplyr)
library(reshape2)
library('knitr')
library(reshape2)


## Calculate Days

fn<-function(ver){
  
  startdate <- as.Date(ver[1])
  
  NumDays <- difftime(as.Date(ver),startdate ,units="days")
  
  return(as.numeric(NumDays))
}

## Persistency measure

Prsistency<-function(entity){
  
  # print(data)
    st<-total_count(data)
    data<-distinct_entity(st)
    # print(st)
    if(data[nrow(data),]$count<data[nrow(data)-1,]$count)
      per=0
    else
      per=1

    return(per)
}






historicalPersistency<-function(entity){
  # print(entity)
  entity$Persistency=1
  for(i in 2:nrow(entity)){
    temp=entity$count
    # print(temp)
    if(temp[i]<temp[i-1] && entity[i,]$className==entity[i-1,]$className)
      entity[i,]$Persistency=0
  }
  # print(entity)
  return(entity)
}

