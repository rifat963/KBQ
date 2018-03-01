library(ggplot2)
library(plyr)
library(dplyr)

# Function to read CSV files
readCSV<-function(dir,fileName){
  location<-paste0(dir,fileName)
  dataSet <- read.csv(location, header = T)
  return(dataSet)
}

filterHeader<-function(dTproperty){
  
  dTproperty=plyr::rename(dTproperty, c("count33"="v2009-05-20", "count34"="v2009-09-20","count201504"="v2015-02-05",
                                        "count201510"="v2015-10-02","count35"="v2010-03-16","count36"="v2010-10-11",
                                        "count37"="v2011-07-22","count38"="v2012-06-01","count39"="v2013-0-03",
                                        "count40"="v2014-05-02","count35.1"="v2016-04-01"))
  return(dTproperty)  
}


filterHeader2<-function(dTproperty){

  dTproperty=plyr::rename(dTproperty, c("count33"="v2009-05-20", "count34"="v2009-09-20","count201504"="v2015-02-05","count201510"="v2015-10-02",
                                        "count35"="v2010-03-16","count36"="v2010-10-11",
                                        "count37"="v2011-07-22","count38"="v2012-06-01","count39"="v2013-0-03",
                                        "count40"="v2014-05-02","count43"="v2016-04-01"))
  
  return(dTproperty)  
}

# Preprocessing DBpedia classes 

dir = "C:/Users/rifat/Desktop/R_milan/githubRepo/KBQ/ExperimentalData/DBpedia/"

fileName = "dbo-Animal.csv"

fileName = "dbo-Artist.csv"

fileName = "dbo-Athlete.csv"

fileName = "dbo-film.csv"

fileName = "dbo-musicalWork.csv"

fileName = "dbo-organisation.csv"

fileName = "dbo-species.csv"

fileName = "dbo-place.csv"

fileName = "dbo-work.csv"

fileName = "foaf-person.csv"

filter = readCSV(dir,fileName)

drops <- c("id")

filter<-filter[ , !(names(filter) %in% drops)]

data = filterHeader(filter)

data = filterHeader2(filter)

head(data)

write.csv(data,paste0(dir,fileName),row.names = F)






