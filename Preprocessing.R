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

library(caret)

st=read.csv("C:/Users/rifat/Desktop/R_milan/githubRepo/FeatureEngineering/dataset/evolution/3cixtyNice/lode-Event-property.csv")

st=read.csv("C:/Users/rifat/Desktop/R_milan/githubRepo/RDFShapeInduction/dataset/3cixtyPropList.csv")

st=read.csv("C:/Users/rifat/Desktop/R_milan/githubRepo/RDFShapeInduction/dataset/EnglishDBpediaPropertyPlaceProperty.csv")

head(st)

nrow(st)

length(unique(st$Property))

train=data.frame(property=unique(st$Property))

head(train)

train$Class="IRI"

train=train[1:215,]

intrain <- createDataPartition(y = train$Class, p= 0.7, list = FALSE)

training <- train[intrain,]

testing <- train[-intrain,]


testing$Class[testing$Class=="IRI"]="LIT"

head(testing)

trainF=rbind(training,testing)

nrow(trainF[trainF$Class=="LIT",])

trainF <- trainF[sample(nrow(trainF)),]

# length(unique(tainF))

write.csv(trainF,"C:/Users/rifat/Desktop/R_milan/githubRepo/RDFShapeInduction/dataset/lode-Event-property-range.csv",row.names = F)


write.csv(trainF,"C:/Users/rifat/Desktop/R_milan/githubRepo/RDFShapeInduction/dataset/dbo-Place-property-range.csv",row.names = F)


set.seed(0)
actual = c('a','b','c')[runif(100, 1,6)] # actual labels

# actual = c('a','b')[128,25] # actual labels

predicted = actual # predicted labels
predicted[runif(30,1,100)] = actual[runif(30,1,100)]  # introduce incorrect predictions
cm = as.matrix(table(Actual = actual, Predicted = predicted)) # create the confusion matrix
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

precision = diag / colsums 

recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

precision
recall
f1

precision=0.7620 
recall=  0.7901 
f1 = 2 * precision * recall / (precision + recall) 
