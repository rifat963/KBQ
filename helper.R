
library(ggplot2)
library(plyr)
library(dplyr)
library(dtplyr)
library(reshape2)
library(hts)
library('knitr')
library(reshape2)


##Entity Plot Function
fnplot<-function(data){
  
  p<-ggplot(data=data, aes(x=Release, y=count , group=class,color=class)) +
    geom_line() +
    geom_point()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  
  
  
  return(p)
}

## Calculate Days
fn<-function(ver){
  startdate <- as.Date(ver[1])
  NumDays <- difftime(as.Date(ver),startdate ,units="days")
  
  return(as.numeric(NumDays))
  
}



plotPerAll<-function(data){
  
  p<-ggplot(data=data, aes(x=Release, y=variation , group=class,color=class)) +
    geom_line() +
    geom_point()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  
  return(p)
  
}

# Persistency line plot

persistencyPlot<-function(allEntity){
  p<-ggplot(data=allEntity, aes(x=Release, y=count,group=class,color=class))+
    geom_line() +
    geom_point()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  return(p)
}

#Shade the area in the persistency plot

shadeAreaP<-function(p,allEntity){
  
  rect <- data.frame(xmin=allEntity[nrow(allEntity)-1,3], xmax=allEntity[nrow(allEntity),3], ymin=-Inf, ymax=Inf)
  p <-p + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                    color="white",
                    alpha=0.2,
                    inherit.aes = FALSE)
  return(p)
}

shadeAreaPC<-function(p,allEntity){
  
  rect <- data.frame(xmin=allEntity[nrow(allEntity)-1,1], xmax=allEntity[nrow(allEntity),1], ymin=-Inf, ymax=Inf)
  p <-p + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                    color="white",
                    alpha=0.2,
                    inherit.aes = FALSE)
  return(p)
}

persistencyMeasure<-function(allEntity){
  
  if(allEntity[nrow(allEntity),4]>=allEntity[nrow(allEntity)-1,4])
    per=1
  
  if(allEntity[nrow(allEntity),4]<allEntity[nrow(allEntity)-1,4])
    per=0
  
  return(per)
}

persistencyMeasureC<-function(allEntity){
  
  if(allEntity[nrow(allEntity),3]>=allEntity[nrow(allEntity)-1,3])
    per=1
  
  if(allEntity[nrow(allEntity),3]<allEntity[nrow(allEntity)-1,3])
    per=0
  
  return(per)
}



#Function to identity variation in the dataset

histPerAll<-function(entity){
  
  entity$Persistency=1
  
  
  for( i in 2:nrow(entity)){
    
    temp=entity[,4]
    
    if(temp[i]<temp[i-1])
      entity[i,6]=0
    
  }
  
  return(entity)
  
}

histPerAllC<-function(entity){
  
  entity$Persistency=1
  
  
  for( i in 2:nrow(entity)){
    
    temp=entity[,3]
    
    if(temp[i]<temp[i-1])
      entity[i,4]=0
    
  }
  
  return(entity)
  
}

HistPersistencyMeasure<-function(entity){
  total<-length(unclass)-1
  per_sum<-nrow(entity[entity$Persistency==0,])
  value=(1-(per_sum/total))*100
  return(value)
}

HistPersistencyMeasureC<-function(entity){
  total<-length(unique(entity$Release))-1
  per_sum<-nrow(entity[entity$Persistency==0,])
  value=(1-(per_sum/total))*100
  return(value)
}

plotEntity<-function(entity){
  
  en=entity[1:nrow(entity)-1,]
  
  enLm=lm(count~days,data=en)
  
  pred=data.frame(days=entity$days,count=predict(enLm,entity))
  
  summary(enLm)
  
  res=mean(abs(enLm$residuals))
  
  p<-ggplot(entity, aes(x = days, y = count)) + 
    geom_point() +
    geom_line(data=pred,color="red")+
    geom_ribbon(data=pred,aes(ymin=count-res,ymax=count+res),alpha=0.2)
  
  return(p)
  
}


NormDist<-function(entity){
  
  en=entity[1:nrow(entity)-1,]
  
  enLm=lm(count~days,data=en)
  
  
  pred=data.frame(days=entity$days,count=predict(enLm,entity))
  
  summary(enLm)
  
  res=mean(abs(enLm$residuals))
  
  
  pr=predict(enLm,entity[nrow(entity),])
  
  lastValue=entity[nrow(entity),]$count
  
  ND=abs(pr-lastValue)/res
  
  return(ND)
}

CheckND<-function(ND){
  if(ND<1)
    stab=1
  
  if(ND>=1)
    stab=0
  
  return(stab)
}



# Function for normalizing property values based on entity count
# Parm: Property dataframe and exact entity count

normalize<-function(property,en){
  
  dTproperty=property
  
  dTproperty<-dTproperty[,order(colnames(dTproperty),decreasing=TRUE)]
  
  d=3
  t=11
  
  for(i in 1:11){
    
    norm= dTproperty[,d]/en[t,]$count
    
    #  round(x, digits = 0)
    #  print(dTproperty[,d])  
    
    dTproperty[,d]=norm
    d=d+1
    #     print(norm)
    t=t-1
    
  }
  
  dTproperty$diffLast=dTproperty[,3]-dTproperty[,4]
  return(dTproperty)  
  
}

#Functrion to identify correct property

identifyCorrectProperty<-function(property,en){
  
  dTproperty=property
  d=3
  t=11
  
  for(i in 1:11){
    
    countEntity=100#en[t,]$count/10000
    
    #   print(countEntity)  
    #   print(length(dTproperty[,d]))
    
    prop=dTproperty[,d]
    
    for(j in 1:length(prop)){
      
      if(prop[j]>countEntity){
        prop[j]=prop[j]
      }
      else{
        prop[j]=0           
      }
    }
    
    dTproperty[,d]=prop
    d=d+1
    t=t-1
  }
  
  
  require(data.table)
  DT=data.table(dTproperty)
  
  
  listCol <- colnames(DT)[grep("-", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  Rproperty=data.frame(DT)
  
  CorrectProperty=Rproperty[Rproperty$Sum!=0,]
  
  return(CorrectProperty)
  
}

#Function to identityfy incorrect property
identifyInCorrectProperty<-function(property,en){
  
  NondTproperty=property
  
  d=3
  t=11
  
  
  for(i in 1:11){
    
    countEntity=100#en[t,]$count/10000
    
    #   print(countEntity)  
    #   print(length(dTproperty[,d]))
    
    Nonprop=NondTproperty[,d]
    
    for(j in 1:length(Nonprop)){
      
      if(Nonprop[j]>countEntity){
        Nonprop[j]=0
      }
      else{
        Nonprop[j]=Nonprop[j]        
      }
    }
    print(Nonprop)  
    NondTproperty[,d]=Nonprop
    d=d+1
    t=t-1
  }
  
  require(data.table)
  DT=data.table(NondTproperty)
  
  listCol <- colnames(DT)[grep("-", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  NonRproperty=data.frame(DT)
  
  IncorrectProperty=NonRproperty[NonRproperty$Sum!=0,]
  
  return(IncorrectProperty)
  
}

SubSetProperty<-function(dTproperty,en){
  
  avg=(en[11,]$count+en[10,]$count+en[9,]$count)/3
  
  NondTproperty=dTproperty
  
  d=3
  t=11
  
  for(i in 1:11){
    
    countEntity=100 #avg/20000#en[t,]$count/10000
    
    #   print(countEntity)  
    #   print(length(dTproperty[,d]))
    
    Nonprop=NondTproperty[,d]
    
    for(j in 1:length(Nonprop)){
      
      if(Nonprop[j]>countEntity){
        Nonprop[j]=0
      }
      else{
        Nonprop[j]=Nonprop[j]        
      }
    }
    print(Nonprop)  
    NondTproperty[,d]=Nonprop
    d=d+1
    t=t-1
  }
  
  require(data.table)
  DT=data.table(NondTproperty)
  
  listCol <- colnames(DT)[grep("-", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  NonRproperty=data.frame(DT)
  
  IncorrectProperty=NonRproperty[NonRproperty$Sum!=0,]
  
  propertySubSet=data.frame(property=IncorrectProperty[,1],v201604=IncorrectProperty[,3],v201510=IncorrectProperty[,4],v201502=IncorrectProperty[,5])
  
  require(data.table)
  
  DT=data.table(propertySubSet)
  
  listCol <- colnames(DT)[grep("v", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  propertySubSet=data.frame(DT)
  
  FilteredProperty=propertySubSet[propertySubSet$Sum!=0,]
  
  FilteredProperty=FilteredProperty[,-c(5)]
  
  return(FilteredProperty)
}


FilteredPropertyfun<-function(dTproperty,entityDataSet){

unclass=unique(entityDataSet$class)

en=entityDataSet[entityDataSet$class==unclass[1],]

dTproperty<-dTproperty[,order(colnames(dTproperty),decreasing=TRUE)]

NondTproperty=dTproperty

NondTproperty=dTproperty

CorrectProperty = identifyCorrectProperty(dTproperty,en)

# InCorrectPropery= identifyInCorrectProperty(NondTproperty,en)


avg=(en[11,]$count+en[10,]$count+en[9,]$count)/3

NondTproperty=dTproperty

d=3
t=11

for(i in 1:11){
  
  countEntity=100#avg/20000#en[t,]$count/10000
  
  
  Nonprop=NondTproperty[,d]
  
  for(j in 1:length(Nonprop)){
    
    if(Nonprop[j]>countEntity){
      Nonprop[j]=0
    }
    else{
      Nonprop[j]=Nonprop[j]        
    }
  }
  print(Nonprop)  
  NondTproperty[,d]=Nonprop
  d=d+1
  t=t-1
}

require(data.table)
DT=data.table(NondTproperty)

listCol <- colnames(DT)[grep("-", colnames(DT))]

DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]

NonRproperty=data.frame(DT)

IncorrectProperty=NonRproperty[NonRproperty$Sum!=0,]

#head(IncorrectProperty)

NdTproperty=CorrectProperty[,-c(14)]

propertySubSet=data.frame(property=IncorrectProperty[,1],v201604=IncorrectProperty[,3],v201510=IncorrectProperty[,4],v201502=IncorrectProperty[,5])


require(data.table)
DT=data.table(propertySubSet)

listCol <- colnames(DT)[grep("v", colnames(DT))]

DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]

propertySubSet=data.frame(DT)

FilteredProperty=propertySubSet[propertySubSet$Sum!=0,]

FilteredProperty=FilteredProperty[,-c(5)]

nrow(FilteredProperty)

CorrectProperty = identifyCorrectProperty(dTproperty,en)

InCorrectPropery= identifyInCorrectProperty(NondTproperty,en)

SubSetProp=SubSetProperty(NondTproperty,en)


NdTproperty=CorrectProperty[,-c(14)]

# normalize(NdTproperty,en)
d=3
t=11
for(i in 1:11){
  
  norm= NdTproperty[,d]/en[t,]$count
  
  NdTproperty[,d]=norm
  d=d+1
  #     print(norm)
  t=t-1
}

NdTproperty$diffLast=NdTproperty[,3]-NdTproperty[,4]


consistencyData=NdTproperty[NdTproperty$diffLast<0,]

nrow(dTproperty)

nrow(consistencyData)

dataSet=data.frame(property=consistencyData$property,Version201604=consistencyData$X2016.04.01,Version201510=consistencyData$X2015.10.02,NormFreqDiff=consistencyData$diffLast)

#entityVal=en

#entityVal$WeightValue=en$count/10000

CorrectProperty$diff=CorrectProperty[,3]+CorrectProperty[,4]

TwoSeries=CorrectProperty[CorrectProperty$diff!=0,]

TwoSeriesData=data.frame(Property=TwoSeries$property,version201604=TwoSeries$X2016.04.01,version201510=TwoSeries$X2015.10.02,version201504=TwoSeries$X2015.02.05)

InCdata=data.frame(Property=InCorrectPropery$property,v201604=InCorrectPropery$X2016.04.01,v201510=InCorrectPropery$X2015.10.02,v201502=InCorrectPropery$X2015.02.05)

return(FilteredProperty)
}


TwoSeriesDataFun<-function(dTproperty,entityDataSet){
  
  unclass=unique(entityDataSet$class)
  
  en=entityDataSet[entityDataSet$class==unclass[1],]
  
  dTproperty<-dTproperty[,order(colnames(dTproperty),decreasing=TRUE)]
  
  NondTproperty=dTproperty
  
  NondTproperty=dTproperty
  
  CorrectProperty = identifyCorrectProperty(dTproperty,en)
  
  # InCorrectPropery= identifyInCorrectProperty(NondTproperty,en)
  
  
  avg=(en[11,]$count+en[10,]$count+en[9,]$count)/3
  
  NondTproperty=dTproperty
  
  d=3
  t=11
  
  for(i in 1:11){
    
    countEntity=100#avg/20000#en[t,]$count/10000
    
    
    Nonprop=NondTproperty[,d]
    
    for(j in 1:length(Nonprop)){
      
      if(Nonprop[j]>countEntity){
        Nonprop[j]=0
      }
      else{
        Nonprop[j]=Nonprop[j]        
      }
    }
    print(Nonprop)  
    NondTproperty[,d]=Nonprop
    d=d+1
    t=t-1
  }
  
  require(data.table)
  DT=data.table(NondTproperty)
  
  listCol <- colnames(DT)[grep("-", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  NonRproperty=data.frame(DT)
  
  IncorrectProperty=NonRproperty[NonRproperty$Sum!=0,]
  
  #head(IncorrectProperty)
  
  NdTproperty=CorrectProperty[,-c(14)]
  
  propertySubSet=data.frame(property=IncorrectProperty[,1],v201604=IncorrectProperty[,3],v201510=IncorrectProperty[,4],v201502=IncorrectProperty[,5])
  
  
  require(data.table)
  DT=data.table(propertySubSet)
  
  listCol <- colnames(DT)[grep("v", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  propertySubSet=data.frame(DT)
  
  FilteredProperty=propertySubSet[propertySubSet$Sum!=0,]
  
  FilteredProperty=FilteredProperty[,-c(5)]
  
  nrow(FilteredProperty)
  
  CorrectProperty = identifyCorrectProperty(dTproperty,en)
  
  InCorrectPropery= identifyInCorrectProperty(NondTproperty,en)
  
  SubSetProp=SubSetProperty(NondTproperty,en)
  
  
  NdTproperty=CorrectProperty[,-c(14)]
  
  # normalize(NdTproperty,en)
  d=3
  t=11
  for(i in 1:11){
    
    norm= NdTproperty[,d]/en[t,]$count
    
    NdTproperty[,d]=norm
    d=d+1
    #     print(norm)
    t=t-1
  }
  
  NdTproperty$diffLast=NdTproperty[,3]-NdTproperty[,4]
  
  
  consistencyData=NdTproperty[NdTproperty$diffLast<0,]
  
  nrow(dTproperty)
  
  nrow(consistencyData)
  
  dataSet=data.frame(property=consistencyData$property,Version201604=consistencyData$X2016.04.01,Version201510=consistencyData$X2015.10.02,NormFreqDiff=consistencyData$diffLast)
  
  #entityVal=en
  
  #entityVal$WeightValue=en$count/10000
  
  CorrectProperty$diff=CorrectProperty[,3]+CorrectProperty[,4]
  
  TwoSeries=CorrectProperty[CorrectProperty$diff!=0,]
  
  TwoSeriesData=data.frame(Property=TwoSeries$property,version201604=TwoSeries$X2016.04.01,version201510=TwoSeries$X2015.10.02,version201504=TwoSeries$X2015.02.05)
  
  InCdata=data.frame(Property=InCorrectPropery$property,v201604=InCorrectPropery$X2016.04.01,v201510=InCorrectPropery$X2015.10.02,v201502=InCorrectPropery$X2015.02.05)
  
  return(TwoSeriesData)
  
}

dataSetFun<-function(dTproperty,entityDataSet){
  
  unclass=unique(entityDataSet$class)
  
  en=entityDataSet[entityDataSet$class==unclass[1],]
  
  dTproperty<-dTproperty[,order(colnames(dTproperty),decreasing=TRUE)]
  
  NondTproperty=dTproperty
  
  NondTproperty=dTproperty
  
  CorrectProperty = identifyCorrectProperty(dTproperty,en)
  
  # InCorrectPropery= identifyInCorrectProperty(NondTproperty,en)
  
  
  avg=(en[11,]$count+en[10,]$count+en[9,]$count)/3
  
  NondTproperty=dTproperty
  
  d=3
  t=11
  
  for(i in 1:11){
    
    countEntity=100#avg/20000#en[t,]$count/10000
    
    
    Nonprop=NondTproperty[,d]
    
    for(j in 1:length(Nonprop)){
      
      if(Nonprop[j]>countEntity){
        Nonprop[j]=0
      }
      else{
        Nonprop[j]=Nonprop[j]        
      }
    }
    print(Nonprop)  
    NondTproperty[,d]=Nonprop
    d=d+1
    t=t-1
  }
  
  require(data.table)
  DT=data.table(NondTproperty)
  
  listCol <- colnames(DT)[grep("-", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  NonRproperty=data.frame(DT)
  
  IncorrectProperty=NonRproperty[NonRproperty$Sum!=0,]
  
  #head(IncorrectProperty)
  
  NdTproperty=CorrectProperty[,-c(14)]
  
  propertySubSet=data.frame(property=IncorrectProperty[,1],v201604=IncorrectProperty[,3],v201510=IncorrectProperty[,4],v201502=IncorrectProperty[,5])
  
  
  require(data.table)
  DT=data.table(propertySubSet)
  
  listCol <- colnames(DT)[grep("v", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  propertySubSet=data.frame(DT)
  
  FilteredProperty=propertySubSet[propertySubSet$Sum!=0,]
  
  FilteredProperty=FilteredProperty[,-c(5)]
  
  nrow(FilteredProperty)
  
  CorrectProperty = identifyCorrectProperty(dTproperty,en)
  
  InCorrectPropery= identifyInCorrectProperty(NondTproperty,en)
  
  SubSetProp=SubSetProperty(NondTproperty,en)
  
  
  NdTproperty=CorrectProperty[,-c(14)]
  
  # normalize(NdTproperty,en)
  d=3
  t=11
  for(i in 1:11){
    
    norm= NdTproperty[,d]/en[t,]$count
    
    NdTproperty[,d]=norm
    d=d+1
    #     print(norm)
    t=t-1
  }
  
  NdTproperty$diffLast=NdTproperty[,3]-NdTproperty[,4]
  
  
  consistencyData=NdTproperty[NdTproperty$diffLast<0,]
  
  nrow(dTproperty)
  
  nrow(consistencyData)
  
  dataSet=data.frame(property=consistencyData$property,Version201604=consistencyData$X2016.04.01,Version201510=consistencyData$X2015.10.02,NormFreqDiff=consistencyData$diffLast)
  
  #entityVal=en
  
  #entityVal$WeightValue=en$count/10000
  
  CorrectProperty$diff=CorrectProperty[,3]+CorrectProperty[,4]
  
  TwoSeries=CorrectProperty[CorrectProperty$diff!=0,]
  
  TwoSeriesData=data.frame(Property=TwoSeries$property,version201604=TwoSeries$X2016.04.01,version201510=TwoSeries$X2015.10.02,version201504=TwoSeries$X2015.02.05)
  
  InCdata=data.frame(Property=InCorrectPropery$property,v201604=InCorrectPropery$X2016.04.01,v201510=InCorrectPropery$X2015.10.02,v201502=InCorrectPropery$X2015.02.05)
  
  return(dataSet)
  
}

TwoSeriesDataFun<-function(dTproperty,entityDataSet){
  
  unclass=unique(entityDataSet$class)
  
  en=entityDataSet[entityDataSet$class==unclass[1],]
  
  dTproperty<-dTproperty[,order(colnames(dTproperty),decreasing=TRUE)]
  
  NondTproperty=dTproperty
  
  NondTproperty=dTproperty
  
  CorrectProperty = identifyCorrectProperty(dTproperty,en)
  
  # InCorrectPropery= identifyInCorrectProperty(NondTproperty,en)
  
  
  avg=(en[11,]$count+en[10,]$count+en[9,]$count)/3
  
  NondTproperty=dTproperty
  
  d=3
  t=11
  
  for(i in 1:11){
    
    countEntity=100#avg/20000#en[t,]$count/10000
    
    
    Nonprop=NondTproperty[,d]
    
    for(j in 1:length(Nonprop)){
      
      if(Nonprop[j]>countEntity){
        Nonprop[j]=0
      }
      else{
        Nonprop[j]=Nonprop[j]        
      }
    }
    print(Nonprop)  
    NondTproperty[,d]=Nonprop
    d=d+1
    t=t-1
  }
  
  require(data.table)
  DT=data.table(NondTproperty)
  
  listCol <- colnames(DT)[grep("-", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  NonRproperty=data.frame(DT)
  
  IncorrectProperty=NonRproperty[NonRproperty$Sum!=0,]
  
  #head(IncorrectProperty)
  
  NdTproperty=CorrectProperty[,-c(14)]
  
  propertySubSet=data.frame(property=IncorrectProperty[,1],v201604=IncorrectProperty[,3],v201510=IncorrectProperty[,4],v201502=IncorrectProperty[,5])
  
  
  require(data.table)
  DT=data.table(propertySubSet)
  
  listCol <- colnames(DT)[grep("v", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  propertySubSet=data.frame(DT)
  
  FilteredProperty=propertySubSet[propertySubSet$Sum!=0,]
  
  FilteredProperty=FilteredProperty[,-c(5)]
  
  nrow(FilteredProperty)
  
  CorrectProperty = identifyCorrectProperty(dTproperty,en)
  
  InCorrectPropery= identifyInCorrectProperty(NondTproperty,en)
  
  SubSetProp=SubSetProperty(NondTproperty,en)
  
  
  NdTproperty=CorrectProperty[,-c(14)]
  
  # normalize(NdTproperty,en)
  d=3
  t=11
  for(i in 1:11){
    
    norm= NdTproperty[,d]/en[t,]$count
    
    NdTproperty[,d]=norm
    d=d+1
    #     print(norm)
    t=t-1
  }
  
  NdTproperty$diffLast=NdTproperty[,3]-NdTproperty[,4]
  
  
  consistencyData=NdTproperty[NdTproperty$diffLast<0,]
  
  nrow(dTproperty)
  
  nrow(consistencyData)
  
  dataSet=data.frame(property=consistencyData$property,Version201604=consistencyData$X2016.04.01,Version201510=consistencyData$X2015.10.02,NormFreqDiff=consistencyData$diffLast)
  
  #entityVal=en
  
  #entityVal$WeightValue=en$count/10000
  
  CorrectProperty$diff=CorrectProperty[,3]+CorrectProperty[,4]
  
  TwoSeries=CorrectProperty[CorrectProperty$diff!=0,]
  
  TwoSeriesData=data.frame(Property=TwoSeries$property,version201604=TwoSeries$X2016.04.01,version201510=TwoSeries$X2015.10.02,version201504=TwoSeries$X2015.02.05)
  
  InCdata=data.frame(Property=InCorrectPropery$property,v201604=InCorrectPropery$X2016.04.01,v201510=InCorrectPropery$X2015.10.02,v201502=InCorrectPropery$X2015.02.05)
  
  return(TwoSeriesData)
  
}

dataSetFun<-function(dTproperty,entityDataSet){
  
  unclass=unique(entityDataSet$class)
  
  en=entityDataSet[entityDataSet$class==unclass[1],]
  
  dTproperty<-dTproperty[,order(colnames(dTproperty),decreasing=TRUE)]
  
  NondTproperty=dTproperty
  
  NondTproperty=dTproperty
  
  CorrectProperty = identifyCorrectProperty(dTproperty,en)
  
  # InCorrectPropery= identifyInCorrectProperty(NondTproperty,en)
  
  
  avg=(en[11,]$count+en[10,]$count+en[9,]$count)/3
  
  NondTproperty=dTproperty
  
  d=3
  t=11
  
  for(i in 1:11){
    
    countEntity=100#avg/20000#en[t,]$count/10000
    
    
    Nonprop=NondTproperty[,d]
    
    for(j in 1:length(Nonprop)){
      
      if(Nonprop[j]>countEntity){
        Nonprop[j]=0
      }
      else{
        Nonprop[j]=Nonprop[j]        
      }
    }
    print(Nonprop)  
    NondTproperty[,d]=Nonprop
    d=d+1
    t=t-1
  }
  
  require(data.table)
  DT=data.table(NondTproperty)
  
  listCol <- colnames(DT)[grep("-", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  NonRproperty=data.frame(DT)
  
  IncorrectProperty=NonRproperty[NonRproperty$Sum!=0,]
  
  #head(IncorrectProperty)
  
  NdTproperty=CorrectProperty[,-c(14)]
  
  propertySubSet=data.frame(property=IncorrectProperty[,1],v201604=IncorrectProperty[,3],v201510=IncorrectProperty[,4],v201502=IncorrectProperty[,5])
  
  
  require(data.table)
  DT=data.table(propertySubSet)
  
  listCol <- colnames(DT)[grep("v", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  propertySubSet=data.frame(DT)
  
  FilteredProperty=propertySubSet[propertySubSet$Sum!=0,]
  
  FilteredProperty=FilteredProperty[,-c(5)]
  
  nrow(FilteredProperty)
  
  CorrectProperty = identifyCorrectProperty(dTproperty,en)
  
  InCorrectPropery= identifyInCorrectProperty(NondTproperty,en)
  
  SubSetProp=SubSetProperty(NondTproperty,en)
  
  
  NdTproperty=CorrectProperty[,-c(14)]
  
  # normalize(NdTproperty,en)
  d=3
  t=11
  for(i in 1:11){
    
    norm= NdTproperty[,d]/en[t,]$count
    
    NdTproperty[,d]=norm
    d=d+1
    #     print(norm)
    t=t-1
  }
  
  NdTproperty$diffLast=NdTproperty[,3]-NdTproperty[,4]
  
  
  consistencyData=NdTproperty[NdTproperty$diffLast<0,]
  
  nrow(dTproperty)
  
  nrow(consistencyData)
  
  dataSet=data.frame(property=consistencyData$property,Version201604=consistencyData$X2016.04.01,Version201510=consistencyData$X2015.10.02,NormFreqDiff=consistencyData$diffLast)
  
  #entityVal=en
  
  #entityVal$WeightValue=en$count/10000
  
  CorrectProperty$diff=CorrectProperty[,3]+CorrectProperty[,4]
  
  TwoSeries=CorrectProperty[CorrectProperty$diff!=0,]
  
  TwoSeriesData=data.frame(Property=TwoSeries$property,version201604=TwoSeries$X2016.04.01,version201510=TwoSeries$X2015.10.02,version201504=TwoSeries$X2015.02.05)
  
  InCdata=data.frame(Property=InCorrectPropery$property,v201604=InCorrectPropery$X2016.04.01,v201510=InCorrectPropery$X2015.10.02,v201502=InCorrectPropery$X2015.02.05)
  
  return(dataSet)
  
}

consistencyDataFun<-function(dTproperty,entityDataSet){
  
  unclass=unique(entityDataSet$class)
  
  en=entityDataSet[entityDataSet$class==unclass[1],]
  
  dTproperty<-dTproperty[,order(colnames(dTproperty),decreasing=TRUE)]
  
  NondTproperty=dTproperty
  
  NondTproperty=dTproperty
  
  CorrectProperty = identifyCorrectProperty(dTproperty,en)
  
  # InCorrectPropery= identifyInCorrectProperty(NondTproperty,en)
  
  
  avg=(en[11,]$count+en[10,]$count+en[9,]$count)/3
  
  NondTproperty=dTproperty
  
  d=3
  t=11
  
  for(i in 1:11){
    
    countEntity=100#avg/20000#en[t,]$count/10000
    
    
    Nonprop=NondTproperty[,d]
    
    for(j in 1:length(Nonprop)){
      
      if(Nonprop[j]>countEntity){
        Nonprop[j]=0
      }
      else{
        Nonprop[j]=Nonprop[j]        
      }
    }
    print(Nonprop)  
    NondTproperty[,d]=Nonprop
    d=d+1
    t=t-1
  }
  
  require(data.table)
  DT=data.table(NondTproperty)
  
  listCol <- colnames(DT)[grep("-", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  NonRproperty=data.frame(DT)
  
  IncorrectProperty=NonRproperty[NonRproperty$Sum!=0,]
  
  #head(IncorrectProperty)
  
  NdTproperty=CorrectProperty[,-c(14)]
  
  propertySubSet=data.frame(property=IncorrectProperty[,1],v201604=IncorrectProperty[,3],v201510=IncorrectProperty[,4],v201502=IncorrectProperty[,5])
  
  
  require(data.table)
  DT=data.table(propertySubSet)
  
  listCol <- colnames(DT)[grep("v", colnames(DT))]
  
  DT[, Sum := Reduce('+', .SD), .SDcols=listCol][]
  
  propertySubSet=data.frame(DT)
  
  FilteredProperty=propertySubSet[propertySubSet$Sum!=0,]
  
  FilteredProperty=FilteredProperty[,-c(5)]
  
  nrow(FilteredProperty)
  
  CorrectProperty = identifyCorrectProperty(dTproperty,en)
  
  InCorrectPropery= identifyInCorrectProperty(NondTproperty,en)
  
  SubSetProp=SubSetProperty(NondTproperty,en)
  
  
  NdTproperty=CorrectProperty[,-c(14)]
  
  # normalize(NdTproperty,en)
  d=3
  t=11
  for(i in 1:11){
    
    norm= NdTproperty[,d]/en[t,]$count
    
    NdTproperty[,d]=norm
    d=d+1
    #     print(norm)
    t=t-1
  }
  
  NdTproperty$diffLast=NdTproperty[,3]-NdTproperty[,4]
  
  
  consistencyData=NdTproperty[NdTproperty$diffLast<0,]
  
  nrow(dTproperty)
  
  nrow(consistencyData)
  
  dataSet=data.frame(property=consistencyData$property,Version201604=consistencyData$X2016.04.01,Version201510=consistencyData$X2015.10.02,NormFreqDiff=consistencyData$diffLast)
  
  #entityVal=en
  
  #entityVal$WeightValue=en$count/10000
  
  CorrectProperty$diff=CorrectProperty[,3]+CorrectProperty[,4]
  
  TwoSeries=CorrectProperty[CorrectProperty$diff!=0,]
  
  TwoSeriesData=data.frame(Property=TwoSeries$property,version201604=TwoSeries$X2016.04.01,version201510=TwoSeries$X2015.10.02,version201504=TwoSeries$X2015.02.05)
  
  InCdata=data.frame(Property=InCorrectPropery$property,v201604=InCorrectPropery$X2016.04.01,v201510=InCorrectPropery$X2015.10.02,v201502=InCorrectPropery$X2015.02.05)
  
  return(consistencyData)
  
}
