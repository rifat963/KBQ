Knowledge Base Data Quality (KBQ)
======

This contains a prototype implementation of KB quality assessment approach (KBQ) using quality measures that are computed using a temporal analysis. KBQ written in R and present in R markdown file. Quality analysis report based on four quality characteristics: (i) Persistency (ii) Historical Persistency (iii) Consistency (iv) Completeness. We divided the report based on each quality characteristics. Also we presented DBpedia growth analysis measure.

## Basic Measure Definition

(i) Persistency of a Classe is 1 if, on the KB releases (i=1....n) , En > (En-1) else Persistency = 0

Where En is the distnct entity of a class. 

(ii) Historical Persistency of a Classe is 1 if, on the KB releases (i=1....n) , Persistency = 0 else historical persistency= 0

Where En is the distnct entity of a class. 

(iii) Consistency of a predicate = 1 if frequency of a predicate fi > 100, where KB releases, i=1...n

else Consistency = 0 ,if fi < 100.

(iv) Completeness of a predicate = 1 if normalized frequency of a predicate, fi> (fi-1) ; where Time Series, i=1....n.

else Completeness = 0 

(v) For KB growth we applied a Linear regression over the KB releases (i=1....n).  

From the linear regression, We calculate the normalize distance. 

The normalized distance(ND) = (abs(Last TimeSeries Entity Count - Predicted Value)/mean(abs(Residuals))

So, KB growth is 1 if ND<1 or KB growth is 0 if ND>=1

## Results Structure

(i) Persistency: A line graph presented to visualize variation on last two Release of KB.

(i) Historical Persistency: A table with this classes with persistency issues over the Releases.

(iii) Consistency: Result of property values presented in a table. 

(iv) Completeness: Result of property values presented in a table with normalized property values and difference between two version of normalized property values.

(v) KB growth: A graph visualizing KB growth.


## Experimental Setup

## Input

In this implementation experiments dataset presented in folder "~/ExperimentalData/". We divided 3cixty Nice and DBpedia KB datasets into two seperate folders.  
For 3cixty-

```{r}
location="~/ExperimentalData/3cixtyNice/"
```
For DBpedia-

```{r}
location="~/ExperimentalData/DBpedia/"
```

## Running the Experiments

The dataset presented in the folder is already processed and build in as intermediate data structure for the quality assessment approach. 

### Requirements

```{r}

install.packages(c("ggplot2", "dplyr", "plyr", "dtplyr", "reshape2","knitr","hts","rmarkdown"))

```


### Running the prototype

```{r}
rmarkdown::run("DBpedia.Rmd")

rmarkdown::run("3cixtyNice.Rmd")

```

### Output

We present experiments results in simple html file as well as in pdf file. 3cixty Nice experiments results present in file 3cixtyNice.html and 3cixtyNice.pdf. DBpedia experiments result present in file DBpedia.html and DBpedia.pdf.


#### Licence
These scripts are free software; you can redistribute it and/or modify it under the terms of the GNU General Public License published by
the Free Software Foundation, either version 3 of the License, or (at your option) any later version. See the file Documentation/GPL3 in the original distribution for details. There is ABSOLUTELY NO warranty. 


