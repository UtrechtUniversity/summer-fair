###################################################################
#   Example code loading and analysing transmission data in R     #
###################################################################
rm(list = ls())

#libraries####
library(tidyverse) # tidier coding
library(SPARQL) # SPARQL querying package
library(lme4)   # generalized linear mixed-effects models # read excel files
### the endpoint where our data is located
endpoint <- "http://host.docker.internal:3030/dataset_a/query"

### SPARQL QUERIES #####

### GET DATES
get.dates.swab <- function(){
  #SPARQL query to get the experiment dates of SWAB sample
  sparql <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX tr: <http://www.semanticweb.org/trans_experiment#>
SELECT distinct ?date  
	WHERE { ?subject tr:experimentDay ?date;
 		          tr:hasSample ?sample.
?sample rdf:type tr:SwabSample.
 }
order by xsd:integer(?date)"
  
  return(SPARQL(url = endpoint,query=sparql)$results)
}


### GET isolator, chicken type, count of ESBL and date


get.chichen.data <- function(){
  sparql <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX tr: <http://www.semanticweb.org/trans_experiment#>
PREFIX om2: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
SELECT ?sex ?isolator ?chicken ?S_I (xsd:integer(?treatment1) as ?treatment) ?count_ESBL ?date
WHERE { ?subject tr:experimentDay ?date;
 		          tr:hasSample ?sample.
	?sample rdf:type tr:SwabSample;
    tr:hasQuantity  ?weight;
	tr:involves ?chicken.
  
?weight tr:hasValue ?value.

?value om2:hasNumericalValue ?count_ESBL.

 ?chicken  rdf:type tr:Individual;
tr:type ?S_I;
tr:sex ?sex;
tr:locatedIn ?group;
tr:treatment ?treatment1.

?group tr:number ?isolator
 }"
  return(SPARQL(url = endpoint,query=sparql)$results)
}
#manipulate data to get the right input####
#function to determine if defined as positive or negative often more complicated than this but same input
status.function <- function(data){
  res <- tibble();print(colnames(data));
  for(i in c(1:dim(data)[1]))
  { 
    tmp <- c(data[i,]>0);#determine positive samples 
    res <- rbind(res,
                 data.frame(t(mapply(FUN = function(x,y){ifelse(x&y,1,0)} ,head(tmp,-1),tail(tmp,-1))))); #only two positive samples after each other are really positive
  }
  colnames(res)<- head(colnames(data),-1);
  print(res);
  return(res)
}

#do data set 1
data1<- get.chichen.data()

##make SPARQL query data untidy
untidy_data <- data1 %>% pivot_wider(names_from = c(date), names_prefix = 'count_ESBL_d',values_from = c(count_ESBL))


sample.days1<- as.numeric(get.dates.swab()) #this is something I know but should be part of the data set
deltat.days1<- tail(sample.days1,-1)-head(sample.days1,-1);#time intervals



status.data1 <- cbind(untidy_data[,c("sex","isolator","treatment","S_I")],data.frame(status.function(select(untidy_data, contains("count_ESBL")&!contains("cae"))))); #sample status except for the caecal sample

#count number of infected at previous time step, number of new cases per group

aggregate.data1<- NULL
for(i in c(1:(length(deltat.days1)-1))){
  #
  aggregate.data1 <-rbind(aggregate.data1,
                          data.frame(C = aggregate(status.data1[,i+4+1]-status.data1[,i+4],by = list(status.data1$isolator),function(input){sum(input>0,na.rm = T)})$x,#cases
                                     S = aggregate(-(status.data1[,i+4]-1),by = list(status.data1$isolator),sum,na.rm = T)$x,#susceptibles at start interval
                                     I = aggregate(status.data1[,i+4],by = list(status.data1$isolator),sum,na.rm = T)$x,#infected at start interval
                                     N = aggregate(status.data1[,i+4],by = list(status.data1$isolator),length)$x,#animals at start of interval
                                     Treatment = aggregate(status.data1[,3],by = list(status.data1$isolator),first)$x,#animals at start of interval
                                     deltat = deltat.days1[i],
                                     day = sample.days1[i+1]
                                     
                          )) 
  
}

#"local algorithm"####
model<- function(data.set)
{
  
  #create likelihood function
  logLikelihood <- function(b){
    with(data.set,-sum(C*(-(b[1] + c(0,b)[Treatment+1]) * (I/N)*deltat) + (S-C)*log((1-exp(-(b[1] + c(0,b)[Treatment+1])*(I/N)*deltat)))))
    
  }
  
  return(c(logLikelihood,length(unique(data.set$Treatment))))
}

#apply local algorithm
sel.data1 <- aggregate.data1%>%filter(I>0)#select time intervals with I>0 
model.data1 <- model(sel.data1)# returns likelihood function and number of classes
fit <- nlm(model.data1[[1]],c(1,1,1))#model.data1 can be used in estimation procedures

print(fit)
capture.output(summary(fit), file = "Estimation.txt")

# general algorithm to improve accuracy####
combine.logLik <- function(lls){
  #sum up the loglikelihoods
  comb.LogLik <- function(b){sum(lls)}
  #
  return(comb.LogLik)
}





