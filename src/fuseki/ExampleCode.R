###################################################################
#   Example code loading and analysing transmission data in R     #
###################################################################
rm(list = ls())

#libraries####
library(tidyverse) # tidier coding
library(SPARQL) # SPARQL querying package
library(lme4)   # generalized linear mixed-effects models # read excel files
### the endpoint where our data is located
endpoint <- "http://host.docker.internal:3030/datasetA/query"

### SPARQL QUERIES #####

### GET isolator, chicken type, count of ESBL and date


get.chichen.data <- function(){
  sparql <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
Prefix : <http://www.semanticweb.org/trans_experiment#>
Prefix om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX tr: <http://www.thomsonreuters.com/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
select ?day ?host ?sample_result ?pathogen_name WHERE {

  ?experiment a :Experiment;
              :experimentDay ?day;
              :hasMeasurement ?measurement.

  ?measurement a :Measurement;
               :hasHost ?host;
               :hasSample ?sample.
  ?sample :hasResult ?sample_result;
          :hasPathogen ?pathogen.
  ?pathogen :name ?pathogen_name

  Filter(?pathogen_name!= '')

} "
  return(SPARQL(url = endpoint,query=sparql)$results)
}
#
#do data set 1
data1<- get.chichen.data()

print(data1)

write.csv(data1, "output.csv", row.names=FALSE, quote=FALSE)
