##########################################################
#                                                        
#                  Algorithm test code                   
#                  Simulation and testing of             
#                  local and global algorithm            
#                                                        
#                  Author: Egil A.J. Fischer                               #
#                  Contact: e.a.j.fischer@uu.nl                              
#                  Creation date: 7-Sept-2021                         
##########################################################

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest -> needs to be in the docker for installation but here only library
packages = c("ggplot2",
             "tidyverse",
             "rje",
             "readxl",
             "magrittr")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#Elena tests if sourcing is possible
source("src/R/DataManipulationRules.R")    #Data manipulation rules are pre- or user defined
source("src/R/LocalAlgorithm.R")           #Estimation methods

##use query to create data####
# NOT YET DONE #
endpoint <- "http://localhost:3030/dataA2/query"

get.data <- function(){
  sparql <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            Prefix : <http://www.semanticweb.org/trans_experiment#>
            Prefix om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX tr: <http://www.thomsonreuters.com/>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
            SELECT ?round ?ex_day ?ex_hour ?group ?host_id ?treatment ?innoculationStatus ?sample_measure ?sample_result ?pathogen_name WHERE {
              ?experiment a :Experiment;
                          :experimentDay ?ex_day;
                          :hasMeasurement ?measurement.
              optional {?experiment :repetition ?round;}
              ?measurement a :Measurement;
                           :hasHost ?host;
                           :hasSample ?sample.
              ?host :id ?host_id;
                    :treatment ?treatment;
                    :innoculationStatus ?innoculationStatus;
                    :locatedIn ?env.
               ?env :groupNumber ?group. 
              optional{ ?measurement :experimentHour ?ex_hour. }
              optional{ ?measurement
                           :hasQuantity ?quantity. 
                ?quantity om:hasValue ?measure.
                ?measure om:hasNumericalValue ?sample_measure.
              }
               optional{ ?sample  :hasPathogen ?pathogen. 
                ?pathogen :name ?pathogen_name }
              optional {?sample :hasResult ?sample_result.}
            } "
  return(SPARQL(url = endpoint,query=sparql)$results)
}
#
#do data set 1
data<- get.data()



##load pre-queried data ####
prequerydata <- read_xlsx("src/R/preprocesseddata/Sample_results.xlsx",
                          sheet = "maldi Swab samples")

## Set data ####
usedata <- prequerydata

## preprocess data ####
#remove redundant spaces
names(usedata)<- str_trim(names(usedata))
usedata$inoculationStatus <- str_trim(usedata$inoculationStatus)

#split group name into house and pen and name these level 1 and level 2
usedata <-usedata%>%separate(group,c("level2","level1"),"_")

#set times to the correct resolution ###
usedata$times <- setTimes(usedata,
                          resolution = "day",
                          decimals =1)

## apply rule to this data set ####
datawithrule <-applyRule(usedata,   #data
          rule.sinceany.recode,     #rule to apply
          c("sample_result"),       #variables with output of tests
          codesposneg = c("+","-","mis")) #specific parameters for this rule. Here we need to recode values containing + or - to 1, 0 or NA.
                                          #this should be removed, because it is in the mapping file                                        
  

## visualize data after applying rules ####
ggplot(data = datawithrule)+
  geom_raster(aes(x = times,y = host_id, fill = factor(sir)))+
  facet_grid(level1~level2)

##arrange data for analysis ####
if(exists("data.arranged")) {rm(data.arranged)}
data.arranged <- arrangeData(data = datawithrule,
                             rule = rule.sinceany.recode,
                             var.id = c("sample_result"),
                             method = "glm",
                             codesposneg = c("+","-","mis"), 
                             covariates = c("ex_day"))

input = data.frame(data.arranged%>% 
                     filter(i > 0 & s>0))
#fit the input data directly
fit.real <- glm(cbind(cases, s - cases) ~ 1 ,
    family = binomial(link = "cloglog"), 
    offset = log(i/n)*dt,
    data = input)
summary(fit.real)

#fit the analyseTransmission function
fit <- analyseTransmission(data = usedata,
                    rule = rule.sinceany.recode,
                    var.id = c("sample_result"),
                    method = "glm",
                    codesposneg = c("+","-","mis"),
                    preventError = TRUE)
#check 
fit$coefficients ==fit.real$coefficients
fit$aic == fit.real$aic
fit$residuals == fit.real$residuals


