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
endpoint <- "http://localhost:3030/datasetA"

get.data <- function(endpoint){
  sparql <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            Prefix : <http://www.semanticweb.org/trans_experiment#>
            Prefix om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX tr: <http://www.thomsonreuters.com/>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
            SELECT ?round ?ex_day ?group ?level1 ?level2 ?level3 ?host_id ?treatment ?innoculationStatus ?sample_measure ?sample_result ?pathogen_name WHERE {
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
              ?env  :groupNumber ?group. 
              optional{?env :level1 ?level1;}
              optional{?env :level2 ?level2;}
              optional{?env :level3 ?level3;}
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
if(exists("usedata")){rm(usedata)};usedata<- get.data(endpoint)
#correct spelling
usedata <- rename(usedata,"inoculationStatus" = "innoculationStatus" )
head(usedata)

#####################################
# Process data for analysis
#####################################

#set times to the correct resolution ####
usedata$times <- setTimes(usedata,
                          resolution = "day",
                          decimals =1)

## apply rule to determine infection states to this data set ####
datawithrule <-applyRule(usedata,          #data
          rule = rule.sinceany.cutoff,     #rule to apply
          var.id = c("sample_measure"),    #variables with output of tests
          cutoff = 0)                      #specific parameters for this rule. 
  
## visualize data after applying rules ####
ggplot(data = datawithrule)+
  geom_raster(aes(x = times,y = host_id, fill = factor(sir)))
ggplot(data = datawithrule)+
  geom_path(aes(x = times,y = log10(as.numeric(sample_measure)+1), colour = factor(host_id)))+
  theme(legend.position = "none")

unique(datawithrule$times)
unique(datawithrule$sample_measure)
unique(datawithrule$sir)

##arrange data for analysis ####
if(exists("data.arranged")) {rm(data.arranged)}
data.arranged <- arrangeData(data = usedata,
                             rule = rule.sinceany.cutoff,
                             var.id = c("sample_measure"),
                             method = "glm",
                             cutoff = 0, 
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
fit <- analyseTransmission(inputdata = usedata,
                    rule = rule.sinceany.cutoff,
                    var.id = c("sample_measure"),
                    method = "glm",
                    cutoff = 0,
                    preventError = TRUE)
#check 
fit$coefficients ==fit.real$coefficients
fit$aic == fit.real$aic
fit$residuals == fit.real$residuals

################################################
# Run the global algorithm over three data sets
################################################

#source the query function
source("src/R/Query.r")

#run local algorithms for each data set####
 
#get data
dataA<- get.data("http://localhost:3030/datasetA")
dataB<- get.data("http://localhost:3030/datasetB")
dataC<- get.data("http://localhost:3030/datasetC")
#correct spelling
dataA <- rename(dataA,"inoculationStatus" = "innoculationStatus" )
dataB <- rename(dataB,"inoculationStatus" = "innoculationStatus" )
dataC <- rename(dataC,"inoculationStatus" = "innoculationStatus" )

#run analysis over each data set
localA <- analyseTransmission(inputdata = dataA,
                              rule = rule.sinceany.cutoff,
                              var.id = c("sample_measure"),
                              method = "glm",
                              cutoff = 0,
                              preventError = TRUE, 
                              covars = "treatment")

localB<- analyseTransmission(inputdata = dataB,
                             rule = rule.sinceany.recode,
                             var.id = c("sample_result"),
                             method = "glm",
                             codesposnegmiss = c("+","-","NA"),
                             preventError = TRUE, 
                             covars = "treatment")

localC<- analyseTransmission(inputdata = dataC,
                             rule = rule.sinceany.cutoff,
                             var.id = c("sample_result"),
                             method = "glm",
                             cutoff = 0,
                             preventError = TRUE, 
                             covars = "treatment")
#perform meta-analysis
combine.estimates <- 