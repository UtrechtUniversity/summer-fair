#########################################################
#                                                        
#                  Tutorial SUMMERFAIR                                  
#                  Local and global algorithms 
#                                                        
#                  Author: Egil A.J. Fischer                               
#                  Contact: e.a.j.fischer@uu.nl                             
#                  Creation date: 7-3-2022                         
#########################################################

#load required packages (install if required)####
lapply(
  c("ggplot2",
    "tidyverse",
    "rje",
    "readxl",
    "magrittr"),
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


# source required R scripts  ####
# scripts should be in subfolder "src/R" 
source("src/R/DataManipulationRules.R")    #Data manipulation rules are pre- or user defined
source("src/R/LocalAlgorithm.R")           #Estimation methods
source("src/R/Query.r") #Query function
source("src/R/GlobalAlgorithm.R")

# get data ####
dataA<- get.data("http://localhost:3030/mydataset")
dataB<- get.data("http://localhost:3031/mydataset")

#run local algorithm for each data set ####
#run analysis over data set A
<<<<<<< HEAD
localA <- analyseTransmission(inputdata = dataA,           #data set
                              rule = rule.sinceany.cutoff, #rule to determine infection status
                              var.id = c("sample_measure"),#variable defining infection status
                              method = "glm",              #estimation method
                              cutoff = 0,                  #cutoff value for infection status
                              preventError = TRUE,         #TRUE = remove entries with > 1 case but FOI = 0
                              covars = "treatment",        #co variates 
                              reference = "control",       #reference category for multivariable estimation
                              control = "0")               #value of control treatment
#run analysis over data set B
localB<- analyseTransmission(inputdata = dataB,            #data set
                             rule = rule.sinceany.recode,  #rule to determine infection status
                             var.id = c("sample_result"),  #variable defining infection status
                             method = "glm",               #estimation method
                             codesposnegmiss = c("+","-","NA"), #values determining infection status pos, neg of missing
                             preventError = TRUE,          #TRUE = remove entries with > 1 case but FOI = 0
                             covars = "treatment",         #co variates 
                             reference = "control",        #reference category for multivariable estimation
                             control = "")                 #value of control treatment
=======
localA <- get.local.trasmission(dataA)
#run analysis over data set B
localB<- get.local.trasmission(dataB)
>>>>>>> d9797775b2eaa479f6c1cee1914a722f60604cf6

#combine estimates of control group with standard meta-analysis techniques ####
metaana <- combine.estimates.glm(list(localA,localB),
                             select.treatment = "control") 
print(metaana)
forest.meta(metaana)
funnel.meta(metaana,studlab=TRUE,contour = c(0.9, 0.95, 0.99))
