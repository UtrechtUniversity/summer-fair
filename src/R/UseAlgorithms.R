#########################################################
#                                                        
#                  Usage of SUMMERFAIR                                  
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
dataA<- get.data("http://localhost:3030/datasetA")
dataB<- get.data("http://localhost:3030/datasetB")
datapigs2018<- get.data("http://localhost:3030/pigs2018data")
datapigs2020<- get.data("http://localhost:3030/pigs2020data")

#run local algorithm for each data set ####
localA <- get.local.transmission(dataA)
localB <- get.local.transmission(dataB)

localpigs2018 <- get.local.transmission(datapigs2018)
localpigs2020 <- get.local.transmission(datapigs2020)


#combine estimates of control group with standard meta-analysis techniques ####
metaana <- combine.estimates.glm(list(localA,localB,localpigs2018,localpigs2020),
                             select.treatment = "reference") 
print(metaana)
forest.meta(metaana, studlab=c('DatasetA', 'DatasetB','DatasetC','DatasetD'))

#combine estimates of control group with standard meta-analysis techniques only broiler data ####
metaana <- combine.estimates.glm(list(localA,localB,localpigs2018,localpigs2020),
                                 select.treatment = "reference",
                                 sub.group =   c("broiler","broiler","pig","pig") ) 
print(metaana)
forest.meta(metaana)

#combine estimates of control group with standard meta-analysis techniques only pig data ####
metaana <- combine.estimates.glm(list(localpigs2018,localpigs2020),
                                 select.treatment = "reference") 
print(metaana)
forest.meta(forest.meta(metaana, colgap.forest.left='29mm',studlab=c('DatasetA', 'DatasetB','DatasetC','DatasetD'),subgroup=FALSE, subgroup.hetstat= FALSE,xlim=c(-3,3)))



#combine estimates of control group with standard meta-analysis techniques ####
metaana <- combine.estimates.glm(list(localA,localB,localpigs2018,localpigs2020),
                                 select.treatment = "All") 
print(metaana)
forest.meta(metaana, colgap.forest.left='24mm', studlab=c('DatasetA', 'DatasetA', 'DatasetA', 'DatasetB', 'DatasetB','DatasetC','DatasetD'))

#combine estimates of control group with standard meta-analysis techniques ####
metaana <- combine.estimates.glm(list(localA,localB),
                                 select.treatment = "All") 
print(metaana)
forest.meta(metaana)


################
#  Do all setting your self
#################

#run analysis over data set A
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

