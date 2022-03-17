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
localA <- get.local.trasmission(dataA)
#run analysis over data set B
localB<- get.local.trasmission(dataB)

#combine estimates of control group with standard meta-analysis techniques ####
metaana <- combine.estimates.glm(list(localA,localB),
                             select.treatment = "control") 
print(metaana)
forest.meta(metaana)
funnel.meta(metaana,studlab=TRUE,contour = c(0.9, 0.95, 0.99))
