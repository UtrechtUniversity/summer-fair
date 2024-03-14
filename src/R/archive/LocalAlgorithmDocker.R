# Title     : Algorithm used in Docker compose.
# Objective : TODO
# Created by: Slavc001
# Created on: 08/07/2022


source("src/R/DataInterpretationRules.R")    #Data manipulation rules are pre- or user defined
source("src/R/LocalAlgorithm.R") #Estimation methods
source("src/R/GlobalAlgorithm.R")
source("src/R/Query.R")  #Query function
packageVersion('meta')
# get data ####
dataA<- get.data("http://localhost:3030/mydataset/query")
args <- commandArgs(trailingOnly = TRUE)
config.file <-args[1]
print("Config file:")
print(config.file)

#run analysis over data set A
localA <- get.local.transmission(dataA,config.file)

print(localA)
saveRDS(localA, 'algorithmOutput.rds')