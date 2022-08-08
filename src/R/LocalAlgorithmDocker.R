# Title     : Algorithm used in Docker compose.
# Objective : TODO
# Created by: Slavc001
# Created on: 08/07/2022


source("DataManipulationRules.R")    #Data manipulation rules are pre- or user defined
source("LocalAlgorithm.R") #Estimation methods
source("GlobalAlgorithm.R")
source("Query.R")  #Query function
packageVersion('meta')
# get data ####
dataA<- get.data("http://host.docker.internal:3032/mydataset/query")

#run analysis over data set A
localA <- get.local.transmission(dataA)

print(localA)
saveRDS(localA, 'algorithmOutput.rds')