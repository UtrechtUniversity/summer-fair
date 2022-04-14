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

# source required R scripts  ####
# scripts should be in subfolder "src/R" 
source("DataManipulationRules.R")    #Data manipulation rules are pre- or user defined
source("LocalAlgorithm.R") #Estimation methods
source("GlobalAlgorithm.R")
source("Query.R")  #Query function
packageVersion('meta')
# get data ####
dataA<- get.data("http://host.docker.internal:3030/mydataset/query")
dataB<- get.data("http://host.docker.internal:3031/mydataset/query")

#run analysis over data set A
localA <- get.local.trasmission(dataA)
#run analysis over data set B
localB<- get.local.trasmission(dataB)

#combine estimates of control group with standard meta-analysis techniques ####
metaana <- combine.estimates.glm(list(localA,localB),
                                 select.treatment = "control") 
print(metaana)
png(file='CombinedResult.png',width=580, height=680) 
forest.meta(metaana)
dev.off() 
