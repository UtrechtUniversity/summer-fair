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

## First specify the packages of interest
packages = c("ggplot2")

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

source("LocalAlgorithm.R")


##create some mock data####
mockdata <- data.frame(
  id = rep(c(1:20),each = 5),         # 20 animals
  time = rep(c(1:5), 20),                  # 5 times
  location = rep(c("A","B"),each = 50),     # 2 locations
  type = rep(c(rep("I",25),rep("S",25)),2),# first 5 animals per group I  
  treatment = "none",                      # no treatment  
  sample1 = c(rbinom(25,1,.7),rbinom(25,1,.3),
               rbinom(25,1,.7),rbinom(25,1,0.3)),               # random positive and negative samples
  sample2 = c(rbinom(25,1,.7),rbinom(25,1,.3),
              rbinom(25,1,.7),rbinom(25,1,0.3))               # random positive and negative samples
  
)

##visualize data ####
ggplot(data = mockdata)+
  geom_raster(aes(x = time,y = chickid, fill = factor(sample1)))
ggplot(data = mockdata)+
  geom_raster(aes(x = time,y = chickid, fill = factor(sample2)))
ggplot(data = mockdata)+
  geom_raster(aes(x = time,y = chickid, fill = factor(sample1+sample2)))


##test rules
rule.sincefirst <- function(timeseries,...){
  new.series <- sign(cumsum(timeseries))
  return(new.series)
}

rule.sinceeither <- function(timeseries1,timeseries2){
  new.series <- sign(cumsum(timeseries1+timeseries2))
  return(new.series)
}

rule.either <- function(timeseries1,timeseries2){
  new.series <- sign(timeseries1+timeseries2)
  return(new.series)
}

rule.testinfectioustestrecovered <- function(timeseries1,timeseries2){
  new.series <- sign(cumsum(timeseries1))+sign(cumsum(timeseries2))
  return(new.series)
}


test1 <- rbinom(10,1,.2)
test1
rule.sincefirst(test1)
test2 <- rbinom(10,1,.2)
test2
test1 +test2
rule.sinceeither(test1,test2)
rule.either(test1,test2)
rule.testinfectioustestrecovered(test1,test2)

dataAfterRule <- applyRule(mockdata,rule.sincefirst)
dataAfterRule <- applyRule(mockdata,rule.sinceeither)
dataAfterRule <- applyRule(mockdata,rule.either)
dataAfterRule <- applyRule(mockdata,rule.testinfectioustestrecovered)
ggplot(data = dataAfterRule)+
  geom_raster(aes(x = time,y = id, fill = factor(sir)))
