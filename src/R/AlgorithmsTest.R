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

source("src/R/LocalAlgorithm.R")


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


##create some rules
rule.generic <-function(timeseries,var.id,...){
  print("generic rule returns first column");
  return(timeseries[,var.id[1]])
}


rule.sincefirst <- function(timeseries,var.id,...){
  if(length(var.id)>1) warning("Only first var.id entry used in rule")
  new.series <- sign(cumsum(timeseries[,var.id[1]]))
  return(new.series)
}

rule.sinceeither <- function(timeseries,var.id,...){
  new.series <- sign(cumsum(rowSums(timeseries[,var.id])));
  return(new.series)
}

rule.either <- function(timeseries,var.id,...){
  new.series <- sign(rowSums(timeseries[,var.id]));
  return(new.series)
}

rule.testinfectioustestrecovered <- function(timeseries,var.id,){
  new.series <- sadkl;hasd; fsign(cumsum(timeseries1))+sign(cumsum(timeseries2))
  return(new.series)
}


ggplot(data = applyRule(mockdata,
                        rule.sincefirst,
                        tail(names(mockdata),2)))+
  geom_raster(aes(x = time,y = id, fill = factor(sir)))
ggplot(data = applyRule(mockdata,
                        rule.either,
                        tail(names(mockdata),2)))+
  geom_raster(aes(x = time,y = id, fill = factor(sir)))
ggplot(data = applyRule(mockdata,
                        rule.sinceeither,
                        tail(names(mockdata),2))
                        )+
  geom_raster(aes(x = time,y = id, fill = factor(sir)))
ggplot(data = applyRule(mockdata,
                        rule.testinfectioustestrecovered))+
  geom_raster(aes(x = time,y = id, fill = factor(sir)))

ggplot(data = applyRule(mockdata,
                        rule.testinfectioustestrecovered))+
  geom_raster(aes(x = time,y = id, fill = factor(sir)))
