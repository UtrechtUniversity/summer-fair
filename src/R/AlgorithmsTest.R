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
packages = c("ggplot2","tidyverse","rje")

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
source("src/R/DataManipulationRules.R")

##create some mock data####
animals.per.group = 4; #number of S and I per group
groups = 4;
sampletimes = 10; #number of sample events
#number of observations
animals.per.group*groups*sampletimes

mockdata <- data.frame(
  id = rep(c(1:(animals.per.group*groups)),each = sampletimes),         # animals
  times = rep(c(1:sampletimes), (animals.per.group*groups)),           # times
  location = rep(LETTERS[1:groups],
                 each = animals.per.group*sampletimes),     #  identify group
  type = rep(c(rep("I",animals.per.group*sampletimes/2),
               rep("S",animals.per.group*sampletimes/2)),groups), # first 1/2 of animals per group I  
  treatment = "none",                      # no treatment  
  sample1 = c(replicate(rbinom(sampletimes*animals.per.group/2,1,.7),
              rbinom(sampletimes*animals.per.group/2,1,.3),n=groups)),               # random positive and negative samples
  sample2 = c(replicate(rbinom(sampletimes*animals.per.group/2,1,.7),
                        rbinom(sampletimes*animals.per.group/2,1,.3),n=groups)),
  sample3 = c(replicate(rbinom(sampletimes*animals.per.group/2,1,.2),
                        rbinom(sampletimes*animals.per.group/2,1,.1),n =groups))
  )               # random positive and negative samples
  


##visualize data ####
ggplot(data = mockdata)+
  geom_raster(aes(x = times,y = id, fill = factor(sample1)))
ggplot(data = mockdata)+
  geom_raster(aes(x = times,y = id, fill = factor(sample2)))
ggplot(data = mockdata)+
  geom_raster(aes(x = times,y = id, fill = factor(sample3)))

ggplot(data = mockdata)+
  geom_raster(aes(x = times,y = id, fill = factor(sample1+sample2+sample3)))


head(applyRule(mockdata,
          rule.sincefirst,
          tail(names(mockdata),1))%>%arrange(id), 2* sampletimes)

ggplot(data = applyRule(mockdata,
                        rule.sincefirst,
                        tail(names(mockdata),3)))+
  geom_raster(aes(x = times,y = id, fill = factor(sir)))
ggplot(data = applyRule(mockdata,
                        rule.either,
                        tail(names(mockdata),2)))+
  geom_raster(aes(x = times,y = id, fill = factor(sir)))
ggplot(data = applyRule(mockdata,
                        rule.sinceany,
                        tail(names(mockdata),2))
                        )+
  geom_raster(aes(x = times,y = id, fill = factor(sir)))

ggplot(data = applyRule(mockdata,
                        rule.testinfectioustestrecovered,
                        var.id = tail(names(mockdata),3),
                        infrec = list(inf=c(1,2),rec=c(3))))+
  geom_raster(aes(x = times,y = id, fill = factor(sir)))


ggplot(data = applyRule(mockdata,
                        rule.sincefirstinfectioustestrecovered,
                        var.id = tail(names(mockdata),3),
                        infrec = list(inf=c(2),rec=c(3))))+
  geom_raster(aes(x = times,y = id, fill = factor(sir)))

rmockdata <- applyRule(mockdata,
                       rule.sincefirstinfectioustestrecovered,
                       var.id = tail(names(mockdata),3),
                       infrec = list(inf=c(2),rec=c(3)))
