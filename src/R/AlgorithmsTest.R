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
packages = c()

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
  chickid = rep(c(1:20),each = 5),         # 20 animals
  time = rep(c(1:5), 20),                  # 5 times
  location = rep(c("A","B"),each = 50),     # 2 locations
  type = rep(c(rep("I",25),rep("S",25)),2),# first 5 animals per group I  
  treatment = "none",                      # no treatment  
  sample = c(rbinom(25,1,.7),rbinom(25,1,.3),
               rbinom(25,1,.7),rbinom(25,1,0.3))               # random positive and negative samples

)

boxplot(mockdata$sample~ mockdata$type+mockdata$location, type = "l" )


cor(x =mockdata$sample[1:50], y = mockdata$sample[51:100])

