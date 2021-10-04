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
packages = c("ggplot2","tidyverse","rje","readxl","magrittr")

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
  



##use query to create data####
# NOT YET DONE #

##load pre-queried data ####
prequerydata <- read_xlsx("src/R/preprocesseddata/Sample_results.xlsx")

## Set data ####
usedata <- prequerydata
## preprocess data ####
#remove redundant space
names(usedata)<- str_trim(names(usedata))

#set times to the correct resolution ###
usedata$times <- setTimes(usedata,
                          resolution = "day",
                          decimals =1)

## apply rule to this data set ####
datawithrule <-applyRule(usedata,   #data
          rule.sinceany.recode,     #rule to apply
          c("sample_result"),       #variables with output of tests
          codesposneg = c("+","-")) #specific parameters for this rule. Here we need to recode values containing + or - to 1, 0 or NA.


## visualize data after applying rules ####
ggplot(data = datawithrule)+
  geom_raster(aes(x = times,y = host_id, fill = factor(sir)))

##arrange data for analysis ####
data.arranged <- arrangeData(data = datawithrule,
                             rule = rule.sinceany.recode,
                             var.id = c("sample_result"),
                             method = "glm",
                             codesposneg = c("+","-"))
head(data.arranged)
