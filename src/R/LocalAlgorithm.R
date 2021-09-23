##########################################################
#                                                        #
#  Local algorithm estimation of transmission paramaters #
#                                                        #
#                                                        #
#                  Author:Egil A.J. Fischer              #
#                  Contact: e.a.j.fischer@uu.nl          #
#                  Creation date: 21-9-2021              #
##########################################################

## install and load packages ####
## First specify the packages of interest
packages = c("SPARQL",
             "tidyverse"
             "rstan",
             "shinystan",
             "rstanarm")

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

### function apply a rule to the data to determine status of a sample ####
# status of a sample is determined by the own value and all other values of that chicken 
# status of a sample can be determined by one value or multiple inputs
applyRule <- function(data,rule){
  #get time sorted information per chick
  dataUntidy <- list(ID = unique(data$chickid),
                     data = ());#select data for this chick
  return(dataUntidy)
  
}


#generic rule function 
rule <- function(...){
  decision = TRUE
  return(decision)}

#extend generic rule function
rule.1 <-function(data){
  decision = TRUE
  return(decision)
}
