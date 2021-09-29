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
             "ggplot2",
             "tidyverse",
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
## The output of this algorithm should be an S4 object ####
# define the S4 class for output ###
setClass("TransmissionEstimate",
         slots = c(n = "numeric", #number of observations
                   likelihood ="function", #log-likelihood function with one or more parameters
                   rule = "character", #rule used to determine if sample is positive or negative
                   pars ="list" #list of estimated parameters options are of each the list contains the name and distribution
                                #R = reproduction number
                                #beta = transmission coefficient within a group
                                #betab = transmission coefficient between groups
                                #InfectiousPeriod = infectious period
                                #alpha = shape parameter distance dependent transmission
                                #r0 = scale parameter distance dependent transmission
                         ))
getClass("TransmissionEstimate")
# obj =new("TransmissionEstimate", 
#          n = 10, 
#          likelihood = function(R){R*(1-R)},
#          rule = "none",
#          pars = list("R"))

# methods 
setMethod("show",signature= c(object ="TransmissionEstimate"),
          function(object){
            print(object@n);
            print(object@rule);
            print(object@pars);
            ggplot(data =data.frame(x = seq(0,2,by = 0.1),
                 y = sapply(X = seq(0,2,by = 0.1), 
                            FUN =object@likelihood))) +
                   geom_path(aes(x,y))+xlab(object@pars[1])+ylab("LL")
          }
)
          

### function apply a rule to the data to determine status of a sample ####
# status of a sample is determined by the own value and all other values of that chicken 
# status of a sample can be determined by one value or multiple inputs
applyRule <- function(data,rule, time.series.id){
  #per chicken define the positive and negative moments
  dataRuled <- data;
  dataRuled$sir<- 0;#0 indicates susceptible individual
  for(id in data$id){
    dataRuled[order(dataRuled$time)&dataRuled$id == id,]$sir <-rule(dataRuled[order(dataRuled$time)&dataRuled$id == id,],time.series.id)  
  }
  return(dataRuled)
  
}


#generic rule function 
rule <- function(...){
  out <- c()
  return(out)}
