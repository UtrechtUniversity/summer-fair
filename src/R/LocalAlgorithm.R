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

###

