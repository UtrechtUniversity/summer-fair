#######################################################################
#                                                        
#                  Global algorithm                                 
#                 - apply meta-analyses methods to output of local algorithm                       
#                                                        
#                  Author: Egil A.J. Fischer                              
#                  Contact: e.a.j.fischer@uu.nl                             
#                  Creation date: 13-12-2021
#######################################################################

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

# ## First specify the packages of interest
# packages = c("tidyverse",
#              "meta",
#              "metafor")
# 
# ## Now load or install&load all
# package.check <- lapply(
#   packages,
#   FUN = function(x) {
#     if (!require(x, character.only = TRUE)) {
#       install.packages(x, dependencies = TRUE)
#       library(x, character.only = TRUE)
#     }
#   }
# )

library(meta)
library(metafor)
library(tidyverse)
#combine.estimates of glm's
combine.estimates.glm <- function(local.output,
                              select.treatment = "All"){
  #get the means and standard errors
  estimates = NULL;
  for(i in c(1:length(local.output)))      {
        estimates <- rbind(estimates,
                           cbind(data.frame(mean = summary(local.output[[i]])$coefficients[,1],
                                            se  = summary(local.output[[i]])$coefficients[,2],
                                 treatment = names(summary(local.output[[i]])$coefficients[,1])),
                                 study = i))
  }
  if(select.treatment == "reference" | select.treatment == "control" )
    estimates <- estimates%>%filter(treatment == "(Intercept)")
  else if(treatment!= "All")
    estimates <- estimates%>%filter(treatment == select.treatment)
  
  print(estimates$mean)
  print(estimates$se)
  print(estimates$study)
  print(estimates$treatment)
  #simply only use the intercepts
  metaout<- metagen(TE =  estimates$mean,
                    seTE = estimates$se,
                    studlab = estimates$study,
                    subgroup = estimates$treatment,
                    fixed = FALSE)
  
  return(metaout)
}
