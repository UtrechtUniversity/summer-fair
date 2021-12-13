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

## First specify the packages of interest
packages = c("tidyverse",
             "meta",
             "metafor")

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

#load output of local algorithms ####
stud1 <- rnorm(100, 1,1)
stud2 <- rnorm(50, 1,1)
stud3 <- rnorm(5, -1,1)
stud4 <- rnorm(50, 1,1)
std.error<-function(v){sd(v)/sqrt(length(v))}
outputlocal<- rbind(out1 = data.frame(output = "1",treat = c("A","B"),mean = c(mean(stud1),mean(stud2)),se = c(std.error(stud1),std.error(stud2)), n = c(length(stud1),length(stud2))),
                   out2 = data.frame(output = "2",treat = c("A","B"),mean = c(mean(stud3),mean(stud4)),se = c(std.error(stud3),std.error(stud4)), n = c(length(stud3),length(stud4))))

#do the meta analysis
metaout<- metagen(TE =  outputlocal$mean,
                  seTE = outputlocal$se,
                  studlab = outputlocal$output,
                  subgroup = outputlocal$treat,
                  fixed = FALSE)
#print summary
summary(metaout)
#plot forest
forest.meta(metaout)
funnel.meta(metaout,studlab=TRUE,contour = c(0.9, 0.95, 0.99))
