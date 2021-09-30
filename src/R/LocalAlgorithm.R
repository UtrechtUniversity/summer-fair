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
                   rule = "function", #rule used to determine if sample is positive or negative
                   pars ="list" #list of estimated parameters options are of each the list contains the name and distribution
                                #R = reproduction number
                                #beta = transmission coefficient within a group
                                #betab = transmission coefficient between groups
                                #InfectiousPeriod = infectious period
                                #alpha = shape parameter distance dependent transmission
                                #r0 = scale parameter distance dependent transmission
                         ))
getClass("TransmissionEstimate")
 obj =new("TransmissionEstimate", 
          n = 10, 
          likelihood = function(R){R*(1-R)},
          rule = rule.sincefirst,
          pars = list("R"))

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
applyRule <- function(data,rule,var.id, ...){
  #set new data set arranged by times
  dataRuled <- data%>%arrange(times);
  sir<- NULL;#0 indicates susceptible individual
  for(cid in unique(dataRuled$id)){

    dataRuled[dataRuled$id ==cid,"sir"] <-  dataRuled%>%           
         filter(id == cid)%>%   #subset the particular individual
         arrange(times)%>%       #arrange samples by time
         rule(var.id,...)         #apply rule to determine infection status (0 = susceptible, 1= latent, 2 = infectious, 3 = recovered)
     
  }
  return(dataRuled)
}

##arrange data for specific method####
arrangeData <- function(data, 
                        rule,
                        id.vars,
                        method = "glm",
                        ...){
  return(eval(parse(text = paste0("arrangeData.", method)))(applyRule(data,rule,...)))

}

arrangeData.glm<-function(rdata){
  #for a standard glm approach requires
  group.data <- NULL;
  #1. time intervals (length)
  group.data$times <- rdata%>%
    group_by(location,times) %>% 
    summarize(mean(times))
  group.data$dt <-data.frame(group.data)%>%
    group_by(times.location)%>%
    summarize(dt = c(-1,tail(times.mean.times.,-1)-head(times.mean.times.,-1)))
  #clean up
  group.data <- data.frame(group.data)[,c("times.location","times.mean.times.","dt.dt")];
  names(group.data)<- c("location","times","dt");
  #2. cases per interval
  indiv.cases <- rdata%>%
    arrange(times)%>% 
    group_by(id) %>%
    summarize(
      location = location,
      times = times,
      case = c(0,as.numeric(head(sir,-1)==0 & tail(sir,-1)>0)));
  cases   <- indiv.cases%>%
    group_by(location,times) %>% 
    summarise(sum(case))
  group.data$cases<- data.frame(cases)[,-1]
  #3. number of infectious individual at start interval
  i <- rdata%>%
    group_by(location,times) %>% 
    summarise(sum(sir == 2))
  group.data$i <- data.frame(i)[,-1]
  
  #4. number of susceptible individuals at start interval
  s <- rdata%>%
    group_by(location,times) %>% 
    summarise(sum(sir == 0))
  group.data$s <- data.frame(s)[,-1]
  #5. number of recovered individuals at start of interval
  r <- rdata%>%
    group_by(location,times) %>% 
    summarise(sum(sir == 3))
  group.data$r <- data.frame(r)[,-1]
  #6. covariates of the group
  
  return(group.data)
}

glmdata <- arrangeData(mockdata,
            rule.sincefirstinfectioustestrecovered, 
            var.id = tail(names(mockdata),3),
            infrec = list(inf=c(2),rec=c(3)) )  

names(data.frame(glmdata))
head(data.frame(glmdata))


## perform analysis ####
analyseTransmission<- function(data,
                               rule,
                               est.par,
                               method ="glm",
                               ...){
  
}





