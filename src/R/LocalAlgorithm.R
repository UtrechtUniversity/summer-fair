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
             "rstan",     #not yet used
             "shinystan", #not yet used
             "rstanarm",  #not yet used
             "bbmle")

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
  for(cid in unique(dataRuled$host_id)){

    dataRuled[dataRuled$host_id ==cid,"sir"] <-  dataRuled%>%           
         filter(host_id == cid)%>%   #subset the particular individual
         arrange(times)%>%       #arrange samples by time
         rule(var.id,...)         #apply rule to determine infection status (0 = susceptible, 1= latent, 2 = infectious, 3 = recovered)
     
  }
  return(dataRuled)
}

##create a numeric time ####
setTimes<- function(input,       #data set
                    resolution, #return time at this resolution allowed is second, minute, hour, day, week, month, year
                    decimals = 1){
  timevars <- c("ex_sec","ex_min","ex_hour","ex_day","ex_week","ex_year");
  timevarsininput<-timevars[timevars%in%names(input)]
  times <-  NA;
  multiplicationfactor <- c(ex_sec = 1,
    ex_min = 60,
    ex_hour= 60*60,
    ex_day = 60*60*24,
    ex_week = 60*60*24*7,
    ex_year = 60*60*24*365);
  #ugly code but does the job of setting it to seconds
  for(t in timevars[timevars%in%names(input)])  {
    times <- as.vector(ifelse(is.na(times), 
                    input[,t]*multiplicationfactor[t],
                    times+input[,t]*multiplicationfactor[t]))
  }; times<- unlist(times)
  #round off to one decimal given the resolution
  return((times/ multiplicationfactor[paste0("ex_",resolution)])%>%round(decimals))
} 
 
##arrange input for analysis####
arrangeData <- function(data, 
                        rule,
                        id.vars,
                        method = "glm",
                        covariates = NULL,
                        InoCase = TRUE,
                        ...){
  print(paste("Arrange data for", method, "analysis."));
  return(eval(parse(text = paste0("arrangeData.", method)))
         (applyRule(data,rule,...),covariates,InoCase))
}

##arrange data for specific analysis
arrangeData.glm<-function(rdata,           #data
                          covariates = NULL, #covariate column names
                          InoCase = TRUE  #remove inoculated animals as potential case
                          ){ 
  #get the group mixinglevels 
  mixinglevels  = names(rdata)[names(rdata)%>%str_detect("level")]%>%sort(decreasing = TRUE)
  #for a standard glm approach requires
  group.data <- NULL;
  #1. time intervals (length)
  group.data <- rdata%>%
    group_by(across(c(mixinglevels ,"times"))) %>% 
    summarize(times = mean(times))
  group.data <-group.data%>%
         summarize(times = times,
          dt = c(-1,tail(times,-1)-head(times,-1)))%>%
        ungroup
    
  
  #2. cases per interval
  indiv.cases <- rdata%>%
    filter(if(InoCase)!str_detect(inoculationStatus,"I")else TRUE)%>%
    arrange(times)%>% 
    summarize(
      host_id = host_id,
      across(mixinglevels ),
      times = times,
      case = c(as.numeric(head(sir,-1)==0 & tail(sir,-1)>0),0));
    
  cases   <- indiv.cases%>%
    group_by(across(c(mixinglevels ,"times"))) %>% 
    summarise(sum(case,na.rm = TRUE))
  group.data$cases<- data.frame(cases)[,ncol(cases)]
  #3. number of infectious individual at start interval at each of the levels
  #level 1
  i <- rdata%>%
    group_by(across(c(mixinglevels ,"times"))) %>% 
    summarise(sum(sir == 2,na.rm = TRUE))
  group.data$i <- data.frame(i)[,ncol(i)]
  group.data$i2 <- 0;#default values
  group.data$i3 <- 0;#default values
  #here also take into account infectious individuals in other levels
  if(length(mixinglevels) > 1){
    #level 2
    i2 <- rdata%>%
      group_by(across(c(mixinglevels[-2] ,"times"))) %>% #negative indexing due to descending ordering
      summarise(sum(sir == 2,na.rm = TRUE))
    group.data$i2 <- data.frame(i2)[,ncol(i2)]-group.data$i
    if(length(mixinglevels)>2)
    {
      #level 3
      i3 <- rdata%>%
        group_by(across(c(mixinglevels[-3] ,"times"))) %>% #negative indexing due to descending ordering
        summarise(sum(sir == 2,na.rm = TRUE))
      group.data$i3 <- data.frame(i3)[,ncol(i3)]-group.data$i2-group.data$i1
    }
  
  }
  #4. number of susceptible individuals at start interval
  s <- rdata%>%
    filter(if(InoCase)!str_detect(inoculationStatus,"I")else TRUE)%>%
    group_by(across(c(mixinglevels ,"times"))) %>% 
    summarise(sum(sir == 0,na.rm = TRUE))
  group.data$s <- data.frame(s)[,ncol(s)]
  #5. number of recovered individuals at start of interval
  r <- rdata%>%
    group_by(across(c(mixinglevels ,"times"))) %>% 
    summarise(sum(sir == 3,na.rm = TRUE))
  group.data$r <- data.frame(r)[,ncol(r)]
  #6. total number of individuals
  n <- rdata%>%
    group_by(across(c(mixinglevels ,"times"))) %>% 
    summarise(sum(!is.na(sir)))
  group.data$n <- data.frame(n)[,ncol(n)]
  group.data$n2 <- 0;#default values
  group.data$n3 <- 0;#default values
  #here also take into account infectious individuals in other levels
  if(length(mixinglevels) > 1){
    #level 2
    n2 <- rdata%>%
      group_by(across(c(mixinglevels[-2] ,"times"))) %>% #negative indexing due to descending ordering
      summarise(sum(!is.na(sir)))
    group.data$n2 <- data.frame(n2)[,ncol(n2)]-group.data$n
    if(length(mixinglevels)>2)
    {
      #level 3
      n3 <- rdata%>%
        group_by(across(c(mixinglevels[-3] ,"times"))) %>% #negative indexing due to descending ordering
        summarise(sum(!is.na(sir)))
      group.data$n3 <- data.frame(n3)[,ncol(n3)]-group.data$n2-group.data$n
    }
    
  }
  #7. check or determine group co-variates
  if(!is.null(covariates)){
    covariate.data <- group.data;
    for(j in covariates){
    covariate <- rdata%>%
        group_by(across(c(mixinglevels ,"times"))) %>% 
        summarize(covar = mean(eval(parse(text = j)), na.rm = TRUE))%>%
        ungroup
    
    covariate.data[j]<-covariate$covar;
    }
  }
  # S, I and R should be the numbers at the beginning of the interval dt
  # cases the numbers after the interval, thus shift SIR values to one time later
  # the first time moment should be removed
  group.data <- group.data %>%
    group_by(across(c(mixinglevels ))) %>% 
    summarize(
      times = tail(times,-1),
      dt = tail(dt,-1),
      cases = tail(cases,-1),
      s = as.numeric(head(s, -1)),
      i = as.numeric(head(i, -1)),
      i2 = as.numeric(head(i2, -1)),
      i3 = as.numeric(head(i3, -1)),
      r = as.numeric(head(r, -1)),
      n = as.numeric(head(n,-1)),
      n2 = as.numeric(head(n2,-1)),
      n3 = as.numeric(head(n3,-1))
      )%>%ungroup
  if(!is.null(covariates)){
    covariate.data <- covariate.data%>%
      group_by(across(c(mixinglevels ))) %>% 
      select(all_of(covariates))%>%
      filter(row_number()>1)%>%
      ungroup
    group.data<- cbind(group.data, covariate.data[,covariates])
    }
  
  return(group.data)
}



## perform analysis ####
analyseTransmission<- function(inputdata,          #input data
                               rule,          #rule to determine whether sample is positive or negative
                               var.id,        #variables to determine apply rule to
                               estpars,       #parameters to estimate
                               method ="glm", #estimation method
                               preventError = FALSE, #remove those entries with FOI = 0 but cases>1
                               ...){
  #arrange data for analysis
  data.arranged <- arrangeData(rdata = inputdata,
                               rule = rule,
                               var.id = var.id,
                               method = method,
                               ...)
  #remove those entries without susceptibles (contain no information and cause errors)
  data.arranged <- data.arranged%>%filter(s>0)
  #deal with potential error
  if(preventError){data.arranged <- data.arranged%>%filter(i>0)}
  #do analysis
  fit <- switch(method,
    glm = glm(cbind(cases, s - cases) ~ 1 ,
              family = binomial(link = "cloglog"), 
              offset = log(i/n)*dt,
              data = data.arranged),
    stop("no other methods than glm")
  )

  
  #return outcome
  return(fit)
}





