############################################################################
#                                                                          #
#  Local algorithm estimation of transmission parameters                   #
#  This script contains functions in three groups:                         #
#  - Functions to arrange the data for use in the estimation procedures    #
#  - Wrapper functions to do the analyses that call on data arrange and estimation functions                                 #
#  - Estimation functions
#
#                  Author:Egil A.J. Fischer                                #
#                  Contact: e.a.j.fischer@uu.nl                            #
#                  Creation date: 21-9-2021                                #
############################################################################

## install and load packages ####
packages =c(#"SPARQL",
             "ggplot2",
             "tidyverse",
             "rlog",
             "yaml",
            # "rstan",     #not yet used
            #"shinystan", #not yet used
            #"rstanarm",  #not yet used
             "bbmle",
            "docstring")# the packages of interest

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
#silence this warning because it has no use in this code
options(dplyr.summarise.inform = FALSE) 


###########################################################################
#   Functions to arrange the data                                         #
###########################################################################

# function apply a rule to the data to determine status of a host####
applyRule <- function(data,
                      rule,
                      var.id, 
                      ...){
  #'@title Apply a data interpretation rule
  #'@description status of a sample is determined by the own value and all other values of that chicken. status of a sample can be determined by one value or multiple inputs
  #' @param data data set or data sets of a host
  #' @param rule a function defined either by the user or selected from DataInterpretationRules.R
  #' @param var.id vector with variable names to be used to determine status of host
  #' @return data frame containing the same variables as data and an additional variable "sir" indicating state of host (0 = susceptible, 1= latent, 2 = infectious, 3 = recovered)
  

  #check data consistency with required variable names
  if(!all(c("host_id","times") %in% names(data))) {
    stop("Not all required column names (host_id,times) are present.")
  }
  
  #set new data arranged by times
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

#create a numeric time from a set of time variables####
setTimes<- function(input,                 #data set
                    resolution = "day",   #return time at this resolution allowed is second, minute, hour, day, week, month, year
                    decimals = 1,
                    ...){
  #' @title Set times to a specific resolution and number of decimals
  #' @description Change a numeric value in input variables into the required time resolution (second, minute, hour, day, week, month, year). Data should thus contain at least one of the following variables c("ex_sec","ex_min","ex_hour","ex_day","ex_week","ex_year")
  #' @param input times
  #' @param resolution string define the resolution (second, minute, hour, day, week, month, year)
  #' @param decimals  number of decimals to round the time to
  #' @return vector of times
  
  
  timevars <- c("ex_sec","ex_min","ex_hour","ex_day","ex_week","ex_year");
  #timevarsininput<-timevars[timevars%in%names(input)]
  times <-  NA;
  multiplicationfactor <- c(ex_sec = 1,
    ex_min = 60,
    ex_hour= 60*60,
    ex_day = 60*60*24,
    ex_week = 60*60*24*7,
    ex_year = 60*60*24*365);
  #ugly code but does the job of setting it to seconds
  for(t in timevars[timevars%in%names(input)])  {
        if(is.na(times[1])){ 
          times <- replace_na(as.numeric(input[,t]),0)*multiplicationfactor[t]} else{
          times <- times+replace_na(as.numeric(input[,t]),0)*multiplicationfactor[t]}
  }; times<- unlist(times)
  #round off to one decimal given the resolution
  return((times/ multiplicationfactor[paste0("ex_",resolution)])%>%round(decimals))
} 
 
##generic function to arrange input for analysis. Based on methods it will call another function####
arrangeData <- function(data, 
                        rule,
                        var.id,
                        method = "glm",   #which analyses method
                        covariates = NULL, #covariate names
                        control = "none",  #control group name
                        remInoCase = TRUE,     #remove inoculated animals as potential case
                        inoMarker = "I",    #marker used for inoculated animals
                        Echo = FALSE,  #print analysis type
                        ...){
  #' @title Arrange data for estimation
  #' @description
    #' This method calls on other method to arrange the input in such a way that it can be used in estimation procedures. 
  #' @param data input data
  #' @param rule rule function to define infection status of host
  #' @param var.id vector of variable names to use for define infection status
  #' @param method estimation method ("glm","mll","finalsize" )
  #' @param covariates covariates in estimation procedure
  #' @param control value in variable treatment defining the control group
  #' @param remInoCase if TRUE remove inoculated hosts that are not excreting as potential cases
  #' @param inoMarker value defining inoculated hosts
  #' @param Echo print analysis type before start of estimation procedure
  #' @return Dataframe that can be used for estimation procedures
  
  
  #remove variables with all NA
  data <- data %>%  select(
      where(
        ~!all(is.na(.x))
      )
    )
  #set times
  data$times <- setTimes(data,...) #To do: should that be here or else where
  #set value of treatment that is the control to "control"
  data<-data%>%
    mutate(treatment = replace(treatment, treatment == control, "control"))%>%as.data.frame
    
  #print the method of analysis
  if(Echo){print(paste("Arrange data for", method, "analysis."))};
  
  if(!is.null(data$sample_measure ))
  {
    #check for and replace non numeric values in sample_measure
    neg.vals = c("neg","-","NEG","Neg")
    data <- data%>%mutate(sample_measure = replace(sample_measure,is.element(sample_measure,neg.vals),-999))
    data <- data%>%mutate(sample_measure = replace(sample_measure,is.na(as.numeric(sample_measure)),NA))
  }
  
  #select specific method
  return(eval(str2expression(text = paste0("arrangeData.", method)))
         (applyRule(data,rule,var.id,...),covariates,remInoCase))
}


### Arrange data for maximum likelihood  models ####
arrangeData.mll<-function(rdata,           #data
                          covariates = NULL, #covariate column names
                          remInoCase = TRUE,  #remove inoculated animals as potential case
                          inoMarker = "I",    #marker used for inoculated animals
                          reference = NULL,
                          ...){ 
  #'@title Arrange data for MLL estimation
  #'@description  function to arrange data such that it can be used in teh MLL estimation procedure. 
  #'@param rdata data arranged for this specific estimation method
  #'@param covariates variable names of covariates
  #'@param inoMarker value defining inoculated hosts
  #'@param reference value in treatment that is used as reference in estimation procedures (can be different than control)
  #'@return Dataframe that can be used for estimation procedures

  #if no round in the data add
  if(is.null(rdata$round))rdata$round <- 1;
  #get the group mixinglevels 
  mixinglevels  = names(rdata)[names(rdata)%>%
                                 str_detect("level")]%>%
                                        sort(decreasing = TRUE)
 
  
 
  #for a standard glm approach requires
  group.data <- NULL;
  #1. time intervals (length)
  group.data <- rdata%>%
      group_by(across(c("round", all_of(mixinglevels) ,"times"))) %>% 
      summarize(times = mean(times))
  group.data <-group.data%>%
         mutate(dt = c(times-lag(times)))%>%
         ungroup%>%as.data.frame
  
  #2. cases per interval
  indiv.cases <- rdata%>%
    filter(if(remInoCase)!str_detect(inoculationStatus,inoMarker)else TRUE)%>%
    arrange(across(c(host_id,times)))%>% 
    mutate(case = as.numeric(sir > 0 & lag(sir)==0))
    
  cases   <- indiv.cases%>%
    group_by(across(c("round",all_of(mixinglevels) ,"times"))) %>% 
    summarise(cases = sum(case,na.rm = TRUE))
  group.data <- group.data%>%full_join(cases, by = c("round",all_of(mixinglevels),"times")) 
  #3. number of infectious individual at start interval at each of the levels
  #level 1
  i1 <- rdata%>%
    group_by(across(c("round",all_of(mixinglevels) ,"times"))) %>% 
    summarise(i1 = sum(sir == 2,na.rm = TRUE))
  group.data <- group.data%>%full_join(i1, by = c("round",all_of(mixinglevels),"times"))
  
 
  #here also take into account infectious individuals in other levels
  if(length(mixinglevels) > 1){
    #level 2
    i2 <- rdata%>%
      group_by(across(c("round",all_of(mixinglevels)[-2] ,"times"))) %>% #negative indexing due to descending ordering
      summarise(i2 = sum(sir == 2,na.rm = TRUE))
    group.data <-full_join(group.data,data.frame(i2));
    group.data$i2 <- group.data$i2-group.data$i1;
    if(length(mixinglevels)>2)
    {
      #level 3
      i3 <- rdata%>%
        group_by(across(c("round",all_of(mixinglevels)[-3] ,"times"))) %>% #negative indexing due to descending ordering
        summarise(i3 = sum(sir == 2,na.rm = TRUE))
      group.data <- full_join(group.data,data.frame(i3));
      group.data$i3 <- group.data$i3-group.data$i2-group.data$i1;
    }else  {group.data$i3 <- 0;} #default value
  
  }else { group.data$i2 <- 0;#default values
    group.data$i3 <- 0;#default values
  }
  #4. number of susceptible individuals at start interval at level 1
  s1 <- rdata%>%
    filter(if(remInoCase)!str_detect(inoculationStatus,inoMarker)else TRUE)%>%
    group_by(across(c("round",all_of(mixinglevels) ,"times"))) %>% 
    summarise(s = sum(sir == 0,na.rm = TRUE))%>%as.data.frame
  group.data <- group.data%>%full_join(s1, by = c("round",all_of(mixinglevels),"times"))
  #5. number of recovered individuals at start of interval at level 1
  r <- rdata%>%
    group_by(across(c("round",all_of(mixinglevels) ,"times"))) %>% 
    summarise(r = sum(sir == 3,na.rm = TRUE))%>%as.data.frame
  group.data <- group.data%>%full_join(r, by = c("round",all_of(mixinglevels),"times"))
  #6. total number of individuals in level 1
  n1 <- rdata%>%
    group_by(across(c("round",all_of(mixinglevels) ,"times"))) %>% 
    summarise(n1 = sum(!is.na(sir)))%>%as.data.frame
  group.data <- group.data%>%full_join(n1, by = c("round",all_of(mixinglevels),"times"))

  #here also take into account infectious individuals in other levels
  if(length(mixinglevels) > 1){
    #level 2
    n2 <- rdata%>%
      group_by(across(c("round",all_of(mixinglevels[-2]) ,"times"))) %>% #negative indexing due to descending ordering
      summarise(n2 = sum(!is.na(sir)))%>%as.data.frame
    group.data <- full_join(group.data,data.frame(n2));
    group.data$n2 <- group.data$n2-group.data$n1
    if(length(mixinglevels)>2)
    {
      #level 3
      n3 <- rdata%>%
        group_by(across(c("round",all_of(mixinglevels[-3]) ,"times"))) %>% #negative indexing due to descending ordering
        summarise(n3 = sum(!is.na(sir)))%>%as.data.frame
      group.data <- full_join(group.data,data.frame(n3));
      group.data$n3 <- group.data$n3-group.data$n2-group.data$n1
    }else{
      group.data$n3 <- 0;#default values
    }
    
  }else{
    group.data$n2 <- 0;#default values
    group.data$n3 <- 0;#default values
  }
  #7. check or determine group co-variates
  if(!is.null(covariates)){
    covariate.data <- group.data;
    cat("\n For group level summarizing in arranging the data for a glm, the minimum of the group is used.")
    for(j in covariates){
    covariate <- rdata%>%
        group_by(across(c("round",all_of(mixinglevels) ,"times"))) %>% 
        summarize(covar = min(eval(parse(text = j))))%>%
        ungroup
    
    covariate.data[j]<-covariate$covar;
    }
  }
  # S, I and R should be the numbers at the beginning of the interval dt
  # cases the numbers after the interval, thus shift SIR values to one time later
  # the first time moment should be removed
  group.data <- group.data %>%
    group_by(across(c("round",all_of(mixinglevels) ))) %>% 
    summarize(
      times = tail(times,-1),
      dt = tail(dt,-1),
      cases = tail(cases,-1),
      s = as.numeric(head(s, -1)),
      i1 = as.numeric(head(i1, -1)),
      i2 = as.numeric(head(i2, -1)),
      i3 = as.numeric(head(i3, -1)),
      r = as.numeric(head(r, -1)),
      n1 = as.numeric(head(n1,-1)),
      n2 = as.numeric(head(n2,-1)),
      n3 = as.numeric(head(n3,-1))
      )%>%ungroup
  if(!is.null(covariates)){
    covariate.data <- covariate.data%>%
      group_by(across(c("round",all_of(mixinglevels) ))) %>% 
      select(all_of(covariates))%>%
      filter(row_number()>1)%>%
      ungroup
    group.data<- full_join(group.data, covariate.data[,c("round",all_of(mixinglevels) ,covariates)])
  }
  #set reference level of treatment
  if(!is.null(reference))
  {
    group.data <- within(group.data, 
                         treatment <- relevel(treatment, ref = eval(reference)))
  }
  return(list(arranged.data  = group.data))
}

#arrange data for glm is same as for mll####
arrangeData.glm <- arrangeData.mll

### For final size ####
arrangeData.finalsize<-function(rdata,           #data
                          covariates = NULL, #covariate column names
                          remInoCase = TRUE,     #remove inoculated animals as potential case
                          inoMarker = "I",    #marker used for inoculated animals
                          ...
){ 
  #'@title Arrange data for Final Size estimation
  #'@description  function to arrange data such that it can be used in Final Size estimation procedure. 
    #' !Not yet implememented! This function will throw an error!
  #'@param rdata data arranged for this specific estimation method
  #'@param covariates variable names of covariates
  #'@param inoMarker value defining inoculated hosts
  #'@param reference value in treatment that is used as reference in estimation procedures (can be different than control)
  #'@return Dataframe that can be used for estimation procedures
  stop("NOT FINISHED ~ to do  in 2024")
  #for a finals approach requires
  #1. Get the initial susceptibles
  #rdata %>% 
  minday <- min(datawithrule$ex_day)
  maxday <- max(datawithrule$ex_day)
  iS <- datawithrule%>%
    filter(inoculationStatus == "S1")%>%
    filter(ex_day == minday)%>%
    group_by(level2)%>%
    summarise(s = sum(sir==0,na.rm =TRUE))
   fS <-  datawithrule%>%
     filter(inoculationStatus == "S1")%>%
     filter(ex_day == maxday)%>%
     group_by(level2)%>%
     summarise(s = sum(sir==0,na.rm =TRUE))
   iI <- (datawithrule%>%
     filter(inoculationStatus == "I")%>%
     group_by(host_id)%>%
     summarise(level2 = level2,
       maxstatus = sum(max(sir,na.rm=TRUE))>1))%>%
     group_by(level2)%>%summarise(i = sum(maxstatus))
     
    group.data <- NULL;
 
  return(group.data)
}

###########################################################################
#   Wrapper functions to arrange the data and do the estimation           #
###########################################################################

# perform analysis ####
analyseTransmission<- function(inputdata,          #input data
                               rule,          #rule to determine whether sample is positive or negative
                               var.id,        #variables to determine apply rule to
                               method ="mll", #estimation method
                               preventError = FALSE, #remove those entries with FOI = 0 but cases>1
                               covars = "1", #covariates
                               remInoCase = TRUE,     #remove inoculated animals as potential case
                               inoMarker = "I",    #marker used for inoculated animals
                               reference = NULL,
                               ...){~
  #'@title Arrange data and perform analyses
  #'@description  This function is a wrapper around the arrangement of data and conducting the analyses. It calls on function arrangeData and run."method"
  #'@param inputdata a dataset that is organized according to the Infectious Transmission ontology
  #'@param rule function to define infection status of host
  #' @param var.id vector of variable names to use for define infection status
  #' @param method estimation method ("glm","mll","finalsize" )
  #' @param preventError True passed to function run.<<method>> 
  #' @param covars covariates in estimation procedure. Default = "1" i.e. only intercept
  #' @param remInoCase if TRUE remove inoculated hosts that are not excreting as potential cases
  #' @param inoMarker value defining inoculated hosts
  #'@param reference value in treatment that is used as reference in estimation procedures (can be different than control)
  #' @return output of estimation procedure
  
  #create a log data frame
  cat("Analysing transmission using ",method, "at", format(Sys.time(), "%a %b %d %X %Y"),"\n");
  #arrange data for analysis
  data.arranged <- arrangeData(data = inputdata,
                               rule = rule,
                               var.id = var.id,
                               method = method,
                               covariates = covars,
                               remInoCase = remInoCase,     #remove inoculated animals as potential case
                               inoMarker = inoMarker,    #marker used for inoculated animals
                               ...)

  
  
  #data
  data.arranged <- data.arranged[[1]]
  
  #remove those entries without susceptibles (contain no information and cause errors)
  data.arranged <- data.arranged%>%filter(s>0)
  data.arranged$treatment <- factor(data.arranged$treatment, 
                                    ordered = FALSE)
 if(!is.null(reference)){ if(any(str_detect(eval(reference),as.character(data.arranged$treatment)))){
  data.arranged <- within(data.arranged, 
                          treatment <- relevel(treatment, ref = eval(reference))) }else print(paste("Control group:  ", eval(reference), "not present"))}
 
  #do analysis by running tthe method
  fit <- eval(str2expression(paste0("run.",method)))(covars = covars,
                                              data.arranged = data.arranged, 
                                              preventError = preventError)
   
  #return covariate entries (categorical = values or numeric = range)
  if(length(covars) > 0)
  {
    covar.values = list()
    for(j in covars){}
      if(is.factor(data.arranged[[j]])){covar.values <- append(covar.values, unlist(levels(data.arranged[[j]])))}
    else{covar.values <- append(covar.values, range(data.arranged[,j]))}
  }

  #return outcome
  return(list(covariates = list(names = covars,
                                values = covar.values),
              estimation = fit))
}



#run local algorithm for each data set ####
get.local.transmission <- function(dataset,config.file =  "src/R/summerfair_config.yaml"){
  #'@title Arrange data and perform analyses based on a configuration file 
  #'@description  This function is a wrapper around the arrangement of data and conducting the analyses. It calls on function arrangeData and run."method" based on a configuration file.
  #'@param dataset a dataset that is organized according to the Infectious Transmission ontology
  #'@param config.file location of configuration file
  #' @return output of estimation procedure
  
  #read configuration file and perform analysis given the configuration
  config <-read_yaml(file = config.file);
  
  #start sink to log-file
  if(config$write_log){
  log.file <- paste0("summerfair.log");
  if(config$over_write_log){
    file.remove(log.file)
  }
  zz <- file(log.file, open = "at");
  sink(zz, type ="message");
  sink(zz, type ="output");
  cat("\n-------*summerfair*-----------\n","Start log at: ",  format(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n")}
   if(length(dataset)==0)stop("Empty data set cannot be analysed")
     
  source("src/R/DataQuality.R") #run the data quality report. Location and file hard coded so this cannot be changed by user
  source(config$rule_script)#run the data interpretation rules
  rule.name <- config$rule_name
             
  #get the variable determining status of individuals
  var.id = if(all(is.na(dataset$sample_measure))){c("sample_result")}else{ c("sample_measure")}
  
  #determine the control group
  control = ifelse(any(grepl('control',dataset$treatment,ignore.case = T)),"control",
                  ifelse(any(grepl('0',dataset$treatment)),"0",""))
  
  
  #determine the rule (add detection limits or recoding if required) 
  rule = if(!all(is.na(dataset$sample_measure))) {
                    # check if dealing with detection limit data or not
                    if(all(dataset$detectionLimit==""|is.na(dataset$detectionLimit))){
                    eval(str2expression(paste0("rule.",rule.name,"cutoff")))}else {eval(str2expression(paste0("rule.",rule.name,".cutoff.detectionLimit")))}}
                             else{
                    #check if the data consists of 0, 1 and empty spaces
                                 if(any(sapply(c('^0$','^1$',"^$"),
                                               FUN = function(x){any(grepl(x,dataset$sample_result ))} ))){
                                   eval(str2expression(paste0("rule.",rule.name)))}else{eval(str2expression(paste0("rule.",rule.name,".recode")))} 
                             }
  cat("Used rule:\t");
  cat(paste(rule.name,"\n"));
  
  
  #marker for inoculation
  inomarker = if(any(str_detect(dataset$inoculationStatus,"2"))){"2"}else {"I"}
 
  #check data quality
  quality.report <- report.DataQuality(dataset);
                   
  #perform analysis based on configuration 
  analysis <- analyseTransmission(inputdata = dataset, #data set
                                             rule = rule, #rule to determine infection status
                                             var.id = var.id,  #variable defining infection status
                                             method = config$method, #estimation method
                                             cutoff = config$cutoff, #cutoff value for infection status
                                             codesposnegmiss = config$codesposnegmiss, #values determining infection status pos, neg of missing
                                             preventError = TRUE, #TRUE = remove entries with > 1 case but FOI = 0
                                             covars = config$covars, #co variants
                                             reference = config$reference, #reference category for multivariable estimation
                                             control = control,
                                             inoMarker = inomarker)    #marker used for inoculated animals)   #value of control treatment
                
                  if(config$write_log){
                    #close log file
                  cat("\n End log at: ", format(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n-------*summerfair*-----------\n");
                  sink(type = "message");
                  sink(type = "output");
                  close(zz);
                  closeAllConnections();}
                
                return(list(analysis = analysis,qualityreport = quality.report));
                
                  }




###########################################################################
#   Functions to perform the estimation                                   #
###########################################################################


# run.glm is a function equal to glm but will filter time points that cannot be used. 
# run.glm cannot deal with multiple levels. If multiple levels are present the run.mml method should be used
run.glm<-function(covars,
                  data.arranged,
                  preventError = TRUE){
  #' @title Estimation using a generalized linear model
  #' @description Estimate the transmission coefficient using a generalized linear model.It cannot deal with transmission at different levels (e.g. within- and between-pen)
  #' @param covars Covariates
  #' @param data.arranged Data set properly arranged for the use of a glm
  #' @param preventError default TRUE. Filter for instances without susceptibles or infectious hosts.
  #' @return returns output of glm
  
  #filter intervals without infected animals. 
  if(preventError){
    data.filtered <- data.arranged %>% filter(i1  > 0 &!is.na(i1  )&!is.na(cases)&!is.na(s)&!is.na(i1)&!is.na(r));
  }else data.filtered <- data.arranged
    
  #if covariates only have one unique value it cannot be used in the glm
  use.covars <- covars[data.arranged[,covars]%>%unique%>%unlist%>%length>1]
  if(length(use.covars)==0){use.covars <- "1"}
  #determine whether other levels exist
  if(sum(data.arranged$i2)+sum(data.arranged$i3)>0) warning("There seem to be more levels in this data set. This cannot be analyzed by this GLM function");
  #check if filtered data contains any rows
  if(length(data.filtered$i1)==0)stop("No rows with data present in this data set after filtering i>0.")
  #Do the analysis
  return(glm(as.formula(paste("cbind(cases, s - cases) ~ ", paste(use.covars, collapse= "+"))),
                  family = binomial(link = "cloglog"), 
                  offset = log((i1/n1)*dt),
                  data = data.filtered 
                  ))
 
}

##################################################################################
#run.mll is a function equal to that will filter time points that cannot be used, 
#and produces a number of estimates for within- and between level transmission
run.mll<-function(covars,
                  data.arranged,
                  preventError = TRUE){
  #' @title Estimation via maximum likelihood
  #' @description Estimate the transmission coefficient using a maximum likelihood method.It can deal with transmission at different levels (e.g. within- and between-pen)
  #' @param covars Covariates -!NOt yet properly implemented!
  #' @param data.arranged Data set properly arranged for the use of a glm
  #' @param preventError default TRUE. Filter for instances without susceptibles or infectious hosts.
  #' @return Outcomes of maximum likelihood estimation
  
  #filter intervals without infected animals. 
  if(preventError){
    data.filtered <- data.arranged %>% filter(i1 + i2 + i3 > 0 &!is.na(i1 + i2 + i3 )&!is.na(cases)&!is.na(s)&!is.na(i1)&!is.na(r));
  }else data.filtered <- data.arranged
  
  
  #determine number of levels
  levels = "L1" ;
  if(sum(data.filtered$i2)>0)levels = "L2";
  if(sum(data.filtered$i3)>0)levels = "L3";
  
  #Do the analysis
  #loop over covariates
  # for(cv in covars){
  # #covariate is numeric - to do
  # 
  # #covariate is categorical
  #   if(is.string(data.filtered[,cv])){
  #     uniquevalues <- data.filtered[,cv]%>%unique
  #   }
  # }
  
  logl <- switch(levels,
               "L1" = {
                  data = data.filtered;
                  logl = function(beta1 = 1.){
                      -sum((data$cases*log(1-exp(-(data$dt*beta1*data$i1/data$n1)))-
                            (data$s-data$cases)*(data$dt*beta1*data$i1/data$n1)))}
                  },
                "L2" = {
                  data = data.filtered;
                  logl = function(beta1 =1, beta2=0.1){
                    -sum((data$cases*log(1-exp(-(beta1*data$i1/data$n1 + 
                                               beta2*data$i2/data$n2)*data$dt))-
                           (data$s-data$cases)*(beta1*data$i1/data$n1 + beta2*data$i2/data$n2)*data$dt))}}
                ,
                "L3" = {
                  data = data.filtered;
                  logl = function(beta1=1,beta2=.1,beta3=0.01){
                    -sum((data$cases*log(1-exp((beta1*data$i1/data$n1 + 
                                               beta2*data$i2/data$n2+
                                               beta3*data$i3/data$n3)*data$dt))+
                            (data$s-data$cases)*(beta1*data$i1/data$n1 + 
                                     beta2*data$i2/data$n2+
                                     beta3*data$i3/data$n3)*data$dt))}}
                
                )
  #fit and return result
  fit <- mle2(minuslogl = logl);
  return(fit)
}
