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
            # "rstan",     #not yet used
            #"shinystan", #not yet used
            #"rstanarm",  #not yet used
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

### function apply a rule to the data to determine status of a sample ####
# status of a sample is determined by the own value and all other values of that chicken 
# status of a sample can be determined by one value or multiple inputs
applyRule <- function(data,
                      rule,
                      var.id, 
                      ...){
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
setTimes<- function(input,                 #data set
                    resolution = "day",   #return time at this resolution allowed is second, minute, hour, day, week, month, year
                    decimals = 1,
                    ...){
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
        if(is.na(times)){ 
          times <- replace_na(as.numeric(input[,t]),0)*multiplicationfactor[t]} else{
          times <- times+replace_na(as.numeric(input[,t]),0)*multiplicationfactor[t]}
  }; times<- unlist(times)
  #round off to one decimal given the resolution
  return((times/ multiplicationfactor[paste0("ex_",resolution)])%>%round(decimals))
} 
 
##arrange input for analysis####
arrangeData <- function(data, 
                        rule,
                        id.vars,
                        method = "glm",   #which analyses method
                        covariates = NULL, #covariate names
                        control = "none",  #control group name
                        remInoCase = TRUE,     #remove inoculated animals as potential case
                        inoMarker = "I",    #marker used for inoculated animals
                        Echo = FALSE,  #print analysis type
                        ...){
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
    mutate(treatment = replace(treatment, treatment == control, "control"))
    
  #print the method of analysis
  if(Echo){print(paste("Arrange data for", method, "analysis."))};
  #select specific method
  return(eval(parse(text = paste0("arrangeData.", method)))
         (applyRule(data,rule,...),covariates,remInoCase))
}

##arrange data for specific analysis ####
### For generalized linear model and mixed models ####
arrangeData.glm<-function(rdata,           #data
                          covariates = NULL, #covariate column names
                          remInoCase = TRUE,  #remove inoculated animals as potential case
                          inoMarker = "I",    #marker used for inoculated animals
                          reference = NULL,
                          ...){ 
  
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
      group_by(across(c("round", mixinglevels ,"times"))) %>% 
      summarize(times = mean(times))
  group.data <-group.data%>%
         mutate(dt = c(times-lag(times)))%>%
         ungroup
  
  #2. cases per interval
  indiv.cases <- rdata%>%
    filter(if(remInoCase)!str_detect(inoculationStatus,inoMarker)else TRUE)%>%
    arrange(across(c(host_id,times)))%>% 
    mutate(case = as.numeric(sir > 0 & lag(sir)==0))
    
  cases   <- indiv.cases%>%
    group_by(across(c("round",mixinglevels ,"times"))) %>% 
    summarise(cases = sum(case,na.rm = TRUE))
  group.data <- group.data%>%right_join(cases, by = c("round",mixinglevels,"times")) 
  #3. number of infectious individual at start interval at each of the levels
  #level 1
  i <- rdata%>%
    group_by(across(c("round",mixinglevels ,"times"))) %>% 
    summarise(i = sum(sir == 2,na.rm = TRUE))
  group.data <- group.data%>%right_join(i, by = c("round",mixinglevels,"times"))
  group.data$i2 <- 0;#default values
  group.data$i3 <- 0;#default values
  #here also take into account infectious individuals in other levels
  if(length(mixinglevels) > 1){
    #level 2
    i2 <- rdata%>%
      group_by(across(c("round",mixinglevels[-2] ,"times"))) %>% #negative indexing due to descending ordering
      summarise(sum(sir == 2,na.rm = TRUE))
    group.data$i2 <- data.frame(i2)[,ncol(i2)]-group.data$i
    if(length(mixinglevels)>2)
    {
      #level 3
      i3 <- rdata%>%
        group_by(across(c("round",mixinglevels[-3] ,"times"))) %>% #negative indexing due to descending ordering
        summarise(sum(sir == 2,na.rm = TRUE))
      group.data$i3 <- data.frame(i3)[,ncol(i3)]-group.data$i2-group.data$i1
    }
  
  }
  #4. number of susceptible individuals at start interval
  s <- rdata%>%
    filter(if(remInoCase)!str_detect(inoculationStatus,inoMarker)else TRUE)%>%
    group_by(across(c("round",mixinglevels ,"times"))) %>% 
    summarise(s = sum(sir == 0,na.rm = TRUE))
  group.data <- group.data%>%right_join(s, by = c("round",mixinglevels,"times"))
  #5. number of recovered individuals at start of interval
  r <- rdata%>%
    group_by(across(c("round",mixinglevels ,"times"))) %>% 
    summarise(r = sum(sir == 3,na.rm = TRUE))
  group.data <- group.data%>%right_join(r, by = c("round",mixinglevels,"times"))
  #6. total number of individuals
  n <- rdata%>%
    group_by(across(c("round",mixinglevels ,"times"))) %>% 
    summarise(n = sum(!is.na(sir)))
  group.data <- group.data%>%right_join(n, by = c("round",mixinglevels,"times"))
  group.data$n2 <- 0;#default values
  group.data$n3 <- 0;#default values
  #here also take into account infectious individuals in other levels
  if(length(mixinglevels) > 1){
    #level 2
    n2 <- rdata%>%
      group_by(across(c("round",mixinglevels[-2] ,"times"))) %>% #negative indexing due to descending ordering
      summarise(sum(!is.na(sir)))
    group.data$n2 <- data.frame(n2)[,ncol(n2)]-group.data$n
    if(length(mixinglevels)>2)
    {
      #level 3
      n3 <- rdata%>%
        group_by(across(c("round",mixinglevels[-3] ,"times"))) %>% #negative indexing due to descending ordering
        summarise(sum(!is.na(sir)))
      group.data$n3 <- data.frame(n3)[,ncol(n3)]-group.data$n2-group.data$n
    }
    
  }
  #7. check or determine group co-variates
  if(!is.null(covariates)){
    covariate.data <- group.data;
    print("Only returns minimum of covariate")
    for(j in covariates){
    covariate <- rdata%>%
        group_by(across(c("round",mixinglevels ,"times"))) %>% 
        summarize(covar = min(eval(parse(text = j))))%>%
        ungroup
    
    covariate.data[j]<-covariate$covar;
    }
  }
  # S, I and R should be the numbers at the beginning of the interval dt
  # cases the numbers after the interval, thus shift SIR values to one time later
  # the first time moment should be removed
  group.data <- group.data %>%
    group_by(across(c("round",mixinglevels ))) %>% 
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
      group_by(across(c("round",mixinglevels ))) %>% 
      select(all_of(covariates))%>%
      filter(row_number()>1)%>%
      ungroup
    group.data<- cbind(group.data, covariate.data[,covariates])
  }
  #set reference level of treatment
  if(!is.null(reference))
  {
    group.data <- within(group.data, 
                         treatment <- relevel(treatment, ref = eval(reference)))
  }
  return(group.data)
}



### For final size ####
arrangeData.finalsize<-function(rdata,           #data
                          covariates = NULL, #covariate column names
                          remInoCase = TRUE,     #remove inoculated animals as potential case
                          inoMarker = "I",    #marker used for inoculated animals
                          ...
){ 
  stop("NOT FINISHED ~ to do  in 2022")
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
  # #1. time intervals (length)
  # group.data <- rdata%>%
  #   group_by(across(c(mixinglevels ,"times"))) %>% 
  #   summarize(times = mean(times))
  # group.data <-group.data%>%
  #   summarize(times = times,
  #             dt = c(-1,tail(times,-1)-head(times,-1)))%>%
  #   ungroup
  # 
  # 
  # #2. cases per interval
  # indiv.cases <- rdata%>%
  #   filter(if(InoCase)!str_detect(inoculationStatus,"I")else TRUE)%>%
  #   arrange(times)%>% 
  #   summarize(
  #     host_id = host_id,
  #     across(mixinglevels ),
  #     times = times,
  #     case = c(as.numeric(head(sir,-1)==0 & tail(sir,-1)>0),0));
  # 
  # cases   <- indiv.cases%>%
  #   group_by(across(c(mixinglevels ,"times"))) %>% 
  #   summarise(sum(case,na.rm = TRUE))
  # group.data$cases<- data.frame(cases)[,ncol(cases)]
  # #3. number of infectious individual at start interval at each of the levels
  # #level 1
  # i <- rdata%>%
  #   group_by(across(c(mixinglevels ,"times"))) %>% 
  #   summarise(sum(sir == 2,na.rm = TRUE))
  # group.data$i <- data.frame(i)[,ncol(i)]
  # group.data$i2 <- 0;#default values
  # group.data$i3 <- 0;#default values
  # #here also take into account infectious individuals in other levels
  # if(length(mixinglevels) > 1){
  #   #level 2
  #   i2 <- rdata%>%
  #     group_by(across(c(mixinglevels[-2] ,"times"))) %>% #negative indexing due to descending ordering
  #     summarise(sum(sir == 2,na.rm = TRUE))
  #   group.data$i2 <- data.frame(i2)[,ncol(i2)]-group.data$i
  #   if(length(mixinglevels)>2)
  #   {
  #     #level 3
  #     i3 <- rdata%>%
  #       group_by(across(c(mixinglevels[-3] ,"times"))) %>% #negative indexing due to descending ordering
  #       summarise(sum(sir == 2,na.rm = TRUE))
  #     group.data$i3 <- data.frame(i3)[,ncol(i3)]-group.data$i2-group.data$i1
  #   }
  #   
  # }
  # #4. number of susceptible individuals at start interval
  # s <- rdata%>%
  #   filter(if(InoCase)!str_detect(inoculationStatus,"I")else TRUE)%>%
  #   group_by(across(c(mixinglevels ,"times"))) %>% 
  #   summarise(sum(sir == 0,na.rm = TRUE))
  # group.data$s <- data.frame(s)[,ncol(s)]
  # #5. number of recovered individuals at start of interval
  # r <- rdata%>%
  #   group_by(across(c(mixinglevels ,"times"))) %>% 
  #   summarise(sum(sir == 3,na.rm = TRUE))
  # group.data$r <- data.frame(r)[,ncol(r)]
  # #6. total number of individuals
  # n <- rdata%>%
  #   group_by(across(c(mixinglevels ,"times"))) %>% 
  #   summarise(sum(!is.na(sir)))
  # group.data$n <- data.frame(n)[,ncol(n)]
  # group.data$n2 <- 0;#default values
  # group.data$n3 <- 0;#default values
  # #here also take into account infectious individuals in other levels
  # if(length(mixinglevels) > 1){
  #   #level 2
  #   n2 <- rdata%>%
  #     group_by(across(c(mixinglevels[-2] ,"times"))) %>% #negative indexing due to descending ordering
  #     summarise(sum(!is.na(sir)))
  #   group.data$n2 <- data.frame(n2)[,ncol(n2)]-group.data$n
  #   if(length(mixinglevels)>2)
  #   {
  #     #level 3
  #     n3 <- rdata%>%
  #       group_by(across(c(mixinglevels[-3] ,"times"))) %>% #negative indexing due to descending ordering
  #       summarise(sum(!is.na(sir)))
  #     group.data$n3 <- data.frame(n3)[,ncol(n3)]-group.data$n2-group.data$n
  #   }
  #   
  # }
  # #7. check or determine group co-variates
  # if(!is.null(covariates)){
  #   covariate.data <- group.data;
  #   for(j in covariates){
  #     covariate <- rdata%>%
  #       group_by(across(c(mixinglevels ,"times"))) %>% 
  #       summarize(covar = mean(eval(parse(text = j)), na.rm = TRUE))%>%
  #       ungroup
  #     
  #     covariate.data[j]<-covariate$covar;
  #   }
  # }
  # # S, I and R should be the numbers at the beginning of the interval dt
  # # cases the numbers after the interval, thus shift SIR values to one time later
  # # the first time moment should be removed
  # group.data <- group.data %>%
  #   group_by(across(c(mixinglevels ))) %>% 
  #   summarize(
  #     times = tail(times,-1),
  #     dt = tail(dt,-1),
  #     cases = tail(cases,-1),
  #     s = as.numeric(head(s, -1)),
  #     i = as.numeric(head(i, -1)),
  #     i2 = as.numeric(head(i2, -1)),
  #     i3 = as.numeric(head(i3, -1)),
  #     r = as.numeric(head(r, -1)),
  #     n = as.numeric(head(n,-1)),
  #     n2 = as.numeric(head(n2,-1)),
  #     n3 = as.numeric(head(n3,-1))
  #   )%>%ungroup
  # if(!is.null(covariates)){
  #   covariate.data <- covariate.data%>%
  #     group_by(across(c(mixinglevels ))) %>% 
  #     select(all_of(covariates))%>%
  #     filter(row_number()>1)%>%
  #     ungroup
  #   group.data<- cbind(group.data, covariate.data[,covariates])
  # }
  # 
  return(group.data)
}




# perform analysis ####
analyseTransmission<- function(inputdata,          #input data
                               rule,          #rule to determine whether sample is positive or negative
                               var.id,        #variables to determine apply rule to
                               method ="glm", #estimation method
                               preventError = FALSE, #remove those entries with FOI = 0 but cases>1
                               covars = "1", #covariates
                               remInoCase = TRUE,     #remove inoculated animals as potential case
                               inoMarker = "I",    #marker used for inoculated animals
                               reference = NULL,
                               ...){
  #arrange data for analysis
  data.arranged <- arrangeData(data = inputdata,
                               rule = rule,
                               var.id = var.id,
                               method = method,
                               covariates = covars,
                               remInoCase = remInoCase,     #remove inoculated animals as potential case
                               inoMarker = inoMarker,    #marker used for inoculated animals
                               ...)

  #remove those entries without susceptibles (contain no information and cause errors)
  data.arranged <- data.arranged%>%filter(s>0)
  data.arranged$treatment <- factor(data.arranged$treatment, 
                                    ordered = FALSE)
 if(!is.null(reference)){ if(any(str_detect(eval(reference),as.character(data.arranged$treatment)))){
  data.arranged <- within(data.arranged, 
                          treatment <- relevel(treatment, ref = eval(reference))) }else print(paste("Control group:  ", eval(reference), "not present"))}
  #deal with potential error
  if(preventError){
    data.arranged <- data.arranged %>% filter(i>0 &!is.na(cases)&!is.na(s)&!is.na(i)&!is.na(r));
    covars = if(length(unique(data.arranged$treatment))>1){"treatment"}else{"1"}}
  #do analysis
  
  #TO DO use covariates
  fit <- switch(method,
    glm = glm(#cbind(cases, s - cases) ~ treatment,
              as.formula(paste("cbind(cases, s - cases) ~ ", paste(covars, collapse= "+"))),
              family = binomial(link = "cloglog"), 
              offset = log(i/n)*dt,
              data = data.arranged),
    mll =stop("mle: not implemented yet"),#deal with number of levels in a maximum likelihood estimation. 
    finalsize = FinalSize,#is not yet implemented as well
    stop("no other methods than glm or maximum likelihood")
  )

  
  #return outcome
  return(fit)
}


# # perform analysis with default decisions ####
# analyseTransmission<- function(inputdata,          #input data
#                                rule = "sinceany",          #rule to determine whether sample is positive or negative
#                                method ="glm", #estimation method
#                                preventError = FALSE, #remove those entries with FOI = 0 but cases>1
#                                ){
#   #decide settings based on input data
#   
#   # determine measure versus result choose for result if present
#   
#   # determine the values to set for result or cut off for measure
#   
#   # get value of reference group
#   
#   #arrange data for analysis
#   data.arranged <- arrangeData(data = inputdata,
#                                rule = rule,
#                                var.id = var.id,
#                                method = method,
#                                covariates = covars,
#                                ...)
#   #remove those entries without susceptibles (contain no information and cause errors)
#   data.arranged <- data.arranged%>%filter(s>0)
#   data.arranged$treatment <- factor(data.arranged$treatment, ordered = FALSE)
#   data.arranged <- within(data.arranged, treatment <- relevel(treatment, ref = "control"))
#   #deal with potential error
#   if(preventError){data.arranged <- data.arranged%>%filter(i>0)}
#   #do analysis
#   #TO DO use covariates
#   fit <- switch(method,
#                 glm = glm(#cbind(cases, s - cases) ~ treatment,
#                   as.formula(paste("cbind(cases, s - cases) ~ ", paste(covars, collapse= "+"))),
#                   family = binomial(link = "cloglog"), 
#                   offset = log(i/n)*dt,
#                   data = data.arranged),
#                 mll =stop("mle: not implemented yet"),#deal with number of levels in a maximum likelihood estimation. 
#                 finalsize = FinalSize,#is not yet implemented as well
#                 stop("no other methods than glm or maximum likelihood")
#   )
#   
#   
#   #return outcome
#   return(fit)
# }
# 
# 
# 
# 

#run local algorithm for each data set ####
get.local.transmission <- function(dataset){
                  var.id = ifelse(all(is.na(dataset$sample_measure)), c("sample_result"), c("sample_measure"))
                  control = ifelse(any(grepl('control',dataset$treatment,ignore.case = T)),"control",
                    ifelse(any(grepl('0',dataset$treatment)),"0",""))
                  rule = if(!all(is.na(dataset$sample_measure))) {rule.sinceany.cutoff}
                             else{#check if the data consists of 0, 1 and empty spaces
                                 if(any(sapply(c('^0$','^1$',"^$"),
                                               FUN = function(x){any(grepl(x,dataset$sample_result ))} ))){
                                        rule.sinceany.numeric}else{rule.sinceany.recode} 
                             }
                  inomarker = if(any(str_detect(dataset$inoculationStatus,"2"))){"2"}else {"I"}
                  

                  return(analyseTransmission(inputdata = dataset, #data set
                                             rule = rule, #rule to determine infection status
                                             var.id = var.id,  #variable defining infection status
                                             method = "glm", #estimation method
                                             cutoff = 0, #cutoff value for infection status
                                             codesposnegmiss = c("+","-","NA"), #values determining infection status pos, neg of missing
                                             preventError = TRUE, #TRUE = remove entries with > 1 case but FOI = 0
                                             covars = 'treatment', #co variants
                                             reference = "control", #reference category for multivariable estimation
                                             control = control,
                                             inoMarker = inomarker))    #marker used for inoculated animals)   #value of control treatment
                  }






