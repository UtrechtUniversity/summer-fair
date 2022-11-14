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
             "rlog",
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
#silence this warning because it has no use in this code
options(dplyr.summarise.inform = FALSE) 

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
        if(is.na(times[1])){ 
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
### For maximum likelihood  models ####
arrangeData.mll<-function(rdata,           #data
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
      group_by(across(c("round", all_of(mixinglevels) ,"times"))) %>% 
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
    group_by(across(c("round",all_of(mixinglevels) ,"times"))) %>% 
    summarise(cases = sum(case,na.rm = TRUE))
  group.data <- group.data%>%right_join(cases, by = c("round",all_of(mixinglevels),"times")) 
  #3. number of infectious individual at start interval at each of the levels
  #level 1
  i1 <- rdata%>%
    group_by(across(c("round",all_of(mixinglevels) ,"times"))) %>% 
    summarise(i1 = sum(sir == 2,na.rm = TRUE))
  group.data <- group.data%>%right_join(i1, by = c("round",all_of(mixinglevels),"times"))
  
 
  #here also take into account infectious individuals in other levels
  if(length(mixinglevels) > 1){
    #level 2
    i2 <- rdata%>%
      group_by(across(c("round",all_of(mixinglevels)[-2] ,"times"))) %>% #negative indexing due to descending ordering
      summarise(i2 = sum(sir == 2,na.rm = TRUE))
    group.data <- merge(group.data,data.frame(i2));
    group.data$i2 <- group.data$i2-group.data$i1;
    if(length(mixinglevels)>2)
    {
      #level 3
      i3 <- rdata%>%
        group_by(across(c("round",all_of(mixinglevels)[-3] ,"times"))) %>% #negative indexing due to descending ordering
        summarise(i3 = sum(sir == 2,na.rm = TRUE))
      group.data <- merge(group.data,data.frame(i3));
      group.data$i3 <- group.data$i3-group.data$i2-group.data$i1;
    }else  {group.data$i3 <- 0;} #default value
  
  }else { group.data$i2 <- 0;#default values
    group.data$i3 <- 0;#default values
  }
  #4. number of susceptible individuals at start interval at level 1
  s1 <- rdata%>%
    filter(if(remInoCase)!str_detect(inoculationStatus,inoMarker)else TRUE)%>%
    group_by(across(c("round",all_of(mixinglevels) ,"times"))) %>% 
    summarise(s = sum(sir == 0,na.rm = TRUE))
  group.data <- group.data%>%right_join(s1, by = c("round",all_of(mixinglevels),"times"))
  #5. number of recovered individuals at start of interval at level 1
  r <- rdata%>%
    group_by(across(c("round",all_of(mixinglevels) ,"times"))) %>% 
    summarise(r = sum(sir == 3,na.rm = TRUE))
  group.data <- group.data%>%right_join(r, by = c("round",all_of(mixinglevels),"times"))
  #6. total number of individuals in level 1
  n1 <- rdata%>%
    group_by(across(c("round",all_of(mixinglevels) ,"times"))) %>% 
    summarise(n1 = sum(!is.na(sir)))
  group.data <- group.data%>%right_join(n1, by = c("round",all_of(mixinglevels),"times"))

  #here also take into account infectious individuals in other levels
  if(length(mixinglevels) > 1){
    #level 2
    n2 <- rdata%>%
      group_by(across(c("round",all_of(mixinglevels[-2]) ,"times"))) %>% #negative indexing due to descending ordering
      summarise(n2 = sum(!is.na(sir)))
    group.data <- merge(group.data,data.frame(n2));
    group.data$n2 <- group.data$n2-group.data$n1
    if(length(mixinglevels)>2)
    {
      #level 3
      n3 <- rdata%>%
        group_by(across(c("round",all_of(mixinglevels[-3]) ,"times"))) %>% #negative indexing due to descending ordering
        summarise(n3 = sum(!is.na(sir)))
      group.data <- merge(group.data,data.frame(n3));
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
    group.data<- cbind(group.data, covariate.data[,covariates])
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
                               method ="mll", #estimation method
                               preventError = FALSE, #remove those entries with FOI = 0 but cases>1
                               covars = "1", #covariates
                               remInoCase = TRUE,     #remove inoculated animals as potential case
                               inoMarker = "I",    #marker used for inoculated animals
                               reference = NULL,
                               ...){
  
  #create a log data frame
  cat("Analysing transmission using ",method, "at", Sys.time(),"\n");
  #arrange data for analysis
  try(data.arranged <- arrangeData(data = inputdata,
                               rule = rule,
                               var.id = var.id,
                               method = method,
                               covariates = covars,
                               remInoCase = remInoCase,     #remove inoculated animals as potential case
                               inoMarker = inoMarker,    #marker used for inoculated animals
                               ...))

  
  
  #data
  data.arranged <- data.arranged[[1]]
  
  #remove those entries without susceptibles (contain no information and cause errors)
  data.arranged <- data.arranged%>%filter(s>0)
  data.arranged$treatment <- factor(data.arranged$treatment, 
                                    ordered = FALSE)
 if(!is.null(reference)){ if(any(str_detect(eval(reference),as.character(data.arranged$treatment)))){
  data.arranged <- within(data.arranged, 
                          treatment <- relevel(treatment, ref = eval(reference))) }else print(paste("Control group:  ", eval(reference), "not present"))}
 
  try(#do analysis
  fit <- switch(method,
  glm = run.glm(covars = covars,
              data.arranged = data.arranged, 
              preventError = preventError),
    mll =run.mll(covars = covars,
                 data.arranged = data.arranged, 
                 preventError = preventError),#deal with number of levels in a maximum likelihood estimation. 
    finalsize = FinalSize,#is not yet implemented as well
    stop("no other methods than glm, maximum likelihood or final size")
  ))

  #return covariate entries (categorical = values or numeric = range)
  if(length(covars) > 0)
  {
    covar.values = list()
    for(j in covars){}
      if(is.factor(data.arranged[,j])){covar.values <- append(covar.values, levels(data.arranged[,j]))}
    else{covar.values <- append(covar.values, range(data.arranged[,j]))}
  }

  #return outcome
  return(list(covariates = list(names = covars,
                                values = covar.values),
              
              estimation = fit))
}



#run local algorithm for each data set ####
get.local.transmission <- function(dataset,writeLog = TRUE){
  #start sink to log-file
  if(writeLog){
     log.file <- paste0("summerfair.log");
  zz <- file(log.file, open = "at");
  sink(zz, type ="message");
  sink(zz, type ="output");
  cat("\n-------*summerfair*-----------\n","Start log at: ",  format(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n")}
   if(length(dataset)==0)stop("Empty data set cannot be analysed")
                  var.id = if(all(is.na(dataset$sample_measure))){c("sample_result")}else{ c("sample_measure","detectionLimit")}
                  
                  control = ifelse(any(grepl('control',dataset$treatment,ignore.case = T)),"control",
                    ifelse(any(grepl('0',dataset$treatment)),"0",""))
                  rule = if(!all(is.na(dataset$sample_measure))) {
                    # check if dealing with detection limit data or not
                    if(all(dataset$detectionLimit==""|is.na(dataset$detectionLimit))){
                    rule.sinceany.cutoff}else {rule.sinceany.cutoff.detectionlimit}}
                             else{#check if the data consists of 0, 1 and empty spaces
                                 if(any(sapply(c('^0$','^1$',"^$"),
                                               FUN = function(x){any(grepl(x,dataset$sample_result ))} ))){
                                        rule.sinceany.numeric}else{rule.sinceany.recode} 
                             }
                  print("Used rule:");
                  print(rule);
                  #marker for inoculation
                  inomarker = if(any(str_detect(dataset$inoculationStatus,"2"))){"2"}else {"I"}
                  

                  analysis <- try(analyseTransmission(inputdata = dataset, #data set
                                             rule = rule, #rule to determine infection status
                                             var.id = var.id,  #variable defining infection status
                                             method = "mll", #estimation method
                                             cutoff = 0, #cutoff value for infection status
                                             codesposnegmiss = c("+","-","NA"), #values determining infection status pos, neg of missing
                                             preventError = TRUE, #TRUE = remove entries with > 1 case but FOI = 0
                                             covars = 'treatment', #co variants
                                             reference = "control", #reference category for multivariable estimation
                                             control = control,
                                             inoMarker = inomarker))    #marker used for inoculated animals)   #value of control treatment
                
                  if(writeLog){
                    #close log file
                  cat("End log at: ", format(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n-------*summerfair*-----------\n");
                  sink(type = "message");
                  sink(type = "output");
                  close(zz);}
                
                return(analysis);
                
                  }





###################################
# run.glm is a function equal to glm but will filter time points that cannot be used. 
# run.glm cannot deal with multiple levels. If multiple levels are present the run.mml method should be used

run.glm<-function(covars,
                  data.arranged,
                  preventError = TRUE){
  
  #filter intervals without infected animals. 
  if(preventError){
    data.filtered <- data.arranged %>% filter(i1  > 0 &!is.na(i1  )&!is.na(cases)&!is.na(s)&!is.na(i1)&!is.na(r));
  }else data.filtered <- data.arranged
    
  #if covariates only have one unique value it cannot be used in the glm
  use.covars <- covars[data.arranged[,covars]%>%unique%>%length>1]
  if(length(use.covars)==0){use.covars <- "1"}
  #determine whether other levels exist
  if(sum(data.arranged$i2)+sum(data.arranged$i3)>0) warning("There seem to be more levels in this data set. This cannot be analyzed by this GLM function");
  #check if filtered data contains any rows
  if(length(data.filtered$i)==0)stop("No rows with data present in this data set after filtering i>0.")
  #Do the analysis
  return(glm(as.formula(paste("cbind(cases, s - cases) ~ ", paste(use.covars, collapse= "+"))),
                  family = binomial(link = "cloglog"), 
                  offset = log(i/n)*dt,
                  data = data.filtered 
                  ))
 
}

########################################################
#run.mll is a function equal to that will filter time points that cannot be used, and produces a number of estimates for within- and between level transmission

run.mll<-function(covars,
                  data.arranged,
                  preventError = TRUE){
  
  #filter intervals without infected animals. 
  if(preventError){
    data.filtered <- data.arranged %>% filter(i1 + i2 + i3 > 0 &!is.na(i1 + i2 + i3 )&!is.na(cases)&!is.na(s)&!is.na(i1)&!is.na(r));
  }else data.filtered <- data.arranged
  
  #if covariates only have one unique value it cannot be used in the glm
  use.covars <- covars[data.arranged[,covars]%>%unique%>%length>1]
  if(length(use.covars)==0){use.covars <- "1"}
  #determine number of levels
  levels = "L1" ;
  if(sum(data.arranged$i2)>0)levels = "L2";
  if(sum(data.arranged$i3)>0)levels = "L3";
  #Do the analysis
  fit <- switch(levels,
                #only one level do simple glm
                "L1" = {warning("Only one level found. Function will return GLM output."); 
                  return(run.glm(covars,
                                  data.arranged,
                                 preventError)) 
                },
                "L2" = {
                  logl = function(beta1,beta2,data){
                    sum((data$cases*log(1-exp(-(beta1*data$i/data$n + 
                                               beta2*data$i2/data$n2)*data$dt))-
                           (data$s-data$cases)*(beta1*data$i1/data$n + beta2*data$i2/data$n2)*data$dt))}
                  return(mle2(minuslogl = logl, data = data.filtered));}
                ,
                "L3" = {stop("incorrect logl")
                  logl = function(beta1,beta2,beta3, data){
                    sum((data$cases*log(exp((beta1*data$i/data$n + 
                                               beta2*data$i2/data$n2+
                                               beta3*data$i3/data$n3)*data$dt)-1)+
                           data$s*(beta1*data$i1/data$n + 
                                     beta2*data$i2/data$n2+
                                     beta3*data$i3/data$n3)*data$dt))}
                  return(mle2(logl, data = data.filtered));}
                
                )
  #return result
  return(fit)
}
