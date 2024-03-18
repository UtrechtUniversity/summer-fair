##############################################################
#                                                        
#                  Data Interpretation Rules                 
# 
#           Rules to determine infections status of an individual based on one or more samples
#           Naming of a rule is as follows:
#           rule.X.Y.Z in which X is a description of the rule, Y indicates using a cuttoff or recoding or numeric value, 
#           and Z defines if there is a specific value per sample for the detectionLimit
#           Coding is Susceptible = 0
#                     Latent = 1
#                     Infectious = 2
#                     Recovered = 3    
#
#           Each rule should have at least 4 versions to deal with 
#           a. results with numeric value (rule.X)
#           b.  results with string code (rule.X.recode)
#           c.  measurements with fixed cutoff (rule.X.cuttoff)
#           d. measurements with individual cuttoff (rule.X.cuttoff.detectionLimit)
#
#                  Author:E.A.J. Fischer                 
#                  Contact: e.a.j.fischer@uu.nl          
#                  Creation date: 30-9-2021              
##############################################################
library(magrittr)
library(docstring)

##generic of a rule ####
rule.generic <-function(timeseries,var.id,...){
  #' @title generic rule
  #' @description
    #' Generic structure of a rule 
  #' @param timeseries vector or data.frame containing values of one host 
  #' @param var.id variables used to determine host infection state
  #' @return Vector with host infection state  Coding is Susceptible = 0
    #'                   Latent = 1
    #'                   Infectious = 2
    #'                   Recovered = 3   
  print("generic rule returns first column");
  return(timeseries[,var.id[1]])
}

# Function for dealing with recoding ####
recodefunction <- function(input,codesposnegmiss,newcodes){
  #' @title Recoding function
  #' @description
    #' Recodes test outcomes into other new codes
  #' @param input data
  #' @param codesposnegmiss vector containing code names in current data set for postives, negative and missing data.
  #' @param newcodes new codes for positive, negative and missing.
  #' @return recoded data
  ifelse(str_detect(pattern = paste0("[",codesposnegmiss[1],"]"),string = input),newcodes[1],
         ifelse(str_detect(pattern=paste0("[",codesposnegmiss[2],"]"),string = input),newcodes[2],
                ifelse(str_detect(pattern=paste0("[",codesposnegmiss[3],"]"),string = input),newcodes[3],NA)))}

# Function for dealing with recoding based on cutoff values ####
cutofffunction <- function(input,co){
  #' @title Recoding by cutoff
  #' @description
  #' Recodes test outcomes simply based on cut-off
  #' @param input data
  #' @param co cut-off value (always positive if > co) 
  #' @return recoded data
  as.numeric(input)> co
  
}

#Function for dealing with detection limit
detectionLimitfunction <- function(input,co = 0,dir =">"){
  #' @title Recoding by a detection limit
  #' @description
  #' Recodes test outcomes  based on cut-off
  #' @param input data
  #' @param co cut-off value 
  #' @param dir direction of comparison (i.e. ">", ">=", "<"or "<=")
  #' @return recoded data
  
  #if the sample measurement has a value 
  num.val <- as.numeric(input["sample_measure"]);
  #no individual detection  limit for this particular sample
  if(is.na(input["detectionLimit"])||input["detectionLimit"]=="") return(as.numeric(eval(str2expression(paste("num.val",dir,"co")))));
  #check numeric value with detection limit
  if(!is.na(num.val))
  {
    if(grepl(input["detectionLimit"],pattern = "<")){
      num.val <- !eval(str2expression(paste(num.val,input["detectionLimit"])))}else{
      num.val <- eval(str2expression(paste(num.val,input["detectionLimit"])))
      }
  }else #positive if contains ">" and negative if "<"
  {
    num.val = as.numeric(!grepl(input["detectionLimit"],pattern = "<")) + as.numeric(grepl(input["detectionLimit"],pattern = ">") ) 
    if(input["detectionLimit"] ==""){num.val = NA}
  }
 return(as.numeric(num.val))
}
   


##rule using first sampletype in the data and determine status S or I####
#Positive means individual is infectious, return data 'asis'

rule.asis <-function(timeseries,var.id,...){
  #' @title rule asis 
  #' @description
  #'  rule return data as it is but change 1's to code for infectious (=2)
  #' @inherit rule.generic param return 
    return(as.numeric(2*(timeseries[,var.id[1]]%>%sign)))
}

#recode 
rule.asis.recoded <-function(timeseries,
                             var.id,
                             codesposnegmiss,
                             newcodes=c(1,0,0), ...)
{
  #' @title rule asis but recoded
  #' @description
    #' Uses functions for recoding and returns data with positive = infectious (code =2)
  #' @param timeseries 
  #' @param var.id 
  #' @param codesposnegmiss 
  #' @param newcodes 
  #' @param ... 
  #'
  #' @inherit rule.generic return
  
  if(length(codesposnegmiss)>3){ 
    stop("too many recodings for this rule!")}
  
  timeseries[,var.id]<- timeseries[,var.id[1]]%>%
        sapply(recodefunction,codesposnegmiss=codesposnegmiss,newcodes=newcodes)
  return(2*(timeseries[,var.id[1]]%>%sign))
}

#recode using a cutoff
rule.asis.cutoff <-function(timeseries,var.id,cutoff,...){
  #' @title Rule asis with cut-off
  #'  #' @description
  #' Uses functions for recoding with a cut-off and returns data with positive = infectious (code =2)
  #' @param timeseries 
  #' @param var.id 
  #' @param cutoff 
  #' @param ... 
  #'
  #' @inherit rule.generic return
  

  timeseries[,var.id]<- timeseries[,var.id[1]]%>%
        sapply(cutofffunction,co = cutoff) %>% as.numeric(var.id)
  return(rule.asis(timeseries, var.id,...))
}

#use a detection limit
rule.asis.detectionLimit <-function(timeseries,var.id,cutoff =0,...){
  #' @title Rule asis with cut-off by a detection limit
  #'  #' @description
  #' Uses functions for recoding with a cut-off by a detection limit and returns data with positive = infectious (code =2)
  #' @param timeseries 
  #' @param var.id 
  #' @param cutoff 
  #' @param ... 
  #'
  #' @inherit rule.generic return
  
  timeseries[,var.id]<- timeseries[,c(var.id[1],"detectionLimit")]%>%
        apply(FUN = detectionLimitfunction,1,co = cutoff) %>% as.numeric(var.id)
  
  return(2*(timeseries[,var.id[1]]%>%sign))
}





##rule using first sampletype in the data and determine status S or I####
# First positive means individual is positive from that time onwards

rule.sincefirst <- function(timeseries,var.id,...){
  #' @title Rule since first positive
  #' @description
  #' All samples of a host are considered positive after the first positive sample
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
    if(length(var.id)>1) warning("Only first var.id entry used in rule")
  new.series <-2*(timeseries %>% 
                    select(all_of(var.id[1]))%>%
                    unlist%>%
                    as.numeric%>%
                    cumsum%>%
                    sign);
  
  return(new.series)
}

rule.sincefirst.recode <- function(timeseries,
                                   var.id,
                                   codesposnegmiss,
                                   newcodes=c(1,0,0),...){
  #' @title Rule since first positive use recoding
  #' @description
  #' All samples of a host are considered positive after the first positive sample
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  if(length(var.id)>1) warning("Only first var.id entry used in rule")
  #recode data
  timeseries[,var.id]<- timeseries[,var.id[1]]%>%
    sapply(recodefunction,codesposnegmiss=codesposnegmiss,newcodes=newcodes)
  
  return(rule.sincefirst(timeseries,var.id,...))
}

#recode using a cutoff
rule.sincefirst.cutoff <-function(timeseries,var.id,cutoff,...){
  #' @title Rule since first positive use cutoff
  #' @description
  #' All samples of a host are considered positive after the first positive sample
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param cutoff 
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  timeseries[,var.id]<- timeseries[,var.id[1]]%>%
    sapply(cutofffunction,co = cutoff) %>% as.numeric(var.id);
  return(rule.sincefirst(timeseries, var.id,...))
}

#use a detection limit
rule.sincefirst.detectionLimit <-function(timeseries,var.id,cutoff = 0,...){
  #' @title Rule since first positive using cutof and detection limit
  #' @description
  #' All samples of a host are considered positive after the first positive sample
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @returntimeseries[,var.id]<- timeseries[,c(var.id[1],"detectionLimit")]%>%
    apply(FUN = detectionLimitfunction,MARGIN = 1,co = cutoff) %>% as.numeric(var.id);
  
  return(rule.sincefirst(timeseries, var.id,...))
}




##rule using any sample in the data and determine status S or I####
# First positive means individual is positive from that time onwards
rule.sinceany <- function(timeseries,var.id,...){
  #' @title Rule since first positive of any sample. 
  #' @description
  #' All samples of a host are considered positive after the first positive sample from multiple assays per time point
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  
  new.series <- 2*(timeseries %>% 
                     select(all_of(var.id))%>%
                     unlist%>%
                     cumsum%>%
                     rowSums(na.rm= T)%>%
                     sign);
  
  return(new.series)
}

#recode
rule.sinceany.recode <- function(timeseries,
                                   var.id,
                                   codesposnegmiss,
                                   newcodes=c(1,0,0),...){
  #' @title Rule since first positive of any sample using recoding
  #' @description
  #' All samples of a host are considered positive after the first positive sample from multiple assays per time point
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  
  if(length(var.id)>1) warning("Only first var.id entry used in rule")
  #recode data
  timeseries[,var.id]<- timeseries[,var.id[1]]%>%
    sapply(recodefunction,codesposnegmiss=codesposnegmiss,newcodes=newcodes)
  
  return(rule.sinceany(timeseries,var.id,...))
}

#recode using a cutoff
rule.sinceany.cutoff <-function(timeseries,var.id,cutoff,...){
  #' @title Rule since first positive of any sample using cutoff
  #' @description
  #' All samples of a host are considered positive after the first positive sample from multiple assays per time point
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  timeseries[,var.id]<- timeseries[,var.id[1]]%>%
    sapply(cutofffunction,co = cutoff) %>% as.numeric(var.id)
  return(rule.sinceany(timeseries, var.id,...))
}

#use a detection limit
rule.sinceany.detectionLimit <-function(timeseries,var.id,cutoff =0,...){
  #' @title Rule since first positive of any sample using d=cutoff and detection limit
  #' @description
  #' All samples of a host are considered positive after the first positive sample from multiple assays per time point
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  timeseries[,var.id]<- timeseries[,c(var.id[1],"detectionLimit")]%>%
    apply(detectionLimitfunction,1,co = cutoff) %>% as.numeric(var.id)
  
  return(rule.sinceany(timeseries, var.id,...))
}

##rule using all sample (all should be positive) in the data and determine status S or I####
rule.all <- function(timeseries,var.id,...){
  #' @title Rule for multiple samples and all need to be positive
  #' @description
  #' All samples of a host are considered infectious if all assays per time point are positive
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  new.series <- 2*(timeseries%>%
                     select(all_of(var.id))%>%
                     rowMins() );
  return(new.series)
}

##rule using some samples to determine status  I and other for R####
# Animals can switch between susceptible, infectious, recovered and back
rule.testinfectioustestrecovered <- function(timeseries,var.id,infrec){
  #' @title Rule for multiple samples in which one determines infection and one recovery
  #' @description
  #' A host is considered infectious if the variables indicating infectiousness are positive and recovered if the variable indicating recovery is positive
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param infrec data.frame containing names of variables that indicate infectiousness (infrec$inf) or recovery (infrec$rec)
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  i <- 2*(timeseries%>%
            select(all_of(var.id[infrec$inf]))%>%
            rowSums%>%
            sign);
  r <- 3*(timeseries%>%
            select(all_of(var.id[infrec$rec]))%>%
            rowSums%>%
            sign);
  new.series <- rowMaxs(data.frame(i,r))
  return(new.series)
}

#
rule.sincefirstinfectioustestrecovered <- function(timeseries,var.id,infrec){
  #' @title Rule for multiple samples in which one determines infection and one recovery
  #' @description
  #' A host is considered infectious from the first time a variable indicating infectiousness are positive and recovered from the first time a variable indicating recovery is positive
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param infrec data.frame containing names of variables that indicate infectiousness (infrec$inf) or recovery (infrec$rec)
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  i <- 2*(timeseries%>%
            select(all_of(var.id[infrec$inf]))%>%
            unlist%>%
            cumsum%>%
            rowSums%>%
            sign);
  r <- 3*(timeseries%>%
            select(all_of(var.id[infrec$rec]))%>%
            unlist%>%
            cumsum%>%
            rowSums%>%
            sign);
  new.series <- rowMaxs(data.frame(i,r))
  return(new.series)
}


##rule uses any sample which requires to be positive for at least n consecutive time moments
#If only the last sample is positive it will  be considered positive
rule.consecutive <- function(timeseries,var.id,n)
{ 
  #' @title Rule with minimum number of positive samples
  #' @description
  #' A host is considered infectious if it has had n days consecutive positive samples.
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param n number of days a host needs to have positive samples
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  
  new.series <- timeseries%>%
                select(all_of(var.id))%>%
                rowSums()%>%
                sign();

  return(sapply(X = c(1:length(new.series)),
         FUN = function(x){min(new.series[x:(min(length(new.series),x+n))])}))

}


##rule using any sample in the data and determine status S or I####
# Animals can switch between susceptible and infectious and back
rule.any <- function(timeseries,var.id,...){
  #' @title Rule any of multiple samples
  #' @description
  #' A host is considered infectious if any of multiple samples is positive.
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  
  new.series <- 2*as.numeric(timeseries%>%
                     select(all_of(var.id))%>%
                     rowSums%>%
                     sign)
  return(new.series)
}

##Samples have another way to mark positive or negative
rule.sinceany.recode<- function(timeseries, 
                                var.id,
                                codesposnegmiss,
                                newcodes=c(1,0,0),
                                ... )
{
  #' @title Rule since any of multiple samples with recoding
  #' @description
  #' A host is considered infectious since first of any of multiple samples is positive.
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param codesposnegmiss
  #' @param newcodes
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  if(length(codesposnegmiss)>3){ 
     stop("too many recodings for this rule!")}
  recodefunction <- function(input){
    ifelse(str_detect(pattern = paste0("[",codesposnegmiss[1],"]"),string = input),newcodes[1],
       ifelse(str_detect(pattern=paste0("[",codesposnegmiss[2],"]"),string = input),newcodes[2],
           ifelse(str_detect(pattern=paste0("[",codesposnegmiss[3],"]"),string = input),newcodes[3],NA)))}
  
  timeseries[,var.id]<- timeseries%>%
    select(all_of(var.id))%>%
    sapply(recodefunction)
  return(rule.sinceany(timeseries, var.id,...))
}


##Measurements
##If pos / neg is indicated by a cutoff
rule.sinceany.cutoff<- function(timeseries, var.id,cutoff,... )
{
  #' @title Rule since any of multiple samples with cutoff
  #' @description
  #' A host is considered infectious since first of any of multiple samples is positive.
  #' 
  #' @param timeseries 
  #' @param var.id 
  #' @param cutoff
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  recodefunction <- function(input){
        as.numeric(input)> cutoff
    
  }
  
  timeseries[,var.id]<- timeseries%>%
    select(all_of(var.id))%>%
    sapply(recodefunction) %>% as.numeric(var.id)
  return(rule.sinceany(timeseries, var.id,...))
}

#if pos / neg is indicated by a cutoff and a detection limit. 
#Measures with a value of the detection limit are defined positive if above detection limit (">") and negative if below detection limit ("<")
rule.sinceany.detectionLimit <- function(timeseries, var.id,  ...)
  {  
  #' @title Rule since any of multiple samples with detection limit
  #' @description
  #' A host is considered infectious since first of any of multiple samples is positive based on detection limit for that variable
  #' 
  #' @param timeseries 
  #' @param var.id 
 
  #' @param ... 
  #' @inherit rule.generic param, return, examples, references
  #' @return
  #determine cutoff
  num.val <- as.numeric(as.numeric(timeseries[,"sample_measure"])>cutoff);
  dl <- sapply(timeseries[,"detectionLimit"],grepl,pattern = "<")
  du <-sapply(timeseries[,"detectionLimit"],grepl,pattern = ">")
  timeseries[,"detectionLimit"]<- num.val;
  #replace below detection limit with a 0 if measured
  timeseries[dl,"detectionLimit"] <- 0;
  #replace above detection limit with a 1 if measured
  timeseries[du,"detectionLimit"] <- 1;
  #select those that  have a value or are above detection limit.
  timeseries[,"sample_measure"]<- num.val;
  return(rule.sinceany(timeseries, var.id,...))
  
 }
