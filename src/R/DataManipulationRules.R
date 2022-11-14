##############################################################
#                                                        
#                  DataManipulationRules                 
# 
#           Examples of rules to determine infections status
#           of an individual based on one or more samples
#           Coding is Susceptible = 0
#                     Latent = 1
#                     Infectious = 2
#                     Recovered = 3                                 
#                     
#
#                  Author:E.A.J. Fischer                 
#                  Contact: e.a.j.fischer@uu.nl          
#                  Creation date: 30-9-2021              
##############################################################
library(magrittr)


##generic of a rule ####
rule.generic <-function(timeseries,var.id,...){
  print("generic rule returns first column");
  return(timeseries[,var.id[1]])
}

##rule using first sampletype in the data and determine status S or I####
#Positive means individual is positive from that time onwards, return data 'asis'
rule.asis <-function(timeseries,var.id,...){
    return(2*(timeseries[,var.id[1]]%>%sign))
}


##rule using first sampletype in the data and determine status S or I####
# First positive means individual is positive from that time onwards
rule.sincefirst <- function(timeseries,var.id,...){
  if(length(var.id)>1) warning("Only first var.id entry used in rule")
  new.series <-2*(timeseries %>% 
                    select(all_of(var.id[1]))%>%
                    cumsum%>%
                    sign);
  
  return(new.series)
}


##rule using any sample in the data and determine status S or I####
# First positive means individual is positive from that time onwards
rule.sinceany <- function(timeseries,var.id,...){
  stop("Deal with NA's")
  new.series <- 2*(timeseries %>% 
                     select(all_of(var.id))%>%
                     cumsum%>%
                     rowSums%>%
                     sign);
  
  return(new.series)
}

##rule using any sample in the data and determine status S or I####
# Animals can switch between susceptible and infectious and back
rule.any <- function(timeseries,var.id,...){
  new.series <- 2*(timeseries%>%
                     select(all_of(var.id))%>%
                     rowSums%>%
                     sign)
  return(new.series)
}

##rule using all sample (all should be positive) in the data and determine status S or I####
# Animals can switch between susceptible and infectious and back
rule.all <- function(timeseries,var.id,...){
  new.series <- 2*(timeseries%>%
                     select(all_of(var.id))%>%
                     rowMins() );
  return(new.series)
}

##rule using some samples to determine status  I and other for R####
# Animals can switch between susceptible, infectious, recovered and back
rule.testinfectioustestrecovered <- function(timeseries,var.id,infrec){
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


rule.sincefirstinfectioustestrecovered <- function(timeseries,var.id,infrec){
  i <- 2*(timeseries%>%
            select(all_of(var.id[infrec$inf]))%>%
            cumsum%>%
            rowSums%>%
            sign);
  r <- 3*(timeseries%>%
            select(all_of(var.id[infrec$rec]))%>%
            cumsum%>%
            rowSums%>%
            sign);
  new.series <- rowMaxs(data.frame(i,r))
  return(new.series)
}


##rule uses any sample which requires to be positive for at least n consecutive time moments
#If only the last sample is positive it will  be considered positive
rule.consecutive <- function(timeseries,var.id,n)
{ new.series <- timeseries%>%
                select(all_of(var.id))%>%
                rowSums()%>%
                sign();

  return(sapply(X = c(1:length(new.series)),
         FUN = function(x){min(new.series[x:(min(length(new.series),x+n))])}))

}

##Samples have another way to mark positive or negative
##If pos / neg is indicated by + or - (possibly including other values)
rule.sinceany.recode<- function(timeseries, 
                                var.id,
                                codesposnegmiss,
                                newcodes=c(1,0,0),
                                ... )
{
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


rule.sinceany.numeric<- function(timeseries, 
                                var.id,
                                ... )
{
  
  timeseries[,var.id]<- as.numeric(timeseries[,var.id])
  return(rule.sinceany(timeseries, var.id,...))
}

##If pos / neg is indicated by a cutoff
rule.sinceany.cutoff<- function(timeseries, var.id,cutoff,... )
{
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
rule.sinceany.cutoff.detectionlimit <- function(timeseries, var.id, cutoff, ...)
  {  #determine cutoff
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
