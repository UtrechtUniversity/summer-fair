###################################################################
#   Example code loading and analysing transmission data in R     #
###################################################################

#libraries####
library(lme4)
library(readxl)
library(dplyr)

#load data file####
data1 <- read_xlsx("C://Surfdrive//Projecten//SUMMERFAIR//ProjectShareSUMMERFAIR//Data//DatasetA//results_exp3_r123_rfile.xlsx")
data2 <- read_xlsx("C://Surfdrive//Projecten//SUMMERFAIR//ProjectShareSUMMERFAIR//Data//DatasetB//analyse resultaten_exp4.xlsx")

#manipulate data to get the write input####
#function to determine if defined as positive or negative often more complicated than this but same input
status.function <- function(data){
  res <- tibble();print(colnames(data));
  for(i in c(1:dim(data)[1]))
  { 
    tmp <- c(data[i,]>0);#determine positive samples 
    res <- rbind(res,
                  data.frame(t(mapply(FUN = function(x,y){ifelse(x&y,1,0)} ,head(tmp,-1),tail(tmp,-1))))); #only two positive samples after each other are really positive
  }
  colnames(res)<- head(colnames(data),-1);
  print(res);
  return(res)
}
#do data set 1
data1<- data1[1:269,]
sample.days1<-c(c(1:14),16,19,21)#this is something I know but should be part of the data set
deltat.days1<- tail(sample.days1,-1)-head(sample.days1,-1);
count.data1 <- cbind(data1[,c("round","isolator","treatment","S_I")], select(data1, contains("count_ESBL")&!contains("cae")))#counts except for the caecal sample
status.data1 <- cbind(data1[,c("round","isolator","treatment","S_I")],data.frame(status.function(select(data1, contains("count_ESBL")&!contains("cae")))))
#count number of infected at previous time step, number of new cases per group
aggregate.data1 <- NULL
for(i in c(1:(length(deltat.days1)-1)))
    {
      #
  aggregate.data1 <-rbind(aggregate.data1,
                          data.frame(C = aggregate(status.data1[,i+4+1]-status.data1[,i+4],by = list(status.data1$isolator),function(input){sum(input>0,na.rm = T)})$x,#cases
                       S = aggregate(-(status.data1[,i+4]-1),by = list(status.data1$isolator),sum,na.rm = T)$x,#susceptibles at start interval
                       I = aggregate(status.data1[,i+4],by = list(status.data1$isolator),sum,na.rm = T)$x,#infected at start interval
                       N = aggregate(status.data1[,i+4],by = list(status.data1$isolator),length)$x,#animals at start of interval
                       Treatment = aggregate(status.data1[,3],by = list(status.data1$isolator),first)$x,#animals at start of interval
                       deltat = deltat.days1[i],
                       day = sample.days1[i+1]
                       
                     )) 
      
}
#"local algorithm"####
model<- function(data.set)
{
  s = data.set$S; i = data.set$I; cases = data.set$C; n = data.set$N; dt = data.set$deltat;
  
  #number of treatments
  num.of.treat <- length(unique(data.set$Treatment))
  t = data.set$Treatment #treatments coded as 0,1,2
  #create likelihood function
  logLikelihood <- function(b){
     -sum(cases*(-(b[1] + c(0,b)[t+1]) * (i/n)*dt) + (s-cases)*log((1-exp(-(b[1] + c(0,b)[t+1])*(i/n)*dt))))
    
  }
  
  return(c(logLikelihood,num.of.treat))
}
#apply local algorithm
sel.data1 <- aggregate.data1%>%filter(I>0)
model.data1 <- model(sel.data1)# returns likelihood function and number of classes
#model.data1 can be used in estimation procedures
fit <- nlm(model.data1[[1]],c(1,1,1))

# general algorithm to improve accuracy####
combine.logLik <- function(lls){
  #sum up the loglikelihoods
  comb.LogLik <- function(b){sum(lls)}
  #
  return(comb.LogLik)
  }







