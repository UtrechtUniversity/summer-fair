##########################################################
#                                                        
#                  Count distribution                     
#                  Estimte parameters of a count distribution                           
#                                                        
#                  Author: E.A.J.Fischer                               
#                  Contact: e.a.j.fischer@uu.nl                             
#                  Creation date: 30-11-2021             
##########################################################

library(tidyverse)
library(bbmle)

#function to estimate the normal distribution of log-transformed count data
#uses maximum likelihood estimation
countdistribution.normal <- function(data){
  #create a function to optimize
  est.fun<- function(mu, sigma){
    #probability of uncensored data (variable censored = 0)
    a <- data%>%
      filter(censored == 0)%>%
      select(count)%>%
      unlist%>%
      dnorm(mean = mu, sd = sigma)%>%
      prod
  
    #probability of left censored data (variable censored = -1)
    b <- data%>%
      filter(censored == -1)%>%
      select(count)%>%
      unlist%>%
      pnorm(mean =  mu, sd = sigma,lower.tail = TRUE)%>%
      prod
    
    #probability of right censored data (variable censored = 1)
    c <- data%>%
      filter(censored == 1)%>%
      select(count)%>%
      unlist%>%
      pnorm(mean =  mu, sd =  sigma,lower.tail = FALSE)%>%
      prod
    
    #return the -loglikelihood
    return(-log(a*b*c))
  }
  #use mean and sd of uncensored as best fit
  init.mean = mean(data$count);
  init.sd = sd(data$count);
  #find optimal values of mu and sigma
  return(mle2(est.fun, 
              start = list(mu = init.mean, 
                           sigma = init.sd)
              ,method = "Nelder-Mead"
              ))
}

#truncated normal distribution to get test data
rtruncnorm <- function(n, a = -Inf,b = Inf, mean =0, sd =1){
  vec <- rnorm(n,mean,sd);
  vec[vec<=a]<- a
  vec[vec>=b]<- b
  return(vec)
}


#create some test data
mu = 8; sigma =2;a  = 6; b = 10; n = 100;

test.data <- data.frame(
  count = c(rtruncnorm(n, a,b, mu, sigma)),
  censored = 0
  
)
test.data$censored<- as.numeric(test.data$count<= a)
test.data$censored<- as.numeric(test.data$count>= b)

hist(test.data$count)
mean(test.data$count)
sd(test.data$count)
#find mu and sigma
x <- countdistribution.normal(test.data)x



