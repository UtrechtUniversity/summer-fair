##############################################################################
#                                          
#                  Estimation methods
#                  Functions to estimate transmission parameters
#                                          
#                  Author: Egil A.J. Fischer                              
#                  Contact: e.a.j.fischer@uu.nl                             
#                  Creation date: Thu Oct 07 13:17:53 2021
###############################################################################

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
packages = c("tidyverse",
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


##Esimation procedures ####
#likelihood function three levels of transmission and frequency based on vector freq#
min.ll<-function(b1, deltat, 
                 cases,s1,i1,n1,
                  b2 = 0,i2=1,n2=1,
                  b3 = 0,i3=1,n3=1,freq= c(T,T,T)){
  foi1<- b1 * i1/ifelse(freq[1],n1,0);
  foi2<- b2 * i2/ifelse(freq[2],n2,0);
  foi3<- b3 * i3/ifelse(freq[3],n3,0);
  p<-(1-exp(-(foi1+foi2+foi3)*deltat))
  lprob <- dbinom(x=cases,size=s1,prob = p,log = TRUE) # 
  -sum(lprob)
}
sel.data <- data.arranged%>%filter(s>0&i>0)
test.ll<- min.ll(b1 = 1,
              sel.data$cases,
              sel.data$s,
              sel.data$i,
              sel.data$n,
              sel.data$dt)
fit<- mle2(minuslogl=min.ll,start=list(b1 =0.5), 
      data=list(level1=sel.data$level1, 
                level2=sel.data$level2,
                cases = sel.data$cases,
                s1 = sel.data$s,
                i1 = sel.data$i,
                n1 = sel.data$n,
                deltat=sel.data$dt),
      method="Nelder-Mead",
      control=list(maxit=50000,type=1,trace=1,reltol=10^(8)*.Machine$double.eps))

fit<- mle2(minuslogl=min.ll,start=list(b1 =0.5,b2 = 0.5), 
           data=list(level1=sel.data$level1, 
                     level2=sel.data$level2,
                     cases = sel.data$cases,
                     deltat=sel.data$dt,
                     s1 = sel.data$s,
                     i1 = sel.data$i,
                     n1 = sel.data$n,
                     i2 = sel.data$i2,
                     n2 = sel.data$n2
                     ),
           method="Nelder-Mead",
           control=list(maxit=50000,type=1,trace=1,reltol=10^(8)*.Machine$double.eps))

