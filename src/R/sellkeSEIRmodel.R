
packages <- c("deSolve","dplyer")

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



##parameters##
animals <- 100
beta <- 0.2 #transmission coefficient
dL <- function(U){return(-log(U)/0.5)} #Duration latency period as function of a random variable U
dI <- function(U){return(-log(U)/0.1)} #Duration infectious period as function of a random variable U
L0 <- 1
I0 <- 0
runs <- 5
#initialize

output <- data.frame(time = 0, N = animals, S = animals - 1 , L =1, I =0, R=0, D = 0, C=0, Th =0, run = 0)
state <- output
#
while(state$run < runs)
{
  QIRtimes<- data.frame(Q = sort(rexp(animals - 1,1)),
                        E = sapply(FUN = dL, X = runif(animals-1)), 
                        I = sapply(FUN = dI, X = runif(animals-1)))
state <- data.frame(time = 0, N = animals, S = animals - 1 , L =1, I =0, R=0, D = 0, C=0, Th =0, run = last(state$run) + 1)
events <-data.frame(type = c("LI","IR"), time = c(dL(runif(1)),dI(runif(1))))
time <- 0
foi <- 0
cumInf <- 0
cLI <- 0
cIR <-0
cSL <-0

handbreak =0
while(length(events$time) > 0 & state$L + state$I > 0)
{
  handbreak = handbreak + 1
  if(handbreak > animals){stop}
  #process the first event in the list
  #update the cummulative infection pressure
  cumInf <- cumInf + foi * (first(events$time) - state$time)
  #set time to current time
  state$time <- first(events$time)
  #determine the next event
  if(first(events$type) == "LI" ){
    cLI <- cLI + 1
    #add one to I type 
    state$I <- state$I + 1
    #subtract one from L type
    state$L <- state$L - 1
  }
  if(first(events$type) == "IR" )
  {
    cIR <- cIR + 1
    #add one to R
    state$R <- state$R + 1
    #subtract one from I
    state$I <- state$I - 1
   
  }
  if(first(events$type) == "SL" )
  {
    cSL <- cSL + 1
    #add one to L
    state$L <- state$L + 1
    #subtract one from S
    state$S <- state$S - 1
    #set transitions
    events<- rbind(events, data.frame(time =c(state$time + QIRtimes[1,]$E), type = c("LI"))) #L -> I
    events<- rbind(events, data.frame(time =c(state$time + QIRtimes[1,]$E + QIRtimes[1,]$I), type = c("IR"))) #I -> R
    
    #remove lowest resistance
    QIRtimes <-QIRtimes[-1,] 
   
  } 
  
  #set force-of-infection
  foi <- beta * state$I /state$N
  
  #order events by time of execution
  events <- events[order(events$time),]

  
  #determine next infection event
  if(state$S * state$I > 0){
    infection <- state$time + (first(QIRtimes$Q) - cumInf)/foi
  } else {infection <- 10^10}
  
  #remove first event
  events <- events[-1,]
  
  #if this event is previous to other events schedule it
  if(length(events$time) > 0)
  {
    if(infection < first(events$time))
    {
      events <- rbind(data.frame(time = c(infection),type = "SL"),events)
      #order events by time of execution
      events <- events[order(events$time),]
    }
  }

  
  #record this moment
  output <- rbind(output, state)
}
}
##plot the output
ggplot(data = output)+
  geom_point(aes(x = time, y = L), color = "blue")+
  geom_point(aes(x = time, y = I), color = "red")+
  geom_point(aes(x = time, y = R), color = "black")
output
