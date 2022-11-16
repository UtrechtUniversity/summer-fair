test.data <- data.frame(s = sample.int(3,10,replace=T),
                        i = sample.int(3,10,replace=T))
test.data$n <- test.data$s+test.data$i +sample.int(2,10,replace=T)-1
test.data$cases <-test.data$s-sapply(test.data$s, sample.int, size =1)  
test.data$dt <- .5



data = test.data;
logl = function(beta1 = 1.){
  -sum((data$cases*log(1-exp(-(data$dt*beta1*data$i/data$n)))-
          (data$s-data$cases)*(data$dt*beta1*data$i/data$n)))}
bbmle::mle2(logl)

glmfit<- glm(as.formula("cbind(cases, s-cases) ~ 1"),
    family = binomial(link = "cloglog"), 
    offset = log(i/n)*dt,
    data = test.data
)

exp(glmfit$coefficients)
