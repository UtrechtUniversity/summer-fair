#S4 objects : to be implemented for local algorithms

## The output of this algorithm should be an S4 object ####
# define the S4 class for output ###
# setClass("TransmissionEstimate",
#          slots = c(n = "numeric", #number of observations
#                    likelihood ="function", #log-likelihood function with one or more parameters
#                    rule = "function", #rule used to determine if sample is positive or negative
#                    pars ="list" #list of estimated parameters options are of each the list contains the name and distribution
#                                 #R = reproduction number
#                                 #beta = transmission coefficient within a group
#                                 #betab = transmission coefficient between groups
#                                 #InfectiousPeriod = infectious period
#                                 #alpha = shape parameter distance dependent transmission
#                                 #r0 = scale parameter distance dependent transmission
#                          ))
# getClass("TransmissionEstimate")
# obj =new("TransmissionEstimate", 
#           n = 10, 
#           likelihood = function(R){R*(1-R)},
#           rule = rule.sincefirst,
#           pars = list("R"))

# methods 
setMethod("show",signature= c(object ="TransmissionEstimate"),
          function(object){
            print(object@n);
            print(object@rule);
            print(object@pars);
            ggplot(data =data.frame(x = seq(0,2,by = 0.1),
                                    y = sapply(X = seq(0,2,by = 0.1), 
                                               FUN =object@likelihood))) +
              geom_path(aes(x,y))+xlab(object@pars[1])+ylab("LL")
          }
)

