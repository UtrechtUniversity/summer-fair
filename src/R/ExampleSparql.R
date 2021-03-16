# Title     : TODO
# Objective : TODO
# Created by: elena.slavco
# Created on: 2021-03-16

source("src/R/SPARQLqueries.R")

# get dates of swab using SPARQL function get.dates.swab()
# and convert it to numeric vector
# time intervals can be calculated as deltat.days1 in ExampleCode.R
dates.swab <- sort(as.double(get.dates.swab()$date))

# rewriting the status.function from ExampleCode.R
# create the data about the final status of swab sample
# positive only if the previous sample is positive as well
# Takes some time as it is creating new triples for each exp date

for(index in c(1:(length(dates.swab)-1))){
  current.date <- dates.swab[index]
  next.date <- dates.swab[index+1]
  cat(paste("Create the triples for date %s"),current.date)
  create.sample.value(current.date,next.date)

}
# run SPARQL query to aggregate the data
# return the table similar to aggregate.data1 in ExampleCode.R
aggregated.data<-aggregate.data()

# Example of the sparql data format not directly related to the algorithm
swab.data <- swab.data()

chicken.data <- chicken.info()