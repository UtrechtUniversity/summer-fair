################################################
# Run the global algorithm over three data sets
################################################

#source the query function
source("Query.R")
source("DataInterpretationRules.R")    #Data manipulation rules are pre- or user defined
source("LocalAlgorithm.R")
source("GlobalAlgorithm.R")#Estimation methods
library(jsonlite)
#run local algorithms for data set####

#* Return local algorithm 
#* @get /algorithm


function(port){#get data
  url = sprintf("http://host.docker.internal:%s/mydataset/query", port)
  dataA<- get.data(url)

  var.id = ifelse(all(is.na(dataA$sample_measure)), c("sample_result"), c("sample_measure"))
  control = ifelse(any(grepl('0',dataA$treatment)),"0","")
  rule = ifelse(all(is.na(dataA$sample_measure)), rule.sinceany.recode, rule.sinceany.cutoff)
  
  dataA$sample_measure = as.numeric(dataA$sample_measure)
   #run analysis over each data set
  local <- analyseTransmission(inputdata = dataA,
                                rule = rule,
                                var.id = var.id,
                                method = "glm",
                                cutoff = 0,
                                codesposnegmiss = c("+","-","NA"),
                                preventError = TRUE,
                                covars = "treatment",
                                reference = "control",
                                control = control)

return(toJSON(step(local),force=TRUE))

}