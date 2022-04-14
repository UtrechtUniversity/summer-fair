################################################
# Run the global algorithm over three data sets
################################################

#source the query function
source("Query.R")
source("DataManipulationRules.R")    #Data manipulation rules are pre- or user defined
source("LocalAlgorithm.R")
source("GlobalAlgorithm.R")#Estimation methods
library(jsonlite)
#run local algorithms for data set####

#* Return local algorithm 
#* @get /algorithm
<<<<<<< HEAD
function(){#get data
dataA<- get.data("http://localhost:3030/datasetA/query")
print(dataA)

# var.id = ifelse(all(is.na(dataA$sample_measure)), c("sample_result"), c("sample_measure"))
# control = ifelse(any(grepl('0',dataA$treatment)),"0","")
# rule = ifelse(all(is.na(dataA$sample_measure)), rule.sinceany.recode, rule.sinceany.cutoff)
# 
# dataA$sample_measure = as.numeric(dataA$sample_measure)
#  #run analysis over each data set
# local <- analyseTransmission(inputdata = dataA,
#                               rule = rule,
#                               var.id = var.id,
#                               method = "glm",
#                               cutoff = 0,
#                               codesposnegmiss = c("+","-","NA"),
#                               preventError = TRUE, 
#                               covars = "treatment",
#                               reference = "control",
#                               control = control)
# 
# return(toJSON(step(local),force=TRUE))
}

#* Return the mean of a chickens per day
#* @param file_s:[file]
#* @post /weight
function(file_s){
  #
  # Works when I do not need to save the file in the directory
  for (file in file_s){
    temp_file<-tempfile() # create tempfile
    writeChar(rawToChar(file),temp_file,useBytes=FALSE) #write data to temp file
    # source(exprs = rawToChar(file))
    source(temp_file)
  }
=======
function(port){#get data
  url = sprintf("http://host.docker.internal:%s/mydataset/query", port)
  dataA<- get.data(url)
>>>>>>> d9797775b2eaa479f6c1cee1914a722f60604cf6
  
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