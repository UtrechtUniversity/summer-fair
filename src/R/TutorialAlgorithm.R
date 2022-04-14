################################################
# Run the global algorithm over three data sets
################################################

#source the query function
source("Query.R")
source("DataManipulationRules.R")    #Data manipulation rules are pre- or user defined
source("LocalAlgorithm.R") #Estimation methods

#run local algorithms for data set####

#* Return local algorithm 
#* @get /algorithm
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
  
  print(weight_func())
  print(weight_func1())}

