#########################################################
#                                                        
#                  Data Quality Check
#                  
#                                                        
#                  Author: Egil Fischer                               
#                  Contact:   e.a.j.fischer@uu.nl                           
#                  Creation date: 16-11-2022                         
#########################################################

report.DataQuality <-function(data){
  #data quality report
  data.quality <- list()
  #check if the data is not empty
  if(is.null(data))  {
    data.quality$data.presence<-"No data present";
    return(data.quality);
  }else {data.quality$data.presence <- "Contains data."}
  
  #give the names of the variables in the data set
  data.quality$var.names <- colnames(data);
  
  #report per variable the summary after setting known variable to be numeric
  data[,c("ex_day", "ex_hour","hour_after_inoc", "inoculationHour","sample_measure")]<-sapply(data[,c("ex_day", "ex_hour","hour_after_inoc", "inoculationHour","sample_measure")],as.numeric)
  data.quality$summary <- summary(data);
  
  return(data.quality)
}