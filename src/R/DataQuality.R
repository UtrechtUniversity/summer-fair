#########################################################
#                                                        
#                  Data Quality Check
#                  
#                                                        
#                  Author: Egil Fischer                               
#                  Contact:   e.a.j.fischer@uu.nl                           
#                  Creation date: 16-11-2022                         
#########################################################
library(dplyr)
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
  #data.quality$IQR <- IQR(data$sample_measure)
  data.quality$UnitOfMeasurement <- unique(data$unit_measure)
  data.quality$QuantityFunction <- unique(data$quantity_function)
   
  return(data.quality)
}


outlier_count <- function(var, factor) {
  var <- as.numeric(var)
  Q1 <- quantile(var, 0.25)
  Q3 <- quantile(var, 0.75)
  
  lower_bound <- Q1 - factor*IQR(var)
  upper_bound <- Q3 + factor*IQR(var)
  outliers <- sum(var < lower_bound | var > upper_bound)
  perc <- round(outliers/length(var), digits = 2)
  print(paste0(outliers, ' outliers (', perc, ' %) outside of ',
               factor, ' x IQR'))
}

missing_times <- function(data){
  ## TODO: what makes sense to have here
  dataHost_days <- data %>% filter(!is.na(sample_measure) | !is.na(sample_result))  %>%  group_by(host_id) %>% summarise(ex_day = list(sort(as.numeric(ex_day))))
  days <- unique(sort(as.numeric(data$ex_day)))
  
  
  for(i in 1:nrow(dataHost_days)) {
    row <- dataHost_days[i,]
    row_days <-as.numeric(unlist(row$ex_day))
    row$diffs<-setdiff(days,row_days)
    
  }
  }

  

