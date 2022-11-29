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


outlier_count <- function(data) {
  data$sample_measure <-as.numeric(data$sample_measure)
  Q1 <- quantile(data$sample_measure, 0.25)
  Q3 <- quantile(data$sample_measure, 0.75)
  lower.bound.mild <- Q1 - 1.5*IQR(data$sample_measure)
  upper.bound.mild <- Q3 + 1.5*IQR(data$sample_measure)
  lower.bound.extreme <- Q1 - 3*IQR(data$sample_measure)
  upper.bound.extreme <- Q3 + 3*IQR(data$sample_measure)
  
  extreme.outliers <- sum(data$sample_measure < lower.bound.extreme | data$sample_measure > upper.bound.extreme)
  extreme.outliers.perc <- round(extreme.outliers/length(data$sample_measure)*100, digits = 2) 
  
  mild.outliers <- sum(data$sample_measure < lower.bound.mild | data$sample_measure > upper.bound.mild )
 
  
  mild.outliers <-mild.outliers - extreme.outliers
  mild.outliers.perc <- round(mild.outliers/length(data$sample_measure)*100, digits = 2)
  

  outliers.sample.measure <- c(paste0(mild.outliers, 
                                      ' (', 
                                      mild.outliers.perc, 
                                      ' %) mild outliers(1.5x IQR)'),
                               paste0(extreme.outliers, 
                                      ' (', 
                                      extreme.outliers.perc , 
                                      ' %) extreme outliers(3x IQR)'))
  
  return(outliers.sample.measure)
}



missing_times <- function(data){
  ## TODO: what makes sense to have here
  dataHost_days <- data %>% filter(!is.na(sample_measure) | !is.na(sample_result))  %>%  group_by(host_id) %>% summarise(ex_day = list(sort(as.numeric(ex_day))))
  days <- unique(sort(as.numeric(data$ex_day)))
  
  dataHost_days$differences <-  0
  dataHost_days$count <- 0
  for(i in 1:nrow(dataHost_days)) {
    
    row <- dataHost_days[i,]
    row_days <-as.numeric(unlist(row$ex_day))
    
    diffs <- setdiff(as.vector(days), as.vector(row_days))
    dataHost_days$differences[i] <- list(diffs)
    dataHost_days$count[i] <- length(diffs)
  }
  
  
  splited<-split(seq(1,length(days)), rep(1:ceiling(length(seq(1,length(days)))/4), each=4)[1:length(seq(1,length(days)))])
  
  
  for(i in 1:length(splited)){
    
    counts <- length(which(dataHost_days$count %in% unlist(splited[i])))
    print(splited[i])
    print(counts)
    

    
    }
  
 
}


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
  data.quality$outliers.sample.measure <- outlier_count(data)
   
  return(data.quality)
}




