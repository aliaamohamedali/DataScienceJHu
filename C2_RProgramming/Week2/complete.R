complete <- function(directory, id =1:332){
  ## 'directory' is a character vector of length 1
  ## indicating location of .CSV files
  
  ## 'id' integer vector indicating monitor ID
  ## numbers to be used
  
  ## Return data frame of form:
  ## id nobs
  ## 1  117
  ## 2  1047
  ## ...
  ## where 'id' is monitor ID number and 'nobs'
  ## is number of complete cases
  
  dir <- paste("D:/MyCourses/DataScienceJohnsHopkins/C2_RProgramming/Week2", directory, sep="/")
  setwd(dir)
  nobs <- vector()
  for(i in id){
    if(i<10){
      filename <- paste("00", i, ".csv", sep="")
    }else if(i<100){
      filename <- paste("0", i, ".csv", sep="")
    }else{
      filename <- paste(i, ".csv", sep="")
    } 
    data <- read.csv(filename, header=TRUE)
    dates_set <- !is.na(data[ , 1])
    sulfate_set <- !is.na(data[ , 2])
    nitrate_set <- !is.na(data[ , 3])
    fullrows <- dates_set&sulfate_set&nitrate_set
    nobs <- c(nobs, nrow(data[fullrows,])) 
  }
  df <- as.data.frame(cbind(id, nobs)) 
  ## resultdfr <- as.data.frame(do.call("rbind", resultlis))
  df
}