corr <- function(directory, threshold=0){
  ## 'directory' character vector of length 1 indicating
  ## location of .CSV files
  
  ## 'threshold' numeric vector of length 1 indicating
  ## number of completely observed observations (on all
  ## variables) required to complete correlation between
  ## nitrate & sulfate; default is 0
  
  ## Return numeric vector of correlations
  ## NOTE: Do not round result
  dir <- paste("D:/MyCourses/DataScienceJohnsHopkins/C2_RProgramming/Week2", directory, sep="/")
  setwd(dir)
  complete_counts <- complete(directory)
  ids <- complete_counts[complete_counts[,2]>threshold, 1]
  corr <- numeric()
  
  for(id in ids){
    if(id<10){
      filename <- paste("00", id, ".csv", sep="")
    }else if(id<100){
      filename <- paste("0", id, ".csv", sep="")
    }else{
      filename <- paste(id, ".csv", sep="")
    }
    data <- read.csv(filename, header=TRUE)
    dates_set <- !is.na(data[ , 1])
    sulfate_set <- !is.na(data[ , 2])
    nitrate_set <- !is.na(data[ , 3])
    fullrows <- dates_set&sulfate_set&nitrate_set
    complete_rows <- data[fullrows, ]
    corr <- c(corr, cor(complete_rows[,2], complete_rows[,3]))
  }
  corr
}