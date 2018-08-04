pollutantmean <- function(directory, pollutant, id = 1:332){
  ## 'directory' character vector of length 1 indicating
  ## location of .CSV files
  
  ## 'pollutant' character vector of length 1 indicating
  ## name of pollutant for which to calculate mean
  ## Either sulphate or Nitrate
  
  ## 'id' integer vector indicating monitor ID members to
  ## to be used
  
  ## Returns mean of pollutant across all monitors list in
  ## 'id' vector (idnoring NA values)
  ## Note: Don't round result
  dir <- paste("D:/MyCourses/DataScienceJohnsHopkins/C2_RProgramming/Week2", directory, sep="/")
  setwd(dir)
  sum <- 0.0
  count <- 0
  for (i in id){
    if(i<10){
      filename <- paste("00", i, ".csv", sep="")
    }else if(i<100){
      filename <- paste("0", i, ".csv", sep="")
    }else{
      filename <- paste(i, ".csv", sep="")
    } 
    data <- read.csv(filename, header=TRUE)
    x <- data[,grep(pollutant, colnames(data))]
    sum <- sum + sum(x, na.rm=TRUE)
    count <- count + length(x[!is.na(x)])
  }
  avg <- sum/count
  print(avg)
  
}