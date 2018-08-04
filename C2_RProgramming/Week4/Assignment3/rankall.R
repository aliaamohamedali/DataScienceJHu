rankall <- function(outcome, num = "best") {
  
  
  
  
  
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  data <- as.data.frame(cbind(data[, 2],      # hospital
                              data[, 7],      # state
                              data[, 11],     # heart attack
                              data[, 17],     # heart failure
                              data[, 23]),    # pneumonia
                        stringsAsFactors = FALSE)
  
  colnames(data) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  
  outcome <- tolower(outcome)
  
  ## Check that state and outcome are valid
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% outcomes) {
    stop('invalid outcome')
  }
  
  ## Divide dataframe according to states and construct List of outputs
  state <- levels(factor(data[, "State"]))
  hospital <- vector(mode="character")
  
  
  ## For each state, find the hospital of the given rank
  for(i in seq(state)){
    ##hospital[i]<- rankhospital(states[i], outcome, num)
    
    ## Return hospital name in that state with the given rank
    chosen <- which(data[,"State"] == state[i])
    data_slice <- data[chosen, ]
    data_slice[, eval(outcome)] <- as.numeric(data_slice[, eval(outcome)]) 
    
    ## 30-day death rate
    if (num == "worst"){
      rankings <- data_slice[order(data_slice[, eval(outcome)], data_slice[, "Hospital"], decreasing = TRUE), ]
      hospital[i] <- (rankings[,"Hospital"][1])

    }
    else{
      rankings <- data_slice[order(data_slice[, eval(outcome)], data_slice[, "Hospital"]), ]
      if (num == "best"){ 
        hospital[i] <- (rankings[, "Hospital"][1])
      }
      else{
        hospital[i] <- (rankings[,"Hospital"][num])
      }  
    }
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(hospital, state)
}