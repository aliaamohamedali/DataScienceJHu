rankhospital <- function(state, outcome, num = "best"){
  ## Argument state: 2-character abbreviated name of a
  ## state
  
  ## parameter outcome: The outcomes can be one 
  ## of "heart attack", "heart failure", or "pneumonia"
  
  ## Argument num: the ranking of a hospital in that 
  ## state for that outcome
  
  
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
  states <- data[, "State"]
  if (!state %in% states) {
    stop('invalid state')
  }
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% outcomes) {
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with the given rank
  chosen <- which(data[,"State"] == state)
  data <- data[chosen, ]
  data[, eval(outcome)] <- as.numeric(data[, eval(outcome)]) 
  
  ## 30-day death rate
  if (num == "worst"){
    rankings <- data[order(data[, eval(outcome)], data[, "Hospital"], decreasing = TRUE), ]
    return(rankings[,"Hospital"][1])
  }
  
  rankings <- data[order(data[, eval(outcome)], data[, "Hospital"]), ]
  
  if (num == "best"){
    ##rankings<- rankings[nrow(rankings):1,]  
    return(rankings[, "Hospital"][1])
  }
  return(rankings[,"Hospital"][num])
  
}