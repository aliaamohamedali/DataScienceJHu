best <- function(state, outcome){
  ## Parameter state: Two character
  ## abbreviation of the name of a state from the dataset
  
  ## parameter outcome: The outcomes can be one 
  ## of "heart attack", "heart failure", or "pneumonia"
  
  ## Reads the outcome-of-care-measures.csv file
  ## Returns a character vector with the name of the hospital that 
  ## has the best (i.e. lowest) 30-day mortality for the 
  ## specified outcomein that state. aka variable Hospital.Name
  ## Should there be a tie then outcomes are sorted in
  ## alphabetical order
  
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
  ##states <- unique(data[["State"]])
  states <- data[, "State"]
  if (!state %in% states) {
    stop('invalid state')
  }
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% outcomes) {
    stop('invalid outcome')
  }
  
  
  ## Return hospital name in that state with lowest 30-day death
  si <- which(data[,"State"] == state)
  data <- data[si, ]
  rate_vals <- as.numeric(data[, eval(outcome)])
    
  ## rate
  min_rate <- min(rate_vals,na.rm=TRUE)
  
  ## Hospital name
  result  <- data[, "Hospital"][which(rate_vals == min_rate)]
  output  <- result[order(result)]

  return(output[1])
}