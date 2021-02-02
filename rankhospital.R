rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fd   <- as.data.frame(cbind(data[, 2],   # hospital
                              data[, 7],   # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% fd[, "state"]){
    stop('invalid state')
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if(is.numeric(num)){
    si <- which(fd[, "state"] == state)
    ts <- fd[si, ]                     # extracting dataframe for the called state
    ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
    ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"]), ]
    output <- ts[, "hospital"][num]
    
  }else if(!is.numeric(num)){
    if(num == "best"){
      output <- best(state, outcome)
    } else if(num == "worst"){
      si <- which(fd[, "state"] == state)
      ts <- fd[si, ]    # extracting data for the called state
      oi <- as.numeric(ts[, eval(outcome)])
      max_val <- max(oi, na.rm = TRUE)
      result  <- ts[, "hospital"][which(oi == max_val)]
      output  <- result[order(result)]
    }
    else { 
      stop('invalid rank')
    }
  }
  return(output)
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}