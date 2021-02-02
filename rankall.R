rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fd   <- as.data.frame(cbind(data[, 2],   # hospital
                              data[, 7],   # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if(is.numeric(num)){
    by_state = with(fd, split(fd,state))
    ordered <- list()
    for(i in seq_along(by_state)){
      by_state[[i]] <- by_state[[i]][order(as.numeric(by_state[[i]][,eval(outcome)]), 
                                           by_state[[i]][,'hospital']),]
      
      ordered[[i]] <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
    }
    result <- do.call(rbind, ordered)
    output <- as.data.frame(result, row.names = result[,2], stringsAsFactors = FALSE)
    names(output) <- c("hospital", "state")
  } else if(!is.numeric(num)){
    if(num == "best"){
      by_state <- with(fd, split(fd, state))
      ordered  <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(as.numeric(by_state[[i]][, eval(outcome)]), 
                                             by_state[[i]][, "hospital"]), ]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, row.names = output[, 2], stringsAsFactors = FALSE)
      
      }else if(num == "worst"){
        by_state <- with(fd, split(fd, state))
        ordered  <- list()
        for (i in seq_along(by_state)){
          by_state[[i]] <- by_state[[i]][order(as.numeric(by_state[[i]][, eval(outcome)]), 
                                               by_state[[i]][, "hospital"],
                                               decreasing = TRUE), ]
          ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
        }
        result <- do.call(rbind, ordered)
        output <- as.data.frame(result, row.names = output[, 2], stringsAsFactors = FALSE)

    }else {
      stop("Invalid num")
    }
  }
  return(output)
}

outcome = "heart failure"





