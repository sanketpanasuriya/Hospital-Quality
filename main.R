outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

source('best.R')
source('rankhospital.R')
best("TX", "heart attack")
best("SC", "heart attack")

rankhospital("MD", "heart attack", "worst")
rankhospital("TX", "heart attack",4)
rankhospital("TX", "heart failure", 4)

source('rankall.R')
head(rankall("heart attack", 4))
head(rankall("heart attack", 20), 10)

tail(rankall("heart failure"), 10)
head(rankall("heart attack", "worst"))
tail(rankall("pneumonia", "worst"), 3)

#Quiz 
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
