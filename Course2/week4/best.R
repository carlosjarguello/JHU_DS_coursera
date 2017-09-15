best <- function(state, outcome_name) {
    #Read outcome data
    outcome <- read.csv("outcome-of-care-measures.csv", 
                        colClasses = "character")   
    #Check that state, outcome are valid
    possible_out <- c('heart attack' = 11, 'heart failure' = 17, 'pneumonia' = 23)
    if(!any(outcome$State == state))
        stop("invalid state")
    if(!any(names(possible_out) == outcome_name)) 
        stop("invalid outcome")
    filtered <- outcome[outcome$State == state,]
    filtered[order(as.numeric(filtered[,possible_out[outcome_name]]), decreasing = F),2][1]
}