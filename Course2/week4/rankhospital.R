rankhospital <- function(state, outcome_name, num = "best") {
    ## Read outcome data
    outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
    ## Check that state and outcome are valid
    possible_out <- c('heart attack' = 11, 'heart failure' = 17, 'pneumonia' = 23)
    if(!any(outcome$State == state))
        stop("invalid state")
    if(!any(names(possible_out) == outcome_name)) 
        stop("invalid outcome")
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    filtered <- outcome[outcome$State == state,]
    sorted <- filtered[order(as.numeric(filtered[,possible_out[outcome_name]]),
                                        filtered[,2],
                                        na.last = NA),2]
    if(num=='best') 
        out <- sorted[1]
    else 
        if (num=='worst')
            out <- sorted[length(sorted)]
        else 
            if (num > length(sorted))
                out <- NA
            else
                out <- sorted[num]
    out
}