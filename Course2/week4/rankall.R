rankall <- function(outcome_name, num = "best") {
    ## Read outcome data
    outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
    ## Check that state and outcome are valid
    possible_out <- c('heart attack' = 11, 'heart failure' = 17, 'pneumonia' = 23)
    if(!any(names(possible_out) == outcome_name)) 
        stop("invalid outcome")
    ## For each state, find the hospital of the given rank
    out_df <- data.frame(matrix(nrow = 0, ncol = 2))
    names(out_df) <- c("hospital", "state")
    
    for(st in sort(unique(outcome$State))) {
        filtered <- outcome[outcome$State == st,]
        sorted <- filtered[order(as.numeric(filtered[,possible_out[outcome_name]]),
                                 filtered[,2],
                                 na.last = NA),2]
        if(num=='best') 
            out <- sorted[1]
        else if (num=='worst')
            out <- sorted[length(sorted)]
        else if (num > length(sorted))
            out <- NA
        else
            out <- sorted[num]
        temp <- data.frame(out, st)
        names(temp) <- names(out_df)
        out_df <- rbind(out_df, temp)
    }
    rownames(out_df) <- out_df$state
    out_df
}
