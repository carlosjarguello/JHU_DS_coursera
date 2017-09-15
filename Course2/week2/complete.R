complete <- function(directory, id = 1:332) {
    files <- list.files(path = directory, full.names = TRUE)
    df_complete <- data.frame()
    for(i in id) {
        nobs <- sum(complete.cases(read.csv(files[i])))    
        df_complete <- rbind(df_complete, c(i, nobs))
    }
    colnames(df_complete) <- c('id', 'nobs')
    df_complete
}