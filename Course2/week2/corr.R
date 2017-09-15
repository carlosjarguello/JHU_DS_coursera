corr <- function(directory, threshold = 0) {
    files <- list.files(path = directory, full.names = TRUE)
    df_nobs <- complete(directory = directory, id = 1:332)
    ids <- df_nobs[df_nobs['nobs'] > threshold,'id']
    corr_vec <- numeric(length(ids))
    count <- 1
    for (id in ids) {
        temp <- read.csv(files[id])
        temp <- temp[complete.cases(temp),]
        corr_vec[count] <- cor(temp$sulfate, temp$nitrate)
        count <- count + 1
    }
    corr_vec
}