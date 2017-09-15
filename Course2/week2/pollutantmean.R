pollutantmean <- function(directory, pollutant, id = 1:332) {
        y <- data.frame() 
        file_list = list.files(path = directory, full.names = TRUE)
        for (x in id) { 
                y <- rbind(y,read.csv(file_list[x]))
        }
        mean(y[,pollutant], na.rm = TRUE)
}