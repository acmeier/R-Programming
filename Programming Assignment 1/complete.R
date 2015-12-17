complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    completeCases <- data.frame()
    for (i in id) {
        filename <- paste(sprintf("%03d", i), "csv", sep = ".")
        data <- read.csv(paste(directory, filename, sep = "/"))
        complete <- data[complete.cases(data),]
        completeCases <- rbind(completeCases, c(i, nrow(complete)))
    }
    colnames(completeCases) <- c("id", "nobs")
    completeCases
}