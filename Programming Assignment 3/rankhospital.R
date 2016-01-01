rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!state %in% df$State) {
        stop("invalid state")
    }
    validOutcomes <- data.frame(row.names = c("heart attack", "heart failure", "pneumonia"), c(11, 17, 23))
    if (!outcome %in% row.names(validOutcomes)) {
        stop("invalid outcome")
    }
    outcomeCol <- validOutcomes[outcome,]
    df[, outcomeCol] <- as.numeric(df[, outcomeCol])
    ## Return hospital name in that state with the given rank 30-day death rate
    # Order by thee outcome column, then hospital name
    df <- df[order(df[outcomeCol], df[2]),]
    hospitals <- df[df$State == state & !is.na(df[outcomeCol]), 2]
    if (num == "best") {
        num <- 1
    } else if (num == "worst") {
        num <- length(hospitals)
    } else {
        num <- as.numeric(num)
        if (num > length(hospitals)) {
            return(NA)
        }
    }
    hospitals[num]
}