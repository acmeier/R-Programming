rankall <- function(outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that outccome is valid
    validOutcomes <- data.frame(row.names = c("heart attack", "heart failure", "pneumonia"), c(11, 17, 23))
    if (!outcome %in% row.names(validOutcomes)) {
        stop("invalid outcome")
    }
    outcomeCol <- validOutcomes[outcome,]
    df[, outcomeCol] <- as.numeric(df[, outcomeCol])
    # Order by outcome and then hospital name
    df <- df[order(df[outcomeCol], df[2]),]
    ## For each state, find the hospital of the given rank
    result <- data.frame(hospital = character(), state = character())
    states <- sort(unique(df$State))
    for (state in states) {
        hospitals <- df[df$State == state & !is.na(df[outcomeCol]), 2]
        tnum <- num
        if (tnum == "best") {
            tnum <- 1
        } else if (tnum == "worst") {
            tnum <- length(hospitals)
        } else {
            tnum <- as.numeric(tnum)
        }
        hospitalName <- ifelse(tnum > length(hospitals), NA, hospitals[tnum])
        t <- data.frame(hospital = hospitalName, state = state)
        result <- rbind(result, t)
    }
    ## Return a data frame with the hospital names and the (abbreviated) state name
    result
}
