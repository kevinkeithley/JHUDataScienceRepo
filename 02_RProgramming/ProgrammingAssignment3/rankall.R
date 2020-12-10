## Function returns a data frame containing the hospitals
## of a chosen rank by state.

rankall <- function (outcome, num = "best") {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", 
                            colClasses="character",
                            na.strings="Not Available")
    outcomeData <- outcomeData[
        order(outcomeData$State),
    ]
    
    ## Create list containing the 3 types of outcomes with their associated 
    ## column numbers for the 30-day mortality rate 
    outcomes <- vector(mode="list", length=3)
    names(outcomes) <- c("heart attack", "heart failure", "pneumonia")
    outcomes[[1]] <- 11; outcomes[[2]] <- 17; outcomes[[3]] <- 23
    ## Create character vector of states/territories used in the dataset
    states <- unique(outcomeData$State)
    ## Convert the 3 outcome columns to numeric for sorting later
    for (i in c(11,17,23)) {
        outcomeData[, i] <- as.numeric(outcomeData[, i])
    }
    
    ## Check that outcome is valid
    if (!(outcome %in% names(outcomes))) {
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    df <- data.frame(matrix(ncol=2, nrow=length(states)))
    x <- c("hospital", "state")
    colnames(df) <- x
    for (i in 1:length(states)) {
        outcomeDataByState <- outcomeData[
            which(outcomeData$State==states[i]),
            ]
        ordered <- outcomeDataByState[
            order(outcomeDataByState[,outcomes[[outcome]]],    ## Order by relevant outcome column first, ASC
                  outcomeDataByState[,2],  ## Then order by hospital name, ASC
                  na.last=NA),
        ]
        listLength <- length(ordered[,1])
        if (num=="best") {
            rankNum <- 1
        } else if (num=="worst") {
            rankNum <- listLength
        } else {
            rankNum <- num
        }
        df$state[i] <- states[i]
        df$hospital[i] <- ordered[rankNum,]$Hospital.Name
    }
        
    ## Return a data frame with the hospital names and the 2-letter state abbr.
    df
}