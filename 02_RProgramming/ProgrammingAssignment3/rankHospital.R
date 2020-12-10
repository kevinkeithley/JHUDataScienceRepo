## Function returns the name of the hospital with rank selected by user
## in a state for one of the following outcomes: heart attack,
## heart failure, and pneumonia


rankhospital <- function(state, outcome, num="best") {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", 
                            colClasses="character",
                            na.strings="Not Available")
    
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
    
    ## Check that state and outcome are valid
    if (!(state %in% states)) {
        stop("invalid state")
    }
    if (!(outcome %in% names(outcomes))) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    outcomeDataByState <- outcomeData[
        which(outcomeData$State==state),
        ]  ## Filter by state
    ordered <- outcomeDataByState[
        order(outcomeDataByState[,outcomes[[outcome]]],    ## Order by relevant outcome column first, ASC
              outcomeDataByState[,2],  ## Then order by hospital name, ASC
              na.last=NA),    ## Remove NAs                        
        ]

    listLength <- length(ordered[,1])
    ## Assign rank numbers for best,
    if (num=="best") {
        rankNum <- 1
    } else if (num=="worst") {
        rankNum <- listLength
    } else if (num > listLength) {
        return(NA)
    } else {
        rankNum <- num
    }
    ordered[rankNum,]$Hospital.Name    ## Return hospital name of chosen rank
}