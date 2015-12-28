rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
    
    ## Check that state and outcome are valid
    factorStates <- factor(outcomeData[, 7])
    validStates <- attr(factorStates, "levels")
    indexState <- which(validStates %in% state)
    if (length(indexState) == 0) {
        stop("invalid state")
    }
    
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    indexOutcome <- which(validOutcomes %in% outcome)
    if (length(indexOutcome) == 0) {
        stop("invalid outcome")
    }

    ## Analyze the data
    ## 30-day mortality rates:
    ## - Heart attach: column 11
    ## - Heart failure: column 17
    ## - Pneumonia: column 23
    outcomeData[, 11] <- as.numeric(outcomeData[, 11])
    outcomeData[, 17] <- as.numeric(outcomeData[, 17])
    outcomeData[, 23] <- as.numeric(outcomeData[, 23])
    
    mortalityValue = numeric()
    if (outcome == "heart attack") {
        hospitalsInState <- 
            subset(outcomeData, 
                   State == state & !is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                   select=c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        
        rankingHospitals <-
            hospitalsInState[order(
                hospitalsInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
                hospitalsInState$Hospital.Name),]
        
        rankingHospitals <-
            data.frame(
                Hospital.Name = rankingHospitals$Hospital.Name, 
                Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack = 
                    rankingHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
                rank = 1:nrow(rankingHospitals))
    } else if (outcome == "heart failure") {
        hospitalsInState <- 
            subset(outcomeData, 
                   State == state & !is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                   select=c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        
        rankingHospitals <-
            hospitalsInState[order(
                hospitalsInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
                hospitalsInState$Hospital.Name),]
        
        rankingHospitals <-
            data.frame(
                Hospital.Name = rankingHospitals$Hospital.Name, 
                Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = 
                    rankingHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
                rank = 1:nrow(rankingHospitals))
    } else if (outcome == "pneumonia") {
        hospitalsInState <- 
            subset(outcomeData, 
                   State == state & !is.na(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),
                   select=c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        
        rankingHospitals <-
            hospitalsInState[order(
                hospitalsInState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, 
                hospitalsInState$Hospital.Name),]
        
        rankingHospitals <-
            data.frame(
                Hospital.Name = rankingHospitals$Hospital.Name, 
                Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = 
                    rankingHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, 
                rank = 1:nrow(rankingHospitals))
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    rankToSelect <- num
    if (num == "best") {
        rankToSelect <- 1
    } else if (num == "worst") {
        rankToSelect <- nrow(rankingHospitals)
    }
    
    rankSelected <- 
        subset(rankingHospitals, rank == rankToSelect, select=c(Hospital.Name))
    if (nrow(rankSelected) == 0) {
        NA
    } else {
        as.vector(rankSelected[1, 1])
    }
}