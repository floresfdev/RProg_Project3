best <- function(state, outcome) {
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
    
    lowestMortalityValue = numeric()
    if (outcome == "heart attack") {
        hospitalsInState <- 
            subset(outcomeData, 
                   State == state, 
                   select=c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))

        lowestMortalityValue <- min(hospitalsInState[, 2], na.rm = TRUE)
        
        bestHospitals <-
            subset(hospitalsInState, 
                   Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == lowestMortalityValue, 
                   select=c(Hospital.Name))
    } else if (outcome == "heart failure") {
        hospitalsInState <- 
            subset(outcomeData, 
                   State == state, 
                   select=c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))

        lowestMortalityValue <- min(hospitalsInState[, 2], na.rm = TRUE)
        
        bestHospitals <-
            subset(hospitalsInState, 
                   Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == lowestMortalityValue, 
                   select=c(Hospital.Name))
    } else if (outcome == "pneumonia") {
        hospitalsInState <- 
            subset(outcomeData, 
                   State == state, 
                   select=c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        
        lowestMortalityValue <- min(hospitalsInState[, 2], na.rm = TRUE)
        
        bestHospitals <-
            subset(hospitalsInState, 
                   Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == lowestMortalityValue, 
                   select=c(Hospital.Name))
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    ## bestHospitalsOrdered <- bestHospitals[order(Hospital.Name),]
    bestHospitals <- bestHospitals[,1]
    bestHospitalsOrdered <- bestHospitals[order(bestHospitals)]
    bestHospitalsOrdered[1]
}