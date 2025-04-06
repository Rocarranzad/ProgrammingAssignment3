best <- function(state, outcome) {
        # Read outcome data.
        csv <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
        
        # Check that state and outcome are valid.
        if (state %in% csv$State) {
                outcome_data <- subset(csv, State == state)
                
                if (outcome == "heart attack") {
                        col_index <- 11
                } else if (outcome == "heart failure") {
                        col_index <- 17
                } else if (outcome == "pneumonia") {
                        col_index <- 23
                } else {
                        stop(paste(outcome, "is an invalid outcome."))
                        return(NULL)
                }
                        
        
                if (!is.null(col_index)) {
                        outcome_data[, col_index] <- suppressWarnings(as.numeric(outcome_data[, col_index]))
                        valid_data <- outcome_data[!is.na(outcome_data[, col_index]), ]
                        
                        min_data <- min(valid_data[, col_index])
                        
                        best_hospitals <- valid_data[valid_data[, col_index] == min_data, ]
                        best_hospitals <- best_hospitals[order(best_hospitals[, 2]), ]
                        
                        # Return hospital name in that state with lowest 30-day death rate.
                        best_hosp <- best_hospitals[1, 2]
                        return(best_hosp)
                } else {
                        return(NULL)
                }
        } else {
                stop(paste(state, "is an invalid state."))
                return(NULL)
        }
}