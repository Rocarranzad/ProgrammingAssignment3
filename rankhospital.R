rankhospital <- function(state, outcome, num = "best") {
        # Read outcome data.
        data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
        
        # Check that state and outcome are valid. 
        if (state %in% data$State) {
                data_state <- subset(data, State == state)
        } else {
                stop(paste(state, "is an invalid state."))
                return(NULL)
        }
        
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
                data_state [, col_index] <- suppressWarnings(as.numeric(data_state[, col_index]))
                valid_data <- data_state[!is.na(data_state[, col_index]), ]
                order_data <- valid_data[order(valid_data[, col_index], valid_data[, 2]), ]
        } else {
                return(NULL)
        }
                
        rank <- if (num == "best") {
                1
        } else if (num == "worst") {
                nrow(order_data)
        } else if (is.numeric(num)) {
                if (num > nrow(order_data)) {
                        return(NA)
                } else {
                        num
                }
        } else {
                stop(paste(rank, "is an invalid rank"))
        }
        
        # Return hospital name in that state with the given rank 30-day death rate. 
        return(order_data[rank, 2])
}