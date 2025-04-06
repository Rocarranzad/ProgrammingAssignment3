rankall <- function(outcome, num = "best") {
        # Read outcome data.
        data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
        
        # Check that data is valid.
        col_index <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else if (outcome == "pneumonia") {
                23
        } else {
                stop(paste(outcome, "is not a valid outcome."))
                return(NULL)
        }
        data[, col_index] <- suppressWarnings(as.numeric(data[, col_index]))
        valid_data <- data[!is.na(data[, col_index]), ]
        
        hospital_list <- character(length(states))
        
        states <- sort(unique(data$State))
        for (i in seq_along(states)) {
                state <- states[i]
                state_data <- valid_data[valid_data$State == state,]
                ordered <- state_data[order(state_data[, col_index], state_data[, 2]), ]
                
                rank <- if (num == "best") {
                        1
                } else if (num == "worst") {
                        nrow(ordered)
                } else if (is.numeric(num)) {
                        if (num > nrow(ordered)) NA else num
                } else {
                        stop(paste(num, "is not valid"))
                }
                if (is.na(rank)) {
                        hospital_list[i] <- NA
                } else {
                        hospital_list[i] <- ordered[rank, 2]
                }
        }
        
        result <- data.frame(hospital = hospital_list, state = states)
        return(result)
}