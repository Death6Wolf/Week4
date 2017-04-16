rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        state <- toupper(state)
        outcome <- tolower(outcome)
        
        validoutcomes <- c('heart attack','heart failure','pneumonia')
        validstates <- names(table(outcomefile[,7]))
        
        
        if (outcome %in% validoutcomes) {
                if (state %in% validstates) {
                        stateoutcome <- outcomefile[which(outcomefile[,7]==state,arr.ind = TRUE),]  
                } else stop(paste(c("Please provide one of the following valid states:",validstates), sep = " ",collapse = ","))
        } else stop(paste(c("Please provide one of the following valid outcomes",validoutcomes), sep = " ",collapse = ","))
        
       
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        case <- which(outcome==validoutcomes)
        x <- c(11,17,23)
        stateoutcome[,x[case]] <- as.numeric(stateoutcome[,x[case]])
        stateoutcome <- stateoutcome[complete.cases(stateoutcome[x[case]]),]
        rankedoutcome <- stateoutcome[order(stateoutcome[,x[case]],stateoutcome[,2]),]
        
        # Setup num
        
        if (num=="best") {
                num <- 1
        }else if (num =="worst") {
                num <- nrow(rankedoutcome)
        }
        
        View(rankedoutcome)

        rankedoutcome[num,2]
}
