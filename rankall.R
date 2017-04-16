#This function is for part 4 of the week 4 assignment of the R programming course by JHU on Coursera

#Programming Assignment 3 Data is needed and should be un packed before use. Working directory should be set manually.

#The function returns the hospital with a specified rank in all states for a specified outcome. The output has been changed to make the feedback more responsive.
#Error feedback is enhanced with more details
#delete the hash marks to give the detailed response

#The outcomes are "heart attack", "heart failure", or "pneumonia"

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        stateoutcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        outcome <- tolower(outcome)
        
        validoutcomes <- c('heart attack','heart failure','pneumonia')

        if (outcome %in% validoutcomes) {
                
        } else stop(paste(c("Please provide one of the following valid outcomes",validoutcomes), sep = " ",collapse = ","))
        
        
        ## For each state, find the hospital of the given rank
        
        case <- which(outcome==validoutcomes)
        x <- c(11,17,23)
        stateoutcome[,x[case]] <- as.numeric(stateoutcome[,x[case]])
        stateoutcome <- stateoutcome[complete.cases(stateoutcome[x[case]]),c(2,7,x[case])]
        stateoutcome <- stateoutcome[order(stateoutcome[,3],stateoutcome[,1]),]
        # Setup num
        
        

        if (num=="best") {
                num <- 1
        }
        
        
        
        splitoutcome <- sapply(split(stateoutcome[,1:3],stateoutcome[,2]),function(y){y[num,]})
        
        splitoutcome <- t(splitoutcome)
        #names(t) <- c("hospital","state")
        cbind.data.frame(splitoutcome)
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}
