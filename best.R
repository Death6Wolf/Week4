#This function is for part 2 of the week 4 assignment of the R programming course by JHU on Coursera

#Programming Assignment 3 Data is needed and should be un packed before use. Working directory should be set manually.

#The function returns the best hospital in a specific state for a specific outcome. The output has been changed to make the feedback more responsive.
#Error feedback is enhanced with more details
#delete the hash marks to give the detailed response

#The outcomes are "heart attack", "heart failure", or "pneumonia"

best <- function(state, outcome) {
        ## Read outcome data
        outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        state <- toupper(state)
        outcome <- tolower(outcome)
        validoutcomes <- c('heart attack','heart failure','pneumonia')
        validstates <- names(table(outcomefile[,7]))
        
        
        if (outcome %in% validoutcomes) {
                if (state %in% validstates) {
                      #  print(paste('Checking for outcome ' ,outcome,' in ', state,sep = ''))
                        stateoutcome <- outcomefile[which(outcomefile[,7]==state,arr.ind = TRUE),]  
                } else stop(paste(c("Please provide one of the following valid states:",validstates), sep = " ",collapse = ","))
        } else stop(paste(c("Please provide one of the following valid outcomes",validoutcomes), sep = " ",collapse = ","))
        
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        case <- which(outcome==validoutcomes)
        x <- c(11,17,23)
        stateoutcome[,x[case]] <- as.numeric(stateoutcome[,x[case]])
        stateoutcome <- stateoutcome[complete.cases(stateoutcome[x(case)]),]
        rankedoutcome <- stateoutcome[order(stateoutcome[,x[case]],stateoutcome[,2]),]
        
        View(rankedoutcome)
        #print("#")
        #print(paste('The best hospital for ',outcome,"in " , state, " is:"))
        rankedoutcome[1,2]
        
        
}
