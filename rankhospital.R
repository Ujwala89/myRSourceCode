rankhospital <- function(state, outcome, num = "best") { 
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank 
  ## 30-day death rate
  
  #validate state
  
  if (state %in% state.abb) {}
  else{
    stop("invalid state")
  } 
  
  #validate outcome
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
  
  if (outcome %in% names(outcomes)) {
    outcome_indx <- outcomes[outcome]
  } else {
    stop("invalid outcome")
  }
  
  #construct vector for selecting columns & read the csv file & eliminate NAs
  select_columns <- c(2,7,outcome_indx)
  
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv" , colClasses = "character") [,select_columns]
 
  #name the columns of the data frame
  names(df) <- c("Hospital", "State", "outcome")
  
  #convert the outcome column (30 day death rate) as numeric
  df[, 3] <- as.numeric(df[, 3])
  #eliminate rows with NA in column 3
  df <- df[complete.cases(df[,3]),]
  
  #select the data for a given state
  df_state <- df[df$State == state,]
  
  #order the data by outcome (30 day death rate) & hospital name
  df_state <- df_state[order(df_state$outcome, df_state$Hospital),]
  
  #set the value of the num variable
   if (num == "best") {
      num <- 1
   } else if (num == "worst") {
      num <- nrow(df_state)
   }
   
  ## Return hospital name in that state with the given rank ## 30-day death rate
   df_state[num, 1]
}
