best <- function(state, outcome) {
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death ## rate
  
  
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
  
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,select_columns]
 
  names(df) <- c("Hospital", "State", "outcome")
  
  df[, 3] <- as.numeric(df[, 3])

  df <- df[complete.cases(df[,c(3)]),]
  
  #extract data for a given state
  df_state <- df[df$State == state,]
  
  #extract rows where the 30day death rate equals minimum for that state
  r1<- df_state[which(df_state$outcome == min(df_state$outcome, na.rm = TRUE)), ]
  
  #sort the output data by Hospital name
  r2<- r1[order(r1$Hospital),]
  
  #return the first row , this represents theminimum death rate and in order of hospital name
  r2[1,1]
}  
  
  
  