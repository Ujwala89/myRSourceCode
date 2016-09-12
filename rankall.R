rankall <- function(outcome, num = "best") {  
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  
  
  ## Check that the outcome is valid
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
  
  if (outcome %in% names(outcomes)) {
    outcome_indx <- outcomes[outcome]
  } else {
    stop("invalid outcome")
  }
  
  #when outcome is best , it will represent the first row of the data frame
  if (num == "best") {
    num <- 1
  } 
  
  #set the columns to be extracted from the input dataset
  select_columns <- c(2,7,outcome_indx)
  
  ## Read outcome data for the selected columns
  df <- read.csv("outcome-of-care-measures.csv" , colClasses = "character") [,select_columns]
  
  #name the columns of the extracted data frame
  names(df) <- c("Hospital", "State", "outcome")
  
  #convert the outcome column (30 day death rate) as numeric
  df[, 3] <- as.numeric(df[, 3])
  #eliminate rows with NA in column 3
  df <- df[complete.cases(df[,3]),]
  
  
  #order the data by state, outcome (30 day death rate) & hospital name
  df_sorted <- df[order(df$State,df$outcome, df$Hospital),]
  
  #split the data by each state                                                         
  df_split <- split(df_sorted,df_sorted$State)
  
  #apply the hospitalNameFunction to each dataframe in the list
  r <- sapply(df_split,hospitalNameFunction,num)
  
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  data.frame(hospital=unlist(r), state=names(r), row.names=names(r))
  
}
