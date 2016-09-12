complete <- function(directory, id = 1:332) {
  
    #   directory is a character vector of length 1 indicating the location of the csv files
    #   id is a integer vector indicating the monitor ids to be used in calculation of mean
  
    #   Return Value:  This function will return number of complete observations all input monitor ids
  
  # create a list of all the files in the directory
  files_full <- list.files(directory, full.name = TRUE)
  
  # Create empty data frame with column heading
 
  result <- data.frame(id=numeric(0), nobs=numeric(0))
                                                        
  
  # for all the monitor ids passed in the argument, read csv files to create a Data frame
  for (i in id) {
    
    dat <- read.csv(files_full[i])                                 ## read csv file 
    dat_complete <- dat[complete.cases(dat),]                      ## remove NA values
    result[nrow(result)+1,] <- c(i, nrow(dat_complete))
                                                   
  }
  # Return Result
  result
  
}