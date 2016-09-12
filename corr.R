corr <- function(directory, threshold = 0)  {
  
    #   directory is a character vector of length 1 indicating the location of the csv files
    #   threshold is a numeric vector of length 1 indicating number of completely observed observations (on all variables) 
    #   required to compute the correlation between nitrate & sulphate ; default value is 0
  
    #   Return Value :   A numeric vector of correltions
  
  # create a list of all the files in the directory
  files_full <- list.files(directory, full.name = TRUE)
  
  cr <- numeric()                                          # initialize result vector
  
  for (i in seq_along(files_full)) {
    dat <- read.csv(files_full[i])                         # read each csv file
    dat_complete <- dat[complete.cases(dat),]              # remove NAs
    nobs <- nrow(dat_complete)
    
    if (nobs > threshold) {
       cr1 <- cor(dat_complete$sulfate, dat_complete$nitrate)       # find correlation between sulphate & nitrate for the dataframe
       cr <- c(cr,cr1)                                              # Append correlation result to the vector
    }
  }
  cr
}
  
  
  
  
                                                        

