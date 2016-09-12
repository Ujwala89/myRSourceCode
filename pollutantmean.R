pollutantmean <- function(directory, pollutant, id = 1:332) {
  #Arguments:
  #   directory is a character vector of length 1 indicating the location of the csv files
  #   pollutant is a character vector of length 1 indicating the polutant for which we will
  #             calculate the mean. The value willbe sulfate or nitrate
  #   id is a integer vector indicating the monitor ids to be used in calculation of mean
  #Return Value
  #   This function will return mean of the pollutant across all monitor ids
  
  # create a list of all the files in the directory
  files_full <- list.files(directory, full.name = TRUE)
  
  # Create empty data frame
  dat <- data.frame()
  
  # for all the monitor ids passed in the argument, read csv files to create a Data frame
  for (i in id) {
    dat <- rbind(dat, read.csv(files_full[i]))
  }
  
  # return mean of the selected data ignoring NA values
  mean(dat[, pollutant], na.rm = TRUE)
}