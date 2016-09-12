complete <- function(directory, id = 1:332) {
  #Arguments:
  #   directory is a character vector of length 1 indicating the location of the csv files
    #   id is a integer vector indicating the monitor ids to be used in calculation of mean
  
  #Return Value
  #   This function will return mean of the pollutant across all monitor ids
  
  # create a list of all the files in the directory
  files_full <- list.files(directory, full.name = TRUE)
  
  # Create empty data frame
  # dat  <- data.frame()
  dat2 <- data.frame()
  r1<-data.frame()
  k <- 0
  # for all the monitor ids passed in the argument, read csv files to create a Data frame
  for (i in id) {
    #########dat <- rbind(dat, read.csv(files_full[i]))
    k <- k + 1
    dat2 <- read.csv(files_full[i])
    dat2_complete <- dat2[complete.cases(dat2),]
    ##r1 <- rbind(r1,paste(i,nrow(dat2_complete),sep="  "))
    r1 <- rbind(r1,c(icomplete("specdata", 1), nrow(dat2_complete)))
  }
  
  #######dat_complete <- dat[complete.cases(dat)]
  
  # return mean of the selected data ignoring NA values
  r1
}